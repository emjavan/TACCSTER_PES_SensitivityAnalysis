# Estimate state-specific proportion of each age group at high risk of severe influenza complications

# BRFSS data: https://www.cdc.gov/brfss/annual_data/annual_data.htm
# Code Book:  HTML saved as PDF in data/metadata/BRFSS_LLCP_2023_CodebookReport.pdf

# NSCH data: https://www.census.gov/programs-surveys/nsch/data/datasets.2023.html
# Code Book: https://www.census.gov/data-tools/demo/uccb/nschdict

# Flu high risk: https://www.cdc.gov/flu/highrisk/index.htm
# Children high risk: https://www.cdc.gov/flu/highrisk/neurologic-pediatric.html
# Including flu shot survey results for scenario modeling

#///////////////////
#### BRFSS VARS ####
#' _STATE   = state fips
#' _STSTR   = strata
#' _LLCPWT  = final weight assigned to each respondent
#' _PSU     = primary sampling unit, ID
#' _AGEG5YR = age reported in 5 year age categories
#' FLUSHOT7 = Adult flu shot/spray in last 12 months
#' ASTHNOW  = current asthma
#' CSRVTRT3 = current cancer treatment
#' CHCCOPD3 = ever had COPD
#' CHCKDNY2 = ever had chronic kidney disease
#' CVDCRHD4 = ever had heart disease
#' _RFBMI5  = BIM obese or morbidly obese
#' SMOKDAY2 = current smoking
#' DIABTYPE = the type of diabetes someone has
#' PREGNANT = current pregnant
#' CVDSTRK3 = ever had a stroke

#///////////////////
#### NSCH VARS ####
#' FIPSST        = state fips
#' STRATUM       = strata
#' FWC           = final weight assigned to each respondent
#' HHID          = ID
#' SC_AGE_YEARS  = age
#' CYSTFIB       = ever cystic fibrosis
#' HEART_CURR    = current heart condition
#' BMICLASS      = BMI percentile
#' DIABETES_CURR = current diabetes 
#' K2Q40B        = current asthma
#' BLOOD         = ever blood disorder
#' K2Q61A        = ever cerebral palsy
#' K2Q42B        = current epilepsy
#' AUTOIMMUNE    = ever autoimmune disorder
#' K2Q60B        = current intellectual disability

#//////////////////
#### LIBRARIES ####
# Load libraries
library(tidyverse)
library(srvyr)

#///////////////////
#### BRFSS DATA ####
# Read .XPT file using haven
# Kentucky (21) and Pennsylvania (42) did not collect enough data for 2023 survey so using 2022
brfss_2022 = haven::read_xpt("../data/BRFSS/LLCP2022.XPT") %>% # .XPT is the SAS transport file
  filter(`_STATE` %in% c(21, 42))
brfss = haven::read_xpt("../data/BRFSS/LLCP2023.XPT") %>% # .XPT is the SAS transport file
  bind_rows(brfss_2022)

#//////////////////
#### NSCH DATA ####
nsch = haven::read_sas("../data/NSCH/nsch_2023e_topical.sas7bdat")  

#////////////////////////
#### BRFSS HIGH RISK ####
brfss_high_risk_df <- brfss %>%
  mutate(`_STATE`= str_pad(as.character(`_STATE`), 2, "left", "0")) %>%
  transmute(
    STATE_FIPS   = `_STATE`,
    AGE_5YR_CAT  = `_AGEG5YR`,
    weight       = `_LLCPWT`,
    PSU          = `_PSU`,
    STRATA       = `_STSTR`,
    # strata unique within states and not between so need to add prefix
    STRATA_STATE = interaction(STATE_FIPS, STRATA, drop = TRUE),
    age_group = case_when(
      AGE_5YR_CAT %in% c(1, 2, 3, 4, 5, 6)   ~ "18-49",
      AGE_5YR_CAT %in% c(7, 8, 9)            ~ "50-64",
      AGE_5YR_CAT %in% c(10, 11, 12, 13, 14) ~ "65+",
      AGE_5YR_CAT == 14 ~ NA_character_,  # Don't know / Refused / Missing
      TRUE ~ NA_character_
    ),
    flu_shot = case_when(
      FLUSHOT7 == 1 ~ 1, 
      FLUSHOT7 == 2 ~ 0,
      FLUSHOT7 %in% c(7,9) ~ NA_integer_,
      TRUE ~ NA_integer_),
    asthma = dplyr::case_when(
      ASTHNOW == 1 ~ 1,
      ASTHNOW == 2 ~ 0,
      ASTHNOW %in% c(7, 9) ~ NA_integer_,
      TRUE ~ NA_integer_),
    cancer = dplyr::case_when(
      CSRVTRT3 %in% c(1, 3, 4) ~ 1,
      CSRVTRT3 %in% c(2, 5)    ~ 0,
      CSRVTRT3 %in% c(7, 9) ~ NA_integer_,
      TRUE ~ NA_integer_),
    copd = dplyr::case_when(
      CHCCOPD3 == 1 ~ 1,
      CHCCOPD3 == 2 ~ 0,
      CHCCOPD3 %in% c(7, 9) ~ NA_integer_,
      TRUE ~ NA_integer_),
    ckd = dplyr::case_when(
      CHCKDNY2 == 1 ~ 1,
      CHCKDNY2 == 2 ~ 0,
      CHCKDNY2 %in% c(7, 9) ~ NA_integer_,
      TRUE ~ NA_integer_),
    heart = dplyr::case_when(
      CVDCRHD4 == 1 ~ 1,
      CVDCRHD4 == 2 ~ 0,
      CVDCRHD4 %in% c(7, 9) ~ NA_integer_,
      TRUE ~ NA_integer_),
    obese = dplyr::case_when(
      `_RFBMI5` == 1 ~ 1,
      `_RFBMI5` == 2 ~ 0,
      `_RFBMI5` == 9 ~ NA_integer_,
      TRUE ~ NA_integer_),
    smoke = dplyr::case_when(
      SMOKDAY2 %in% c(1, 2) ~ 1,
      SMOKDAY2 == 3 ~ 0,
      SMOKDAY2 %in% c(7, 9) ~ NA_integer_,
      TRUE ~ NA_integer_),
    diab = dplyr::case_when(
      DIABTYPE %in% c(1, 2) ~ 1,
      SMOKDAY2 %in% c(7, 9) ~ NA_integer_,
      TRUE ~ NA_integer_),
    preg = dplyr::case_when(
      PREGNANT == 1 ~ 1,
      PREGNANT == 2 ~ 0,
      PREGNANT %in% c(7, 9) ~ NA_integer_,
      TRUE ~ NA_integer_),
    stroke = dplyr::case_when(
      CVDSTRK3 == 1 ~ 1,
      CVDSTRK3 == 2 ~ 0,
      CVDSTRK3 %in% c(7, 9) ~ NA_integer_,
      TRUE ~ NA_integer_)
  ) %>%
  mutate(
    any_known = if_any(c(asthma, cancer, copd, ckd, heart,
                         obese, smoke, diab, preg, stroke), ~ !is.na(.x)),
    any_yes   = if_any(c(asthma, cancer, copd, ckd, heart,
                         obese, smoke, diab, preg, stroke), ~ .x == 1L),
    high_risk = case_when(
      any_yes ~ 1,
      !any_known ~ NA_integer_,   # all comorbidities responses missing
      TRUE ~ 0                    # at least one known and none are "yes"
    )
  ) %>%
  filter(!(STATE_FIPS %in% c("66", "78", "72") )) # remove Guam, Puerto Rico, Virgin Islands if present

#///////////////////////
#### NSCH HIGH RISK ####
nsch_high_risk_df <- nsch %>%
  mutate(FIPSST  = str_pad(as.character(FIPSST), 2, "left", "0")) %>%
  transmute(
    STATE_FIPS   = FIPSST,
    weight       = FWC,
    PSU          = HHID,
    STRATA       = STRATUM,
    STRATA_STATE = interaction(STATE_FIPS, STRATA, drop = TRUE),
    age_group = case_when(
      SC_AGE_YEARS %in% c(0, 1, 2, 3, 4)      ~ "0-4",
      SC_AGE_YEARS %in% c( 5,  6,  7,  8,  9,
                          10, 11, 12, 13, 14,
                          15, 16, 17)         ~ "5-17",
      TRUE ~ NA_character_
    ),
    asthma = dplyr::case_when(
      K2Q40B == 1 ~ 1,
      K2Q40B == 2 ~ 0,
      TRUE ~ NA_integer_),
    cystfib = dplyr::case_when(
      CYSTFIB == 1 ~ 1,
      CYSTFIB == 2 ~ 0,
      TRUE ~ NA_integer_),
    heart = dplyr::case_when(
      HEART_CURR == 1 ~ 1,
      HEART_CURR == 2 ~ 0,
      TRUE ~ NA_integer_),
    obese = dplyr::case_when(
      BMICLASS %in% c(3, 4) ~ 1,
      BMICLASS %in% c(1, 2) ~ 0,
      TRUE ~ NA_integer_),
    diab = dplyr::case_when(
      DIABETES_CURR == 1 ~ 1,
      DIABETES_CURR == 2 ~ 0,
      TRUE ~ NA_integer_),
    blood = dplyr::case_when(
      BLOOD == 1 ~ 1,
      BLOOD == 2 ~ 0,
      TRUE ~ NA_integer_),
    cerpal = dplyr::case_when(
      K2Q61A == 1 ~ 1,
      K2Q61A == 2 ~ 0,
      TRUE ~ NA_integer_),
    epilep = dplyr::case_when(
      K2Q42B == 1 ~ 1,
      K2Q42B == 2 ~ 0,
      TRUE ~ NA_integer_),
    autoimm = dplyr::case_when(
      AUTOIMMUNE == 1 ~ 1,
      AUTOIMMUNE == 2 ~ 0,
      TRUE ~ NA_integer_),
    intell = dplyr::case_when(
      K2Q60B == 1 ~ 1,
      K2Q60B == 2 ~ 0,
      TRUE ~ NA_integer_)
  ) %>%
  mutate(
    any_known = if_any(c(asthma, cystfib, heart, obese, diab, 
                         blood, cerpal, epilep, autoimm, intell), ~ !is.na(.x)),
    any_yes   = if_any(c(asthma, cystfib, heart, obese, diab, 
                         blood, cerpal, epilep, autoimm, intell), ~ .x == 1),
    high_risk = case_when(
      any_yes ~ 1,
      !any_known ~ NA_integer_,   # all comorbidities responses missing
      TRUE ~ 0                    # at least one known and none are "yes"
    )
  ) %>%
  filter(!(STATE_FIPS %in% c("66", "78", "72") )) # remove Guam, Puerto Rico, Virgin Islands if present

#///////////////////////
#### DESIGN SURVEYS ####
options(survey.lonely.psu = "certainty") # treat single PSU strata as certainty

brfss_design_survey <- brfss_high_risk_df %>%
  dplyr::select(PSU, STRATA_STATE, weight, STATE_FIPS, age_group, high_risk) %>%
  drop_na() %>%
  srvyr::as_survey_design(
    ids     = PSU,
    strata  = STRATA_STATE,
    weights = weight,
    nest    = TRUE
  ) %>%
  ungroup()
brfss_age_results <- brfss_design_survey %>%
  group_by(STATE_FIPS, age_group) %>%
  summarise(
    frac_high_risk = srvyr::survey_mean(high_risk == 1, vartype = "ci", na.rm = TRUE),
    n_unw = srvyr::unweighted(n()) ) %>%
  ungroup()

nsch_design_survey <- nsch_high_risk_df %>%
  dplyr::select(PSU, STRATA_STATE, weight, STATE_FIPS, age_group, high_risk) %>%
  drop_na() %>%
  srvyr::as_survey_design(
    ids     = PSU,
    strata  = STRATA_STATE,
    weights = weight,
    nest    = TRUE
  ) %>%
  ungroup()
nsch_age_results <- nsch_design_survey %>%
  group_by(STATE_FIPS, age_group) %>%
  summarise(
    frac_high_risk = srvyr::survey_mean(high_risk == 1, vartype = "ci", na.rm = TRUE),
    n_unw = srvyr::unweighted(n()) ) %>%
  ungroup()

#////////////////////
#### ROWBIND DFS ####
state_names_df = read_csv("../data/county_lookup_2019-2023ACS.csv") %>%
  mutate(STATE_FIPS = as.character(str_sub(COUNTY_FIPS, 1, 2))) %>%
  dplyr::select(STATE_NAME, STATE_FIPS) %>%
  distinct()

all_age_df = brfss_age_results %>%
  bind_rows(nsch_age_results) %>%
  left_join(state_names_df, by="STATE_FIPS") %>%
  mutate(age_group = factor(age_group),
         age_group = fct_relevel(age_group, c("0-4", "5-17", "18-49", "50-64", "65+")) ) %>%
  dplyr::select(STATE_NAME, STATE_FIPS, everything())

length(unique(all_age_df$STATE_NAME)) # 51 as expected

file_path_all = "../data/all_US_high-risk-ratios-detailed.csv"
write.csv(all_age_df,
          file_path_all,
          row.names = FALSE, quote = FALSE)

#///////////////////////////////////
#### WRITE STATE SPECIFIC FILES ####
states = unique(all_age_df$STATE_NAME)
for(state in states){
  state_name_hypen = str_replace_all(state, " ", "-")
  state_age_df = all_age_df %>%
    filter(STATE_NAME == state) %>%
    arrange(age_group)
  
  file_path_detailed = paste0("../data/", state_name_hypen, "/state_", state_name_hypen, "_high-risk-ratios-detailed.csv")
  write.csv(state_age_df,
            file_path_detailed,
            row.names = FALSE, quote = FALSE)
  
  file_path_only = paste0("../data/", state_name_hypen, "/state_", state_name_hypen, "_high-risk-ratios-only.csv")
  write.table(state_age_df %>%
                dplyr::select(frac_high_risk), 
              file_path_only,
              sep = ",", col.names = FALSE,  row.names = FALSE, quote = FALSE)
} # end loop over states











