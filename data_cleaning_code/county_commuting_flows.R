#' Get county to county work flow within (not between) each US state
#' Removed header:
#' Table 1. Residence County to Workplace County Commuting Flows for the United States and Puerto Rico Sorted by Residence Geography: 5-Year ACS, 2016-2020									
#' For more information on sampling and estimation methods, confidentiality protection, and sampling and nonsampling errors, see 
#' <https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2020.pdf>.			
#' Universe: Workers 16 years and over.									
#' Commuting flows are sorted by residence state, residence county, workplace state, and workplace county.									

#### NOTE ####
# 1. Connecticut changed county fips with new census and the commuting data uses the new fips rather than old
# see https://www.cdc.gov/nchs/data/data-analysis/County-Geography.pdf for details
# Simplest thing to do was to use recent ACS data and increase the commuting flow to plus the error
#  before dividing by population
# 2. The flow count is only 16+ but we divide by entire population of county because the compartmental model
#  will loop over all age groups and apply work contact pattern


library(tidyverse)
library(tidycensus)

#/////////////////////////
#### READ TRAVEL DATA ####
# Take in excel file and clean header column names
us_county_flows = readxl::read_xlsx("../data/County_Commuting_Flow_ACS_2016-2020.xlsx")
header_row <- which(
  apply(us_county_flows, 1, \(x) any(str_detect(as.character(x), regex("State\\s*FIPS\\s*Code", i = TRUE)))))[1]

us_county_flows_clean = readxl::read_xlsx("../data/County_Commuting_Flow_ACS_2016-2020.xlsx",
                                    skip = header_row)
expected_cols <- c(
  "state_fips_residence", "county_fips_residence", "state_name_residence", "county_name_residence",
  "state_fips_workplace", "county_fips_workplace", "state_name_workplace", "county_name_workplace",
  "workers_in_commuting_flow", "workers_margin_of_error"
)
names(us_county_flows_clean) = expected_cols

#/////////////////////////
#### READ POP DATA ####
# ACS 2019-2023 County total population
source("../data/private_input_data/api_keys.R")
county_pops = 
  tidycensus::get_acs(geography = "county", variables="B01001_001",
                      year = 2023, geometry=F) %>%
  dplyr::select(-moe, -NAME, -variable) %>%
  rename(county_fips_residence = GEOID,
         county_pop_2020 = estimate)

#///////////////////
#### CLEAN DATA ####
state_county_flows = us_county_flows_clean %>% # 122,339 rows
  dplyr::filter(state_name_residence==state_name_workplace) %>% # 63,212
  mutate(state_fips_residence = str_pad(as.numeric(state_fips_residence), 2, side="left", pad="0"),
         state_fips_workplace = str_pad(as.numeric(state_fips_workplace), 2, side="left", pad="0"),
         
         county_fips_residence = str_pad(as.numeric(county_fips_residence), 3, side="left", pad="0"),
         county_fips_workplace = str_pad(as.numeric(county_fips_workplace), 3, side="left", pad="0"),
         
         county_fips_residence = paste0(state_fips_residence, county_fips_residence),
         county_fips_workplace = paste0(state_fips_workplace, county_fips_workplace)
         ) %>%
  #dplyr::select(state_name_residence, contains(c("fips", "workers")), -starts_with("state_fips"), -ends_with("_name_workplace") ) %>%
  full_join(county_pops, by="county_fips_residence") %>% # 63,212
  rowwise() %>%
  mutate(upper_limit_workers_in_commuting_flow = as.numeric(workers_in_commuting_flow) + as.numeric(workers_margin_of_error),
         fraction_pop_in_commuting_flow = round(upper_limit_workers_in_commuting_flow/county_pop_2020, 2)
         ) %>%
  ungroup() %>%
  dplyr::filter(!(state_name_residence=="Puerto Rico")) # 61,508


#/////////////////////////////
#### WRITE TO STATE FILES ####

# Useful look up dict I should write some tests around
county_look_up_df = state_county_flows %>%
  dplyr::select(state_name_residence, county_name_residence, county_fips_residence) %>%
  distinct() %>% 
  rename(STATE_NAME = state_name_residence,
         COUNTY_NAME = county_name_residence, 
         COUNTY_FIPS = county_fips_residence)
write.csv(county_look_up_df, "../data/county_lookup_2019-2023ACS.csv", row.names = F)

length(unique(state_county_flows$county_fips_residence)) # 3,144

state_names = unique(state_county_flows$state_name_residence)
for(state in state_names){
  state_specific_df = state_county_flows %>%
    dplyr::filter(state_name_residence==state) %>%
    dplyr::select(county_fips_residence, county_fips_workplace, fraction_pop_in_commuting_flow) %>%
    arrange(county_fips_residence, county_fips_workplace)
  
  state_specific_matrix = state_specific_df %>%
    spread(county_fips_workplace, fraction_pop_in_commuting_flow, fill = 0.00) %>%
    dplyr::select(-county_fips_residence)
  
  state_name_hypen = str_replace_all(state, " ", "-")
  file_path = paste0("../data/", state_name_hypen, "/commuting_flow_", state_name_hypen, "_2016-2020ACS.csv")
  
  # write.csv was ignoring direction to ignore header
  write.table(state_specific_matrix, file_path,
              sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
} # end loop over states















