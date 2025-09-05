
library(tidycensus)
library(tidyverse)

#//////////////////////
#### READ POP DATA ####
# ACS 2019-2023 County total population
county_lookup_df = read_csv("../data/county_lookup_2019-2023ACS.csv")
source("../data/private_input_data/api_keys.R")
## Get population estimates
acs_vars = 
  tibble(acs_variable_code = sprintf("B01001_%0.3d", c(3:25, 27:49)), ## 3:25 males, 27:49 females
       age_grouping = rep(c('0-4', '5-9', '10-14', '15-17', 
                            '18-19', '20', '21','22-24',
                            '25-29', '30-34', '35-39', '40-44', 
                            '45-49', '50-54', '55-59', '60-61', 
                            '62-64', '65-66', '67-69', '70-74', 
                            '75-79', '80-84', '85+'), 2))

age_dict = tibble(acs_age_group = c('0-4', '5-9', '10-14', '15-17', 
                                    '18-19', '20', '21','22-24',
                                    '25-29', '30-34', '35-39', '40-44', 
                                    '45-49', '50-54', '55-59', '60-61', 
                                    '62-64', '65-66', '67-69', '70-74', 
                                    '75-79', '80-84', '85+'),
                  age_group = c('0-4', '5-17', '5-17', '5-17', 
                                '18-49', '18-49', '18-49', '18-49',
                                '18-49', '18-49', '18-49', '18-49',
                                '18-49', '50-64', '50-64', '50-64',
                                '50-64', '65+', '65+', '65+',
                                '65+', '65+', '65+'))

county_age_pop = get_acs(geography="county", variables= acs_vars$acs_variable_code, geometry=FALSE, year = 2023) %>% 
  left_join(acs_vars, by = c('variable' = 'acs_variable_code')) %>% 
  left_join(age_dict, by = c('age_grouping' = 'acs_age_group')) %>%
  group_by(GEOID, age_group) %>% 
  summarize(pop = sum(estimate), .groups = "drop") %>%
  rename(fips=GEOID) %>%
  spread(age_group, pop) %>%
  left_join(county_lookup_df, by=c("fips"="COUNTY_FIPS")) %>%
  dplyr::select(STATE_NAME, COUNTY_NAME, fips, `0-4`, `5-17`, `18-49`, `50-64`, `65+`) %>%
  drop_na() # drops na's for Puerto Rico

#/////////////////////////////
#### WRITE TO STATE FILES ####
state_names = unique(county_age_pop$STATE_NAME) # length(state_names) = 51
for(state in state_names){
  state_specific_df = county_age_pop %>%
    dplyr::filter(STATE_NAME==state)
  
  state_name_hypen = str_replace_all(state, " ", "-")
  file_path = paste0("../data/", state_name_hypen, "/county_pop_by_age_", state_name_hypen, "_2019-2023ACS.csv")
  
  # write.csv was ignoring direction to ignore header
  write.table(state_specific_df, 
              file_path,
              sep = ",", row.names = FALSE, quote = FALSE)
} # end loop over states

#///////////////////////
#### LARGEST COUNTY ####
# Initially will infect 1% of most populous county that is LOW risk
risk_ratios = read_csv("../data/all_US_high-risk-ratios-detailed.csv")
init_inf_df = county_age_pop %>%
  gather(age_group, POP_ACS, -STATE_NAME, -COUNTY_NAME, -fips) %>%
  left_join(risk_ratios, by=c("age_group", "STATE_NAME")) %>%
  filter(age_group=="18-49") %>%
  group_by(STATE_NAME) %>%
  arrange(desc(POP_ACS)) %>%
  slice(1) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(low_risk_POP = floor((1-frac_high_risk)*POP_ACS)) %>%
  ungroup() %>%
  mutate(`init_inf_1percent_18-49`            = floor(0.01*low_risk_POP),
         `init_inf_half-percent_18-49`        = floor(0.005*low_risk_POP),
         `init_inf_1percent_18-49_capped`     = ifelse(`init_inf_1percent_18-49`    >10000, 10000, `init_inf_1percent_18-49`),
         `init_inf_half-percent_18-49_capped` = ifelse(`init_inf_half-percent_18-49`>10000, 10000, `init_inf_half-percent_18-49`)
         )

write.csv(init_inf_df,
          "../data/all_US_initial_infected.csv",
          row.names = FALSE, quote = FALSE
          )  
  































  