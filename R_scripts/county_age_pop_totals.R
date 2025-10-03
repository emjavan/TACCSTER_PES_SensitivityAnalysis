
library(tidycensus)
library(tidyverse)

#//////////////////////
#### READ POP DATA ####
# ACS 2019-2023 County total population
us_file_path = "../data/all_US_county_pop_by_age_2019-2023ACS.csv"
if(!file.exists(us_file_path)){
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
    left_join(county_lookup_df, by=c("fips"="COUNTY_FIPS"))
  
  write.csv(county_age_pop, us_file_path, row.names = FALSE, quote = FALSE)
}else{
  county_age_pop = read_csv(us_file_path)
} # end if county pop data already exists


#/////////////////////////////
#### WRITE TO STATE FILES ####
county_age_pop_spread = county_age_pop %>%
  spread(age_group, pop) %>%
  dplyr::select(STATE_NAME, COUNTY_NAME, fips, `0-4`, `5-17`, `18-49`, `50-64`, `65+`) %>%
  drop_na() # drops na's for Puerto Rico

state_names = unique(county_age_pop_spread$STATE_NAME) # length(state_names) = 51
for(state in state_names){
  state_specific_df = county_age_pop_spread %>%
    dplyr::filter(STATE_NAME==state) %>%
    # first col expected to be "fips" in file
    dplyr::select(-STATE_NAME, -COUNTY_NAME)
  
  state_name_hypen = str_replace_all(state, " ", "-")
  file_path = paste0("../data/", state_name_hypen, "/county_pop_by_age_", state_name_hypen, "_2019-2023ACS.csv")
  
  # write.csv was ignoring direction to ignore header
  write.table(state_specific_df, 
              file_path,
              sep = ",", row.names = FALSE, quote = FALSE)
} # end loop over states

#///////////////////////
#### LARGEST COUNTY ####
# Initially will infect 1 per 1M of most populous county that is LOW risk

init_inf_file = "../data/all_US_initial_infected.csv"
if(!file.exists(init_inf_file)){
  
  init_inf_df = county_age_pop %>%
    group_by(STATE_NAME, COUNTY_NAME, fips) %>%
    summarise(county_pop = sum(pop), .groups = "drop") %>%
    group_by(STATE_NAME) %>%
    arrange(desc(county_pop), .by_group = T) %>%
    summarise(total_pop = sum(county_pop), 
              COUNTY_NAME = first(COUNTY_NAME),
              fips = first(fips),
              .groups = "drop") %>%
    mutate(init_inf_per_1M = ceiling(total_pop/1000000)) %>%
    drop_na()
  
  
  ' # If making within county age/risk based init inf
  risk_ratios = read_csv("../data/all_US_high-risk-ratios-detailed.csv")
  init_inf_df = county_age_pop %>%
    left_join(risk_ratios, by=c("age_group", "STATE_NAME")) %>%
    dplyr::filter(age_group=="18-49") %>%
    group_by(STATE_NAME) %>%
    arrange(desc(pop)) %>%
    slice(1) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(low_risk_POP = floor((1-frac_high_risk)*pop)) %>%
    ungroup() %>%
    mutate(init_inf_per_1M                      = ceiling(low_risk_POP/1000000), # has to be at least 1 person even if <1M pop
           `init_inf_1percent_18-49`            = floor(0.01*low_risk_POP),
           `init_inf_half-percent_18-49`        = floor(0.005*low_risk_POP),
           `init_inf_1percent_18-49_capped`     = ifelse(`init_inf_1percent_18-49`    >10000, 10000, `init_inf_1percent_18-49`),
           `init_inf_half-percent_18-49_capped` = ifelse(`init_inf_half-percent_18-49`>10000, 10000, `init_inf_half-percent_18-49`)
    ) %>%
    drop_na()
    '
  
  write.csv(init_inf_df,
            init_inf_file,
            row.names = FALSE, quote = FALSE
  )
}else{
  init_inf_df = read_csv("../data/all_US_initial_infected.csv")
} # end if file needs to be made or exists


  































  