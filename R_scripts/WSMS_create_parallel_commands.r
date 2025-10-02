
#///////////////////////
#### LOAD LIBRARIES ####
library(jsonlite)
library(tidyverse)

# helper to replace "STATE" tokens in all strings
replace_STATE_tokens <- function(x, state_dir) {
  # match STATE when not adjacent to letters/digits (underscore is OK)
  pat <- "(?<![A-Za-z0-9])STATE(?![A-Za-z0-9])"
  if (is.character(x)) {
    stringr::str_replace_all(x, pat, state_dir)
  } else if (is.list(x)) {
    lapply(x, replace_STATE_tokens, state_dir = state_dir)
  } else x
}

# Find the base files
input_dir_path   = "../data/INPUT_FILE_TEMPLATES"      # where files are written (FS path from here)
base_file = list.files(path        = input_dir_path,
                        pattern     = "^INPUT_STOCH-SEIHRD_STATE.*\\.json$",
                        full.names  = TRUE, recursive  = TRUE
)
template = jsonlite::fromJSON(base_file, simplifyVector = FALSE)

# Get total counties per state to determine num batches
county_df = read_csv("../data/county_lookup_2019-2023ACS.csv")
county_per_state = county_df %>%
  group_by(STATE_NAME) %>%
  summarise(num_county = n(), .groups = "drop")

# Get all county initial infected
county_init_inf = read_csv("../data/all_US_initial_infected.csv") %>%
  drop_na() %>%
  dplyr::select("fips", "age_group", "pop", "STATE_NAME", "COUNTY_NAME", "STATE_FIPS", "init_inf_per_1M") %>%
  left_join(county_per_state, by="STATE_NAME") %>%
  mutate(
    STATE_NAME_DIR = str_replace_all(STATE_NAME, " ", "-"),
    base_file = base_file
  ) %>%
  separate(base_file, into = c(NA, NA, NA, "FILENAME_ONLY"), sep="\\/", remove=T) %>%
  mutate(
    FILENAME_ONLY = replace_STATE_tokens(FILENAME_ONLY, STATE_NAME_DIR),
    OUTPUT_FILE_PATH = paste0("../data/", STATE_NAME_DIR, "/", FILENAME_ONLY)
  )
  

total_states = length(unique(county_init_inf$STATE_NAME_DIR))
for(i in 1:total_states){
  print(i)
  # grab row just for single state
  single_state = county_init_inf %>%
    slice(i)
  
  state_template_copy = template
  state_template = replace_STATE_tokens(state_template_copy, state_dir = single_state$STATE_NAME_DIR)
  state_template$initial_infected[[1]]$county   = single_state$fips
  state_template$initial_infected[[1]]$infected = single_state$init_inf_per_1M
  state_template$output_dir_path = single_state$STATE_NAME_DIR
  
  write_json(state_template, single_state$OUTPUT_FILE_PATH, auto_unbox = TRUE, pretty = TRUE)
  print(paste0("wrote file to ", single_state$OUTPUT_FILE_PATH))
  
} # end loop over states


#////////////////////////
#### CREATE COMMANDS ####
commands_script = county_init_inf %>%
  mutate(poetry_command_start = "poetry run python3 ../src/simulator_WSMS.py -l INFO -d 180 -i") %>%
  rowwise() %>%
  mutate(final_poetry_command = paste(poetry_command_start, OUTPUT_FILE_PATH)) %>%
  ungroup() %>%
  dplyr::select(final_poetry_command)

write.table(commands_script,
            "../R0_sensitivity_analysis/r0_sensitivity_commands.txt",
            sep = "", col.names = FALSE,  row.names = FALSE, quote = FALSE)







