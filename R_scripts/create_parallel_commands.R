#//////////////////////////////////////////////////////////////////////
# Generate state-specific commands txt file based on initial templates
# Only param being varied is R0 so easier to compare
# Math done separately to try and match the infectious period despite 
#   changes in terminal compartments (R vs R & D)
#//////////////////////////////////////////////////////////////////////

#///////////////////////
#### LOAD LIBRARIES ####
library(jsonlite)
library(tidyverse)

#///////////////////////////
#### CREATE INPUT FILES ####
# Make keep periods out of R0 to ensure no file type issues
r0_period_to_hypen <- function(R0) str_replace(formatC(R0, format = "f", digits = 1), "\\.", "-")

# R0 options to test
R0_grid = seq(0.5, 4.1, by = 0.2) # 19, R0=3 template not in set
 
# Find the base files
input_dir_path   = "../data/INPUT_FILE_TEMPLATES"      # where files are written (FS path from here)
final_output_dir_path  = "../data/INPUT_FILES"
if(!dir.exists(final_output_dir_path)){ dir.create(final_output_dir_path) }
base_files = list.files(path        = input_dir_path,
                        pattern     = "^INPUT_.*R0-3\\.json$",
                        full.names  = TRUE, recursive  = TRUE
                        )

# Generate new inputs
inputs = expand_grid(input_file_template = base_files, R0 = R0_grid) %>%
  mutate(out_dir_prefix = final_output_dir_path) %>%
  # assuming in folders ../data/INPUT_FILE_TEMPLATES/Network_*_Node/
  separate(input_file_template, into = c(NA, NA, NA, NA, "filename_only"), sep="\\/", remove=F) %>%
  rowwise() %>%
  mutate(output_file_path = paste0(out_dir_prefix, "/", filename_only),
         r0_hyphen        = r0_period_to_hypen(R0),
         output_file_path = str_replace(output_file_path, "3", as.character(R0))
         ) %>%
  ungroup() %>%
  dplyr::select(input_file_template, R0, output_file_path)
  
input_exanded_tbl = inputs %>%
  rename(fin  = input_file_template,
         r0   = R0,
         fout = output_file_path) %>%
  pmap_chr(function(fin, r0, fout) {
           tpl <- read_json(fin, simplifyVector = TRUE)
           
           # change R0 inside the template
           tpl$disease_model$parameters$R0 <- as.character(r0)
    
           # update output_dir_path inside template to include R0 at the end
           #r0_conv <- r0_period_to_hypen(r0)
           tpl$output_dir_path = paste0(tpl$output_dir_path, "_R0-", r0)
    
           # write to new file name
           write_json(tpl, fout, auto_unbox = TRUE, pretty = TRUE)
           fout
           })
  
#////////////////////////
#### CREATE COMMANDS ####
commands_script = input_exanded_tbl %>%
  as_tibble() %>%
  rename(out_file = value) %>%
  mutate(poetry_command_start = "poetry run python3 ../src/simulator.py -l INFO -d 500 -i ") %>%
  rowwise() %>%
  mutate(final_poetry_command = paste(poetry_command_start, out_file)) %>%
  ungroup() %>%
  dplyr::select(final_poetry_command)

write.table(commands_script,
            "../R0_sensitivity_analysis/r0_sensitivity_commands.txt",
            sep = "", col.names = FALSE,  row.names = FALSE, quote = FALSE)






















