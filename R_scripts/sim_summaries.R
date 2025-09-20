# Process a set of simulations into an RDS with time series

library(jsonlite)
library(tidyverse)

source("TACCSTER_fig_fns.R")
input_dir_path = "../R0_sensitivity_analysis/"

args <- commandArgs()
network_size = as.integer(args[[1]])
batch_num    = as.integer(args[[2]])
start        = as.integer(args[[3]])
end          = as.integer(args[[4]])

sims = read_csv(paste0(input_dir_path, "parsed_file_paths_Network_", network_size, "_Nodes.csv"))
sim_sub = sims %>%
  slice(start:end)

sim_summaries = 
  summarize_all_sims(
    sim_sub,
    network_size   = network_size,
    input_dir_path = input_dir_path,
    batch_num      = batch_num
  )