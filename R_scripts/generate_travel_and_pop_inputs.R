# Create the county_age_matrix_1M.csv and travel_work_matrix.csv
#  for theoretical population with 1M people and we vary the num nodes
# Everything is uniformly distributed to and between nodes
# Contact matrix between age groups and risk ratios same for all networks 
# Used for TACCSTER 2025 - Emily Javan - ATX

options(scipen=999) # disable scientific notation
library(tidyverse)

# helper: integer rounding that preserves a target sum exactly
round_preserve_sum = function(x, target_sum) {
  raw <- x / sum(x)
  flo <- floor(raw * target_sum)
  remainder <- target_sum - sum(flo)
  if (remainder > 0) {
    frac <- (raw * target_sum) - flo
    add_idx <- order(frac, decreasing = TRUE)[seq_len(remainder)]
    flo[add_idx] <- flo[add_idx] + 1L
  }
  return(flo)
} # end round_preserve_sum 

round_preserve_sum_char <- function(x, target_sum) {
  flo = round_preserve_sum(x, target_sum)
  comma_separated_string <- paste0(flo, collapse = ",")
  return(comma_separated_string)
} # end round_preserve_sum_char

# Generate n unique 5-digit FIPS-like IDs (character)
# e.g., start=10000 -> "10000","10001","10002",...
generate_fips_5digit = function(n, start = 10000L) {
  stopifnot(n >= 1, start >= 0, start + n - 1 <= 99999)
  sprintf("%05d", seq.int(from = start, length.out = n))
}

# 1M is hard coded in name output which will be issue if this value changes
pop_size     = 1000000 # 1M
age_grp_labs = c("0-4","5-17","18-49","50-64","65+")
num_age_grp  = length(age_grp_labs)
num_nodes    = c(2, 10, 100, 250) # made 1 node by hand but could add
age_props    = rep(1/length(age_grp_labs), length(age_grp_labs))

for(n in num_nodes){
  out_dir = file.path("..", "data", paste0("Network_", n, "_Node"))
  if(!dir.exists(out_dir)){dir_create(out_dir, recurse = TRUE)}
  
  # fips: base_fips, base_fips+10000, ... up to num nodes and at most length 5
  fips = generate_fips_5digit(n)
  stopifnot(anyDuplicated(fips) == 0)
  
  # ---- county_age_matrix_1M.csv ----
  # total people per node (≈ pop_size/n, adjust for rounding)
  node_sizes = round_preserve_sum(rep(1, n), pop_size)
  age_mat = tibble(fips = fips, node_size = node_sizes) %>%
    mutate(counts = map(node_size, ~ round_preserve_sum_char(age_props, .x))) %>%
    dplyr::select(-node_size) %>%
    separate(counts, into=age_grp_labs, sep=",")
  write.csv(
    age_mat, 
    file = file.path(out_dir, "county_age_matrix_1M.csv"), 
    row.names=F)
  
  # ---- travel_work_matrix.csv (no headers/rows names) ----
  # travel not population dependent just num_node dependent
  travel <- matrix(1/n, nrow = n, ncol = n)
  write.table(
    as.data.frame(travel),
    file = file.path(out_dir, "travel_work_matrix.csv"),
    sep=",", col.names=F, row.names=F
  )
} # end loop over network sizes
