




# Replace "STATE" tokens in all strings
replace_STATE_tokens = function(x, state_dir) {
  # match STATE when not adjacent to letters/digits (underscore is OK)
  pat = "(?<![A-Za-z0-9])STATE(?![A-Za-z0-9])"
  if (is.character(x)) {
    stringr::str_replace_all(x, pat, state_dir)
  } else if (is.list(x)) {
    lapply(x, replace_STATE_tokens, state_dir = state_dir)
  } else x
}

# Turn a state's schedule into the vaccine_stockpile JSON list
make_stockpile_json = function(state_df) {
  state_df %>%
    arrange(ReleaseDay) %>%
    transmute(
      day    = as.character(ReleaseDay),                  # keep as strings to match template
      amount = as.character(round(TotalWeeklyNewFullProtect))
    ) %>%
    transpose()                                           # list of lists: list(list(day=..., amount=...), ...)
}



# Parse STATE / SCENARIO / FIPS / batch from path and read
read_network = function(path,
                        pattern = ".*/US_States/([^/_]+)_([^/]+)/network_batch-([0-9]+)\\.csv$") {
  m = str_match(
    path,
    pattern
  )
  network_df = read_csv(path, show_col_types = FALSE) %>%
    mutate(
      STATE_NAME  = m[2],
      SCENARIO    = m[3],
      batch_num   = as.integer(m[4]),
      .before = 1
    )
  return(network_df)
} # end read_node fn

summarise_one_network_file = function(path,
                                   comp_cols = c("S","E","IA","IP","IS","H","R","D", "cum_hosp")) {
  df = read_network(path) %>%
    group_by(batch_num, STATE_NAME, SCENARIO, sim_id) %>%
    arrange(batch_num, STATE_NAME, SCENARIO, sim_id, day) %>%
    mutate(
      new_hosp = pmax(H - dplyr::lag(H, default = 0), 0),  # daily incident hospitalizations
      cum_hosp = cumsum(new_hosp)                   # cumulative ever hospitalized
    ) %>%
    ungroup() %>%
    group_by(STATE_NAME, SCENARIO, sim_id) %>%
    complete(day = full_seq(range(day), 1)) %>%
    fill(H, cum_hosp, new_hosp, .direction = "down") %>%
    replace_na(list(H = 0, cum_hosp = 0, new_hosp = 0)) %>%
    ungroup()
  
  df %>%
    arrange(STATE_NAME, SCENARIO, sim_id, day) %>%                               # ensure order for "first" occurrence
    group_by(STATE_NAME, SCENARIO, batch_num, sim_id) %>%
    pivot_longer(all_of(comp_cols), names_to = "comp", values_to = "val") %>%
    ungroup() %>%
    group_by(STATE_NAME, SCENARIO, batch_num, sim_id, comp) %>%
    summarise(
      max_value = if (all(is.na(val))) NA_real_ else max(val, na.rm = TRUE),
      max_day   = if (all(is.na(val))) NA_integer_
      else day[which.max(replace_na(val, -Inf))],
      min_value = if (all(is.na(val))) NA_real_ else min(val, na.rm = TRUE),
      min_day   = if (all(is.na(val))) NA_integer_
      else day[which.min(replace_na(val,  Inf))],
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    # Wide columns like S_max_value, S_max_day, ...
    pivot_wider(
      id_cols = c(STATE_NAME, SCENARIO, batch_num, sim_id),
      names_from = comp,
      values_from = c(max_value, max_day, min_value, min_day),
      names_glue = "{comp}_{.value}"
    )
} # end summarise_one_file fn


















# Summarize one file
read_time_stats = function(csv_path) {
  df = readr::read_csv(csv_path, show_col_types = FALSE)
  x = df[["time_seconds"]]
  tibble(
    file        = csv_path,
    n           = length(x),
    mean_sec    = mean(x),
    median_sec  = median(x),
    sd_sec      = sd(x),
    q05_sec     = quantile(x, 0.05, names = FALSE, type = 7),
    q25_sec     = quantile(x, 0.25, names = FALSE, type = 7),
    q75_sec     = quantile(x, 0.75, names = FALSE, type = 7),
    q95_sec     = quantile(x, 0.95, names = FALSE, type = 7)
  )
}

# Parse STATE / SCENARIO / FIPS / batch from path and read
read_node = function(path,
                     pattern = ".*/US_States/([^/_]+)_([^/]+)/node_([0-9]{4,5})_batch-([0-9]+)\\.csv$") {
  m = str_match(
    path,
    pattern
  )
  node_df = read_csv(path, show_col_types = FALSE) %>%
    mutate(
      STATE_NAME  = m[2],
      SCENARIO    = m[3],
      county_fips = m[4],
      batch_num   = as.integer(m[5]),
      .before = 1
    )
  return(node_df)
} # end read_node fn

# Summarize one node file: per sim_id × compartment min/max value + day (first occurrence)
summarise_one_node_file = function(path,
                              comp_cols = c("S","E","IA","IP","IS","H","R","D", "cum_hosp")) {
  df = read_node(path) %>%
      group_by(batch_num, STATE_NAME, SCENARIO, sim_id) %>%
      arrange(batch_num, STATE_NAME, SCENARIO, sim_id, day) %>%
      mutate(
        new_hosp = pmax(H - dplyr::lag(H, default = 0), 0),  # daily incident hospitalizations
        cum_hosp = cumsum(new_hosp)                   # cumulative ever hospitalized
      ) %>%
      ungroup() %>%
      group_by(STATE_NAME, SCENARIO, sim_id) %>%
      complete(day = full_seq(range(day), 1)) %>%
      fill(H, cum_hosp, new_hosp, .direction = "down") %>%
      replace_na(list(H = 0, cum_hosp = 0, new_hosp = 0)) %>%
      ungroup()
  
  df %>%
    arrange(STATE_NAME, SCENARIO, sim_id, day) %>%                               # ensure order for "first" occurrence
    group_by(STATE_NAME, SCENARIO, county_fips, batch_num, sim_id) %>%
    pivot_longer(all_of(comp_cols), names_to = "comp", values_to = "val") %>%
    ungroup() %>%
    group_by(STATE_NAME, SCENARIO, county_fips, batch_num, sim_id, comp) %>%
    summarise(
      max_value = if (all(is.na(val))) NA_real_ else max(val, na.rm = TRUE),
      max_day   = if (all(is.na(val))) NA_integer_
      else day[which.max(replace_na(val, -Inf))],
      min_value = if (all(is.na(val))) NA_real_ else min(val, na.rm = TRUE),
      min_day   = if (all(is.na(val))) NA_integer_
      else day[which.min(replace_na(val,  Inf))],
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    # Wide columns like S_max_value, S_max_day, ...
    pivot_wider(
      id_cols = c(STATE_NAME, SCENARIO, county_fips, batch_num, sim_id),
      names_from = comp,
      values_from = c(max_value, max_day, min_value, min_day),
      names_glue = "{comp}_{.value}"
    )
} # end summarise_one_node_file fn
