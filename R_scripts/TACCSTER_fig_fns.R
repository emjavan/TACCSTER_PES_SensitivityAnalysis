
# Summarize one file
read_time_stats = function(csv_path) {
  df = readr::read_csv(csv_path, show_col_types = FALSE)
  x = df[["time_seconds"]]
  meta = parse_path_meta(csv_path)
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
  ) %>%
    bind_cols(meta)
}


# Helper: parse model metadata from the path
parse_path_meta = function(path) {
  parent_dir      = basename(dirname(path))                 # e.g., seirs-deterministic_POP-500_R0-3.3
  network_dir     = basename(dirname(dirname(path)))        # e.g., Network_10_Node
  network_size    = stringr::str_match(network_dir, "(?<=Network_)([0-9]+)(?=_Node)")[,2] %>% as.integer()
  
  tibble(
    parent_dir   = parent_dir,
    model        = str_remove(parent_dir, "_.*$"),
    POP          = str_match(parent_dir, "(?<=_POP-)([0-9A-Za-z]+)")[,2],
    R0           = str_match(parent_dir, "(?<=_R0-)([0-9.]+)")[,2] %>% as.numeric(),
    NetworkDir   = network_dir,
    NetworkSize  = network_size
  ) %>%
    mutate(
      # Keep your preferred orderings
      model = factor(
        model,
        levels = c("seir-deterministic", "seirs-deterministic", 
                   "seatird-deterministic", "seatird-stochastic")),
      POP = factor(POP, levels = c("500", "100K", "1M")),
      # Nice label for faceting (e.g., "N=1", "N=10", etc.)
      NetworkLabel = factor(paste0("Nodes: ", NetworkSize),
                            levels = paste0("Nodes: ", sort(unique(NetworkSize))))
    )
} # end parse_path_meta fn


# 1) Find sims + parse path metadata
parse_one = function(simdir) {
  parent_dir  = basename(dirname(simdir))                    # e.g. "seatird-deterministic_POP-1M_R0-0.7"
  network_dir = basename(dirname(dirname(simdir)))           # e.g. "Network_1_Node"
  tibble(
    simdir       = simdir,
    NetworkSize  = as.integer(str_match(network_dir, "(?<=Network_)([0-9]+)(?=_Node)")[,2]),
    model        = sub("_.*$", "", parent_dir),
    POP          = str_match(parent_dir, "(?<=_POP-)([^_]+)")[,2],
    R0           = as.numeric(str_match(parent_dir, "(?<=_R0-)([0-9.]+)")[,2]),
    sim_id       = as.integer(str_match(basename(simdir), "(?<=output_sim)([0-9]+)")[,2])
  )
}

# Not used as it's too slow
list_simulations = function(root_dir, pop_filter = NULL) {
  sim_dirs = fs::dir_ls(
    root_dir, recurse = TRUE, type = "directory",
    regexp = "Network_[0-9]+_Node/.+?/output_sim[0-9]+$"
  )
  if (!is.null(pop_filter)) {
    pat = paste0("_POP-(", paste(pop_filter, collapse = "|"), ")_")
    sim_dirs = stringr::str_subset(sim_dirs, pat)
  }
  
  bind_rows(lapply(sim_dirs, parse_one))
}

# 2) Read one simulation's time series
read_timeseries = function(simdir) {
  files = fs::dir_ls(simdir, type = "file", regexp = "output_[0-9]+\\.json$")
  if (length(files) == 0) return(NULL)
  
  # sort by numeric day from filename
  ord   = as.integer(str_match(basename(files), "(?<=output_)([0-9]+)")[,2])
  files = files[order(ord)]
  
  read_one = function(f) {
    j   = jsonlite::fromJSON(f, simplifyVector = TRUE)
    day = as.integer(str_match(basename(f), "(?<=output_)([0-9]+)")[,2])
    if (is.null(j$total_summary)) stop("Missing total_summary in: ", f)
    row = as.data.frame(as.list(j$total_summary), stringsAsFactors = FALSE)
    for (nm in names(row)) row[[nm]] = suppressWarnings(as.numeric(row[[nm]]))
    row$day = day
    row[, c("day", setdiff(names(row), "day")), drop = FALSE]
  }
  
  ts = bind_rows(lapply(files, read_one))
  ts[order(ts$day), , drop = FALSE] %>% as_tibble()
}

# 3) Build wide peaks and keep nested ts
summarize_all_sims = function(sim_index, 
                              network_size=250,
                              input_dir_path="../R0_sensitivity_analysis/",
                              batch_num = 1){
  
  total_sims = nrow(sim_index)
  out = vector("list", total_sims)
  for(i in seq_len(total_sims)){
    simdir = sim_index$simdir[i]
    print(paste0("Sim ", i, " of ", total_sims, " started for ", simdir))
    NetworkSize = sim_index$NetworkSize[i]
    model       = sim_index$model[i]
    POP         = sim_index$POP[i]
    R0          = sim_index$R0[i]
    sim_id      = sim_index$sim_id[i]
    ts   = read_timeseries(simdir)
    if(is.null(ts) || nrow(ts) == 0) next
    total_days = as.integer(max(ts$day))
    comp_cols  = setdiff(names(ts), "day")
    # ensure numeric
    for(nm in comp_cols) ts[[nm]] = suppressWarnings(as.numeric(ts[[nm]]))
    
    # compute peak value and first day of peak for EACH compartment
    outcomes = list()
    for(comp in comp_cols){
      series = ts[[comp]]
      if (all(is.na(series))) {
        outcomes[[paste0(comp, "_max_value")]] = NA_real_
        outcomes[[paste0(comp, "_max_day")]]   = NA_integer_
        outcomes[[paste0(comp, "_min_value")]] = NA_real_
        outcomes[[paste0(comp, "_min_day")]]   = NA_integer_
      } else {
        # max value
        max_val = max(series, na.rm = TRUE)
        idx_mx  = which(series == max_val)[1]
        outcomes[[paste0(comp, "_max_value")]] = as.numeric(max_val)
        outcomes[[paste0(comp, "_max_day")]]   = as.integer(ts$day[idx_mx])
        
        # min value
        min_val = min(series, na.rm = TRUE)
        idx_mn  = which(series == min_val)[1]
        outcomes[[paste0(comp, "_min_value")]] = as.numeric(min_val)
        outcomes[[paste0(comp, "_min_day")]]   = as.integer(ts$day[idx_mn])
      }
    } # end loop over compartments
    
    out[[i]] = bind_cols(
      setNames(as_tibble(simdir), "simdir"),
      setNames(as_tibble(NetworkSize), "NetworkSize"),
      setNames(as_tibble(model), "model"),
      setNames(as_tibble(POP), "POP"),
      setNames(as_tibble(R0), "R0"),
      setNames(as_tibble(sim_id), "sim_id"),
      setNames(as_tibble(total_days), "total_days"),
      as_tibble(outcomes),
      tibble(ts = list(ts))   # keep full time series nested
    )
    
    saveRDS(
      bind_rows(out),
      paste0(input_dir_path, "summarised_sims/summarised_sims_Network_", network_size, "_Nodes_batch-", batch_num, ".rds"),
      compress = "gzip"
    )
    
  }
  bind_rows(out)
}


#///////////////////////////////
#### Bootstrap Outcome Vars ####
metric_cols <- function(df) {
  # any column ending in "_value" (e.g., S_peak_value, R_final_value, etc.)
  grep("_value$", names(df), value = TRUE)
}

full_sample_ref <- function(df_grp, metrics) {
  # “truth” within a group (mean over all sims in the group)
  df_grp %>%
    summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}")) %>%
    tidyr::pivot_longer(everything(), names_to = "metric", values_to = "ref_mean")
}

# One bootstrap draw for one group
boot_once <- function(df_grp, metrics = metric_cols(df_grp), k=10, eps = 0.05, z = 1.96) {
  stopifnot(nrow(df_grp) >= 1)
  k <- min(k, nrow(df_grp))                      # clamp to available
  samp <- df_grp %>% slice_sample(n = k, replace = TRUE)
  
  stats <- tibble(metric = metrics) %>%
    mutate(
      # raw (unrounded) stats first
      mean_raw = map_dbl(metric, ~ mean(samp[[.x]], na.rm = TRUE)),
      sd_raw   = map_dbl(metric, ~ sd(samp[[.x]], na.rm = TRUE)),
      n_non_na = map_int(metric, ~ sum(!is.na(samp[[.x]]))),
      
      # rounded display stats
      mean   = round(mean_raw, 2),
      median = round(map_dbl(metric, ~ median(samp[[.x]], na.rm = TRUE)), 2),
      sd     = round(sd_raw, 2),
      se     = round(if_else(n_non_na > 0, sd_raw / sqrt(n_non_na), NA_real_), 2),
      q05    = round(map_dbl(metric, ~ quantile(samp[[.x]], 0.05, na.rm = TRUE, type = 7)), 2),
      q25    = round(map_dbl(metric, ~ quantile(samp[[.x]], 0.25, na.rm = TRUE, type = 7)), 2),
      q75    = round(map_dbl(metric, ~ quantile(samp[[.x]], 0.75, na.rm = TRUE, type = 7)), 2),
      q95    = round(map_dbl(metric, ~ quantile(samp[[.x]], 0.95, na.rm = TRUE, type = 7)), 2),
      n_est  = if_else(
        sd == 0, 1L, ceiling(((z * sd) / (eps * mean) )^2)
      )
    )
  stats
}

bootstrap_group_kgrid <- function(
    df_grp,
    keys          = c("model", "POP", "R0", "NetworkSize"),
    k_grid        = NULL,
    bootsrap_reps = 300,
    eps           = 0.05, # eps and z are dependent
    z             = 1.96) {
  n_avail = nrow(df_grp)
  mets    = metric_cols(df_grp)
  if (length(mets) == 0) stop("No *_value metrics found.")
  
  # Default k grid (monotone, within available)
  if (is.null(k_grid)) {
    k_grid = unique(pmin(c(5, 10, 20, 50, 100, 200, 500, n_avail), n_avail))
  }
  
  ref_tbl = full_sample_ref(df_grp, mets)  # empirical “truth” for stabilization
  
  map_dfr(k_grid, function(k) {
    reps = map_dfr(seq_len(bootsrap_reps), ~ boot_once(df_grp, mets, k, eps, z)) %>%
      mutate(k = k)
    
    # summarize bootstrap distribution per metric
    out = reps %>%
      group_by(metric, k) %>%
      summarise(
        n_avail      = n_avail,
        bootsrap_reps= dplyr::n(),
        mean_mean    = mean(mean, na.rm = TRUE),
        med_mean     = median(mean, na.rm = TRUE),
        sd_mean      = sd(mean, na.rm = TRUE),
        q05_mean     = quantile(mean, 0.05, na.rm = TRUE, type = 7),
        q95_mean     = quantile(mean, 0.95, na.rm = TRUE, type = 7),
        med_sd       = median(sd, na.rm = TRUE),
        med_n_est    = median(n_est, na.rm = TRUE),
        .groups      = "drop"
      ) %>%
      left_join(ref_tbl, by = "metric") %>%
      mutate(
        rel_err_med = abs(med_mean - ref_mean) / (abs(ref_mean) + .Machine$double.eps),
        rel_err_q95 = abs(q95_mean - ref_mean) / (abs(ref_mean) + .Machine$double.eps),
        meets_eps   = rel_err_med <= eps
      )
    
    # attach group keys once
    bind_cols(as_tibble(df_grp[1, keys, drop = FALSE]), out)
  })
}


# Bootstrap
run_bootstrap_pipeline = function(sim_summaries,
                                  keys = c("model","POP","R0","NetworkSize"),
                                  bootsrap_reps = 300, eps = 0.05, z = 1.96,
                                  k_grid = NULL) {
  sim_summaries %>%
    group_by(across(all_of(keys))) %>%
    group_split() %>%
    map_dfr(~ bootstrap_group_kgrid(.x, keys = keys, k_grid = k_grid, bootsrap_reps = bootsrap_reps, eps = eps, z = z))
}






