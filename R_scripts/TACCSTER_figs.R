#' Create TACCSTER poster figures
#' Needs to be run on the cluster because there are too many files to scp
#' 1. Line Plot run times across models (horizontal) and network sizes (vertical)
#'    3 populations sizes tested for a single node network across 4 models
#'      3 pops   = 500, 100K, and 1M uniformly distributed across 5 age & 2 risk grps
#'      4 models = 3 proportional/deterministic & 1 individual/stochastic
#'      19 R0s from 0.5 to 4.1 by 0.2
#'      median and quantiles => re-running to get 100 sims rather than 10K
#' 2. Outcome variables of the 1M pop by model and network size change
#'    Really focused on how many sims to stabilize vs predicted 
#'    

#### SOURCE ####
library(jsonlite)
library(tidyverse)
library(gt)
options(scipen = 999) # disable scientific notation
source("TACCSTER_fig_fns.R")

#//////////////
#### FIG 1 ####
# list.dirs too slow used bash instead
# ls Network_*_Node/*/simulation_times.csv >> sim_time_paths.csv

input_dir_path = "../R0_sensitivity_analysis/"
output_file = paste0(input_dir_path, "sim_time_summaries.csv")
if(!file.exists(output_file)){
  time_file_paths = read_csv(paste0(input_dir_path, "sim_time_paths.csv"), col_names = F) %>%
    mutate(X1 = paste0(input_dir_path, X1))
  
  # Apply to all files
  time_summaries = purrr::map_dfr(time_file_paths$X1, read_time_stats)
  write.csv(time_summaries,
            output_file,
            row.names = F
  )
}else{
  time_summaries = read_csv(output_file) %>%
    mutate(POP = factor(POP, levels=c("500", "100K", "1M")),
           NetworkLabel = factor( NetworkLabel, levels=c("Nodes: 1", "Nodes: 2", "Nodes: 10", 
                                                         "Nodes: 100", "Nodes: 250")),
           mean_min = round(mean_sec/60, 0),
           sd_min = sd_sec/60,
           model = toupper(model),
           model = factor(model, levels=c("SEIR-DETERMINISTIC", "SEIRS-DETERMINISTIC",
                                          "SEATIRD-DETERMINISTIC", "SEATIRD-STOCHASTIC"),
                          labels = c("SEIR-Fixed", "SEIRS-Fixed", "SEATIRD-Fixed", "SEATIRD-Random"))
           )
}

# color hex codes come from
# pal = nationalparkcolors::park_palette("Everglades")
runtime_plot =
  ggplot(time_summaries, aes(x = R0, y = mean_min, color = POP, group = POP)) +
  geom_ribbon(aes(ymin = mean_min - sd_min, ymax = mean_min + sd_min, fill = POP), alpha = 0.3) +
  geom_line(alpha = 0.7, size = 1) +
  geom_point(size = 2) +
  facet_grid(
    NetworkLabel ~ model,
    scales   = "free_y",
    labeller = labeller(
      NetworkLabel = label_wrap_gen(width = 20),
      model        = label_wrap_gen(width = 25)
    )
  ) +
  theme_bw(base_size = 30) +
  labs(x = "R0", y = "Mean \u00B1 SD Run Time (minutes)", color = "Population", fill = "Population") +
  scale_color_manual(values = c("500"="#EAAE37","100K"="#9A8146","1M"="#565F41")) +
  scale_fill_manual(values = c("500"="#EAAE37","100K"="#9A8146","1M"="#565F41")) +
  theme(
    strip.text.x = element_text(size = rel(0.9), lineheight = 0.95, margin = margin(3,6,3,6)),
    strip.text.y = element_text(size = rel(0.9), lineheight = 0.95, margin = margin(6,3,6,3)),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom" #,
    #plot.margin = margin(10, 14, 10, 14) 
  )


ggsave(filename = paste0(input_dir_path, "figs/runtime_mean.png"),
       plot = runtime_plot, 
       bg="white", width=14, height=12, units="in", dpi=1600
       )


#//////////////
#### FIG 2 ####


#//////////////////
#### Sim Paths ####
input_dir_path = "../R0_sensitivity_analysis/"
network_size = 1
sim_dir_path = paste0(input_dir_path, "sim_output_paths_Network_", network_size, "_Nodes_sub100.csv")
sim_file_paths = read_csv(sim_dir_path, col_names = FALSE, show_col_types = FALSE) %>%
  pull(X1) %>%
  paste0(input_dir_path, .)
sim_file_paths_parse = parse_one(sim_file_paths)
write.csv(sim_file_paths_parse,
          paste0(input_dir_path, "parsed_file_paths_Network_", network_size, "_Nodes.csv"),
          row.names = F)


#//////////////////////
#### Sim Summaries ####
rds_files = list.files(path="../R0_sensitivity_analysis/summarised_sims/", 
                       pattern = ".rds$", full.names=T)
sim_summaries = map_dfr(rds_files, readRDS)
complete_sims = sim_summaries %>% # 2908
  mutate(finished_sims = ifelse((E_min_value>1 & E_min_day<500), F, T)) %>%
  dplyr::filter(finished_sims) # 2707

keys = c("model", "POP", "R0", "NetworkSize")
grpd = complete_sims %>%
  group_by(across(all_of(keys))) %>%        # <- dynamic grouping with keys
  group_modify(~ boot_once(.x, k=100)) %>%  # <- run once per group, returns rows
  ungroup() %>%
  dplyr::filter(metric %in% c("S_min_value", "E_max_value", "A_max_value", "T_max_value",
                              "I_max_value", "R_max_value", "D_max_value"))



net_size = 100; R0_choice=4.1
not_matched_examp = sim_summaries %>%
  dplyr::filter(NetworkSize==net_size) %>%
  dplyr::filter(R0==R0_choice) %>% # 1.1 is the mostly not matched example
  unnest(ts) %>%
  dplyr::select(model, sim_id, day, S, E, R, D) %>%
  gather("compartments", "people", -model, -sim_id, -day) %>%
  mutate(compartments = factor(compartments, level=c("S", "E", "R", "D"), 
                               labels = c("Susceptible", "Exposed", "Recovered", "Deceased") ),
         model=toupper(model),
         model = factor(model, levels=c("SEIR-DETERMINISTIC", "SEIRS-DETERMINISTIC",
                                        "SEATIRD-DETERMINISTIC", "SEATIRD-STOCHASTIC"),
                        labels = c("SEIR-Fixed", "SEIRS-Fixed", "SEATIRD-Fixed", "SEATIRD-Random"))
         )

comp_compare = ggplot(not_matched_examp, aes(x=day, y=people, group=interaction(model, sim_id), color=model))+
  geom_line(alpha=0.5)+
  facet_wrap(~compartments, nrow=2, scales="free_y")+
  scale_color_manual(values = c("SEATIRD-Fixed"="#EAAE37", "SEATIRD-Random"="#565F41"))+
  guides(color = guide_legend(override.aes = list(alpha = 1))) + 
  labs(x="Simulation Day", y="Population", 
       title=paste0(net_size, " Node Network, ", " R0=", R0_choice))+
  theme_bw(base_size=25)+ 
  theme(legend.position = "bottom")

ggsave(filename = paste0(input_dir_path, "figs/compartment_ts_plot_Net", net_size, "R0-", R0_choice, ".png"),
       plot = comp_compare, 
       bg="white", width=10, height=8, units="in", dpi=1600
)

plotly::ggplotly(comp_compare)


# Example: unnest one sim to inspect
ex_ts = sim_summaries$ts[[1]]






#///////////////////////
#### Bootstrap Sims ####
# min of everything was 1, so not getting much useful details
boot_grid = run_bootstrap_pipeline(complete_sims, keys = keys, bootsrap_reps = 100, k_grid = c(1, 5, 10)) %>%
  # Metrics of interest min S and max the rest
  dplyr::filter(metric %in% c("S_min_value", "E_max_value", "A_max_value", "T_max_value",
                              "I_max_value", "R_max_value", "D_max_value"))
# Find empirical stabilization: minimal k where relative error ≤ 5%
stability <- boot_grid %>%
  group_by(across(all_of(c(keys,"metric")))) %>%
  arrange(k) %>%
  summarise(k_empirical = min(k[meets_eps], na.rm = TRUE), .groups = "drop")

# Compare empirical k vs theoretical n (median across boot reps)
alignment <- boot_grid %>%
  group_by(across(all_of(c(keys,"metric")))) %>%
  summarise(n_est_theoretical = min(ceiling(median(med_n_est, na.rm = TRUE)), first(n_avail)),
            .groups = "drop") %>%
  left_join(stability, by = c(keys,"metric"))


# Plot stabilization curves (mean of means vs k)
# all are POP = 1M
ggplot(boot_grid, aes(R0, mean_mean, color = metric, group=metric)) + 
  geom_point() +
  facet_grid(model ~ NetworkSize, scales = "free_y") + 
  theme_bw()

#//////////////////////
#### Summary Table ####
# Keep only I, R, D rows
# Bootstapping not useful because there is no little stochasticity
df_filtered = grpd %>%
  left_join(boot_grid,
            by=c("model", "NetworkSize", "POP", "R0", "metric")) %>%
  dplyr::filter(k==1) %>%
  dplyr::filter(metric %in% c("I_max_value", "R_max_value", "D_max_value")) %>%
  mutate(#delta = n_est_theoretical - k_empirical,
         model = toupper(model),
         model = factor(model, levels=c("SEATIRD-DETERMINISTIC", "SEATIRD-STOCHASTIC"),
                        labels = c("Fixed", "Random"))
         ) 


# Graphical table with gt
# Pivot wider so each metric has its own set of columns
df_wide = df_filtered %>%
  pivot_wider(
    id_cols = c(model, R0, NetworkSize),
    names_from = metric,
    values_from = c(n_est, mean, sd) # k_empirical,
  ) %>%
  rename(
    I_mean = mean_I_max_value,
    I_sd = sd_I_max_value,
    I_n_est = n_est_I_max_value,
    #I_empirical = k_empirical_I_max_value,
    R_mean = mean_R_max_value,
    R_sd = sd_R_max_value,
    R_n_est = n_est_R_max_value,
    #R_empirical = k_empirical_R_max_value,
    D_mean = mean_D_max_value,
    D_sd = sd_D_max_value,
    D_n_est = n_est_D_max_value,
    #D_empirical = k_empirical_D_max_value
  ) %>%
  arrange(R0, NetworkSize, desc(model)) %>% # deterministic first, stochastic second
  mutate(across(matches("(_mean|_sd)$"), ~ round(.x, 0)))


# light grey in powerpoint "#D1D1D1"

# Build graphical table
empirical_compare_table = 
  df_wide %>%
  # Reorder columns before gt
  dplyr::select(model, R0, NetworkSize,
                I_mean, I_sd, I_n_est, #I_empirical,
                R_mean, R_sd, R_n_est, #R_empirical,
                D_mean, D_sd, D_n_est, #D_empirical
         ) %>%
  dplyr::filter(R0 %in% c(1.1, 4.1)) %>%
  gt(groupname_col = "R0", rowname_col = "model") %>%
  tab_spanner(label = "Max Infected",  columns = c(I_mean, I_sd, I_n_est)) %>%
  tab_spanner(label = "Max Recovered", columns = c(R_mean, R_sd, R_n_est)) %>%
  tab_spanner(label = "Max Deceased",  columns = c(D_mean, D_sd, D_n_est)) %>%
  cols_label(
    I_mean=md("$\\bar{x}$"), I_sd=md("$sd$"), I_n_est = html("Sim<sub>est</sub>"), #I_empirical = "Actual",
    R_mean=md("$\\bar{x}$"), R_sd=md("$sd$"), R_n_est = html("Sim<sub>est</sub>"), #R_empirical = "Actual",
    D_mean=md("$\\bar{x}$"), D_sd=md("$sd$"), D_n_est = html("Sim<sub>est</sub>"), #D_empirical = "Actual"
    NetworkSize = "Nodes"
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(everything())
  ) %>%
  opt_row_striping() %>% # row_striping = TRUE, stripe_color = "#D1D1D1"
  fmt_number(
    columns = matches("(_mean|_sd)$"),
    decimals = 0,        # number of decimal places
    use_seps = TRUE       # add thousands separators
  ) %>%
  tab_options(
    table.font.size = px(24)   # or "14pt"
  )


gtsave(
  data = empirical_compare_table,
  filename = paste0("empirical_compare_table_R0-", R0_choice, ".png"),
  path = paste0(input_dir_path, "figs/"),
  vwidth = 6000,   # pixel width
  vheight = 3000,  # pixel height
  expand = 5      # zoom factor, improves resolution
)













