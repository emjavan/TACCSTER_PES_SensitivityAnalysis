#/////////////////////////////////////////////////////////////////////
#' Make plots used in Western State Modeling Symposim talk
#' Oct 8, 2025 - Emily M Javan, PhD - Texas Advanced Computing Center
#' 
#/////////////////////////////////////////////////////////////////////

#///////////////////////
#### LOAD LIBRARIES ####
library(tidyverse)
source("WSMS_fns.R")

library(tidycensus) # API Key required
library(tigris)
source("../data/private_input_data/api_keys.R")

#///////////////////////
#### COLOR PALLETES ####
purple_pal = NatParksPalettes::natparks.pals("Arches2")
#"#3A1F46" "#7F4B89" "#B46DB3" "#E3A5D6" "#F3DAE4"
blue2red_pal = NatParksPalettes::natparks.pals("Arches")
# "#1A3D82" "#0C62AF" "#4499F5" "#8FCAFD" "#F2F2F2" "#F0AC7D" "#CD622E" "#B14311" "#832B0F"
rb_pal = c(BASELINE = "#832B0F", VAX = "#1A3D82")  # match your levels
rb_pal_light = c(BASELINE = "#B14311", VAX = "#0C62AF")
rb_pal_light2 = c(BASELINE = "#CD622E", VAX = "#4499F5")

#////////////////
#### FIG DIR ####
fig_dir = "../US_States/figs/"
dir.create(fig_dir, recursive = T)

#/////////////////////////////////
#### MEAN DEATH & CUM HOSP TS ####
# list all matching files
network_files = list.files("../US_States", pattern = "^network_batch-[0-9]+\\.csv$", full.names = TRUE, recursive = TRUE)
df = map_dfr(network_files, read_network)

df_death_mean = df %>%
  group_by(STATE_NAME, SCENARIO, sim_id) %>%
  complete(day = full_seq(c(0,212), 1)) %>%
  fill(D, .direction = "down") %>%
  #replace_na(list(D = 0)) %>%
  ungroup() %>%
  group_by(STATE_NAME, SCENARIO, day) %>%
  summarise(mean_dead = mean(D), .groups = "drop") %>%
  arrange(STATE_NAME, SCENARIO, day) 
# %>%
#   group_by(STATE_NAME, SCENARIO) %>%
#   mutate(mean_dead = cumsum(mean_dead)) %>%
#   ungroup()

# 0. Lag to get new hosps per day then cumulative
# 1. Pad each sim to a full day sequence and carry forward cumulative values
df_cum_padded = df %>%
  group_by(batch_num, STATE_NAME, SCENARIO, sim_id) %>%
  arrange(batch_num, STATE_NAME, SCENARIO, sim_id, day) %>%
  mutate(
    new_hosp = pmax(H - dplyr::lag(H, default = 0), 0),  # daily incident hospitalizations
    cum_hosp = cumsum(new_hosp)                          # cumulative ever hospitalized
  ) %>%
  ungroup() %>%
  group_by(STATE_NAME, SCENARIO, sim_id) %>%
  complete(day = full_seq(c(0,212), 1)) %>%
  fill(H, cum_hosp, new_hosp, .direction = "down") %>%
  replace_na(list(H = 0, cum_hosp = 0, new_hosp = 0)) %>%
  ungroup()

# 2. Average daily incidents, then cumsum -> guarantees monotone mean
df_cum_hosp_mean = df_cum_padded %>%
  group_by(STATE_NAME, SCENARIO, day) %>%
  summarise(mean_new = mean(new_hosp), .groups = "drop") %>%
  arrange(STATE_NAME, SCENARIO, day) %>%
  group_by(STATE_NAME, SCENARIO) %>%
  mutate(mean_cum = cumsum(mean_new)) %>%
  ungroup()


#//////////////////////
#### PLOT STATE TS ####
dir.create(paste0(fig_dir, "State-Sims/"), recursive = T)
all_states = unique(df_death_mean$STATE_NAME)
for(i in 1:length(all_states)){
  state_name = all_states[i]
  
  #--------Cum Deaths----------
  dead_plt = ggplot(df %>% 
                      dplyr::filter(STATE_NAME == state_name),
                    aes(x = day, y = D, group = interaction(SCENARIO, sim_id), color = SCENARIO)) +
    geom_line(alpha = 0.2) +
    geom_line(data = df_death_mean %>% 
                dplyr::filter(STATE_NAME == state_name),
              aes(y = mean_dead, group = SCENARIO), linewidth = 1.2) +
    labs(y = "Cumulative Deaths", x = "Simulation Day",
         title = state_name) +
  scale_color_manual(values = rb_pal) +
    theme_bw(base_size=30)+
    theme(legend.position = "bottom")
  ggsave(
    paste0(fig_dir, "State-Sims/", state_name, "_mean_cum_death_ts.png"),
    dead_plt,
    width=12, height=10, bg="white", units="in", dpi=600
  )

  #--------Cum Hosps----------
  cum_hosp_plt = ggplot(df_cum_padded %>% 
           dplyr::filter(STATE_NAME == state_name),
         aes(x = day, y = cum_hosp, group = interaction(SCENARIO, sim_id), color = SCENARIO)) +
    geom_line(alpha = 0.2) +
    geom_line(data = df_cum_hosp_mean %>% 
                dplyr::filter(STATE_NAME == state_name),
              aes(y = mean_cum, group = SCENARIO), linewidth = 1.2) +
    labs(y = "Cumulative Hospitalizations", x = "Simulation Day",
         title = state_name) +
  scale_color_manual(values = rb_pal_light) +
    theme_bw(base_size=30)+
    theme(legend.position = "bottom")
  ggsave(
    paste0(fig_dir, "State-Sims/", state_name, "_mean_cum_hosp_ts.png"),
    cum_hosp_plt,
    width=12, height=10, bg="white", units="in", dpi=600
  )
} # end loop over states

#///////////////////////////////
#### PLOT RUN TIME vs NODES ####
initial_inf_df = read_csv("../data/all_US_initial_infected.csv") %>%
  mutate(STATE_NAME = str_replace_all(STATE_NAME, " ", "-"))
runtime_files = list.files("../US_States", pattern = "^simulation_times_batch-[0-9]+\\.csv$", full.names = TRUE, recursive = TRUE)
time_summaries = purrr::map_dfr(runtime_files, read_time_stats) %>%
  mutate(parent_dir = basename(dirname(file))) %>%
  tidyr::separate(parent_dir, into = c("STATE_NAME", "SCENARIO"), sep = "_", remove = FALSE) %>%
  left_join(initial_inf_df, by="STATE_NAME") %>%
  mutate(mean_min = mean_sec/60)

brks <- c(5e5, 1e6, 5e6, 1e7, 2e7, 4e7) # based on range(time_summaries$total_pop)
runtime_plt = 
  ggplot(time_summaries, aes(x=total_counties, y=mean_min))+
  geom_point(
    aes(size = total_pop, fill=SCENARIO),
    alpha=0.5,
    shape = 21,                 # filled circle
    color = "black",            # outline
    stroke = 0.6                # outline thickness
  ) +
  scale_fill_manual(values = rb_pal_light) +
  scale_size_area(
    name   = "Population",
    max_size = 12,
    breaks = brks,
    labels = scales::label_number(accuracy = 1, scale_cut = scales::cut_short_scale())  # → 0.5M, 1M, 5M, ...
  ) +
  guides(
    size = guide_legend(override.aes = list(shape = 21, fill = "gray95", color = "black"), label.hjust = 1),
    fill = guide_legend(override.aes = list(shape = 21, color = "black", size=7))
  ) +
  labs(x="Counties per US State", y="Mean Run Time per Sim (min)",
       fill="Scenario")+
  theme_bw(base_size=25)

ggsave(
  paste0(fig_dir, "mean_runtime_min_by_nodes.png"),
  runtime_plt,
  width=14, height=10, bg="white", units="in", dpi=600
)

plotly::ggplotly(runtime_plt)

#/////////////////////////////
#### CHECK TOTAL COUNTIES ####
# 3,144 matches Wikipedia
# 100 more county equivalents in territories beyond DC
wiki_county_count = 3144
if(wiki_county_count != sum(time_summaries$total_counties)/2){
  warning("Counties expected does not match calculted")
}

#/////////////////////////////////////////////
#### MIN-MAX EVERY NODE-LEVEL COMPARTMENT ####
results_dir = "../US_States"
node_dir = paste0(results_dir, "/node_comp_min-max/")
if(!dir.exists(node_dir)){
  dir.create(node_dir)
  county_files = list.files(
    results_dir,
    pattern = "^node_[0-9]{4,5}_batch-[0-9]+\\.csv$",
    recursive = TRUE, full.names = TRUE
  )
  county_files_sort = sort(county_files) # 6288 = 3144*2 as expected
  for(i in 1:length(all_states)){ 
    print(paste0("Began ", all_states[i]))
    # Only files related to single state
    state_specific_files <- grep(
      sprintf("/US_States/%s_", all_states[i]),
      county_files_sort,
      value = TRUE
    )
    
    # Summaries the min and max with event timing per compartment
    outcomes_all = map_dfr(state_specific_files, summarise_one_node_file) %>%
      mutate(county_fips = str_pad(as.character(county_fips), width=5, side="left", pad="0"))
    write.csv(
      outcomes_all,
      paste0(node_dir, all_states[i], "_node_comp_min-max.csv"),
      row.names = F
    )
    
    # Get sum stats of all the min-max values so we collapse 100 sim per scenario
    outcomes_summary = outcomes_all %>%
      group_by(STATE_NAME, SCENARIO, county_fips) %>% # , batch_num = 0 for all sims here
      summarise(
        n_sims = n_distinct(sim_id),
        across(
          .cols  = matches("_(value|day)$"),   # only the min/max value/day fields
          .fns   = list(
            mean   = ~ mean(.x, na.rm = TRUE),
            median = ~ median(.x, na.rm = TRUE),
            sd     = ~ sd(.x, na.rm = TRUE),
            q05    = ~ quantile(.x, 0.05, names = FALSE, type = 7, na.rm = TRUE),
            q25    = ~ quantile(.x, 0.25, names = FALSE, type = 7, na.rm = TRUE),
            q75    = ~ quantile(.x, 0.75, names = FALSE, type = 7, na.rm = TRUE),
            q95    = ~ quantile(.x, 0.95, names = FALSE, type = 7, na.rm = TRUE)),
          .names = "{.col}_{.fn}"),
        .groups = "drop")
    
    write.csv(
      outcomes_summary,
      paste0(node_dir, all_states[i], "_summary_node_compartments.csv"),
      row.names = F
    )
  } # end loop over states
} # end processing all the node specific files

#////////////////////////////
#### PLOT US COUNTY MAPS ####
summary_file = paste0(node_dir, "all_county_min-max_summary.csv")
if(!file.exists(summary_file )){
  node_summary_files = 
    list.files(node_dir, pattern = "summary_node_compartments.csv$", full.names = TRUE, recursive = TRUE)
  if(length(all_states) != length(node_summary_files)){ # should be 51
    warning("Total States expected does not match calculated") }
  
  all_county_summary_df = map_dfr(node_summary_files, 
                                  ~read_csv(.x, col_types = c("county_fips"="c")) %>%
                                    mutate(file_path = .x))
}else{
  all_county_summary_df = read_csv(summary_file)
} # end if summary file exists

# Check to be sure county count correct
if((wiki_county_count*2) != nrow(all_county_summary_df)){
  warning("Counties expected does not match calculted")
}

state_sf = tigris::states(cb = TRUE,   year = 2023, class = "sf") %>%
  tigris::shift_geometry() %>%
  dplyr::filter(!(STUSPS %in% c("AS", "MP", "GU", "PR", "VI")))
  
county_sf = tigris::counties(cb = TRUE, year = 2023, class = "sf") %>%
  mutate(county_fips = paste0(STATEFP, COUNTYFP)) %>%
  tigris::shift_geometry()

init_county_sf = initial_inf_df %>%
  left_join(county_sf %>%
              mutate(STATE_NAME = str_replace_all(STATE_NAME, " ", "-")), 
            by=c("fips"="county_fips", "STATE_NAME"))

county_cum_hosp_df = all_county_summary_df %>%
  dplyr::select(STATE_NAME, SCENARIO, county_fips, cum_hosp_max_value_mean) %>% # file_path, 
  spread(SCENARIO, cum_hosp_max_value_mean) %>%
  rowwise() %>%
  mutate(DIFF_BASE_VAX = BASELINE - VAX,
         DIFF_BASE_VAX = ifelse(DIFF_BASE_VAX<0, 0, DIFF_BASE_VAX) ) %>%
  ungroup() %>%
  gather(SCENARIO, cum_hosp_max_value_mean, -STATE_NAME, -county_fips) %>%
  left_join(county_sf, by="county_fips")

#---------Baseline map----------------
red_increase = c("#EEE8AA", blue2red_pal[6:9])
no_vax_scenario = 
  ggplot(county_cum_hosp_df %>%
         dplyr::filter(SCENARIO=="BASELINE") ) +
  geom_sf(aes(fill=cum_hosp_max_value_mean, geometry=geometry), linewidth = 0.2, color="white") +
  geom_sf(data = state_sf, fill = NA, color = "grey", linewidth = 0.2) +
  geom_sf(data = init_county_sf, aes(geometry=geometry), fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn(
    colors = red_increase,
    trans  = "sqrt",
    name   = "Cum. Hosp."
  ) +
  theme_void(base_size = 16) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )
ggsave(
  paste0(fig_dir, "cumhosp_baseline_map.png"),
  no_vax_scenario,
  width=12, height=9, units="in", dpi=600, bg="white"
)

#---------Vax map----------------
blue_increase = c("#EEE8AA", rev(blue2red_pal[1:4]) ) # 
vax_scenario = 
  ggplot(county_cum_hosp_df %>%
           dplyr::filter(SCENARIO=="VAX") ) +
  geom_sf(aes(fill=cum_hosp_max_value_mean, geometry=geometry), linewidth = 0.2, color="white") +
  geom_sf(data = state_sf, fill = NA, color = "grey", linewidth = 0.2) +
  geom_sf(data = init_county_sf, aes(geometry=geometry), fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn(
    colors = blue_increase,
    trans  = "sqrt",
    name   = "Cum. Hosp."
  ) +
  theme_void(base_size = 16) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )
ggsave(
  paste0(fig_dir, "cumhosp_vax_map.png"),
  vax_scenario,
  width=12, height=9, units="in", dpi=600, bg="white"
)

#---------Diff map----------------
purple_increase = c("#EEE8AA", rev(purple_pal)[2:5]) # "#EEE8AA", "#FFFACD",
diff_scenario = 
  ggplot(county_cum_hosp_df %>%
           dplyr::filter(SCENARIO=="DIFF_BASE_VAX") ) +
  geom_sf(aes(fill=cum_hosp_max_value_mean, geometry=geometry), linewidth = 0.2, color="white") +
  geom_sf(data = state_sf, fill = NA, color = "grey", linewidth = 0.2) +
  geom_sf(data = init_county_sf, aes(geometry=geometry), fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn(
    colors = purple_increase,
    trans  = "sqrt",
    name   = "Cum. Hosp."
  ) +
  theme_void(base_size = 16) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )
ggsave(
  paste0(fig_dir, "cumhosp_diff_map.png"),
  diff_scenario,
  width=12, height=9, units="in", dpi=600, bg="white"
)


#////////////////////////////////////////////////
#### MIN-MAX EVERY NETWORK-LEVEL COMPARTMENT ####
network_dir = paste0(results_dir, "/network_comp_min-max/")
if(!dir.exists(network_dir)){
  dir.create(network_dir)
  network_min_max_df = map_dfr(network_files, summarise_one_network_file)
  write.csv(
    network_min_max_df,
    paste0(network_dir, "network_comp_min-max.csv"),
    row.names = F
  )
  
  # Get sum stats of all the min-max values so we collapse 100 sim per scenario
  eps = 0.05
  z = qnorm(1 - (eps/2)) # ~1.96 and is dependent on 1-eps = % variation captured
  network_summary = network_min_max_df %>%
    group_by(STATE_NAME, SCENARIO) %>% # , batch_num
    summarise(
      n_sims = n_distinct(sim_id),
      across(
        .cols  = matches("_(value|day)$"),   # only the min/max value/day fields
        .fns   = list(
          mean   = ~ mean(.x, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          sd     = ~ sd(.x, na.rm = TRUE),
          q05    = ~ quantile(.x, 0.05, names = FALSE, type = 7, na.rm = TRUE),
          q25    = ~ quantile(.x, 0.25, names = FALSE, type = 7, na.rm = TRUE),
          q75    = ~ quantile(.x, 0.75, names = FALSE, type = 7, na.rm = TRUE),
          q95    = ~ quantile(.x, 0.95, names = FALSE, type = 7, na.rm = TRUE),
          n_est  = ~ {
            mu <- mean(.x, na.rm = TRUE)
            s  <- sd(.x,   na.rm = TRUE)
            if (is.na(s) || s == 0) 1L # ceiling bc min sims has to be at least 1
            else if (is.na(mu) || mu == 0) ceiling((z * s)^2)
            else ceiling(((z * s) / (eps * mu))^2)
          }),
        .names = "{.col}_{.fn}"),
      .groups = "drop")
  
  write.csv(
    network_summary,
    paste0(network_dir, "summary_network_compartments.csv"),
    row.names = F
  )
}else{
  network_min_max_df = read_csv(paste0(network_dir, "network_comp_min-max.csv"))
  network_summary    = read_csv(paste0(network_dir, "summary_network_compartments.csv"))
} # end if network compartment summary exists

#///////////////////////////
#### PLOT HOW MANY MORE SIMS ####
cum_hosp_only = network_summary %>%
  dplyr::select(STATE_NAME, SCENARIO, contains("cum_hosp_max_value")) %>%
  dplyr::select(STATE_NAME, SCENARIO, contains(c("mean", "sd", "n_est") )) %>%
  left_join(initial_inf_df, by="STATE_NAME")

sim_brks = c(1e0, 1e1, 2e1, 3e1, 4e1)
nsim_plt = 
  ggplot(cum_hosp_only, aes(x=total_counties, y=cum_hosp_max_value_n_est))+
  geom_point(
    aes(size = init_inf_per_1M, fill=SCENARIO),
    alpha=0.5,
    shape = 21,                 # filled circle
    color = "black",            # outline
    stroke = 0.6                # outline thickness
  ) +
  scale_fill_manual(values = rb_pal_light2) +
  scale_size_area(
    name   = "Init Infected",
    max_size = 12,
    breaks = sim_brks,
    labels = scales::label_number(accuracy = 1, scale_cut = scales::cut_short_scale())  # 1M, 5M, ...
  ) +
  guides(
    size = guide_legend(override.aes = list(shape = 21, fill = "gray95", color = "black"), label.hjust = 1),
    fill = guide_legend(override.aes = list(shape = 21, color = "black", size=7))
  ) +
  labs(x="Counties per US State", y="Est. Sims Needed",
       fill="Scenario")+
  theme_bw(base_size=25)

ggsave(
  paste0(fig_dir, "est_sims_needed.png"),
  nsim_plt,
  width=14, height=10, bg="white", units="in", dpi=600
)

plotly::ggplotly(nsim_plt)


#/////////////////////
#### VAX DOSES ####
day0 = as.Date("2024-10-01")
vax_dose_total = read_csv("../data/all_US_weekly_vax_distribution.csv") %>%
  mutate(STATE_NAME = str_replace_all(State, " ", "-")) %>%
  dplyr::select(-State)

vax_dose_age = read_csv("../data/all_US_weekly_vax_adult-ped.csv") %>%
  mutate(STATE_NAME = str_replace_all(State, " ", "-")) %>%
  dplyr::select(-State) %>%
  group_by(STATE_NAME, WeekEnd) %>%
  mutate(ReleaseDay = as.integer(WeekEnd - day0)) %>%
  ungroup() %>%
  left_join(vax_dose_total, by=c("STATE_NAME", "ReleaseDay")) %>%
  mutate( AgeGroupVE = paste0(AgeGroup, ", VE=", (VE_Inpatient*100), "%" ))

dir.create(paste0(fig_dir, "State-VE/"))
for(i in 1:length(all_states)){
  state = all_states[i]
  
  state_dose_long = vax_dose_age %>%
    dplyr::filter(STATE_NAME==state) %>%
    pivot_longer(c(TotalVax, TotalFullProtect),
                 names_to = "Series", values_to = "Value") %>%
    mutate(Series = recode(Series,
                           TotalVax = "Doses Given",
                           TotalFullProtect = "Doses 100% VE"))
  
  # label is 20% below the max
  y_top = (max(state_dose_long$Value, na.rm = TRUE)*0.8)
  
  state_vax_plt =
    ggplot(state_dose_long, aes(x = WeekEnd, y = Value, 
                              color = AgeGroupVE, linetype = Series,
                              group = interaction(AgeGroupVE, Series))) +
    geom_line(size=2) +
    geom_vline(xintercept = day0, linetype = "dotted",
      color = "grey30", linewidth = 0.5, show.legend = FALSE
    ) +
    annotate("label",
             x = day0, y = y_top, label = "Sim Day 0",
             vjust = -0.6, color = "grey30", fontface = "bold", size = 10) +
    scale_color_manual(values = c("#CD622E", "#4499F5"), name = NULL) +
    scale_linetype_manual(
      values = c("Doses Given" = "solid",
                 "Doses 100% VE" = "dashed"), name = NULL,
      breaks = c("Doses Given", "Doses 100% VE"),
    ) +
    labs(y="2024-25 Influenza Vaccine Doses", title=state,
         x="Week Ending Date")+
    scale_y_continuous(labels = scales::label_number(accuracy = 1, scale_cut = scales::cut_short_scale()))+
    coord_cartesian(clip = "off") +
    guides(
      color   = guide_legend(order = 1, override.aes = list(linetype = "solid", linewidth = 3)),
      linetype= guide_legend(order = 2, override.aes = list(color = "black", linewidth=1 ))
    ) +
    theme_bw(base_size = 30) +
    theme(legend.position = "bottom")

  ggsave(
    paste0(fig_dir, "State-VE/", state, "_VE_ts.png"),
    state_vax_plt,
    width=12, height=10, bg="white", units="in", dpi=600
  )
} # end loop over states


#### HIGH RISK POP ####
high_risk_age = read_csv("../data/all_US_high-risk-ratios-detailed.csv") %>%
  mutate(age_group = factor(age_group, 
                            levels=c("0-4", "5-17", "18-49", "50-64", "65+")
                            ))

high_risk_heatmap = 
  ggplot(high_risk_age, aes(x=age_group, y=STATE_NAME , fill=(frac_high_risk*100) ))+
  geom_tile(color = "white")+
  scale_y_discrete(limits = function(x) rev(x)) +
  scale_fill_gradientn(
    colors = red_increase,
    #trans  = "sqrt",
    name   = "% High Risk",
    limits=c(0,100)
  ) +
  geom_text(aes(label = round(frac_high_risk*100, 0)), color = "black", size = 4)+
  labs(x=NULL, y=NULL)+
  #coord_fixed() +
  guides(fill = guide_colourbar(barheight = 0.5,
                                barwidth = 20))+
  theme_classic(base_size = 15) +
  theme(legend.position = "bottom")
  

ggsave(
  paste0(fig_dir, "high_risk_heatmap.png"),
  high_risk_heatmap,
  width=8, height=11, bg="white", units="in", dpi=600
)

