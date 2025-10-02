
# Get sim times, pretty fast because there's only one file per param combination
ls Network_*_Node/*/simulation_times.csv >> sim_time_paths.csv

module load tacc-apptainer
apptainer exec $WORK/geospatial_latest.sif R

# 1M population size only
# 900 rows
find Network_250_Node -type d -name "output_sim*" > sim_output_paths_Network_250_Nodes.csv

# 7291 rows
find Network_100_Node -type d -name "output_sim*" > sim_output_paths_Network_100_Nodes.csv

# 61740 rows
find Network_10_Node -type d -name "output_sim*" > sim_output_paths_Network_10_Nodes.csv

# 61485 rows, took 12min 24sec
time find Network_2_Node -type d -name "output_sim*" > sim_output_paths_Network_2_Nodes.csv

# Took way too long since it's closer to like 2M files
#time find Network_1_Node -type d -name "output_sim*" > sim_output_paths_Network_1_Nodes.csv

# ~40sec
time find Network_1_Node/seirs-deterministic_POP-1M_R0-* -type d \
   \( -name 'output_sim[0-9]' -o -name 'output_sim[1-9][0-9]' \) \
   -print > sim_output_paths_Network_1_Nodes_sub100.csv

# Taking a long time, possibly bc it needs to compare all file names
# 17m8.730s
time find Network_1_Node/seir-deterministic_POP-1M_R0-* -type d \
   \( -name 'output_sim[0-9]' -o -name 'output_sim[1-9][0-9]' \) \
   -print >> sim_output_paths_Network_1_Nodes_sub100.csv

# 53m23.675s
time find Network_1_Node/seatird-deterministic_POP-1M_R0-* -type d \
   \( -name 'output_sim[0-9]' -o -name 'output_sim[1-9][0-9]' \) \
   -print >> sim_output_paths_Network_1_Nodes_sub100.csv

# 0m31.113s
time find Network_1_Node/seatird-stochastic_POP-1M_R0-* -type d \
   \( -name 'output_sim[0-9]' -o -name 'output_sim[1-9][0-9]' \) \
   -print >> sim_output_paths_Network_1_Nodes_sub100.csv



# Get a much smaller sample of files to parse
grep -i 'seatird' parsed_file_paths_Network_1_Nodes.csv \
| grep -E 'R0-[0-9]+\.1([^0-9]|$)' \
| grep -E 'output_sim([0-9]|[1-9][0-9])([^0-9]|$)' \
> sim_output_paths_Network_1_Nodes-seatird-r01-sim0_99.csv

grep -i 'seatird' parsed_file_paths_Network_10_Nodes.csv \
| grep -E 'R0-[0-9]+\.1([^0-9]|$)' \
| grep -E 'output_sim([0-9]|[1-9][0-9])([^0-9]|$)' \
> sim_output_paths_Network_10_Nodes-seatird-r01-sim0_99.csv

grep -i 'seatird' parsed_file_paths_Network_100_Nodes.csv \
| grep -E 'R0-[0-9]+\.1([^0-9]|$)' \
| grep -E 'output_sim([0-9]|[1-9][0-9])([^0-9]|$)' \
> sim_output_paths_Network_100_Nodes-seatird-r01-sim0_99.csv

grep -i 'seatird' parsed_file_paths_Network_2_Nodes.csv \
| grep -E 'R0-[0-9]+\.1([^0-9]|$)' \
| grep -E 'output_sim([0-9]|[1-9][0-9])([^0-9]|$)' \
> sim_output_paths_Network_2_Nodes-seatird-r01-sim0_99.csv

grep -i 'seatird' parsed_file_paths_Network_250_Nodes.csv \
| grep -E 'R0-[0-9]+\.1([^0-9]|$)' \
| grep -E 'output_sim([0-9]|[1-9][0-9])([^0-9]|$)' \
> sim_output_paths_Network_250_Nodes-seatird-r01-sim0_99.csv

# Add header row back in, should re-write to keep it to being with
hdr='"simdir","NetworkSize","model","POP","R0","sim_id"'
for f in sim_output_paths_Network_*_Nodes-seatird-r01-sim0_99.csv; do
   head -n1 "$f" | grep -q '^"simdir","NetworkSize","model","POP","R0","sim_id"' && continue
   { echo "$hdr"; cat "$f"; } > "$f.tmp" && mv "$f.tmp" "$f"
done