#!/bin/bash
#SBATCH   -p development
#SBATCH   -J r0_sensitivity
#SBATCH   -o r0_sensitivity.o%j
#SBATCH   -e r0_sensitivity.o%j
#SBATCH   –ntasks-per-node 1                # this parameter is ignored
#SBATCH   -N 2
#SBATCH   -t 0:40:00
#SBATCH   -A  TACC-SCI
#SBATCH   --mail-user=emjavan@utexas.edu    # Email for notifications
#SBATCH   --mail-type=all                   # Type of notifications, begin, end, fail, all

# Only have to run once when git repo just cloned
#poetry install --no-root


module load python3
python3 r0_sensitivity_launcher.py
