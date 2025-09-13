#!/bin/bash
#SBATCH   -p normal
#SBATCH   -J full_r0_sens
#SBATCH   -o full_r0_sens.o%j
#SBATCH   -e full_r0_sens.o%j
#SBATCH   --ntasks-per-node 1               # this parameter is ignored
#SBATCH   -N 3
#SBATCH   -t 6:00:00
#SBATCH   -A TACC-SCI
#SBATCH   --mail-user=emjavan@utexas.edu    # Email for notifications
#SBATCH   --mail-type=all                   # Type of notifications, begin, end, fail, all

# Only have to run once when git repo just cloned
#poetry install --no-root

module load python3
module load pylauncher
python3 r0_sensitivity_launcher.py
