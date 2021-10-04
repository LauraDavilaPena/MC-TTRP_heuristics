#!/bin/bash
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -t 10:00:00
#SBATCH -p thinnodes

module load gcc
module load R/4.0.2

# 1 5 3 1 300 "TTRP"
Rscript script_run_ALNS.R  $1 $2 $3 $4 $5 $6 100 1000
