#!/bin/bash
#SBATCH -n 48
#SBATCH --job-name=bawsala
#SBATCH -p extended
#SBATCH -N 1
#SBATCH --time=100:00:00
#SBATCH -o job.ideal1.out
#SBATCH -e job.ideal1.err

module load slurm
module load R/gcc12.2


Rscript --verbose R_Scripts/run_bawsala_v3.R
