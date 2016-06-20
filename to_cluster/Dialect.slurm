#!/bin/bash
#SBATCH --ntasks-per-node=10
#SBATCH --nodes=1
#SBATCH --partition=economy
#SBATCH --account=arab_spring_analysis
#SBATCH --time=4-00:00:00

module load R/openmpi/3.1.1

R --no-save < "Run_model.R" 
