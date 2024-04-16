#!/bin/bash
#SBATCH	--ntasks=1
#SBATCH --cpus-per-task=128
#SBATCH --time=72:00:00
#SBATCH -o /home/rmk7/ARP_Research/job.ideal1.out
#SBATCH -e /home/rmk7/ARP_Research/job.ideal1.err

module purge

export FITTYPE=${SLURM_ARRAY_TASK_ID}
export DATATYPE=all
export TREEDEPTH=12
export OLLAMA_MODELS="/scratch/rmk7/ollama_models"

module load R/4.2.1
module load gcc/9.2.0
module load openblas/0.3.12
module load lapack/3.9.0
module load intel-mkl/2022.1.0 
module load libGl/1.0
module load llvm/11.0.1
module load cmake/3.18.4
module load eigen/3.3.8
module load openmpi/4.1.1rc1 
module load udunits/2.2.24
module load tcl/8.6.5
module load libpng/1.6.37
module load libxml2/2.9.10
module load lua/5.3.5
module load boost/1.74.0
module load bzip2/1.0.8
module load cairo/1.16.0
module load curl/7.72.0
module load rstudio-server/2022.07.2

Rscript --verbose /home/rmk7/ARP_Research/R_Scripts/run_bawsala_v3.R
