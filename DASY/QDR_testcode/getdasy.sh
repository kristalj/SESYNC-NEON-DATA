#!/bin/bash
#SBATCH --nodes=1
#SBATCH --job-name=DASYtest

Rscript --vanilla /research-home/qread/group_repos/SESYNC-NEON-DATA/DASY/Get_Dasy_Data_parallel.R 

