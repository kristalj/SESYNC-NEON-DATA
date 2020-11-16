# Code to test Get_Dasy_Data

# Load packages
library(tidycensus)
library(FedData) #has to be dev version or won't dl 2016 data
library(tidyverse)
library(raster)
library(sf)
library(glue)

# Test on a single county.
Get_Dasy_Data(stid = "24", ctyid = "001") # Works.
Get_Dasy_Data(stid = "24", ctyid = "003") # Works after removing empty geometries from the pop object.
Get_Dasy_Data(stid = "24", ctyid = "013")

#run through a subset of the fips codes for a sample of counties from different states
states_sample <- sample(unique(fipscodes$stid), 20)
fipscodes_mini <- fipscodes %>% filter(stid %in% states_sample) %>% group_by(stid) %>% slice(1) %>% ungroup

# Submit Slurm job with the 10 counties done across 4 CPUs
library(rslurm)
sjob = slurm_apply(Get_Dasy_Data, fipscodes_mini, jobname = 'DASYtest',
                   nodes = 1, cpus_per_node = 4, pkgs = c("tidycensus", "raster", "tidyverse", "FedData", "sf", "dplyr", "glue"),
                   submit = TRUE)

# Check status (this can be repeated as many times as you want until jstatus$completed is TRUE)
# Warnings will be issued but you can ignore them.
# You can also see whether your job is still running by typing "squeue" into the Terminal tab.
jstatus <- get_job_status(sjob)
jstatus$completed
jstatus$queue

# Get output after job is done. This is just for diagnostics. The actual files are written to /nfs/rswanwick-data/DASY/tifs/
joutput <- get_slurm_out(sjob)
# Run cleanup_files to delete the temporary diagnostic files.
cleanup_files(sjob)

# Also delete the temporarily downloaded files
# This is needed because now that we've created the file download directory manually, it does not automatically go away once the job finishes.
system2('rm', '-r /nfs/rswanwick-data/DASY/temp_files/*') 

# Jobs that did not work for states in CONUS are 23 (ME), 36 (NY), and 56 (WY)
# Test
Get_Dasy_Data(stid = "23", ctyid = "001") # ME
Get_Dasy_Data(stid = "36", ctyid = "001") # NY
Get_Dasy_Data(stid = "56", ctyid = "001") # WY
sjob = slurm_apply(Get_Dasy_Data, data.frame(stid = c("23","36","56"), ctyid="001"), jobname = 'DASYtest2',
                   nodes = 1, cpus_per_node = 4, pkgs = c("tidycensus", "raster", "tidyverse", "FedData", "sf", "dplyr", "glue"),
                   submit = TRUE)

# Some of the codes will not work because they aren't in the continental United States. Either remove them first or just ignore those errors.

# Test slurm_map version