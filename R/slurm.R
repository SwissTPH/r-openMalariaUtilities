.slurmSkeleton <- function() {
  cat(
    "#!/bin/bash
#SBATCH --job-name=scenario_", nameExperiment, "
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=", mem, "
#SBATCH --output=", name_e, "scenario_%A.LOG
#SBATCH --error=", name_e, "scenario_%A.o
#SBATCH --array=1-", number_loops, "
#SBATCH --time=", time, "
#SBATCH --qos=", qos, "
module purge
## FIXME I am not sure if we should hardcode a R version here...
##       But it unfucks the cluster submission for the time being.
module load R/4.0.3-foss-2018b
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 0 )
Rscript ", file.path(ExperimentDir, "write_scen_code.R"), " $ID",
sep = "", file = submit_name
)

}
