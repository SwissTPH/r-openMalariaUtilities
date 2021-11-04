## NOTE Limit number of files (if used) to 200 per process

.slurmSkeleton <- function(name, memCPU) {
  cat(
    "#!/bin/bash
#SBATCH --job-name=scenario_", name, "
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=", memCPU, "
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


## Divide a sequence x into chunks of size n. Any rest is appended.
splitSeq <- function(x, n) {
  ## Determine number of chunks
  g <- length(x) %/% n
  ## Determine rest, if any
  rest <- length(x) %% n
  ## Sequence of whole groups
  d <- x[1:(n * g)]
  ## Create chunks
  chunks <- split(d, ceiling(seq_along(d) / n))
  ## Add rest, if any
  if (rest != 0) {
    chunks[[paste(length(chunks) + 1)]] <- x[((n * g) + 1):((n * g) + rest)]
  }
  return(chunks)
}
