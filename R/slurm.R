### Generate SLURM compatible bash scripts

## Right now, we simply paste these files togeter via cat(). This is working,
## but not flexible.

## NOTE Limit number of files (if used) to 200 per process

##' @title Generate top section of SLURM script
##' @param jobName Name of the job
##' @param ntasks Number of tasks
##' @param memCPU RAM per CPU
##' @param output Output file
##' @param error Error log file
##' @param array Array range
##' @param time Maximum time
##' @param qos Quality of service
##' @keywords internal
.slurmOptions <- function(jobName, ntasks = NULL, memCPU = NULL, output = NULL,
                          error = NULL, array = NULL, time = NULL, qos = NULL) {
  ## Header and job name
  paste0(
    "#!/bin/bash", "\n",
    "#SBATCH --job-name=", jobName, "\n",
    ## Number of tasks per CPU
    if (!is.null(ntasks)) {
      paste0("#SBATCH --ntasks=", ntasks, "\n")
    },
    ## RAM per CPU
    if (!is.null(memCPU)) {
      paste0("#SBATCH --mem-per-cpu=", memCPU, "\n")
    },
    ## Output log file destination
    if (!is.null(output)) {
      paste0("#SBATCH --output=", output, "_%A_%a.log", "\n")
    },
    ## Error log file destination
    if (!is.null(error)) {
      paste0("#SBATCH --error=", error, "_%A_%a_error.log", "\n")
    },
    ## Job array
    if (!is.null(array)) {
      paste0("#SBATCH --array=1-", array, "\n")
    },
    ## Time limit
    if (!is.null(time)) {
      paste0("#SBATCH --time=", time, "\n")
    },
    ## Quality of service
    if (!is.null(qos)) {
      paste0("#SBATCH --qos=", qos, "\n")
    },
    sep = ""
  )
}

##' @title Write SLURM script to file
##' @param jobName Name of the job
##' @param ntasks Number of tasks
##' @param memCPU RAM per CPU
##' @param output Output file
##' @param error Error log file
##' @param array Array range
##' @param time Maximum time
##' @param qos Quality of service
##' @param pre List of commands before main command
##' @param cmd Main command
##' @param post List of commands after main command
##' @param file Filename
##' @keywords internal
.writeSlurm <- function(jobName, ntasks = NULL, memCPU = NULL, output = NULL,
                        error = NULL, array = NULL, time = NULL, qos = NULL,
                        pre = NULL, cmd = NULL, post = NULL, file = NULL) {
  cat(
    .slurmOptions(jobName, ntasks, memCPU, output, error, array, time, qos),
    if (!is.null(array)) {
      paste0("ID=$(expr ${SLURM_ARRAY_TASK_ID} - 0)", "\n")
    },
    "\n",
    if (!is.null(pre)) {
      paste0(unlist(pre), "\n")
    },
    "\n",
    if (!is.null(cmd)) {
      paste0(unlist(cmd), "\n")
    },
    "\n",
    if (!is.null(post)) {
      paste0(unlist(post), "\n")
    },
    sep = "",
    file = if (!is.null(file)) {
      file
    } else {
      ""
    }
  )
}


##' @title Divide a sequence x into chunks of size n. Any rest is appended.
##' @param x Sequence like 1:10
##' @param n Chunk size
##' @export
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
