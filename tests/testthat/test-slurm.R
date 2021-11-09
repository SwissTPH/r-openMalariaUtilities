test_that("splitSeq works (no rest)", {
  actual <- splitSeq(1:20, 5)
  expected <- list(
    "1" = 1:5,
    "2" = 6:10,
    "3" = 11:15,
    "4" = 16:20
  )
  expect_equal(actual, expected)
})

test_that("splitSeq works (with rest)", {
  actual <- splitSeq(1:21, 5)
  expected <- list(
    "1" = 1:5,
    "2" = 6:10,
    "3" = 11:15,
    "4" = 16:20,
    "5" = 21
  )
  expect_equal(actual, expected)
})

test_that(".slurmOptions works", {
  actual <- paste(capture.output(
    .slurmOptions(
      jobName = "test", ntasks = "1", memCPU = "512", output = "output",
      error = "error", array = "400", time = "30min", qos = "30min"
    )
  ), sep = "", collapse = "\n")
  expected <- "#!/bin/bash
#SBATCH --job-name=test
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=512
#SBATCH --output=output_%A_%a.log
#SBATCH --error=error_%A_%a_error.log
#SBATCH --array=1-400
#SBATCH --time=30min
#SBATCH --qos=30min"

  expect_equal(actual, expected)
})

test_that("writeSlurm works", {
  actual <- paste(capture.output(
    writeSlurm(
      jobName = "test", ntasks = "1", memCPU = "512", output = "output",
      error = "error", array = "400", time = "30min", qos = "30min",
      pre = list("module purge", "module load R/foo"),
      cmd = list("Rscript foo.R"), post = list("echo \"Done!\"")
    )
  ), sep = "", collapse = "\n")
  expected <- "#!/bin/bash
#SBATCH --job-name=test
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=512
#SBATCH --output=output_%A_%a.log
#SBATCH --error=error_%A_%a_error.log
#SBATCH --array=1-400
#SBATCH --time=30min
#SBATCH --qos=30min
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 0)

module purge
module load R/foo

Rscript foo.R

echo \"Done!\""

  expect_equal(actual, expected)
})
