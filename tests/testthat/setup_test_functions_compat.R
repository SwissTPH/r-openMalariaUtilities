## DEPRECATED
## These are helpers to make some compat tests work

.create_test_full <- function() {
  #' example of 'full' object in experiment

  full <- list()
  full$ futITNcov <- c(.65)
  full$ futIRScov <- c(0, .8)
  full$ EIR <- c(5, 25)
  full$ setting <- c("alpha")
  full$ pop <- 500
  full$ seed <- 1

  return(full)
} # end test full

.create_test_scens <- function() {
  #' example of 'scens' object in experiment
  full <- .create_test_full()
  scens <- expand.grid(full)
  return(scens)
} # end test scens

.create_test_CombinedDat_Aggr <- function() {
  scens <- .create_test_scens()
  scens$ fut <- c(1, 2, 1, 2)
  scens$ HistScen_nr <- c(1, 1, 2, 2)

  ages <- c("0to5", "All")
  years <- c(2005, 2006)
  PR <- c(.15, .25, .35, .45)
  CombinedDat_Aggr <- cbind.data.frame(expand.grid(ages, years, PR), scens)
  colnames(CombinedDat_Aggr)[1:3] <- c("age", "year", "PR")

  CombinedDat_Aggr <- CombinedDat_Aggr %>% dplyr::arrange(HistScen_nr, fut, age)
  CombinedDat_Aggr$ year <- rep(2005:2008, times = 4)
  set.seed(9021)
  CombinedDat_Aggr$ PR <- CombinedDat_Aggr$ PR + rnorm(nrow(CombinedDat_Aggr), 0, .03)

  return(CombinedDat_Aggr)
} # end test combined dat
