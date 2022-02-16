## Read in data and assign to variables
for (file in list.files(path = "data-raw/demography", pattern = ".*.csv")) {
  name <- tools::file_path_sans_ext(file)
  temp <- read.csv(file.path("data-raw", "demography", file))
  temp[c("poppercent", "upperbound")] <- lapply(
    temp[c("poppercent", "upperbound")], as.double
  )
  assign(name, temp)
}

## And yes, we have to use it like this because tidyverse
## https://github.com/r-lib/devtools/issues/988
usethis::use_data(BEN, overwrite = TRUE)
usethis::use_data(CMR, overwrite = TRUE)
usethis::use_data(GHA, overwrite = TRUE)
usethis::use_data(HTI, overwrite = TRUE)
usethis::use_data(MOZ, overwrite = TRUE)
usethis::use_data(TZA, overwrite = TRUE)
usethis::use_data(UGA, overwrite = TRUE)
