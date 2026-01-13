library(tidyverse)
library(readxl)

# Read in the data file with the name you want it to have
smast_ex2 <- read_excel('inst/Chaut Example Data_122925.XLS')

# Create the rda file in the data directory
usethis::use_data(
  smast_ex2,
  overwrite = T
)

# Document the dataset - this will o0pen an R script to be edited with metadata
usethis::use_r("smast_ex2")


# Save as internal dataset
usethis::use_data(badparams, overwrite = TRUE)
usethis::use_r("badparams")


# Internal data set fo field dup names
dup_patterns <- c("field dup", "dup", "fd", "duplicate", "f.d", "f/d")
usethis::use_data(dup_patterns, overwrite = TRUE)
usethis::use_r("dup_patterns")


