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
