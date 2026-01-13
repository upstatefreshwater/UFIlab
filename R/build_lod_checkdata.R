lod_checkdata <-
tibble(Parameter = c('TP', 'TDP', 'SRP', 'Tn_L','DOC','POC','tNH3','NOx','TSS','FSS','VSS','SiO2','SC_L','TN','TDN','Chla'),
       units     = c('ugL','ugL', 'ugL', 'NTU', 'mgL','mgL','ugL', 'ugL','mgDW','mgDW','mgDW','mgL','uScm','ugL','ugL','ugL'))

# Create the rda file in the data directory
usethis::use_data(
  lod_checkdata,
  overwrite = T
)

# Document the dataset - this will o0pen an R script to be edited with metadata
usethis::use_r("lod_checkdata")
