

badparams <- c("%Solids", "105°C Wt", "550°C Wt", "Avg Std Titrant", "Diameter",
               "Digestion Vol", "Dry Height", "Dry Volume", "Dry Weight", "DW 105°C Wt",
               "E3 SRP", "Eq Wt CaCO3", "Eq Wt DO", "Extracted Vol", "Filtered Vol",
               "Fluoroprobe", "Initial wt", "Mass C", "Moisture Content", "MW 550°C Wt",
               "Raw Conc", "Raw SRP", "Raw TP", "Sample Vol", "Std Conc", "Std Titrant R1",
               "Std Titrant R2", "Std Titrant R3", "Std Vol", "Titrant Vol",
               "Titrant Vol (pH4.5) R1", "Titrant Vol (pH4.5) R2", "Titrant Vol (pH4.5) R3",
               "Titrant Vol (pH5) R1", "Titrant Vol (pH5) R2", "Titrant Vol (pH5) R3",
               "Titrant Vol R1", "Titrant Vol R2", "Titrant Vol R3", "Titrant Vol Total R1",
               "Titrant Vol Total R2", "Titrant Vol Total R3", "Wet Height",
               "Wet Volume", "Wet Weight", "fp_%T", "fp_Ylw")

dup_patterns <- c("field dup", "dup", "fd", "duplicate", "f.d", "f/d")

usethis::use_data(
  badparams,
  dup_patterns,
  internal = TRUE,
  overwrite = TRUE
)
