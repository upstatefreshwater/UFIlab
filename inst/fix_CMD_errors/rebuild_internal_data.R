# 1️⃣ Remove old compiled/internal files (help & sysdata)
unlink("R/sysdata.rda")        # internal data
unlink("R/sysdata.rdb")        # lazy-load database (if exists)
unlink("R/sysdata.rdx")        # lazy-load index (if exists)
unlink("man", recursive = TRUE) # remove old .Rd files (optional, forces re-document)

# 2️⃣ Rebuild internal data
source('data-raw/internal_data.R')

# 3️⃣ Rebuild documentation
devtools::document()

# 4️⃣ Reinstall the package
devtools::install()

# 5️⃣ Restart R (VERY IMPORTANT on Windows)
.rs.restartR()  # If using RStudio
# Or close and reopen R completely

# ✅ Now the package should load cleanly
library(UFIlab)
clean_sample_data()
