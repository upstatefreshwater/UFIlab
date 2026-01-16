# Make absolutely sure it isn't attached
if ("UFIlab" %in% loadedNamespaces()) {
  detach("package:UFIlab", unload = TRUE, character.only = TRUE)
}

# Kill stale artifacts
unlink("man", recursive = TRUE)
unlink("NAMESPACE")
unlink("src", recursive = TRUE)     # if you don't have compiled code

.rs.restartR()

# setwd("path/to/UFIlab")  # project root

devtools::document(roclets = c("rd", "namespace"))

devtools::check(
  document = FALSE,   # IMPORTANT
  clean = TRUE
)


# Run this to install on machine w/o git
devtools::build()

# Move this to another machine
# UFIlab_0.?.?.tar.gz
# then install
# install.packages("UFIlab_0.?.?.tar.gz", repos = NULL, type = "source")

