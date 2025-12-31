
#
# # Function to replace character strings with LOD, LOQ, or halfLOD value
# replace_char <- function(x, LOD, LOQ, halfLOD, replace_with_LOD) {
#   if (replace_with_LOD == "halfLOD") {
#     ifelse(tolower(x) == "bd" | grepl("<lod", x, ignore.case = TRUE), as.character(halfLOD), as.character(x))
#   } else if (replace_with_LOD == "LOD") {
#     ifelse(tolower(x) == "bd" | grepl("<lod", x, ignore.case = TRUE), as.character(LOD), as.character(x))
#   } else if (replace_with_LOD == "LOQ") {
#     ifelse(tolower(x) == "bd" | grepl("<loq", x, ignore.case = TRUE), as.character(LOQ), as.character(x))
#   } else if (replace_with_LOD == "halfLOQ") {
#     ifelse(tolower(x) == "bd" | grepl("<loq", x, ignore.case = TRUE), as.character(LOQ/2), as.character(x))
#   } else {
#     warning("Invalid value for replace_with_LOD. Using halfLOD as default.")
#     ifelse(tolower(x) == "bd" | grepl("<lod", x, ignore.case = TRUE), as.character(halfLOD), as.character(x))
#   }
# }

get_lod <- function() {
  # build file name
  file_name <- "LOD_values.xlsx"

  # full path inside installed package
  path <- system.file("lod_files", file_name, package = "UFIlab")

  if (path == "") stop("LOD file not found in package inst/lod_files")

  # read with readxl
  readxl::read_excel(path)
}

update_below_LOD <- function(data, LOD_table, replace_with_LOD = "halfLOD") {
#
#
#   # Function to convert to numeric values
#   to_numeric <- function(x) {
#     as.numeric(as.character(x))
#   }
#
  # Iterate through each column in data
  for (param in colnames(data)) {

    # Find the best matching parameter name from LOD_table$Parameter
    match_idx <- which.max(sapply(LOD_table$Parameter, function(p) grepl(p, param, ignore.case = TRUE)))

    # If no match is found, skip this column
    if (match_idx == 0 || !grepl(LOD_table$Parameter[match_idx], param, ignore.case = TRUE)) {
      next
    }

    param_name <- LOD_table$Parameter[match_idx]
    param_LOD <- LOD_table$LOD[match_idx]
    param_LOQ <- LOD_table$LOQ[match_idx]
    param_halfLOD <- LOD_table$halfLOD[match_idx]

    # Replace "bd" values and values below LOD/LOQ with specified LOD, LOQ, or halfLOD value
    data[[param]] <- replace_char(data[[param]], param_LOD, param_LOQ, param_halfLOD, replace_with_LOD)

    # Convert to numeric
    data[[param]] <- to_numeric(data[[param]])
  }

  return(data)
}
###############################################
xx <-
create_LOD_table()

smast_ex2 %>%
  # select(Param, Result) %>%
  left_join(xx, by = c("Param"="Parameter")) %>%   # attach the LOD for each Param
  mutate(Result = if_else(Result < LOD, "<lod", as.character(Result))) %>%
  select(-LOD) %>% view # remove the LOD column if you don’t want it
