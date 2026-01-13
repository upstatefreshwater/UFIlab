# Helper to append warnings
add_warn <- function(current, new) {
  if (current == "" | is.na(current)) {
    new
  } else {
    paste(current, new, sep = "; ")
  }
}


# Read raw Excel data
#
# Reads the first sheet of an Excel file and returns it as a data frame.

read_raw_data <- function(path) {

  # Check that the path is valid
  if (!is.character(path) || length(path) != 1) {
    stop("Error in read_raw_data(): 'path' must be a single character string.")
  }

  if (!file.exists(path)) {
    stop("Error in read_raw_data(): file does not exist at the provided path.")
  }

  # Read the Excel file
  dat <- readxl::read_excel(path)

  # Ensure output is a data frame
  if (!is.data.frame(dat)) {
    stop("Error in read_raw_data(): failed to read Excel file as a data frame.")
  }

  return(dat)
}


#' Clean and validate sample data (simpler, mostly base R)
#'
#' Performs standard cleaning and validation on sample data:
#' * Renames OrderDetails columns to standardized names.
#' * Removes rows with invalid `Param` values (from `badparams`) if present.
#' * Replaces field duplicate site names with their `Location`.
#' * Adds cumulative warnings for:
#'    - Remaining duplicate site indicators (e.g., "dup").
#'    - Missing or blank `Site` values.
#'
#' @param path Character. Complete file location of SampleMaster output file to be processed.
#'
#' @return A cleaned data frame with standardized column names and a `Warning` column listing any issues.
#'
#' @examples
#' test_df <- data.frame(
#'   OrderDetails_User1 = 20,
#'   OrderDetails_User2 = "note",
#'   Site = c("Field Dup", "Station A", NA, ""),
#'   Location = c("Loc1", NA, "Loc3", "Loc4"),
#'   Param = c("NO2", "TP", "SRP", "PtCoN")
#' )
#'
#' clean_sample_data(test_df)
#'
#' @export
clean_sample_data <- function(path) {
  # -1. Read data
  data <- read_raw_data(path)
  # 0. Ensure input is a data frame
  if (!is.data.frame(data)) {
    stop("Error in clean_sample_data(): input 'data' must be a data frame.")
  }

  # 1. Remove bad params if Param exists
  if ("Param" %in% names(data) && exists("badparams")) {
    data <- data[!data$Param %in% badparams, ]
  }

  # 2. Rename OrderDetails columns
  data <- data %>%
    dplyr::rename(
      `Receipt Temp (⁰C)`      = dplyr::any_of("OrderDetails_User1"),
      Comments                 = dplyr::any_of("OrderDetails_User2"),
      `Depth (m)`              = dplyr::any_of("OrderDetails_User3"),
      Replicate                = dplyr::any_of("OrderDetails_User4"),
      `Mc_T Receipt Temp (⁰C)` = dplyr::any_of("OrderDetails_User5")
    )


  dup_text <- dup_patterns

  # 3. Fix duplicate site names
  if (all(c("Site", "Location") %in% names(data))) {
    idx_dup <- tolower(data$Site) %in% dup_text & !is.na(data$Location)
    data$Site[idx_dup] <- data$Location[idx_dup]
  }

  # 4. Initialize Warning column
  if (!"Warning" %in% names(data)) {
    data$Warning <- ""
  }

  # Helper to append warnings
  add_warn <- function(current, new) {
    if (current == "" | is.na(current)) {
      new
    } else {
      paste(current, new, sep = "; ")
    }
  }

  # 5. Warning for remaining duplicates
  dup_idx <- grepl("dup", tolower(data$Site))
  data$Warning[dup_idx] <- mapply(add_warn, data$Warning[dup_idx],
                                  "Check sample type / site name")

  # 6. Warning for missing/blank site names
  missing_idx <- is.na(data$Site) | trimws(data$Site) == ""
  data$Warning[missing_idx] <- mapply(add_warn, data$Warning[missing_idx],
                                      "Missing site name")

  # Replace empty strings with NA
  data$Warning[data$Warning == ""] <- NA_character_

  #normalize parameter
  data$Param <- tolower(data$Param)
  paramvals <- unique(data$Param)

  if (any(paramvals %in% c("srp", "no2", "ptcon"))) {

    collecttimes <- data$CollectTime[data$Param %in% c("srp", "no2", "ptcon")]

    if (any(is.na(collecttimes)) || length(collecttimes) == 0) {
      warning("Collection Time is Missing")
    }
  }



  return(data)
}


