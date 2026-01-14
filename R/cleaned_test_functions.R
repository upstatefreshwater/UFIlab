# Helper function to update a value and log the change
update_value <- function(df, meta = NULL, row, col, new_value) {
  old_value <- df[[col]][row]

  # Update the main data
  df[[col]][row] <- new_value

  # Log change only if metadata is provided
  if (!is.null(meta)) {
    meta[[col]][row] <- paste0("Changed from ", old_value, " to ", new_value)
  }

  # Return updated objects
  list(data = df, qc_meta = meta)
}


# Helper to rename OrderDetails columns
rename_if_present <- function(df, old, new) {
  if (old %in% names(df)) {
    dplyr::rename(df, !!new := !!rlang::sym(old))
  } else {
    df
  }
}

# Read raw Excel data
#
# Reads the first sheet of an Excel file and returns it as a data frame.

read_raw_data <- function() {
  message("Select the data file")

  path <- file.choose()

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # Read the Excel file
  dat <- readxl::read_excel(path)

  attr(dat, "source_path") <- path

  return(dat)
}
# Helper to append warnings
add_warn <- function(current, new) {
  if (current == "" | is.na(current)) {
    new
  } else {
    paste(current, new, sep = "; ")
  }
}

#' Perform QC checks, and updates, on raw output from SampleMaster
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
#' @importFrom magrittr %>%
#' @export
clean_sample_data <- function(return_QC_meta = TRUE) {
  # -1. Read data
  data <- read_raw_data()
  path <- attr(data, "source_path") # extract path of source data file

  # Read column names to object for use in downstream logic
  dat_colnames <- names(data)

  # 0. Confirm argument type
  if(!is.logical(return_QC_meta)){
    stop(paste('"return_QC_meta" incorrectly specified as',return_QC_meta,
    '\n"return_QC_meta" requires TRUE/FALSE as input'))
  }else if (return_QC_meta) {
    # Create a metadata template: same dimensions and column names as `data`, but all values NA
    qc_meta <- data
    qc_meta[] <- NA  # sets all values to NA while keeping column types
  }


  # 1. Remove bad params if Param exists
  if ("Param" %in% names(data) && exists("badparams")) {
    data <- data[!data$Param %in% badparams, ]
  }else{
    if(!exists("badparams")){stop('Contact DaveA, internal data "badparams" is missing.')}
    stop('"Param" column missing from SampleMaster data.')
  }

  # 2. Rename OrderDetails columns
  data <- data %>%
    rename_if_present("OrderDetails_User1", "Receipt Temp (⁰C)") %>%
    rename_if_present("OrderDetails_User2", "Comments") %>%
    rename_if_present("OrderDetails_User3", "Depth (m)") %>%
    rename_if_present("OrderDetails_User4", "Replicate") %>%
    rename_if_present("OrderDetails_User5", "Mc_T Receipt Temp (⁰C)")

  # 3. Fix duplicate site names
  if (!all(c("Site", "Location") %in% dat_colnames)) {
    stop('"Site" and/or "Location" columns missing from SampleMaster data.')
  }
  # Identify rows where "Site" is labelled as a field duplicate
  # Create an index of locations where dup_patterns is found in "Site"
  idx_dup <- tolower(data$Site) %in% dup_patterns # Don't look at "Location" here, error handling below
  # Extract rows whwere "dup_patterns" are found in "Site"
  site_duplabel <- data[idx_dup,]

  # Identify rows labelled as duplicates with no Location info to replace with
  no_locations <- site_duplabel[is.na(site_duplabel$Location) | site_duplabel$Location == "",]
  # Error if no location data exists to replace FD in "Site"
  if (nrow(no_locations) > 0) {
    err_ids <- no_locations$SampleNumber
    err_ids_str <- paste(err_ids, collapse = ", ")

    stop(
      paste0(
        '⚠ Field Duplicate designations identified in "Site" column.\n',
        'The "Location" column is missing information for the following Field Duplicate sample IDs:\n',
        err_ids_str,
        "\nPlease update the data before proceeding."
      ),
      call. = FALSE
    )
  }

  # If field dups in "Site" can be fixed using "Location", do so ()
  if(nrow(site_duplabel)>0){
    # Log changes if metadata is enabled
    if (!is.null(qc_meta)) {
      qc_meta$Site[idx_dup] <- paste0("Changed from ", data$Site[idx_dup],
                                      " to ", data$Location[idx_dup])
    }
    # Make the actual change in the data
    data$Site[idx_dup] <- data$Location[idx_dup]
  }

  # 4. Initialize Warning column
  if (!"Warning" %in% names(data)) {
    data$Warning <- ""
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


