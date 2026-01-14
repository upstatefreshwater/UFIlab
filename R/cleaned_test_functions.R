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


  # 1. Remove bad params from data ----
  if (!"Param" %in% names(data)) {
    stop('"Param" column missing from SampleMaster data.')
  }

  # Identify rows to remove
  bad_idx <- data$Param %in% badparams

  if (any(bad_idx)) {
    bad_dat <- data[bad_idx, ]
    bad_Params <- unique(bad_dat$Param)
    message(paste('"bad" parameter data removed for these parameters:', paste(bad_Params, collapse = ", ")))

    # Remove bad rows
    data <- data[!bad_idx, ]
  } else {
    message("No bad parameters found in data.")
  }

  # 2. Rename OrderDetails columns ----
  data <- data %>%
    rename_if_present("OrderDetails_User1", "Receipt Temp (⁰C)") %>%
    rename_if_present("OrderDetails_User2", "Comments") %>%
    rename_if_present("OrderDetails_User3", "Depth (m)") %>%
    rename_if_present("OrderDetails_User4", "Replicate") %>%
    rename_if_present("OrderDetails_User5", "Mc_T Receipt Temp (⁰C)")

  # 3. Fix duplicate site names ----
  if (!all(c("Site", "Location") %in% dat_colnames)) {
    stop('"Site" and/or "Location" columns missing from SampleMaster data.')
  }
  # Identify rows where "Site" is labelled as a field duplicate
  # Create an index of locations where dup_patterns is found in "Site"
  idx_dup <- tolower(data$Site) %in% dup_patterns # Don't look at "Location" here, error handling below
  # Extract rows whwere "dup_patterns" are found in "Site"
  site_duplabel <- data[idx_dup,]

  # Identify rows labelled as duplicates with no Location info to replace with
  no_locations <- site_duplabel[is.na(site_duplabel$Location) | trimws(site_duplabel$Location) == "",]
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

  # 4. Error for missing/blank site names ----
  missing_idx <- is.na(data$Site) | trimws(data$Site) == "" # trimws = trim white space
  missing_sites <- data[missing_idx,]
  if(nrow(missing_sites)>0){
    # Identify empty site names without Location info
    no_loc <- missing_sites[is.na(missing_sites$Location) | trimws(missing_sites$Location) == "",]
    # Error if missing site w/o Location info
    if(nrow(no_loc)>0){
      misserr_ids <- missing_sites$SampleNumber # Pull sample ID's
      misserr_ids_str <- paste(misserr_ids, collapse = ", ")

      stop(
        paste0(
          '⚠ Site data are missing with no Location information provided.\n',
          'Empty "Site" data found, with no "Location" specified for samples:\n',
          err_ids_str,
          "\nPlease update the data before proceeding.\n If no Location was provided,",
          "refer to the SOP for what to enter"
        ),
        call. = FALSE
      )
    }

  }

  # 5. Check for missing collection times for SRP, NO2, PTCoN
  # Pull out Param values
  paramvals <- unique(tolower(data$Param))

  if (any(paramvals %in% c("srp", "no2", "ptcon"))) {
    time_idx <- tolower(data$Param) %in% c("srp", "no2", "ptcon") # pull out rows with Param = SRP, NO2, or PTCoN
    time_dat <- data[time_idx,]
    missing_times <- time_dat[is.na(time_dat$CollectTime) | trimws(time_dat$CollectTime)==""]

    if (nrow(missing_times)>0) {
      timeerr_ids <- missing_times$SampleNumber # Pull sample ID's
      timeerr_ids_str <- paste(timeerr_ids, collapse = ", ")

      stop(
        paste0(
          '⚠ Collection times are missing for SRP, NO2, and/or PTCoN\n',
          'Affected samples are:\n',
          timeerr_ids_str,
          "\nPlease update the data before proceeding.\n If no CollectTime was provided,",
          "refer to the SOP for what to enter"
        ),
        call. = FALSE
      )
    }


  }

  # 6. Check sample type column contains data

  if(anyNA(data$SampleType) | trimws(data$SampleType) == ""){
    samptype_missing_idx <- is.na(data$SampleType) | trimws(data$SampleType) == ""
    sampids_typ_missing <- data$SampleNumber[samptyp_missing_idx]
    samptype_missing_str <- paste(sampids_typ_missing, collapse = ", ")

    stop(
      paste0(
        '⚠ Data contains rows with missing Sample Type',
        'Affected samples are:\n',
        samptype_missing_str,
        "\nPlease update the data before proceeding.r"
      ),
      call. = FALSE
    )
  }
  # Check that field duplicates were specified correctly in SampleType
  dup_samptype <- tolower(site_duplabel$SampleType)

  if(any(dup_samptype != "field dup")) {
    dup_type_idx <- tolower(site_duplabel$SampleType) != 'field dup'
    dup_type_wrong <- site_duplabel$SampleNumber[dup_type_idx]
    dup_type_str <- paste(dup_type_wrong, collapse = ", ")

    stop(
      paste0(
        '⚠ Duplicate sample identified in site column, incorrectly labelled in "SampleType" column.',
        'Affected samples are:\n',
        dup_type_str,
        "\nPlease update the data before proceeding.r"
      ),
      call. = FALSE
    )
  }

  return(data)
}


