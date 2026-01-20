# Helper to rename OrderDetails columns
rename_if_present <- function(df, old, new) {
  if (old %in% names(df)) {
    dplyr::rename(df, !!new := !!rlang::sym(old))
  } else {
    df
  }
}

# Reads the first sheet of an Excel file and returns it as a data frame.

read_raw_data <- function() {
  message("Select the data file")

  path <- file.choose()

  message(paste("Data file @",path,"selected"))

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # Read the Excel file
  dat <- readxl::read_excel(path)

  attr(dat, "source_path") <- path

  return(dat)
}

# Helper for appending QC messages, if multiple are needed, into one column
add_flag <- function(current, new) {
  if (any(is.na(current) | current == "")) new else paste(current, new, sep = "; ")
}

add_note <- function(current, new) {
  if (any(is.na(current) | current == "")) new else paste(current, new, sep = "; ")
}

#' Perform QC checks and updates on raw output from SampleMaster
#'
#' Accepts raw SampleMaster input and performs automated QC actions where
#' permissible, or stops with informative errors when manual intervention
#' is required. When the function completes without error, cleaned outputs
#' and QC metadata are written to the same directory as the input file.
#'
#' The following QC actions are performed:
#' \itemize{
#'   \item Removes rows with invalid `Param` values (from `badparams`), if present.
#'   \item Renames `OrderDetails_*` columns to standardized names.
#'   \item Replaces field duplicate values in `Site` with `Location` where available.
#'   \item Errors if any `Site` values are missing and no `Location` is provided.
#'   \item Errors if `CollectTime` is missing for `Param` values SRP, NO2, or PTCoN.
#'   \item Replaces missing `SampleType` with `"Field Dup"` when field duplicates are identified.
#'   \item Flags results below the limit of detection (`<LOD`) in the `Result` column.
#' }
#'
#' @param return_QC_meta Logical. If `TRUE`, writes a QC metadata workbook to the
#' input file directory with the suffix `_QC.xlsx`.
#' @param add_lod_units Optional data frame specifying additional LOD units.
#' Must contain columns `Parameter` and `units`.
#'
#' @details
#' Two Excel workbooks are written to the input data directory if the function
#' completes without error:
#'
#' \itemize{
#'   \item \strong{`*_QC.xlsx`}: A copy of the raw data with QC annotations.
#'   Contains the following columns:
#'   \describe{
#'     \item{QC_flag}{Short codes describing QC actions taken.}
#'     \item{QC_notes}{Detailed descriptions of QC actions.}
#'   }
#'   \item \strong{`*_cleaned.xlsx`}: The final cleaned dataset.
#' }
#' If any QC rule fails, execution stops and no output files are written.
#' Error messages include affected sample IDs when applicable to aid manual correction.
#'
#' @return
#' This function is called for its side effects. It writes cleaned data and
#' QC metadata to disk and returns `NULL` invisibly.
#'
#' @importFrom magrittr %>%
#' @export

clean_sample_data <- function(return_QC_meta = TRUE,
                              add_lod_units = NULL) {
  # -1. Read data
  data <- read_raw_data()
  path <- attr(data, "source_path") # extract path of source data file

  # 0. Input checks, build qc_meta object ----
  if(!is.logical(return_QC_meta)){
    stop(paste('"return_QC_meta" incorrectly specified as',return_QC_meta,
               '\n"return_QC_meta" requires TRUE/FALSE as input'))
  }else if (return_QC_meta) {
    # Create a metadata template: same dimensions and column names as `data`, but all values NA
    qc_meta <- data
    qc_meta$QC_flag <- NA_character_   # create column for filtering on when QC actions are taken
    qc_meta$QC_notes <- NA_character_  # sets all values to NA while keeping column types
    qc_meta <- qc_meta[, c(names(data), "QC_flag","QC_notes")]
  }else{
    qc_meta <- NULL
  }
  # Create qc_check regardless of if qc_meta will be returned
  qc_check <- 0 # used to make sure code rean when no QC_flags are present

  # 1. Remove bad params from data ----
  if (!"Param" %in% names(data)) {
    stop('"Param" column missing from SampleMaster data.')
  }

  # Identify rows to remove
  bad_idx <- data$Param %in% badparams

  # If rows are removed, mark them in the qc_metadata
  if (any(bad_idx)) {
    # Message pasted to console
    bad_Params <- unique(data$Param[bad_idx])
    message(paste('"bad" parameter data removed for these parameters:', paste(bad_Params, collapse = ", ")))

    # qc_meta object updated
    if(!is.null(qc_meta)){
      qc_meta$QC_flag[bad_idx] <- add_flag(qc_meta$QC_flag[bad_idx],
                                           "REMOVED") # Flag message
      qc_meta$QC_notes[bad_idx] <- add_note(qc_meta$QC_notes,
                                            "row_removed: bad parameter") # Note message
      # Update qc_check
      qc_check <- qc_check + 1
    }
  }else {
    message("No bad parameters found in data.")
  }

  # 2. Rename OrderDetails columns ----
  userdets <- c("OrderDetails_User1","OrderDetails_User2","OrderDetails_User3",
                "OrderDetails_User4","OrderDetails_User5")
  # Enter into this chunk if and "OrderDetails" columns exist
  if(any(userdets %in% names(data))){
    present_userdet_cols <- userdets[userdets %in% names(data)]
    # Return a message about which columns were renamed (no way to include in qc_meta)
    message("Order_Details columns present in data were renamed. Renamed column(s): \n - ",
            paste(present_userdet_cols, collapse = ",\n - "))

    data <- data %>%
      rename_if_present("OrderDetails_User1", "Receipt Temp (C)") %>%
      rename_if_present("OrderDetails_User2", "Comments") %>%
      rename_if_present("OrderDetails_User3", "Depth (m)") %>%
      rename_if_present("OrderDetails_User4", "Replicate") %>%
      rename_if_present("OrderDetails_User5", "Mc_T Receipt Temp (C)")
  }

  # 3. Replace Field Duplicate in Site with Location if possible ----
  if (!all(c("Site", "Location") %in% names(data))) {
    stop('"Site" and/or "Location" columns missing from SampleMaster data.')
  }
  # Identify rows where "Site" is labelled as a field duplicate
  # Create an index of locations where dup_patterns is found in "Site"
  idx_dup <- tolower(data$Site) %in% dup_patterns # Don't look at "Location" here, error handling below

  # Extract rows where "dup_patterns" are found in "Site"
  site_duplabel <- data[idx_dup,]

  # Identify rows labelled as duplicates with no Location info to replace with
  no_locations <- site_duplabel[is.na(site_duplabel$Location) | trimws(site_duplabel$Location) == "",]
  # Error if no location data exists to replace FD in "Site"
  if (nrow(no_locations) > 0) {
    err_ids <- unique(no_locations$SampleNumber)

    stop(
      paste0(
        '⚠ Field Duplicate designations identified in the "Site" column.\n\n',
        'The "Location" column is missing information for the following sample IDs:\n\n',
        paste0("  • ", err_ids, collapse = "\n"),
        "\n\nPlease update the data before proceeding."
      ),
      call. = FALSE
    )
  }

  # If field dups in "Site" can be fixed using "Location", do so
  if(nrow(site_duplabel)>0){
    # Log changes if metadata is enabled
    if (!is.null(qc_meta)) {
      qc_meta$QC_flag[idx_dup] <- add_flag(qc_meta$QC_flag[idx_dup], "SITE")
      qc_meta$QC_notes[idx_dup] <- add_note(qc_meta$QC_notes[idx_dup],
                                            paste0("\"Site\" changed from ", data$Site[idx_dup],
                                                   " to ", data$Location[idx_dup]))
    }
    # Make the actual change in the data (not removing rows, so can do the operation here)
    data$Site[idx_dup] <- data$Location[idx_dup]

    # Update qc_check
    qc_check <- qc_check + 1
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

      stop(
        paste0(
          '⚠ Site data are missing with no Location information provided.\n\n',
          'Empty "Site" data found, with no "Location" specified for samples:\n\n',
          paste0("  • ", misserr_ids, collapse = "\n"),
          "Please update the data before proceeding.\n If no Location was provided,",
          "refer to the SOP for what to enter"
        ),
        call. = FALSE
      )
    }else {# If location exists, fill in "Site" with Location
      # Make the actual change in the data (not removing rows, so can do the operation here)
      data$Site[missing_idx] <- data$Location[missing_idx]
      # Update qc_check
      qc_check <- qc_check + 1
      # Update the qc_meta object
      if(!is.null(qc_meta)){
        qc_meta$QC_flag[missing_idx] <- add_flag(qc_meta$QC_flag[missing_idx], "SITE")
        qc_meta$QC_notes[missing_idx] <- add_note(qc_meta$QC_notes[missing_idx],
                                                  paste0("\"Site\" was blank, \"Location\" used instead = "
                                                         , data$Location[missing_idx]))
      }
    }
  }

  # 5. Error for missing collection times for SRP, NO2, PTCoN (Error only, no data mutation) ----
  # Pull out Param values
  paramvals <- unique(tolower(data$Param))

  if (any(paramvals %in% c("srp", "no2", "ptcon"))) {
    time_idx <- tolower(data$Param) %in% c("srp", "no2", "ptcon") # pull out rows with Param = SRP, NO2, or PTCoN
    time_dat <- data[time_idx,]
    ct <- as.character(time_dat$CollectTime) # Convert to character to avoid error with `trimws()`
    missing_times <- time_dat[is.na(ct) | trimws(ct) == "", ] # trimws to catch empty cells

    if (nrow(missing_times)>0) {
      timeerr_ids <- missing_times$SampleNumber # Pull sample ID's

      stop(
        paste0(
          '⚠ Collection times are missing for SRP, NO2, and/or PTCoN\n\n',
          'Affected samples are:\n\n',
          paste0("  • ", timeerr_ids, collapse = "\n"),
          "\nPlease update the data before proceeding.\n If no CollectTime was provided,",
          "refer to the SOP for what to enter"
        ),
        call. = FALSE
      )
    }
  }

  # 6. Replace blank SampleType if possible ----
  if("SampleType" %in% names(data)){
    # If "SampleType" data is missing enter this chunk
    if (any(is.na(data$SampleType) | trimws(data$SampleType) == "")) {
      samptype_missing_idx <- is.na(data$SampleType) | trimws(data$SampleType) == ""

      # Check for Field Duplicates identified in "Site" that match missing "SampleType"
      fd_match_idx <- samptype_missing_idx & idx_dup
      # If any exist, overwrite "SampleType" with "Field Dup"
      if(any(fd_match_idx)){
        # Make the actual change in the data (not removing rows, so can do the operation here)
        data$SampleType[fd_match_idx] <- "Field Dup"
        # Update qc_check
        qc_check <- qc_check + 1
        # Update qc_meta object
        if(!is.null(qc_meta)){
          qc_meta$QC_flag[fd_match_idx] <- add_flag(qc_meta$QC_flag[fd_match_idx], "SAMPLETYPE")
          qc_meta$QC_notes[fd_match_idx] <- add_note(qc_meta$QC_notes[fd_match_idx],
                                                     paste0("\"SampleType\" changed to \"Field Dup\" based on \"Site\""))
        }
      }

      # Re-compile after possible replacement
      samptype_missing_idx <- is.na(data$SampleType) | trimws(data$SampleType) == ""

      # Error if there are still blanks in "SampleType"
      if(any(samptype_missing_idx)){
        sampids_typ_missing <- data$SampleNumber[samptype_missing_idx]

        stop(
          paste0(
            '⚠ Data contains rows with missing Sample Type\n\n',
            'Affected samples are:\n\n',
            paste0("  • ", sampids_typ_missing, collapse = "\n"),
            "\nPlease update the data before proceeding."
          ),
          call. = FALSE
        )
      }
    }

    # Check that field duplicates were specified correctly in SampleType (Error only, no data mutation)
    # Recompute site_duplabel
    dup_idx <- tolower(data$Site) %in% dup_patterns
    dup_samptype <- tolower(data$SampleType[dup_idx])

    if(any(dup_samptype != "field dup")) {
      dup_type_idx <- tolower(data$SampleType[dup_idx]) != 'field dup'
      dup_type_wrong <- data$SampleNumber[dup_idx][dup_type_idx] # First subset to dup_idx then subset that using dup_type_idx

      stop(
        paste0(
          '⚠ Duplicate sample identified in site column, incorrectly labelled in "SampleType" column.\n\n',
          'Affected samples are:\n\n',
          paste0("  • ", dup_type_wrong, collapse = "\n"),
          "\nPlease update the data before proceeding.r"
        ),
        call. = FALSE
      )
    }
  }else { # Error if SampleType is missing
    stop("\"SampleType\" column missing from data.")
  }

  # Remove rows at the end to ensure upstream alignment
  if (!is.null(qc_meta)) {
    # Identify any rows removed during QC
    remove_idx <- !is.na(qc_meta$QC_notes) &
      grepl("^row_removed", qc_meta$QC_notes)
    # Remove rows from data (this is done at the end to keep upstream alignment btwn data and qc_meta)
    if (any(remove_idx)) {
      data <- data[!remove_idx, ]

    }
    # Replace NA's with blanks for ease of use in Excel
    # Have to use this crazy code to keep column types and avoid errors with blanking POSIXct's
    char_cols <- vapply(qc_meta, is.character, logical(1)) # vapply for column selection

    qc_meta[char_cols] <- lapply( # lapply for column mutation
      qc_meta[char_cols],
      function(x) {
        x[is.na(x)] <- ""
        x
      }
    )
  }

  # 8. Update <LOD values in the "Result" column ----
  # Retrieve lod table data using helper
  lod_data <- get_lod()

  # Check LOD units using helper
  lod_data <-
  lod_unit_check(df_lod = lod_data,
                 add_lod_units = add_lod_units, # This helper is where additional LOD units may be added
                 return_LOD_LOQ = "LOD")

  # Update Results column to "<lod" using helper
  lod_res <-
  flag_below_lod(df = data,
                 lod_df = lod_data,
                 df_path = path) # raw data path specified for Error message within helper

  # Only mutate data if Results <LOD were found in the input (helper returns NULL if no data <LOD exist)
  if (!is.null(lod_res)) {

    # apply data mutation
    data <- lod_res$data
    qc_check <- qc_check + 1

    # apply QC logging
    if (!is.null(qc_meta)) {
      qc_meta$QC_flag[lod_res$flag_idx] <-
        add_flag(qc_meta$QC_flag[lod_res$flag_idx], lod_res$flag)

      qc_meta$QC_notes[lod_res$flag_idx] <-
        add_note(qc_meta$QC_notes[lod_res$flag_idx], lod_res$note)
    }
  }
  # Save the metadata to file
  if (return_QC_meta && !is.null(qc_meta)) {
    # Construct QC filename based on original file
    orig_file <- basename(path)
    file_base <- tools::file_path_sans_ext(orig_file)
    qc_file <- file.path(dirname(path), paste0(file_base, "_QC.xlsx"))

    # Write QC metadata to Excel
    writexl::write_xlsx(qc_meta, qc_file)

    message(paste("QC metadata saved to:", qc_file))
  }

  # Print the qc_check regardless of returning the qc_meta object
  if(qc_check>0){
    message(qc_check, ifelse(qc_check == 1, " operation was carried out", " operations were carried out"))

  }else if (qc_check == 0){
    message("No QC operations were necessary. QC_flag and QC_notes columns will be empty if saved."
    )
  }

# 9. Write output data to file  ----
  in_data_name <- tools::file_path_sans_ext(basename(path))
  out_file <- paste0(path,"/",in_data_name, "_cleaned.xlsx")
  writexl::write_xlsx(data,out_file)
  message(paste0("What's this say?",out_file))
  message("Function completed without Errors!")
  return(data)
}


