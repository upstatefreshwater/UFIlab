# 1. Retrieve LOD data specified by Lab staff ----
get_lod <- function() {
  # full path inside installed package
  path <- fs::path_package("UFIlab", "extdata", "LOD_values.xlsx")
  # Throw error if LOD file is missing
  if (!fs::file_exists(path)) stop("LOD_values.xlsx file not found at: ", path)

  # read with readxl
  readxl::read_excel(path)
}


# 2. Helper to append row(s) to LOD_values ----
append_lod <- function(new_row, lod_data) {
  if (!is.null(new_row)) {
    # Ensure it's a data frame or tibble
    checkmate::assert_data_frame(new_row, min.rows = 1, ncols = 2)

    # Ensure column names match exactly
    expected_cols <- c("Parameter", "units")
    if (!all(expected_cols %in% colnames(new_row))) {
      stop("new_row must contain columns: ", paste(expected_cols, collapse = ", "))
    }
    # Check each column type
    checkmate::assert_character(new_row$Parameter)
    checkmate::assert_character(new_row$units)
  }

  # Append if not NULL
  if (!is.null(new_row)) {
    data.out <- dplyr::bind_rows(lod_data, new_row)
  }

  return(data.out)
}

# 3. Helper to confirm LOD units against internal data----
lod_unit_check <- function(df_lod,
                           add_lod_units,
                           return_LOD_LOQ = c("LOD", "LOQ", "BOTH")){

  # Validate argument type
  valid_opts <- c("LOD", "LOQ", "BOTH")

  if (!is.character(return_LOD_LOQ) || length(return_LOD_LOQ) != 1 || !return_LOD_LOQ %in% valid_opts) {
    stop(
      sprintf(
        "'return_LOD_LOQ' must be one of %s",
        paste(shQuote(valid_opts), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  lod.data <- df_lod
  # If user wishes to append LOD data update internal check here
  int_lodcheck <- lod_checkdata
  if(!is.null(add_lod_units)){
    int_lodcheck <-  append_lod(new_row = add_lod_units,
                                lod_data = int_lodcheck)
  }
  # Ensure consistent casing for matching
  lod_units <- lod.data %>%
    dplyr::select(Parameter, units) %>%
    dplyr::mutate(Parameter = toupper(Parameter))

  expected_units <- int_lodcheck %>%
    dplyr::select(Parameter, units) %>%
    dplyr::mutate(Parameter = toupper(Parameter))

  # Join LOD file units to expected units and flag mismatches
  unit_check <- lod_units %>%
    dplyr::inner_join(
      expected_units,
      by = "Parameter",
      suffix = c("_lod", "_expected")
    ) %>%
    dplyr::filter(units_lod != units_expected)

  # Throw a single informative error if any mismatches are found
  if (nrow(unit_check) > 0) {
    stop(
      "LOD unit mismatch detected:\n",
      paste(
        sprintf(
          "  %s: %s (LOD file) vs %s (expected)",
          unit_check$Parameter,
          unit_check$units_lod,
          unit_check$units_expected
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  # Once check clear, subset to just LOD or LOQ or keep both
  if(return_LOD_LOQ == 'LOD'){# Return LOD only
    lod.data <- lod.data %>%
      dplyr::select(Parameter,LOD) %>%
      dplyr::mutate(Parameter = toupper(Parameter))
  }else if(return_LOD_LOQ == "LOQ"){ # Return LOQ only
    lod.data <- lod.data %>%
      dplyr::select(Parameter,LOQ) %>%
      dplyr::mutate(Parameter = toupper(Parameter))
  }else { # Return both LOD and LOQ
    lod.data <- lod.data %>%
      dplyr::select(Parameter,LOD,LOQ) %>%
      dplyr::mutate(Parameter = toupper(Parameter))
  }

  # function output
  lod.data
}

# 4. Update results column to 1/2*LOD where values are <LOD

flag_below_lod <- function(df,
                           lod_df,
                           df_path = NULL){
  # Check for the correct columns
  if(!"Result" %in% names(df)){
    stop('"Result" column missing from raw data.')
  }
  if(!"Param" %in% names(df)){
    stop('"Param" column missing from raw data.')
  }

  # Check for corrct column in lod data
  if (!all(c("Parameter", "LOD") %in% names(lod_df))) {
    stop('LOD data are missing "Parameter" and/or "LOD" columns.')
  }

  # Data assignment
  raw_dat <- df %>%
    dplyr::mutate(Param = toupper(Param)) # normalize Param case
  lod.data <- lod_df %>%
    dplyr::mutate(Parameter = toupper(Parameter)) # normalize Parameter case

  # Coerce result column to numeric
  if(!is.numeric(raw_dat$Result)){
    raw_dat$Result <- as.numeric(raw_dat$Result)
    if(anyNA(raw_dat$Result)){
      stop(
        paste0(
          "NAs present in Result column following coercion to numeric.\n\n",
          "Check for text/characters in the \"Result\" column.",
          if (!is.null(df_path)) paste0("\n\nFile: ", df_path),
          "\n\nDebug: LOD_helpers.R — flag_below_lod()"
        ),
        call. = FALSE
      )
    }
  }

  # Subset raw data to just Param + results, join with LOD dataframe
  lod_join <-
    raw_dat %>%
    dplyr::select(Param, Result) %>%
    dplyr::left_join(lod.data, by = c("Param"="Parameter"))

  # Check for missing LOD values
  if(anyNA(lod_join$LOD)){
    missing_lod <- unique(lod_join$Param[is.na(lod_join$LOD)])
    stop(
      "Missing LOD values for parameters: ",
      paste(missing_lod, collapse = ", "),
      ".\nPlease update the \"LOD_values.xlsx\" file!",
      call. = FALSE
    )
  }

  # Create an index of changes made to the data
  idx <- lod_join$Result < lod_join$LOD

  # End execution of no Result < LOD exist
  if (!any(idx)) {
    return(NULL)  # <-- nothing to QC, no changes to raw data
  }

  # Pull out the values that are <LOD
  old_vals <- lod_join$Result[idx]

  # Overwrite the Raw Results where they are <LOD
  raw_dat$Result[idx] <- "<lod"

  # Return data + information
  list(
    data = raw_dat,
    flag_idx = idx,
    flag = "LOD",
    note = paste0(
      "Result < LOD (", old_vals, "); replaced with <lod"
    )
  )
}
