# Helpers located in "LOD_helpers.R"

#' Format SampleMaster Raw Data for Reporting
#'
#' This function reads raw output from SampleMaster software and formats it for dissemination.
#' It performs the following tasks:
#'
#' 1. Reads raw Excel data from SampleMaster.
#' 2. Coerces the `Result` column to numeric (with warning if non-numeric values are present).
#' 3. Pulls the lab’s limit-of-detection (LOD) values from the internal `LOD_values.xlsx` file,
#' to be updated annually by lab staff.
#' 5. Checks that the units in the LOD file match expected units.
#' 6. Replaces results below the LOD with `"<lod"`.
#'
#' @param path Character. Path to the SampleMaster raw Excel file to process.
#' @param add_lod_units Optional tibble or data frame with columns `Parameter` (character)
#' and `units` (character) to append to the internal LOD unit check. To be used only as a
#' temporary stopgap until the internal lod_checkdata object can be updated. Default is `NULL`.
#'
#' @return A tibble identical to the raw SampleMaster data, except:
#'   - The `Result` column is coerced to numeric (unless replaced with `"<lod"`).
#'   - Values below the LOD are replaced with the string `"<lod"`.
#'
#' @details
#' - Units in the LOD file (`LOD_values.xlsx`) are compared against `lod_checkdata`. A mismatch will throw an error.
#' - Missing LOD values for any parameter will also throw an error.
#' - The function returns a modified tibble; it does **not write to disk**.
#'
#' @examples
#' \dontrun{
#' # Process a raw SampleMaster file
#' formatted <- format_sampleMaster("data/raw_samplemaster.xlsx")
#'
#' # Append new LOD unit before formatting
#' new_units <- tibble(Parameter = "Chloride", units = "mg/L")
#' formatted <- format_sampleMaster("data/raw_samplemaster.xlsx",
#'                                  add_lod_units = new_units)
#' }
#'
#' @export


flag_result_belowLOD <- function(path,
                                 add_lod_units = NULL){
  raw_dat <- readxl::read_excel(path)
  # ----------------------------------LOD detection------------------------- ----
  # Pull LOD data from the updateable Excel file
  lod.data <- get_lod() # read from the helper function

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

  # Once check clear, subset to just LOD
  lod.data <- lod.data %>%
    dplyr::select(Parameter,LOD) %>%
    dplyr::mutate(Parameter = toupper(Parameter))

  # Coerce result column to numeric
  if(!is.numeric(raw_dat$Result)){
    raw_dat$Result <- as.numeric(raw_dat$Result)
    if(anyNA(raw_dat$Result)){
      warning('NAs present in Result column following coercion of raw data to numeric,\n',
              'Check for text/characters in "Result" column of input',path)
    }
  }

  #----------------------Update Result values <LOD  -------------- ----
  # Subset raw data to just Param + results, join with LOD dataframe
  lod_join <-
    raw_dat %>%
    dplyr::mutate(Param = toupper(Param)) %>%
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

  # Replace <LOD values with "<lod"
  lesslod_replaced <-
  lod_join %>%   # attach the LOD for each Param
    dplyr::mutate(Result = dplyr::if_else(Result < LOD, "<lod", as.character(Result))) %>%
    dplyr::select(-LOD) # remove the LOD column

  # Replace the Results column in the raw data
  raw_dat$Result <- lesslod_replaced$Result

  return(raw_dat)

}
