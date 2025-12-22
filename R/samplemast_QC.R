#This is a scratch example function




if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)
library(tidyverse)


junk <-
 smast_ex %>%
  dplyr::rename(
    `Receipt Temp (⁰C)`      = any_of("OrderDetails_User1"),
    Comments                 = any_of("OrderDetails_User2"),
    `Depth (m)`              = any_of("OrderDetails_User3"),
    Replicate                = any_of("OrderDetails_User4"),
    `Mc_T Receipt Temp (⁰C)` = any_of("OrderDetails_User5")
  )







  samplemast_format <- function(samplemaster_csv){

  # Convert data to tibble for better error handling
  dat <- tibble::tibble(samplemaster_csv)
  # Perform a check for unrecognized parameters in samplemaster data
  param_check <- unique(dat$Param)
  if(is.null(param_check)){stop('Column "Param" not found in data')}

  if (any(!param_check %in% param_list)) {
    message()
    stop(paste('Unrecognized parameter(s) : "',
               param_check[!param_check %in% param_list],
               '" found in samplemaster input file',
               sep = ''))
    # 'Unidentified parameters detected in samplemaster input file')

  }

  # Identify NULL's in the result column & throw warming that includes Sample # and Param

  # Detect rows where Result is NA, NULL, or empty
  null_rows <- which(
    is.na(dat$Result) |
      dat$Result == "" |
      purrr::map_lgl(dat$Result, is.null)
  )

  # Throw warnings
  if (length(null_rows) > 0) {
    apply(dat[null_rows, ], 1, function(row) {
      warning(
        sprintf(
          'Missing Result detected: Sample %s | Param %s',
          row[['SampleNumber']],
          row[['Param']]
        ),
        call. = FALSE
      )
    })
  }
  # Check for missing site names in the Site column for FD samples

  # Check for <LOD values and change them to "<LOD"

  # For (NO2, SRP, Pt_Co_N) check for analysis time and throw warning if missing

  # Rename weird Param names to something intelligible

  # Logical check for properly formatted Results column (are there characters in any results value)?

  # Change ODUs to their actual purpose
  Data <- Data %>%
    dplyr::rename(
      `Receipt Temp (⁰C)`        = OrderDetails_User1,
      Comments                    = OrderDetails_User2,
      `Depth (m)`                 = OrderDetails_User3,
      Replicate                   = OrderDetails_User4,
      `Mc_T Receipt Temp (⁰C)`   = OrderDetails_User5
    )

  # Remove parameter values that are not needed (e.g. raw Chl, extract vol)


  param_names <- unique(data)

  smastex_filtered <- smast_ex %>%
    dplyr::filter(!Param %in% badparams)

