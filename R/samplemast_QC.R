


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

  # Check for missing site names in the Site column for FD samples

  # Check for <LOD values and change them to "<LOD"

  # For (NO2, SRP, Pt_Co_N) check for analysis time and throw warning if missing

  # Rename weird Param names to something intelligible

  # Logical check for properly formatted Results column (are there characters in any results value)?

  # Change ODUs to their actual purpose

  # Remove parameter values that are not needed (e.g. raw Chl, extract vol)
}
