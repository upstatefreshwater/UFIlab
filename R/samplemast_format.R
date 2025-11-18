# Create an explicit list of parameter names to validate samplemaster output
param_list <- c('TP','TDP','SRP','TN','TDN','NOX','tNH3','TSS')

#' Calculate Stream Discharge from Cross-Sectional Measurements
#'
#' Computes total stream discharge (flow) at each sampling location and date
#' using the midsection method (average depth of two adjacent locations), optionally
#' accounting for multiple branches.
#'
#' @param data A data frame or tibble containing the required cross-sectional measurements.
#' @param location Name of the column identifying the sampling location. Default is `"Location"`.
#' @param date Name of the column identifying the date. Default is `"Date"`.
#' @param distance (Required) Name of the column with distance from the stream's edge (in feet). Default is `"Distance from 0 (ft)"`.
#' @param depth (Required) Name of the column with measured depths (in feet). Default is `"Depth (ft)"`.
#' @param velocity (Required)Name of the column with measured water velocities (in ft/s). Default is `"Velocity (ft/s)"`.
#' @param branch (Optional) Name of a numeric column identifying branches (may include any number of stream channels). Default is `NULL`.
#' @param depth_unit Character string identifying units used to measure depth . May be "ft" or "m".
#' @param distance_unit Character string identifying units used to measure segment width. May be "ft" or "m".
#' @param velocity_unit Character string identifying units used to measure velocity. May be "ft/s" or m/s"
#'
#' @return A tibble summarizing total discharge (in cubic feet per second) for each location and date.
#'
#' @details
#' This function applies the **midsection method** to estimate discharge using
#' measurements of depth and velocity across a stream transect. For each
#' segment between two distance points, the segment area is estimated as the
#' product of the segment width and the average depth. Segment discharge is
#' then computed as the product of area and mean velocity. Discharge is summed
#' across segments, and optionally, across branches.
#'
#' @examples
#' # Calculate discharge from example dataset
#' data(fieldvelocity)
#' discharge_results <- calculate_discharge(fieldvelocity,
#'                                          location = "Location",
#'                                          date = "Date",
#'                                          branch = "branch",
#'                                          distance = "Distance from 0 (ft)",
#'                                          depth = "Depth (ft)",
#'                                          velocity = "Velocity (ft/s)",
#'                                          depth_unit = "ft",
#'                                          distance_unit = "ft",
#'                                          velocity_unit = "ft/s")
#'
#' # View a few rows
#' head(discharge_results)
#'
#' @importFrom dplyr group_by across all_of row_number if_else lag ungroup summarise
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @export

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
}

#
######################################
calculate_discharge <- function(data,
                                location = "Location",
                                date = "Date",
                                branch = NULL,    # optional branch identifier
                                distance = "Distance from 0 (ft)",
                                depth = "Depth (ft)",
                                velocity = "Velocity (ft/s)",
                                depth_unit = "ft",
                                distance_unit = "ft",
                                velocity_unit = "ft/s") {

  # ---- Unit checks ----
  allowed_depth_units <- c("ft", "m")
  allowed_distance_units <- c("ft", "m")
  allowed_velocity_units <- c("ft/s", "m/s")

  if (!depth_unit %in% allowed_depth_units) {
    stop("Invalid depth_unit. Allowed values: 'ft', 'm'. Please convert before proceeding.")
  }
  if (!distance_unit %in% allowed_distance_units) {
    stop("Invalid distance_unit. Allowed values: 'ft', 'm'. Please convert before proceeding.")
  }
  if (!velocity_unit %in% allowed_velocity_units) {
    stop("Invalid velocity_unit. Allowed values: 'ft/s', 'm/s'. Please convert before proceeding.")
  }

  if (depth_unit != distance_unit) {
    stop("Depth and distance units must match (both 'ft' or both 'm').")
  }

  if ((depth_unit == "ft" && velocity_unit != "ft/s") ||
      (depth_unit == "m"  && velocity_unit != "m/s")) {
    stop("Velocity unit does not match depth/distance units.
         Use 'ft/s' with 'ft', or 'm/s' with 'm'.")
  }

  # Pick discharge column name
  discharge_colname <- ifelse(depth_unit == "ft", "discharge_ft3_s", "discharge_m3s")

  # Define grouping variables dynamically
  grouping_vars <- c(location, date)
  if (!is.null(branch)) {
    grouping_vars <- c(grouping_vars, branch)
  }

  # ---- Calculate segment discharge ----
  discharge <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
    dplyr::mutate(
      segment = dplyr::row_number(),
      width = dplyr::if_else(segment == 1,
                             !!rlang::sym(distance),
                             !!rlang::sym(distance) - lag(!!rlang::sym(distance))),
      mean_depth = ( !!rlang::sym(depth) + lag(!!rlang::sym(depth)) ) / 2,
      mean_depth = dplyr::if_else(segment == 1, !!rlang::sym(depth), mean_depth),
      mean_depth = dplyr::if_else(segment == max(segment), lag(!!rlang::sym(depth)), mean_depth),
      mean_velocity = ( !!rlang::sym(velocity) + lag(!!rlang::sym(velocity)) ) / 2,
      mean_velocity = dplyr::if_else(segment == 1, !!rlang::sym(velocity), mean_velocity),
      mean_velocity = dplyr::if_else(segment == max(segment), lag(!!rlang::sym(velocity)), mean_velocity),
      area = dplyr::if_else(segment == 1 | segment == max(segment),
                            0.5 * mean_depth * width,
                            width * mean_depth),
      discharge = area * mean_velocity
    ) %>%
    dplyr::ungroup()

  # ---- Summarise discharge per site/date ----
  flow_summary <- discharge %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(location, date)))) %>%
    dplyr::summarise(discharge = sum(discharge), .groups = "drop") %>%
    dplyr::rename(!!discharge_colname := discharge)

  return(flow_summary)
}

