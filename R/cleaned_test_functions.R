library(dplyr)
library(stringr)

#' Read an Excel file
#'
#' Reads the first sheet of an Excel file and returns it as a tibble.
#'
#' @param file_path Character. Path to your Excel file (e.g., "data/myfile.xlsx").
#' @return A tibble containing the data from the first sheet.
#' @examples
#' # Read Excel file
#' mydata <- read_raw_data("data/myfile.xlsx")
#' @export
read_raw_data <- function(file_path) {

  # Check that the file exists
  stopifnot(is.character(file_path), length(file_path) == 1, file.exists(file_path))

  # Read the first sheet of the Excel file
  dat <- readxl::read_excel(file_path)

  return(dat)
}

# -------------------------
# Constants
# -------------------------

# Common patterns indicating field duplicate samples
dup_patterns <- c("field dup", "dup", "fd", "duplicate", "f.d", "f/d")

# -------------------------
# Functions
# -------------------------

#' Rename OrderDetails user columns
#'
#' Renames OrderDetails user fields to standardized, readable column
#' names. If a \code{Param} column exists, rows containing values listed in
#' \code{badparams} are removed prior to renaming.
#'
#' @param data A data frame containing OrderDetails user columns.
#'
#' @return A data frame with OrderDetails columns renamed. If present,
#'   rows with invalid parameters are removed.
#'
#' @details
#' This function is pipeline-safe and will silently skip renaming for any
#' OrderDetails columns that are not present in the input data.
#'
#' @examples
#' test_df <- data.frame(
#'   OrderDetails_User1 = 10,
#'   OrderDetails_User2 = "note"
#' )
#'
#' rename_od(test_df)
#'
#' @export
rename_od <- function(data) {
  stopifnot(is.data.frame(data))

  if ("Param" %in% names(data)) {
    data <- data %>%
      dplyr::filter(!.data$Param %in% badparams)
  }

  data %>%
    dplyr::rename(
      `Receipt Temp (⁰C)`      = dplyr::any_of("OrderDetails_User1"),
      Comments                 = dplyr::any_of("OrderDetails_User2"),
      `Depth (m)`              = dplyr::any_of("OrderDetails_User3"),
      Replicate                = dplyr::any_of("OrderDetails_User4"),
      `Mc_T Receipt Temp (⁰C)` = dplyr::any_of("OrderDetails_User5")
    )
}

#' Replace field duplicate site names with location
#'
#' Replaces common field duplicate site labels (e.g., "dup", "field dup")
#' with the corresponding \code{Location} value when available.
#'
#' @param data A data frame containing \code{Site} and \code{Location}
#'   columns.
#'
#' @return A data frame with field duplicate site names replaced by their
#'   associated location values where applicable.
#'
#' @details
#' Site names are only replaced when a non-missing \code{Location} value
#' exists. Matching is case-insensitive.
#'
#' @examples
#' test_df <- data.frame(
#'   Site = c("Field Dup", "Station A"),
#'   Location = c("Loc1", NA)
#' )
#'
#' fix_field_dup_site(test_df)
#'
#' @export
fix_field_dup_site <- function(data) {
  stopifnot(
    is.data.frame(data),
    all(c("Site", "Location") %in% names(data))
  )

  data %>%
    dplyr::mutate(
      Site = dplyr::case_when(
        stringr::str_to_lower(Site) %in% dup_patterns &
          !is.na(Location) ~ Location,
        TRUE ~ Site
      )
    )
}

#' Flag remaining duplicate site names
#'
#' Adds a warning flag to records whose \code{Site} value still contains
#' a duplicate indicator (e.g., "dup") after site name cleanup.
#'
#' @param data A data frame containing a \code{Site} column.
#'
#' @return A data frame with a \code{Warning} column indicating potential
#'   duplicate site names.
#'
#' @examples
#' test_df <- data.frame(
#'   Site = c("Great Gully DUP", "Station A")
#' )
#'
#' add_dup_warning(test_df)
#'
#' @export
add_dup_warning <- function(data) {
  stopifnot(
    is.data.frame(data),
    "Site" %in% names(data)
  )

  data %>%
    dplyr::mutate(
      Warning = dplyr::if_else(
        !is.na(Site) &
          stringr::str_detect(stringr::str_to_lower(Site), "dup"),
        "Check sample type / site name",
        NA_character_
      )
    )
}

#' Flag missing or blank site names
#'
#' Adds a warning flag for records with missing or blank \code{Site}
#' values.
#'
#' @param data A data frame containing a \code{Site} column.
#'
#' @return A data frame with a \code{Warning} column indicating missing
#'   site names.
#'
#' @details
#' If a \code{Warning} column does not already exist, it is created.
#' Existing warning values are preserved unless the site name is missing.
#'
#' @examples
#' test_df <- data.frame(
#'   Site = c("Station 1", NA, "", "  ")
#' )
#'
#' add_missing_site_warning(test_df)
#'
#' @export
add_missing_site_warning <- function(data) {
  stopifnot(
    is.data.frame(data),
    "Site" %in% names(data)
  )

  if (!"Warning" %in% names(data)) {
    data <- dplyr::mutate(data, Warning = NA_character_)
  }

  data %>%
    dplyr::mutate(
      Warning = dplyr::case_when(
        is.na(Site) | stringr::str_trim(Site) == "" ~ "Missing site name",
        TRUE ~ Warning
      )
    )
}

# -------------------------
# Example workflow
# -------------------------

# data <- smast_ex3
#
# cleaned_data <- data %>%
#   rename_od() %>%
#   fix_field_dup_site() %>%
#   add_dup_warning() %>%
#   add_missing_site_warning()


