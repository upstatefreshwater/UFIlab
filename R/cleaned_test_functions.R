library(dplyr)
library(tidyverse)
library(stringr)

data <- smast_ex3



dup_patterns <- c("field dup", "dup", "fd", "duplicate", "f.d", "f/d")

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

data <- rename_od(data)

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

add_missing_site_warning <- function(data) {
  stopifnot(
    is.data.frame(data),
    "Site" %in% names(data)
  )

  data %>%
    dplyr::mutate(
      Warning = dplyr::case_when(
        is.na(Site) | stringr::str_trim(Site) == "" ~ "Missing site name",
        TRUE ~ NA_character_  # default for all other rows
      )
    )
}

data <- data %>%
  rename_od() %>%
  fix_field_dup_site() %>%
  add_dup_warning() %>%
  add_missing_site_warning()



