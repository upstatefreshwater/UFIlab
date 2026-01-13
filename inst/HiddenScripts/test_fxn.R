library(dplyr)
library(tidyverse)
library(stringr)

data <- smast_ex3  # Example dataset


dup_patterns <- c("field dup", "dup", "fd", "duplicate", "f.d", "f/d")


rename_od <- function(data) {
  stopifnot(is.data.frame(data))

  # Filter out bad parameters if Param exists
  if ("Param" %in% names(data)) {
    data <- data %>% dplyr::filter(!.data$Param %in% badparams)
  }

  # Rename columns
  data %>%
    dplyr::rename(
      `Receipt Temp (⁰C)`      = dplyr::any_of("OrderDetails_User1"),
      Comments                 = dplyr::any_of("OrderDetails_User2"),
      `Depth (m)`              = dplyr::any_of("OrderDetails_User3"),
      Replicate                = dplyr::any_of("OrderDetails_User4"),
      `Mc_T Receipt Temp (⁰C)` = dplyr::any_of("OrderDetails_User5")
    )
}



# Fix Field Duplicate Site Names

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


# Test Field duplicate replacement

test_df1<- data.frame(
  Site = c("Field Dup", "Station A", "dup", "FD"),
  Location = c("Loc1", NA, "Loc3", "Loc4")
)
fix_field_dup_site(test_df1)


# Add Duplicate Site Warning

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


# Test Dup warning

test_df2<- data.frame(
  Site = c("Great Gully DUP", "Control Site", "dup_sample_01", NA)
)
add_dup_warning(test_df2)


#Add Missing Site Warning
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

# Test Missing site warning
test_df3 <- data.frame(
  Site = c("Station 1", NA, "", "  ")
)

add_missing_site_warning(test_df3)

#Piped together
data_clean <- data %>%
  rename_od() %>%
  fix_field_dup_site() %>%
  add_dup_warning() %>%
  add_missing_site_warning()


