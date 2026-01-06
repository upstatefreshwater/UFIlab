library(dplyr)
library(tidyverse)
library(stringr)



testfxn <- function(data){  #data is the only argument passed to the "testfxn"
  smastex_filtered <- (data) %>%  #create a data object
    dplyr::filter(!Param %in% badparams) %>% #filter out any bad params


    dplyr::rename( #rename order details cols
      `Receipt Temp (⁰C)`      = any_of("OrderDetails_User1"),
      Comments                 = any_of("OrderDetails_User2"),
      `Depth (m)`              = any_of("OrderDetails_User3"),
      Replicate                = any_of("OrderDetails_User4"),
      `Mc_T Receipt Temp (⁰C)` = any_of("OrderDetails_User5")
    )
  return(smastex_filtered) #be sure to return the data object

}

testfxn(smast_ex)

data <- data %>% #When site is "Field Dup" and there is a location the site value is replaced with Location
  dplyr::mutate(
    Site = dplyr::case_when(
      stringr::str_to_lower(Site) %in% c("field dup", "dup", "fd", "duplicate", "f.d", "f/d") & !is.na(Location) ~ Location,
      TRUE ~ Site
    )
  )



fix_field_dup_site <- function(data) { #function to remove dupes idk
  data %>%
    dplyr::mutate(
      Site = dplyr::case_when(
        stringr::str_to_lower(Site) %in%
          c("field dup", "dup", "fd", "duplicate", "f.d", "f/d") &
          !is.na(Location) ~ Location,
        TRUE ~ Site
      )
    )
}

data <- fix_field_dup_site(data)

test_df <- data.frame( #test with a data frame that include "dup" to see if function worked
  Site = c("Field Dup", "Station A", "dup", "FD"),
  Location = c("Loc1", NA, "Loc3", "Loc4"),
  stringsAsFactors = FALSE
)

result <- fix_field_dup_site(test_df)
result #to see site name replaced with location name


data <- data %>% #mutate is adding a warning column and creating a warning to any site that still have "dup" in the name after replacing with location
  dplyr::mutate(
    Warning = dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(Site), "dup") ~ "Check sample type / site name",
      TRUE ~ NA_character_
    )
  )

data %>%
  dplyr::filter(!is.na(Warning)) #shows the rows where the warning exists


add_dup_warning <- function(data) { #dup WARNING function if "dup" is in site name after
  stopifnot(is.data.frame(data))

  data %>%
    dplyr::mutate(
      Warning = dplyr::case_when(
        stringr::str_detect(stringr::str_to_lower(Site), "dup") ~
          "Check sample type / site name",
        TRUE ~ NA_character_
      )
    )
}


test_df <- data.frame( #test with data frame with dup in the name to see if it throws warning
  Site = c(
    "Great Gully DUP",
    "Control Site",
    "dup_sample_01",
    NA
  ),
  stringsAsFactors = FALSE
)

add_dup_warning(test_df)


data <- data %>% #pipe both functions together
  fix_field_dup_site() %>%
  add_dup_warning()

test_df <- data.frame(
  Site = c("Field Dup", "Station A", "dup", "FD", "dup_sample_01"),
  Location = c("Loc1", NA, "Loc3", "Loc4", NA),
  stringsAsFactors = FALSE
)

result <- test_df %>%  #running the full dup replaced with location and throw warning pipeline with test data
  fix_field_dup_site() %>%
  add_dup_warning()

result #check to see warnings and replaced data
