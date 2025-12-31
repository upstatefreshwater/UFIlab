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


data <- data %>% #mutate is adding a warning column and creating a warning to any site that still have "dup" in the name after replacing with location
  dplyr::mutate(
    Warning = dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(Site), "dup") ~ "Check sample type / site name",
      TRUE ~ NA_character_
    )
  )

data %>%
  dplyr::filter(!is.na(Warning)) #shows the rows where the warning exists




