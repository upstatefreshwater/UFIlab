dat <- smast_ex3
# Helper to read the LOD file
get_lod <- function() {
  # full path inside installed package
  path <- fs::path_package("UFIlab", "extdata", "LOD_values.xlsx")
  # Throw error if LOD file is missing
  if (path == "") stop(paste("LOD file not found at location: ", path))

  # read with readxl
  readxl::read_excel(path)
}


# Pull LOD data from the updatable Excel file
lod.data <- get_lod()
# Check that units are correct
for (i in unique(lod_checkdata$Parameter)) {
  user_unit <- lod.data$units[lod.data$Parameter==i]
  check_unit <- lod_checkdata$units[lod_checkdata$Parameter==i]
  if(user_unit != check_unit){
    stop(paste('The provided units in "LOD_values.xlsx" :',user_unit,
               "\nfor the parameter:", i,
               "\ndoes not match with the expected units:",check_unit))
  }
}


lod.data <- lod.data %>%
  dplyr::select(Parameter,LOD) %>%
  dplyr::mutate(Parameter = toupper(Parameter))


if(!is.numeric(dat$Result)){
  dat$Result <- as.numeric(dat$Result)
  if(any(is.na(unique(dat$Result)))){
    warning('NAs present in Result column following coercion of data to numeric,\n',
            'Check for text/characters in results column')
  }
}

lod_join <-
dat %>%
  dplyr::mutate(Param = toupper(Param)) %>%
  select(Param, Result) %>%
  left_join(lod.data, by = c("Param"="Parameter"))

if(anyNA(lod_join$LOD)){
  missing_lod <- unique(lod_join$Param[is.na(lod_join$LOD)])
  stop(
    "Missing LOD values for parameters: ",
    paste(missing_lod, collapse = ", "),
    ".\nPlease update the \"LOD_values.xlsx\" file!",
    call. = FALSE
  )
}

lod_join %>%   # attach the LOD for each Param
  mutate(Result = if_else(Result < LOD, "<lod", as.character(Result))) %>%
  select(-LOD) %>% view # remove the LOD column if you don’t want it

