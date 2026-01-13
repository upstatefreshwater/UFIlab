#' Parameter names to be ignored when QC'ing samplemaster data
#'
#' This dataset contains parameter names that should be skipped during quality
#' control of the `samplemaster` data.
#'
#' @format A character vector of parameter names
#' @examples
#' head(badparams)
#' "%Solids" %in% badparams
#' @source Internal package dataset
"badparams"
