#' Example Sample Master laboratory results dataset
#'
#' Location - Chautauqua Lake, UFI sampling summer 2025
#'
#' Known issues:
#'  -
#'
#' @format A tibble with 538 rows and 20 variables:
#' \describe{
#'   \item{SampleNumber}{Character. Unique identifier for each sample.}
#'   \item{Test}{Character. Test type performed (e.g., TDP, TP, tNH3).}
#'   \item{AnalysisTime}{POSIXct. Timestamp when the analysis was performed.}
#'   \item{Site}{Character. Sampling site name.}
#'   \item{CustomerSampleNumber}{Logical. Customer-provided sample ID (mostly NA).}
#'   \item{Location}{Logical. Location information (mostly NA).}
#'   \item{CollectDate}{POSIXct. Date when the sample was collected.}
#'   \item{CollectTime}{POSIXct. Time when the sample was collected.}
#'   \item{ReceiveDate}{POSIXct. Date and time when the sample was received in the lab.}
#'   \item{OrderDetails_User1}{Character. User-entered details for the order (e.g., concentration).}
#'   \item{OrderDetails_User2}{Logical. Additional order details (mostly NA).}
#'   \item{OrderDetails_User3}{Character. Additional order details (mostly NA).}
#'   \item{OrderDetails_User4}{Character. Additional order details (mostly NA).}
#'   \item{SampleType}{Character. Type of sample (e.g., Field Blank).}
#'   \item{Param}{Character. Parameter measured (e.g., TDP, TP).}
#'   \item{Result}{Character. Analytical result (numeric values stored as character).}
#'   \item{Dilution}{Numeric. Dilution factor applied during analysis.}
#'   \item{Qualifier}{Character. Qualifier code for result (e.g., F22).}
#'   \item{Results_Commnt}{Logical. Comments on the result (mostly NA).}
#'   \item{AnalysisDate}{POSIXct. Date of the analysis.}
#' }
#'
#' @source Example laboratory export provided for package demonstration purposes.
"smast_ex_chaut_missinloc"
