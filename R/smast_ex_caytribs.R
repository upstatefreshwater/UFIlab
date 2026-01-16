#' Example Sample Master water quality dataset
#'
#' Known data issues:
#'  - Missing location information where "Field Duplicate" entered in the Site Column
#'
#' @format A data frame with 280 rows and 24 variables:
#' \describe{
#'   \item{SampleNumber}{Laboratory sample identifier}
#'   \item{Test}{Analyte or test name as reported by the laboratory}
#'   \item{CollectDate}{Sample collection date (character)}
#'   \item{CollectTime}{Sample collection time (character)}
#'   \item{ReceiveDate}{Date and time the lab received the sample}
#'   \item{Result}{Analytical result (numeric)}
#'   \item{Qualifier}{Result qualifier code}
#'   \item{Results_Commnt}{Optional comments (logical/NA in this example)}
#'   \item{Location}{Location information (character; often blank)}
#'   \item{Dilution}{Dilution factor applied to the sample}
#'   \item{AnalysisTime}{Time of analysis (character)}
#'   \item{CustomerSampleNumber}{Customer-provided sample ID (logical/NA)}
#'   \item{Site}{Sampling site identifier}
#'   \item{Param}{Standardized parameter name}
#'   \item{QCBatchID}{QC batch identifier}
#'   \item{MeasuredResult}{Measured result (numeric)}
#'   \item{OrderDetails_User1}{User-defined order details (numeric)}
#'   \item{AnalysisDate}{Date of analysis (character)}
#'   \item{SampleDetails_User1}{User-defined sample details (logical/NA)}
#'   \item{SampleType}{Sample type (e.g., Grab, Composite)}
#'   \item{CustomerID}{Customer or project identifier}
#'   \item{OrderDetails_User2}{User-defined order detail (logical/NA)}
#'   \item{OrderDetails_User3}{User-defined order detail (logical/NA)}
#'   \item{OrderDetails_User4}{User-defined order detail (logical/NA)}
#' }
#'
#' @source Example laboratory export provided for SMAST package demonstration.
"smast_ex_caytribs"
