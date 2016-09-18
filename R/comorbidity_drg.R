#' Comorbidity mapping using ICD-9-CM
#'
#' A dataset containing DRG MDC codes which can be used to exclude an ICD code
#' as a comorbidity.
#'
#' @format A data frame with comorbidity and DRG MDC codes.
#' \describe{
#'   \item{comorbidity}{name of the comorbidity}
#'   \item{drg}{the DRG Major Disease Classification (MDC) code}
#' }
#'
"comorbidity_drg"
