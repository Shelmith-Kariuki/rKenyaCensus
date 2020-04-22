#' Volume I: Table 2.5
#'
#' Distribution of Population by Sex and Sub-County.
#' The rows tagged "County" in the "AdminArea" variable contain the sub totals of their
#' respective counties e.g the number of males in Nyeri County (374288),
#' is the summation of the number of males of all the subcounties of that county.
#' Note: The first row contains the national total, for each of the variables.
#' @docType data
#'
#' @usage data(V1_T2.5)
#'
#' @format A data frame with 7 variables:
#' \describe{
#' \item{\code{County}}{County}
#' \item{\code{SubCounty}}{Sub County}
#' \item{\code{AdminArea}}{Shows whether the area is a County or SubCounty}
#' \item{\code{Male}}{Number of Males}
#' \item{\code{Female}}{Number of Females}
#' \item{\code{Intersex}}{Number of Intersex persons}
#' \item{\code{Total}}{Total number of individuals (Male + Female + Intersex)}
#'
#'}
#' @keywords datasets
#'
"V1_T2.5"
