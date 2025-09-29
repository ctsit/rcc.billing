#' @title CTS-IT Staff
#' @description usernames of CTS-IT staff
#' @format A data frame with 6 rows and 1 variable:
#' \describe{
#'   \item{\code{redcap_username}}{character: a REDCap username. Typically this is Gatorlink ID.}
#' }
"ctsit_staff"

#' @title ctsit_staff_employment_periods
#' @description Usernames and employment periods of CTS-IT staff. This
#' dataset will be used to inform default data ownership and setting billable flags
#' in the REDCap Entity / Project Ownership table. Multiple employment periods are
#' represented as multiple rows
#' @format A data frame with 7 rows and 2 variables:
#' \describe{
#'   \item{\code{redcap_username}}{character a REDCap username. Typically this is Gatorlink ID.}
#'   \item{\code{employment_interval}}{double a lubridate time interval}
#'   \item{\code{term_date_is_accurate}}{boolean indicating the quality of the termination date}
#'}
"ctsit_staff_employment_periods"

#' @title csbt_column_names
#' @description Column names useful for CTSI-IT billing
#' @format A data frame with 12 rows and 2 variables:
#' \describe{
#'   \item{\code{ctsit}}{character: Internal database columns}
#'   \item{\code{csbt}}{character: Billing invoice database column names}
#'}
"csbt_column_names"

#' @title fiscal_years
#' @description A dataframe of fiscal years and labels from 2019-2020 to 2039-2040
#' @format A data frame with 21 rows and 2 variables:
#' \describe{
#'   \item{\code{csbt_label}}{character A string representing the fiscal year range, in the format YYYY-YYYY}
#'   \item{\code{fy_interval}}{double A lubridate interval object from the start of the UF fiscal year (July 1) to the end}
#'}
#' @details DETAILS
"fiscal_years"

#' @title log_event_tables
#' @description A vector of the names of the 9 redcap log event tables
#' @format A vector with 9 elements
#' @details DETAILS
"log_event_tables"
