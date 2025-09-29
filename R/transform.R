#' mutate_columns_to_posixct
#'
#' Mutates column data types to POSIXct
#'
#' @param data - a dataframe to mutate
#' @param column_names - a vector of column names to mutate
#'
#' @return The input dataframe with revised data types
#' @export
#'
#' @examples
#' \dontrun{
#' time_columns <- c("created", "updated")
#' mutate_columns_to_posixct(data, time_columns)
#' }
#' @export
mutate_columns_to_posixct <- function(data, column_names) {
  result <- data |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(column_names),
      ~ as.POSIXct(., origin = "1970-01-01 00:00.00 UTC", tz = "UTC")
    ))

  return(result)
}

#' Creates a invoice_line_item data from invoice_line_item_communications_data
#'
#' @param invoice_line_item_communications, data that follows the format of invoice_line_item_communications_test_data located in R/data.R
#'
#' @returns invoice_line_item dataframe
#'
#' @examples
#' \dontrun{
#' invoice_line_item_df_from(invoice_line_item_communications_test_data)
#' }
#' @export
invoice_line_item_df_from <- function(invoice_line_item_communications) {
  excluded_columns <- "created"

  id_columns <- c(
    "service_identifier", "fiscal_year", "month_invoiced"
  )

  created <- invoice_line_item_communications |>
    dplyr::arrange(created) |>
    dplyr::distinct(.data$service_identifier, .data$fiscal_year, .data$month_invoiced, .keep_all = T) |>
    dplyr::select(dplyr::any_of(id_columns), "created")

  invoice_line_item <- invoice_line_item_communications |>
    dplyr::arrange(dplyr::desc(created)) |>
    dplyr::distinct(.data$service_identifier, .data$fiscal_year, .data$month_invoiced, .keep_all = T) |>
    dplyr::select(-dplyr::any_of(excluded_columns)) |>
    dplyr::inner_join(created, by = id_columns) |>
    dplyr::relocate("updated", .after = "created") |>
    dplyr::arrange((.data$je_number)) |>
    dplyr::mutate(id = as.double(dplyr::row_number()))

  return(invoice_line_item)
}

#' fix_data_in_invoice_line_item
#'
#' Fixes column data types that can vary between MySQL/MariaDB and SQLite3.
#' This allows testing in SQLite3 while production is MariaDB
#'
#' @param data - a dataframe with data from the invoice_line_item table
#'
#' @return The input dataframe with revised data types
#' @export
#'
#' @examples
#' \dontrun{
#' fix_data_in_invoice_line_item(invoice_line_item_test_data)
#' }
#' @export
fix_data_in_invoice_line_item <- function(data) {
  time_columns <- c(
    "created",
    "updated",
    "je_posting_date",
    "date_sent",
    "date_received"
  )

  return(mutate_columns_to_posixct(data, time_columns))
}

#' fix_data_in_invoice_line_item_communication
#'
#' Fixes column data types that can vary between MySQL/MariaDB and SQLite3.
#' This allows testing in SQLite3 while production is MariaDB
#'
#' @param data - a dataframe with data from the invoice_line_item_communication table
#'
#' @return The input dataframe with revised data types
#' @export
#'
#' @examples
#' \dontrun{
#' fix_data_in_invoice_line_item_communication(invoice_line_item_communication_test_data)
#' }
#' @export
fix_data_in_invoice_line_item_communication <- function(data) {
  time_columns <- c(
    "created",
    "updated",
    "je_posting_date",
    "date_sent",
    "date_received"
  )

  return(mutate_columns_to_posixct(data, time_columns))
}

#' fix_data_in_redcap_projects
#'
#' Fixes column data types that can vary between MySQL/MariaDB and SQLite3.
#' This allows testing in SQLite3 while production is MariaDB
#'
#' @param data - a dataframe with data from the redcap_projects table
#'
#' @return The input dataframe with revised data types
#' @export
#'
#' @examples
#' \dontrun{
#' fix_data_in_redcap_projects(redcap_projects)
#' }
#' @export
fix_data_in_redcap_projects <- function(data) {
  time_columns <- c(
    "creation_time",
    "production_time",
    "inactive_time",
    "completed_time",
    "date_deleted",
    "last_logged_event",
    "datamart_cron_end_date",
    "twilio_request_inspector_checked"
  )

  return(mutate_columns_to_posixct(data, time_columns))
}

#' fix_data_in_redcap_user_information
#'
#' Fixes column data types that can vary between MySQL/MariaDB and SQLite3.
#' This allows testing in SQLite3 while production is MariaDB
#'
#' @param data - a dataframe with data from the redcap_user_information table
#'
#' @return The input dataframe with revised data types
#' @export
#'
#' @examples
#' \dontrun{
#' fix_data_in_redcap_user_information(redcap_user_information_test_data)
#' }
#' @export
fix_data_in_redcap_user_information <- function(data) {
  time_columns <- c(
    "user_creation",
    "user_firstvisit",
    "user_firstactivity",
    "user_lastactivity",
    "user_lastlogin",
    "user_suspended_time",
    "user_expiration",
    "user_access_dashboard_view",
    "messaging_email_ts",
    "messaging_email_queue_time"
  )

  return(mutate_columns_to_posixct(data, time_columns))
}

#' Renames columns of a dataframe from CTSIT format to CSBT format
#'
#' Excludes non-CSBT columns and renames CTSIT column names to the corresponding CSBT names.
#' This function is the inverse of \code{\link{transform_invoice_line_items_for_ctsit}}
#' @param invoice_line_items A dataframe with the CTSIT column names
#' @return The input dataframe with columns adjusted to match CSBT format
#' @details DETAILS
#' @examples
#' \dontrun{
#' tbl(conn, "invoice_line_item") |>
#'   collect() |>
#'   transform_invoice_line_items_for_csbt()
#' }
#' @export
#' @seealso \code{\link{csbt_column_names}}
transform_invoice_line_items_for_csbt <- function(invoice_line_items) {

  new_names <- function(old_column_names) {
    rcc.billing::csbt_column_names |>
      dplyr::filter(.data$ctsit %in% old_column_names) |>
      dplyr::pull(.data$csbt)
  }

  result <- invoice_line_items |>
    dplyr::select(dplyr::any_of(rcc.billing::csbt_column_names$ctsit)) |>
    dplyr::rename_with(.fn = ~ new_names(.), .cols = dplyr::any_of(rcc.billing::csbt_column_names$ctsit))

  return(result)
}

#' Renames columns of a dataframe from CSBT format to CTSIT format
#'
#' Renames CSBT column names to the corresponding CTSIT names.
#' This function is the inverse of \code{\link{transform_invoice_line_items_for_csbt}}, however it does NOT exclude columns not in CTSIT column names.
#' @param invoice_line_items A dataframe with the CSBT column names
#' @return The input dataframe with columns adjusted to match CTSIT format
#' @details DETAILS
#' @examples
#' \dontrun{
#' df_from_csbt |>
#'   transform_invoice_line_items_for_ctsit() |>
#'   janitor::clean_names()
#' }
#' @export
#' @seealso \code{\link{csbt_column_names}}
transform_invoice_line_items_for_ctsit <- function(invoice_line_items) {

  expected_columns <- c(
    "CTSI Study ID",
    "Other System Billing ID",
    "Fiscal Year",
    "Month Invoiced",
    "Auxiliary Name",
    "PI Last Name",
    "PI First Name",
    "Invoice #",
    "Name of Service",
    "Qty Provided",
    "Cost of Service",
    "Amount Due",
    "Amt Paid",
    "Do Not Bill",
    "Do Not Bill Reason",
    "Do Not Bill Invoice #",
    "Deposit or JE #",
    "Date of Pmt",
    "FY Month Number",
    "Charged to CTSI Voucher",
    "CTSIT ID"
  )

  expected_months <- c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
    NA_character_
  )

  expected_fiscal_years <- c(rcc.billing::fiscal_years$csbt_label, NA)

  checkmate::assert_names(names(invoice_line_items), permutation.of = expected_columns)
  checkmate::assert_numeric(invoice_line_items$`CTSI Study ID`)                     #: num [1:41] 4678 466 5614 5594 5416 ...
  checkmate::assert_character(invoice_line_items$`CTSIT ID`)                        #: chr [1:41] "1-8931" "1-5716" "1-13800" "1-15575" ...
  checkmate::assert_character(invoice_line_items$`Other System Billing ID`)         #: chr [1:41] "1-8931" "1-5716" "1-13800" "1-15575" ...
  checkmate::assert_subset(invoice_line_items$`Fiscal Year`, expected_fiscal_years) #: chr [1:41] "2025-2026" "2025-2026" "2025-2026" "2025-2026" ...
  checkmate::assert_subset(invoice_line_items$`Month Invoiced`, expected_months)    #: chr [1:41] "July" "July" "July" "July" ...
  checkmate::assert_subset(invoice_line_items$`Auxiliary Name`, c("CTS-IT", NA) )   #: chr [1:41] "CTS-IT" "CTS-IT" "CTS-IT" "CTS-IT" ...
  checkmate::assert_character(invoice_line_items$`PI Last Name`)                    #: chr [1:41] "Merendino" "Staud" "Maile" "Strath" ...
  checkmate::assert_character(invoice_line_items$`PI First Name`)                   #: chr [1:41] "Antony" "Roland" "Robert" "Larissa" ...
  checkmate::assert_character(invoice_line_items$`Invoice #`)                       #: chr [1:41] "MerendinoA-4678-JUL25" "StaudR-466-JUL25" "MaileR-5614-JUL25" "StrathL-5594-JUL25" ...
  checkmate::assert_character(invoice_line_items$`Name of Service`)                 #: chr [1:41] "Biomedical Informatics Consulting" "Biomedical Informatics Consulting" "Biomedical Informatics Consulting" "Biomedical Informatics Consulting" ...
  checkmate::assert_numeric(invoice_line_items$`Qty Provided`)                      #: num [1:41] 1 1 1 1 5.75 1 1 1 1 1 ...
  checkmate::assert_numeric(invoice_line_items$`Cost of Service`)                   #: num [1:41] 130 130 260 130 130 130 130 130 130 130 ...
  checkmate::assert_numeric(invoice_line_items$`Amount Due`)                        #: num [1:41] 130 130 130 130 748 ...
  checkmate::assert_numeric(invoice_line_items$`Amt Paid`)                          #: num [1:41] 130 130 130 130 748 ...
  checkmate::assert_numeric(invoice_line_items$`Do Not Bill`)                       #: num [1:41] 0 0 0 0 0 0 0 0 0 0 ...
  checkmate::assert_character(invoice_line_items$`Do Not Bill Reason`)              #: logi [1:41] NA NA NA NA NA NA ...
  checkmate::assert_character(invoice_line_items$`Do Not Bill Invoice #`)           #: logi [1:41] NA NA NA NA NA NA ...
  checkmate::assert_character(invoice_line_items$`Deposit or JE #`)                 #: chr [1:41] "CTSI082725" "CTSI082725" "CTSI082725" "CTSI082725" ...
  checkmate::assert_true(
    checkmate::test_posixct(invoice_line_items$`Date of Pmt`) |
    checkmate::test_character(invoice_line_items$`Date of Pmt`)
  )                                                                                 #: POSIXct[1:41], format: "2025-08-26" "2025-08-26" "2025-08-26" "2025-08-26" ...
  checkmate::assert_numeric(invoice_line_items$`FY Month Number`)                   #: num [1:41] 1 1 1 1 1 1 1 1 1 1 ...
  checkmate::assert_subset(invoice_line_items$`FY Month Number`, c(seq(1,12), NA))  #: num [1:41] 1 1 1 1 1 1 1 1 1 1 ...
  checkmate::assert_logical(invoice_line_items$`Charged to CTSI Voucher`)           #: logi [1:41] FALSE FALSE FALSE FALSE FALSE FALSE ...

  new_names <- function(old_column_names) {
    rcc.billing::csbt_column_names |>
      dplyr::filter(.data$csbt %in% old_column_names) |>
      dplyr::pull(.data$ctsit)
  }

  result <- invoice_line_items |>
    # NOTE: we do not want to lose column names here
    # dplyr::select(dplyr::any_of(rcc.billing::csbt_column_names$csbt)) |>
    dplyr::rename_with(.fn = ~ new_names(.), .cols = dplyr::any_of(rcc.billing::csbt_column_names$csbt))

  return(result)
}

#' Adds metadata necessary for sending emails to an invoice_line_item dataframe, e.g. \code{\link{transform_invoice_line_items_for_csbt}}
#'
#' @param invoice_line_items A dataframe from the invoice_line_item table
#' @return The input dataframe with the following columns added:
#' \itemize{
#'   \item updated - A timestamp provided by \code{\link[redcapcustodian]{get_script_run_time}}
#'   \item sender - The value set in \code{Sys.getenv("EMAIL_FROM")}
#'   \item recipient - The value set in \code{Sys.getenv("EMAIL_TO")}
#'   \item date_sent - A timestamp provided by \code{\link[redcapcustodian]{get_script_run_time}}
#'   \item date_received - A placeholder timestamp, \code{as.POSIXct(NA)}
#'   \item script_name - The script name returned by \code{\link[redcapcustodian]{get_script_name}}
#' }
#' @examples
#' \dontrun{
#' tbl(conn, "invoice_line_item") |>
#'   collect() |>
#'   draft_communication_record_from_line_item()
#' }
#' @export
draft_communication_record_from_line_item <- function(invoice_line_items) {
  result <- invoice_line_items |>
    dplyr::mutate(
      updated = redcapcustodian::get_script_run_time(),
      sender = Sys.getenv("EMAIL_FROM"),
      recipient = Sys.getenv("EMAIL_TO"),
      date_sent = redcapcustodian::get_script_run_time(),
      date_received = as.POSIXct(NA),
      script_name = redcapcustodian::get_script_name()
    )

  return(result)
}

#' Calculate Service Request Time
#'
#' This function takes time in minutes and hours and returns a unified time format in hours.
#'
#' @param time_minutes Numeric vector representing time in minutes.
#' @param time_hours Numeric vector representing time in hours.
#'
#' @return A numeric vector with the processed time in hours. For input times in minutes
#'         that are part of the set \{15, 30, 45, 60\}, the time is converted to hours.
#'         For time in hours greater than 1, the original hours are returned. Otherwise,
#'         `NA_real_` is returned for those cases not matching the conditions.
#'
#' @examples
#' \dontrun{
#' service_request_time(30, 120)
#' }
#'
#' @export
service_request_time <- function(time_minutes, time_hours) {
  result <- dplyr::tibble(
    time_minutes = time_minutes,
    time_hours = time_hours
  ) |>
    dplyr::mutate(time = dplyr::case_when(
      .data$time_minutes %in% c(15, 30, 45, 60) ~ .data$time_minutes / 60,
      .data$time_hours >= 0 ~ .data$time_hours,
      TRUE ~ NA_real_
    )) |>
    dplyr::pull(.data$time)

  return(result)
}

