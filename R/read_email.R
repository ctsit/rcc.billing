#' Scrape an inbox for processed payment emails and return data
#'
#' Connect to an imap mailbox, identify processed payment emails sent
#' after `messages_since_date`, and extract the data from those emails.
#'
#' @param url The IMAP URL of the host that houses the mailbox
#' @param username The username of the IMAP mailbox
#' @param password The password of the IMAP mailbox
#' @param messages_since_date The sent date of the oldest message that should be inspected
#'
#' @return A dataframe of processed payment facts
#' ##\itemize{
#' ##  \item email - stuff and things
#' ##}
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' get_processed_payment_data_from_email(
#'   username = "jdoe",
#'   password = "jane_does_password",
#'   url ="imaps://outlook.office365.com",
#'   messages_since_date = as.Date("2022-01-01", format = "%Y-%m-%d")
#'   )
#' }
get_processed_payment_data_from_email <- function(username,
                                                  password,
                                                  url = "imaps://outlook.office365.com",
                                                  messages_since_date) {
  imap_con <- mRpostman::configure_imap(
    url = url,
    username = username,
    password = password
  )

  imap_con$select_folder("INBOX")
  emails_by_subject_search <- imap_con$search_string(expr = "Your payment for CTSI Study", where = "SUBJECT")
  messages_since_date <- imap_con$search_since(date_char = format(messages_since_date, format = "%d-%b-%Y"))
  emails_found <- dplyr::intersect(emails_by_subject_search, messages_since_date)

  patterns <- c(
    "Study Name:.*\r\n",
    "CRC #:.*\r\n",
    "IDS #:.*\r\n",
    "OCR #:.*\r\n",
    "Invoice Number:.*\r\n",
    "Amount Paid:.*\r\n",
    "JE #:.*\r\n",
    "JE Posting Date:.*\r\n"
  )

  data_from_emails <- tibble::tribble(
    ~study_name, ~crc_number, ~ids_number, ~ocr_number, ~invoice_number, ~amount_paid, ~je_number, ~je_posting_date, ~ctsi_study_id,
    "a", "a", "a", "a", "a", "a", "a", "a", "a"
  ) %>% dplyr::filter(F)

  if (length(emails_found) > 0) {
    for (email in emails_found) {
      ctsi_study_id <- email %>%
        imap_con$fetch_header() %>%
        stringr::str_extract_all("Subject.*:.*\r\n") %>%
        sub(".*CTSI Study ", "", .) %>%
        sub(" has been processed.*", "", .)

      data_row <- email %>%
        imap_con$fetch_text() %>%
        stringr::str_extract_all(patterns) %>%
        # remove html encoded < and > characters
        sub("&lt;.*&gt;", "", .) %>%
        # remove literal < and > characters
        sub("<.*>", "", .) %>%
        sub("\r\n", "", .) %>%
        sub("#", "number", .) %>%
        stringr::str_split(": ") %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame() %>%
        tidyr::pivot_wider(
          names_from = "V1",
          values_from = "V2"
        ) %>%
        janitor::clean_names() %>%
        dplyr::mutate(ctsi_study_id = ctsi_study_id)
      data_from_emails <- dplyr::bind_rows(data_from_emails, data_row)
    }
  }

  return(data_from_emails)
}

#' Clean the output of get_processed_payment_data_from_email and format for alignment
#' with invoice_line_item data
#'
#' @param processed_payment_data_from_email, the dataframe output of get_processed_payment_data_from_email
#'
#' @return The inpout data frame reformatted for alignment with invoice_line_item
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' processed_payment_data <- clean_processed_payment_data_from_email(
#'   processed_payment_data_from_email
#' )
#' }
clean_processed_payment_data_from_email <- function(processed_payment_data_from_email) {
  result <- processed_payment_data_from_email %>%
    dplyr::rename(name_of_service = .data$study_name) %>%
    dplyr::mutate(crc_number = as.numeric(.data$crc_number)) %>%
    dplyr::mutate(ids_number = dplyr::na_if(.data$ids_number, "")) %>%
    dplyr::mutate(ocr_number = dplyr::na_if(.data$ocr_number, "")) %>%
    dplyr::mutate(invoice_number = stringr::str_replace(.data$invoice_number, " .*", "")) %>%
    dplyr::mutate(amount_paid_sign = dplyr::if_else(stringr::str_detect(.data$amount_paid, "\\(.*\\)"), -1, 1)) %>%
    dplyr::mutate(amount_paid = .data$amount_paid_sign * readr::parse_number(
      .data$amount_paid,
      locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    )) %>%
    dplyr::select(-.data$amount_paid_sign) %>%
    dplyr::mutate(je_posting_date = lubridate::mdy(.data$je_posting_date))

  return(result)
}
