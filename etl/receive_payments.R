library(rcc.billing)
library(redcapcustodian)
library(RMariaDB)
library(DBI)
library(tidyverse)
library(lubridate)
library(dotenv)

init_etl("receive_payments")

rcc_billing_conn <- connect_to_rcc_billing_db()

earliest_date <- now(tzone = "America/New_York") - ddays(2)
earliest_date <- ymd("2022-07-06")
processed_payment_data_from_email <- get_processed_payment_data_from_email(
  username = Sys.getenv("IMAP_USERNAME"),
  password = Sys.getenv("IMAP_PASSWORD"),
  messages_since_date = earliest_date
)

# These lines are useful for testing get_processed_payment_data_from_email()
# username = Sys.getenv("IMAP_USERNAME")
# password = Sys.getenv("IMAP_PASSWORD")
# url = Sys.getenv("IMAP_URL")
# earliest_date <- ymd("2022-07-06")
# messages_since_date = earliest_date

processed_payment_data <- clean_processed_payment_data_from_email(
  processed_payment_data_from_email
)
