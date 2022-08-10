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

max_communications_id <-
  tbl(rcc_billing_conn, "invoice_line_item_communications") %>%
  summarise(max_id = max(id, na.rm = TRUE)) %>%
  collect() %>%
  pull(max_id)

new_invoice_line_item_communications <- processed_payment_data %>%
  mutate(
    id = max_communications_id + row_number(),
    updated = get_script_run_time(),
    created = get_script_run_time(),
    script_name = get_script_name()
  )

redcapcustodian::write_to_sql_db(
  conn = rcc_billing_conn,
  table_name = "invoice_line_item_communications",
  df_to_write = new_invoice_line_item_communications,
  schema = NA,
  overwrite = F,
  db_name = "rcc_billing",
  append = T
)

# TODO Add code to update the invoice_line_item table.
#      At the moment we are missing a map from the CTSI Study ID to CTSIT's service identifier.
#      That omission makes this task hacky at best, impossible at worst.
# new_invoice_line_updates <- processed_payment_data %>%
#   mutate(
#     updated = get_script_run_time()
#   )
#
# tbl(rcc_billing_conn, "invoice_line_item") %>%
#   filter()
