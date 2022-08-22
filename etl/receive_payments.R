library(rcc.billing)
library(redcapcustodian)
library(RMariaDB)
library(DBI)
library(tidyverse)
library(lubridate)
library(dotenv)

init_etl("receive_payments")

rcc_billing_conn <- connect_to_rcc_billing_db()
rc_conn <- connect_to_redcap_db()

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

# Write to the invoice_line_item_communications table. --------------
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

# Update the invoice_line_item table. -------------------------------
# The script update_ctsi_study_ids.R should have already updated the
# CTSI Study IDs, but this is not guaranteed.
# We get a lot of invoice payment data from CSBT and it covers a variety
# of services--not just this one--so it's hard to say that a payment
# record we can't join to is a problem. There will always be payments
# we don't recognize. As such, its unclear what we shoudl do with the
# non-matched payments.

possible_invoice_line_items_needing_update <- tbl(rcc_billing_conn, "invoice_line_item") %>%
  filter(ctsi_study_id %in% !!processed_payment_data$ctsi_study_id) %>%
  filter(fiscal_year %in% !!processed_payment_data$fiscal_year) %>%
  filter(month_invoiced %in% !!processed_payment_data$month_invoiced) %>%
  filter(status %in% c("sent")) %>%
  collect()

strip_dot_y_suffix <- function(x) {
  sub(".y", "", x)
}

new_invoice_line_updates <-
  possible_invoice_line_items_needing_update %>%
  inner_join(processed_payment_data %>%
    select(-sender, -recipient, -date_sent),
  by = c("ctsi_study_id", "fiscal_year", "month_invoiced")
  ) %>%
  select(-ends_with(".x")) %>%
  rename_with(strip_dot_y_suffix, ends_with(".y")) %>%
  mutate(
    updated = get_script_run_time()
  )

invoice_line_item_sync <- redcapcustodian::sync_table_2(
  conn = rcc_billing_conn,
  table_name = "invoice_line_item",
  source = new_invoice_line_updates,
  source_pk = "id",
  target = possible_invoice_line_items_needing_update,
  target_pk = "id"
)

# Write to redcap_entity_project_ownership  -------------------------
# Do this only for service_type == 1 aka redcap projects on prod
project_ids_of_new_invoice_line_updates <- new_invoice_line_updates %>%
  separate(service_instance_id,
           into = c("service_type_code", "pid"),
           sep = "-") %>%
  filter(service_type_code == 1) %>%
  select(pid) %>%
  pull()

project_ownership_records_needing_update <-
 tbl(rc_conn, "redcap_entity_project_ownership") %>%
  filter(sequestered == 1) %>%
  filter(pid %in% project_ids_of_new_invoice_line_updates) %>%
  collect()

project_ownership_updates <- project_ownership_records_needing_update %>%
  mutate(
    sequestered = 0,
    updated = as.numeric(get_script_run_time())
  )

project_ownership_sync <- sync_table_2(
  conn = rc_conn,
  table_name = "redcap_entity_project_ownership",
  source = project_ownership_updates,
  source_pk = "id",
  target = project_ownership_records_needing_update,
  target_pk = "id"
)

activity_log <- list(
  project_ownership_updates = project_ownership_sync$update_records,
  invoice_line_item_updates = invoice_line_item_sync$update_records,
  invoice_line_item_communications_inserts = new_invoice_line_item_communications
)

log_job_success(jsonlite::toJSON(activity_log))
