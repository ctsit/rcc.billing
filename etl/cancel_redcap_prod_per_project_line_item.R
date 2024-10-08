library(rcc.billing)
library(redcapcustodian)
library(RMariaDB)
library(DBI)
library(tidyverse)
library(lubridate)
library(dotenv)

init_etl("cancel_redcap_prod_per_project_line_item")

rc_conn <- connect_to_redcap_db()
rcc_billing_conn <- connect_to_rcc_billing_db()

sent_line_items <- tbl(rcc_billing_conn, "invoice_line_item") %>%
  filter(service_type_code == 1 & status == "sent") %>%
  collect() %>%
  mutate(project_id = str_replace(service_instance_id, "1-", "") )

invoiced_project_log_event_tables <- tbl(rc_conn, "redcap_projects") %>%
  filter(project_id %in% local(sent_line_items$project_id)) %>%
  select(project_id, log_event_table) %>%
  distinct() %>%
  collect() %>%
  mutate(project_id = as.character(project_id))

sent_line_items_with_log_event_table <- sent_line_items %>%
  inner_join(invoiced_project_log_event_tables, by = "project_id")

new_deleted_projects <- c()

for (f in invoiced_project_log_event_tables$log_event_table) {
  tmp_deleted_projects <- tbl(rc_conn, f) %>%
    filter(description == "Delete project") %>%
    filter(project_id %in% invoiced_project_log_event_tables$project_id) %>%
    collect() %>%
    pull(project_id)

  new_deleted_projects <- append(new_deleted_projects, tmp_deleted_projects)
}

updates_to_invoice_line_item <- sent_line_items %>%
  filter(project_id %in% new_deleted_projects) %>%
  mutate(
    status = "canceled",
    reason = "Project deleted"
  ) %>%
  select(
    id,
    service_instance_id,
    fiscal_year,
    month_invoiced,
    reason,
    status
  )

# TODO: log and terminate if no updates

updates_to_invoice_line_item_filename = "updates_to_invoice_line_item.csv"
tmp_invoice_file <- paste0(tempdir(), updates_to_invoice_line_item_filename)

updates_to_invoice_line_item %>%
  select(-id, -status) %>%
  write_csv(tmp_invoice_file)

# Email CSBT
email_subject <- paste("Cancelled invoice line items for REDCap Project billing")
attachment_object <- sendmailR::mime_part(tmp_invoice_file, updates_to_invoice_line_item_filename)
body <- "The attached file has cancelled invoice line items for REDCap Project billing. Please load these into the CSBT invoicing system."
email_body <- list(body, attachment_object)
send_email(
  email_body = email_body,
  email_subject = email_subject,
  email_to = Sys.getenv("CSBT_EMAIL")
)

invoice_line_item_sync_activity <- redcapcustodian::sync_table_2(
  conn = rcc_billing_conn,
  table_name = "invoice_line_item",
  source = updates_to_invoice_line_item,
  source_pk = "id",
  target = sent_line_items,
  target_pk = "id"
)

n_communication_records <- dbGetQuery(rcc_billing_conn, "SELECT count(*) FROM invoice_line_item_communications") %>%
  pull()

new_invoice_line_item_communications <- draft_communication_record_from_line_item(updates_to_invoice_line_item) %>%
  mutate(id = n_communication_records + row_number()) %>%
  select(id, everything())

redcapcustodian::write_to_sql_db(
  conn = rcc_billing_conn,
  table_name = "invoice_line_item_communications",
  df_to_write = new_invoice_line_item_communications,
  schema = NA,
  overwrite = F,
  db_name = "rcc_billing",
  append = T
)

activity_log <- list(
  invoice_line_item_communications = new_invoice_line_item_communications %>%
    mutate(diff_type = "insert") %>%
    select(diff_type, everything()),
  invoice_line_item = invoice_line_item_sync_activity$update_records %>%
    mutate(diff_type = "update") %>%
    select(diff_type, everything())
)

log_job_success(jsonlite::toJSON(activity_log))

unlink(tmp_invoice_file)
DBI::dbDisconnect(rcc_billing_conn)
DBI::dbDisconnect(rc_conn)
