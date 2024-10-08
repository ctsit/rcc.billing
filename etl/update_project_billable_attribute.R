library(tidyverse)
library(redcapcustodian)
library(DBI)
library(dotenv)
library(rcc.billing)

init_etl("update_project_billable_attribute")

conn <- connect_to_redcap_db()

diff_output <- update_billable_by_ownership(conn)

sync_activity <- redcapcustodian::sync_table(
  conn = conn,
  table_name = "redcap_entity_project_ownership",
  primary_key = "id",
  data_diff_output = diff_output,
  insert = F,
  update = T,
  delete = F
)

# Fix projects owned by CTS-IT staff
ctsit_fixes <- update_billable_if_owned_by_ctsit(conn)

sync_activity <- redcapcustodian::sync_table(
  conn = conn,
  table_name = "redcap_entity_project_ownership",
  primary_key = "id",
  data_diff_output = ctsit_fixes,
  insert = F,
  update = T,
  delete = F
)

activity_log <- diff_output$update_records %>%
  bind_rows(ctsit_fixes$update_records) %>%
  select(pid, billable, updated)

log_job_success(jsonlite::toJSON(activity_log))
