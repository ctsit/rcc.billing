library(DBI)
library(redcapcustodian)
library(redcapcustodian)
library(rcc.billing)
library(RMariaDB)
library(tidyverse)
library(dotenv)

init_etl("sync_redcap_projects_to_history")

rc_conn <- connect_to_redcap_db()
rc_billing_conn <- connect_to_rcc_billing_db()

# create the target table from REDCap's live DDL if it does not exist
if (!"redcap_projects_history" %in% DBI::dbListTables(rc_billing_conn)) {
  create_redcap_projects_history <-
    DBI::dbGetQuery(rc_conn, "SHOW CREATE TABLE redcap_projects") |>
    janitor::clean_names() |>
    mutate(create_table = str_replace(create_table, "`redcap_projects`", "`redcap_projects_history`")) |>
    mutate(create_table = str_replace_all(create_table, ",\n  CONSTRAINT [^,]+ CASCADE", "")) |>
    pull(create_table)
  DBI::dbExecute(rc_billing_conn, create_redcap_projects_history)
}

# reconcile any drift between REDCap's live column definitions and the frozen history table
# schema (e.g. widened VARCHARs or new ENUM values added by a REDCap upgrade) before syncing data
schema_drift <- reconcile_redcap_projects_history_schema(
  source_conn = rc_conn,
  source_table = "redcap_projects",
  target_conn = rc_billing_conn,
  target_table = "redcap_projects_history"
)

if (nrow(schema_drift) > 0) {
  log_job_success(jsonlite::toJSON(list(schema_columns_altered = schema_drift$column_name)))
}

redcap_projects_history <- tbl(rc_billing_conn, "redcap_projects_history") |> collect()

# Ensure redcap_projects_source does not have any columns not already in redcap_projects_history
# This protects us from the more common alterations to redcap_projects
redcap_projects_source <- tbl(rc_conn, "redcap_projects") |>
  collect() |>
  select(any_of(names(redcap_projects_history)))

# If there is novel data in the source, sync the data
novel_data <- all.equal(
  redcap_projects_source,
  redcap_projects_history |>
    filter(project_id %in% redcap_projects_source$project_id)
)
novel_data <- if_else(length(novel_data) > 1, T, F)

if (novel_data) {
  # sync the data
  sync_activity_results <- redcapcustodian::sync_table_2(
    conn = rc_billing_conn,
    table_name = "redcap_projects_history",
    source = redcap_projects_source,
    source_pk = "project_id",
    target = redcap_projects_history,
    target_pk = "project_id",
    insert = T,
    update = T,
    delete = F
  )

  # log the work
  summary_data <- list(
    updates = sync_activity_results$update_n,
    inserts = sync_activity_results$insert_n,
    records_updated = sync_activity_results$update_records,
    records_inserted = sync_activity_results$insert_records,
    records_deleted = sync_activity_results$delete_records
  )

  log_job_success(jsonlite::toJSON(summary_data))
}
