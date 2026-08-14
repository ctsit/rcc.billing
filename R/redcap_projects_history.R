#' get_column_type_drift
#'
#' Compares the column definitions of a source table and a target table via
#' \code{information_schema.columns} and returns the columns that are present in both
#' tables but whose type differs. Used to detect when REDCap's own DDL revisions to
#' \code{redcap_projects} (e.g. a widened \code{VARCHAR} or a new \code{ENUM} value) have
#' drifted out from under the frozen \code{redcap_projects_history} mirror table.
#'
#' Nullability is intentionally not compared: \code{redcap_projects_history} accumulates
#' rows over time, so older rows may legitimately hold \code{NULL} in a column that REDCap
#' has since made \code{NOT NULL}. Tightening the target to match the source's nullability
#' would fail against that pre-existing data, so \code{target_is_nullable} is returned only
#' so callers can preserve the target's own current nullability when reconciling type.
#'
#' @param source_conn, a DBI connection to the database containing \code{source_table}
#' @param source_table, the name of the table to treat as the source of truth for column definitions
#' @param target_conn, a DBI connection to the database containing \code{target_table}
#' @param target_table, the name of the table to check for drifted column definitions
#'
#' @importFrom rlang .data
#'
#' @return a dataframe with one row per drifted column: \code{column_name},
#'   \code{source_column_type}, \code{target_column_type}, \code{target_is_nullable}
#' @export
#'
#' @examples
#' \dontrun{
#' rc_conn <- redcapcustodian::connect_to_redcap_db()
#' rcc_billing_conn <- connect_to_rcc_billing_db()
#'
#' drift <- get_column_type_drift(
#'   source_conn = rc_conn,
#'   source_table = "redcap_projects",
#'   target_conn = rcc_billing_conn,
#'   target_table = "redcap_projects_history"
#' )
#' }
get_column_type_drift <- function(source_conn, source_table, target_conn, target_table) {
  column_info_query <- "
    SELECT
      COLUMN_NAME AS column_name,
      COLUMN_TYPE AS column_type,
      IS_NULLABLE AS is_nullable
    FROM information_schema.columns
    WHERE table_schema = DATABASE() AND table_name = ?
  "

  source_columns <- DBI::dbGetQuery(source_conn, column_info_query, params = list(source_table)) |>
    dplyr::rename(source_column_type = "column_type") |>
    dplyr::select("column_name", "source_column_type")

  target_columns <- DBI::dbGetQuery(target_conn, column_info_query, params = list(target_table)) |>
    dplyr::rename(target_column_type = "column_type", target_is_nullable = "is_nullable")

  dplyr::inner_join(source_columns, target_columns, by = "column_name") |>
    dplyr::filter(.data$source_column_type != .data$target_column_type)
}

#' build_column_alter_statements
#'
#' Builds the \code{ALTER TABLE ... MODIFY COLUMN} statements needed to bring a target
#' table's drifted columns in line with the source table's current column types, while
#' preserving each column's existing nullability on the target (see
#' \code{\link{get_column_type_drift}} for why nullability is never tightened to match the
#' source). Pure string-building function with no database access, given the output of
#' \code{\link{get_column_type_drift}}.
#'
#' @param drift, a dataframe as returned by \code{\link{get_column_type_drift}}
#' @param target_table, the name of the table the generated statements will alter
#'
#' @return a character vector of \code{ALTER TABLE} statements, one per drifted column
#' @export
#'
#' @examples
#' \dontrun{
#' build_column_alter_statements(drift, "redcap_projects_history")
#' }
build_column_alter_statements <- function(drift, target_table) {
  if (nrow(drift) == 0) {
    return(character(0))
  }

  nullability <- ifelse(drift$target_is_nullable == "YES", "NULL", "NOT NULL")

  sprintf(
    "ALTER TABLE %s MODIFY COLUMN `%s` %s %s",
    target_table,
    drift$column_name,
    drift$source_column_type,
    nullability
  )
}

#' reconcile_redcap_projects_history_schema
#'
#' Detects columns shared between a source and target table whose type has drifted, and
#' alters the target table's type to match the source (preserving the target's existing
#' nullability). Used to keep
#' \code{redcap_projects_history} in sync with upstream DDL revisions to REDCap's own
#' \code{redcap_projects} table (e.g. widened \code{VARCHAR}s or new \code{ENUM} values)
#' so that syncing data no longer fails with truncation errors.
#'
#' @param source_conn, a DBI connection to the database containing \code{source_table}
#' @param source_table, the name of the table to treat as the source of truth for column definitions
#' @param target_conn, a DBI connection to the database containing \code{target_table}
#' @param target_table, the name of the table to reconcile to match the source
#'
#' @return a dataframe of the drifted columns that were altered, as returned by
#'   \code{\link{get_column_type_drift}}
#' @export
#'
#' @examples
#' \dontrun{
#' rc_conn <- redcapcustodian::connect_to_redcap_db()
#' rcc_billing_conn <- connect_to_rcc_billing_db()
#'
#' drift <- reconcile_redcap_projects_history_schema(
#'   source_conn = rc_conn,
#'   source_table = "redcap_projects",
#'   target_conn = rcc_billing_conn,
#'   target_table = "redcap_projects_history"
#' )
#' }
reconcile_redcap_projects_history_schema <- function(source_conn, source_table, target_conn, target_table) {
  drift <- get_column_type_drift(source_conn, source_table, target_conn, target_table)

  alter_statements <- build_column_alter_statements(drift, target_table)

  for (statement in alter_statements) {
    DBI::dbExecute(target_conn, statement)
  }

  drift
}
