# reconcile_redcap_projects_history_schema

Detects columns shared between a source and target table whose target
type does not match its desired type (see
[`desired_column_type`](https://ctsit.github.io/rcc.billing/reference/desired_column_type.md)),
and alters the target table's type accordingly, preserving the target's
existing nullability. Used to keep `redcap_projects_history` in sync
with upstream DDL revisions to REDCap's own `redcap_projects` table
(e.g. widened `VARCHAR`s), and to migrate any `ENUM` column to `VARCHAR`
so that syncing data no longer fails with truncation errors caused by
new or pre-existing values outside the `ENUM`'s declared member list.

## Usage

``` r
reconcile_redcap_projects_history_schema(
  source_conn,
  source_table,
  target_conn,
  target_table
)
```

## Arguments

- source_conn, :

  a DBI connection to the database containing `source_table`

- source_table, :

  the name of the table to treat as the source of truth for column
  definitions

- target_conn, :

  a DBI connection to the database containing `target_table`

- target_table, :

  the name of the table to reconcile to match the source

## Value

a dataframe of the drifted columns that were altered, as returned by
[`get_column_type_drift`](https://ctsit.github.io/rcc.billing/reference/get_column_type_drift.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rc_conn <- redcapcustodian::connect_to_redcap_db()
rcc_billing_conn <- connect_to_rcc_billing_db()

drift <- reconcile_redcap_projects_history_schema(
  source_conn = rc_conn,
  source_table = "redcap_projects",
  target_conn = rcc_billing_conn,
  target_table = "redcap_projects_history"
)
} # }
```
