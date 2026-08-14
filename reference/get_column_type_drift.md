# get_column_type_drift

Compares the column definitions of a source table and a target table via
`information_schema.columns` and returns the columns that are present in
both tables but whose target type differs from the type the target
should have (see
[`desired_column_type`](https://ctsit.github.io/rcc.billing/reference/desired_column_type.md)).
Used to detect when REDCap's own DDL revisions to `redcap_projects`
(e.g. a widened `VARCHAR`) have drifted out from under the frozen
`redcap_projects_history` mirror table, and to migrate any `ENUM` column
in the target to `VARCHAR`.

## Usage

``` r
get_column_type_drift(source_conn, source_table, target_conn, target_table)
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

  the name of the table to check for drifted column definitions

## Value

a dataframe with one row per drifted column: `column_name`,
`source_column_type`, `target_column_type`, `desired_column_type`,
`target_is_nullable`

## Details

Nullability is intentionally not compared: `redcap_projects_history`
accumulates rows over time, so older rows may legitimately hold `NULL`
in a column that REDCap has since made `NOT NULL`. Tightening the target
to match the source's nullability would fail against that pre-existing
data, so `target_is_nullable` is returned only so callers can preserve
the target's own current nullability when reconciling type.

## Examples

``` r
if (FALSE) { # \dontrun{
rc_conn <- redcapcustodian::connect_to_redcap_db()
rcc_billing_conn <- connect_to_rcc_billing_db()

drift <- get_column_type_drift(
  source_conn = rc_conn,
  source_table = "redcap_projects",
  target_conn = rcc_billing_conn,
  target_table = "redcap_projects_history"
)
} # }
```
