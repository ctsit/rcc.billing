# Get a dataframe of updated billable status for project ownership projects, set all projects as billable except those created by CTS-IT staff

Get a dataframe of updated billable status for project ownership
projects, set all projects as billable except those created by CTS-IT
staff

## Usage

``` r
update_billable_by_ownership(conn)
```

## Arguments

- conn:

  \- A REDCap database connection, e.g. the object returned from
  [`connect_to_redcap_db`](https://ctsit.github.io/redcapcustodian/reference/connect_to_redcap_db.html)

## Value

A
[`dataset_diff`](https://ctsit.github.io/redcapcustodian/reference/dataset_diff.html)
containing updates to project ownership's "billable" column

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- redcapcustodian::connect_to_redcap_db()
billable_updates <- update_billable_by_ownership(conn)
dbx::dbxUpdate(conn,
  table = "redcap_entity_project_ownership",
  records = billable_updates$update_records,
  where_cols = c("id")
)
} # }
```
