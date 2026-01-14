# Get Project Details for Billing

This function retrieves detailed information about specific projects for
billing purposes. It queries:

- redcap_projects

- redcap_entity_project_ownership

- redcap_user_information

- invoice_line_item

## Usage

``` r
get_project_details_for_billing(rc_conn, rcc_billing_conn, project_ids)
```

## Arguments

- rc_conn:

  A REDCap database connection, e.g. the object returned from
  [`connect_to_redcap_db`](https://ctsit.github.io/redcapcustodian/reference/connect_to_redcap_db.html)

- rcc_billing_conn:

  A connection to REDCap billing database.
  [`connect_to_rcc_billing_db`](https://ctsit.github.io/rcc.billing/reference/connect_to_rcc_billing_db.md)

- project_ids:

  Vector of project IDs to retrieve details for.

## Value

A data frame with project details.

## Examples

``` r
if (FALSE) { # \dontrun{
rc_conn <- connect_to_redcap_db()
rcc_billing_conn <- connect_to_rcc_billing_db()
project_ids <- c(12, 14, 22)
project_details <- get_project_details_for_billing(rc_conn, rcc_billing_con, project_ids)
} # }
```
