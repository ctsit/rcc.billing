# Map CTSI Study IDs to Project IDs

This function generates a mapping between CTSI study IDs and project IDs
from invoice_line_item and REDCap Service Request project. It filters
invoice line items for service type codes 1 and 2, ensuring CTSI study
IDs are present, and collects distinct pairs of project IDs and CTSI
study IDs.

## Usage

``` r
get_ctsi_study_id_to_project_id_map(service_requests, rcc_billing_conn)
```

## Arguments

- service_requests:

  A data frame of service requests, REDCap Service Request PID 1414.

- rcc_billing_conn:

  A connection to REDCap billing database.
  [`connect_to_rcc_billing_db`](https://ctsit.github.io/rcc.billing/reference/connect_to_rcc_billing_db.md)

## Value

A data frame with distinct project_id and ctsi_study_id columns,
representing the mapping between project IDs and CTSI study IDs.

## Examples

``` r
if (FALSE) { # \dontrun{
get_ctsi_study_id_to_project_id_map(service_requests, rcc_billing_conn)
} # }
```
