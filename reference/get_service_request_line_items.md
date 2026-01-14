# Assemble line items for service requests billing

Assemble line items for service requests billing

## Usage

``` r
get_service_request_line_items(service_requests, rc_billing_conn, rc_conn)
```

## Arguments

- service_requests:

  A data frame of service requests, REDCap Service Request PID 1414.

- rc_billing_conn:

  A connection to REDCap billing database containing an
  invoice_line_items table.
  [`connect_to_rcc_billing_db`](https://ctsit.github.io/rcc.billing/reference/connect_to_rcc_billing_db.md)

- rc_conn:

  A connection to REDCap database.
  [`connect_to_redcap_db`](https://ctsit.github.io/redcapcustodian/reference/connect_to_redcap_db.html)

## Value

A data frame of line items for service requests billing.

## Examples

``` r
if (FALSE) { # \dontrun{
line_items <- get_service_request_line_items(service_requests, rc_billing_conn, rc_conn)
} # }
```
