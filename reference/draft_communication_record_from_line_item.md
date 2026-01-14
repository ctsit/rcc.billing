# Adds metadata necessary for sending emails to an invoice_line_item dataframe, e.g. [`transform_invoice_line_items_for_csbt`](https://ctsit.github.io/rcc.billing/reference/transform_invoice_line_items_for_csbt.md)

Adds metadata necessary for sending emails to an invoice_line_item
dataframe, e.g.
[`transform_invoice_line_items_for_csbt`](https://ctsit.github.io/rcc.billing/reference/transform_invoice_line_items_for_csbt.md)

## Usage

``` r
draft_communication_record_from_line_item(invoice_line_items)
```

## Arguments

- invoice_line_items:

  A dataframe from the invoice_line_item table

## Value

The input dataframe with the following columns added:

- updated - A timestamp provided by
  [`get_script_run_time`](https://ctsit.github.io/redcapcustodian/reference/get_script_run_time.html)

- sender - The value set in `Sys.getenv("EMAIL_FROM")`

- recipient - The value set in `Sys.getenv("EMAIL_TO")`

- date_sent - A timestamp provided by
  [`get_script_run_time`](https://ctsit.github.io/redcapcustodian/reference/get_script_run_time.html)

- date_received - A placeholder timestamp, `as.POSIXct(NA)`

- script_name - The script name returned by
  [`get_script_name`](https://ctsit.github.io/redcapcustodian/reference/get_script_name.html)

## Examples

``` r
if (FALSE) { # \dontrun{
tbl(conn, "invoice_line_item") |>
  collect() |>
  draft_communication_record_from_line_item()
} # }
```
