# Renames columns of a dataframe from CTSIT format to CSBT format

Excludes non-CSBT columns and renames CTSIT column names to the
corresponding CSBT names. This function is the inverse of
[`transform_invoice_line_items_for_ctsit`](https://ctsit.github.io/rcc.billing/reference/transform_invoice_line_items_for_ctsit.md)

## Usage

``` r
transform_invoice_line_items_for_csbt(invoice_line_items)
```

## Arguments

- invoice_line_items:

  A dataframe with the CTSIT column names

## Value

The input dataframe with columns adjusted to match CSBT format

## Details

DETAILS

## See also

[`csbt_column_names`](https://ctsit.github.io/rcc.billing/reference/csbt_column_names.md)

## Examples

``` r
if (FALSE) { # \dontrun{
tbl(conn, "invoice_line_item") |>
  collect() |>
  transform_invoice_line_items_for_csbt()
} # }
```
