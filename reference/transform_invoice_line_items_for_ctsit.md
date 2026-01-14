# Renames columns of a dataframe from CSBT format to CTSIT format

Renames CSBT column names to the corresponding CTSIT names. This
function is the inverse of
[`transform_invoice_line_items_for_csbt`](https://ctsit.github.io/rcc.billing/reference/transform_invoice_line_items_for_csbt.md),
however it does NOT exclude columns not in CTSIT column names.

## Usage

``` r
transform_invoice_line_items_for_ctsit(invoice_line_items)
```

## Arguments

- invoice_line_items:

  A dataframe with the CSBT column names

## Value

The input dataframe with columns adjusted to match CTSIT format

## Details

DETAILS

## See also

[`csbt_column_names`](https://ctsit.github.io/rcc.billing/reference/csbt_column_names.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df_from_csbt |>
  transform_invoice_line_items_for_ctsit() |>
  janitor::clean_names()
} # }
```
