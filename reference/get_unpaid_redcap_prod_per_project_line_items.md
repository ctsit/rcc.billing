# Create a dataframe of unpaid REDCap production per project line items that were sent

Create a dataframe of unpaid REDCap production per project line items
that were sent

## Usage

``` r
get_unpaid_redcap_prod_per_project_line_items(rcc_billing_conn)
```

## Arguments

- rcc_billing_conn:

  \- A connection to REDCap billing database.
  [`connect_to_rcc_billing_db`](https://ctsit.github.io/rcc.billing/reference/connect_to_rcc_billing_db.md)

## Value

A dataframe containing unpaid sent line items

## Examples

``` r
if (FALSE) sent_line_items <- get_unpaid_redcap_prod_per_project_line_items(rcc_billing_conn) # \dontrun{}
```
