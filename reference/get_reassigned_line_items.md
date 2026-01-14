# Get a dataframe of reassigned line items

After invoices are sent, investigators will report that some projects do
not belong to them. CTS-IT will change the ownership of these projects
to a new owner. In other cases, the customer will change the ownership
of a project themselves. CTS-IT will create a report of differences in
project ownership and investigators of each unpaid, invoiced project.
CTS-IT will update its tables and tell CSBT about the change.

## Usage

``` r
get_reassigned_line_items(sent_line_items, rc_conn)
```

## Arguments

- sent_line_items:

  a dataframe returned by
  [`get_unpaid_redcap_prod_per_project_line_items`](https://ctsit.github.io/rcc.billing/reference/get_unpaid_redcap_prod_per_project_line_items.md)

- rc_conn:

  \- A REDCap database connection, e.g. the object returned from
  [`connect_to_redcap_db`](https://ctsit.github.io/redcapcustodian/reference/connect_to_redcap_db.html)

## Value

A dataframe of revised redcap project invoice_line_items reassigned to
new owners

## See also

[`csbt_column_names`](https://ctsit.github.io/rcc.billing/reference/csbt_column_names.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rc_conn <- connect_to_redcap_db()
rcc_billing_conn <- connect_to_rcc_billing_db()

sent_line_items <- get_unpaid_redcap_prod_per_project_line_items(rcc_billing_conn)
reassigned_line_items <- get_reassigned_line_items(sent_line_items, rc_conn)
} # }
```
