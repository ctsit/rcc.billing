# Get new project billing invoice line items given a dataframe of projects to invoice, the initial state of the invoice_line_item table, a connection to the redcap database, and a connection to the rcc billing database.

Get new project billing invoice line items given a dataframe of projects
to invoice, the initial state of the invoice_line_item table, a
connection to the redcap database, and a connection to the rcc billing
database.

## Usage

``` r
get_new_project_invoice_line_items(
  projects_to_invoice,
  initial_invoice_line_item,
  rc_conn,
  rcc_billing_conn,
  api_uri
)
```

## Arguments

- projects_to_invoice:

  a dataframe of projects to invoice

- initial_invoice_line_item:

  a dataframe with the initial state of the invoice_line_item table

- rc_conn:

  a DBI connection to the REDCap database

- rcc_billing_conn:

  a DBI connection to the rcc billing database

- api_uri:

  the URI to the redcap host's API interface

## Value

a data frame of new invoice line items

## Examples
