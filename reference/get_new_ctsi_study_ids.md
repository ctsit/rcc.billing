# Find previously unknown CTSI Study IDs in invoice_line_item data

Given the service instance data and invoice line item data, identify the
new CTSI Study IDs in the invoice line item data and return them on
revised service instance records.

## Usage

``` r
get_new_ctsi_study_ids(service_instance, invoice_line_item)
```

## Arguments

- service_instance:

  \- a dataframe of service_instance wiht the columns
  \`service_instance_id\` and \`ctsi_study_id\`. Most likely this is the
  entire contents of that table.

- invoice_line_item:

  \- a dataframe of invoice line item records with the columns
  \`service_instance_id\` and \`ctsi_study_id\`.

## Value

service_instance records from the service_instance where
\`ctsi_study_id\` was NA but is now known.

## Examples

``` r
if (FALSE) { # \dontrun{
library(redcapcustodian)
library(rcc.billing)
library(RMariaDB)
library(DBI)
library(tidyverse)
library(dotenv)

rcc_billing_conn <- connect_to_rcc_billing_db()

service_instance <- tbl(rcc_billing_conn, "service_instance") %>%
collect()

invoice_line_item <- tbl(rcc_billing_conn, "invoice_line_item") %>%
collect()

get_new_ctsi_study_ids(service_instance, invoice_line_item)
} # }
```
