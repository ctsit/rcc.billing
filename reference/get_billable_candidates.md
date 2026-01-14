# get_billable_candidates

Return a data frame of REDCap projects with relevant billing details

## Usage

``` r
get_billable_candidates(rc_conn, rcc_billing_conn)
```

## Arguments

- rc_conn:

  \- DBI connection object to a REDCap database

- rcc_billing_conn:

  \- DBI connection object to an rcc.billing database

## Value

A dataframe of REDCap projects with relevant billing details

## Examples

``` r
if (FALSE) { # \dontrun{

library(tidyverse)
library(rcc.billing)
library(lubridate)
library(DBI)
library(dotenv)
library(redcapcustodian)

init_etl("billable_candidates")

rc_conn <- connect_to_redcap_db()
rcc_billing_conn <- connect_to_rcc_billing_db()

billable_candidates <- get_billable_candidates(rc_conn, rcc_billing_conn)
} # }
```
