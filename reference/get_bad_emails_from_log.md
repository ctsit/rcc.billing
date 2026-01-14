# Get bad email addresses from the rcc.billing log

Get bad email addresses from the rcc.billing log

## Usage

``` r
get_bad_emails_from_log(age_of_oldest_log_in_days = 8)
```

## Arguments

- age_of_oldest_log_in_days:

  \- an optional parameter indicating age in days of the oldest log fie
  to be read. Defaults to 8 days.

## Value

A vector of email addresses that resulted in an error in the rcc_job_log
table

## Examples

``` r
if (FALSE) { # \dontrun{
bad_recipients <- get_bad_emails_from_log(age_of_oldest_log_in_days = 31)
} # }
```
