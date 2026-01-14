# ctsit_staff_employment_periods

Usernames and employment periods of CTS-IT staff. This dataset will be
used to inform default data ownership and setting billable flags in the
REDCap Entity / Project Ownership table. Multiple employment periods are
represented as multiple rows

## Usage

``` r
ctsit_staff_employment_periods
```

## Format

A data frame with 7 rows and 2 variables:

- `redcap_username`:

  character a REDCap username. Typically this is Gatorlink ID.

- `employment_interval`:

  double a lubridate time interval

- `term_date_is_accurate`:

  boolean indicating the quality of the termination date
