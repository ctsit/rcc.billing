# Calculate Service Request Time

This function takes time in minutes and hours and returns a unified time
format in hours.

## Usage

``` r
service_request_time(time_minutes, time_hours)
```

## Arguments

- time_minutes:

  Numeric vector representing time in minutes.

- time_hours:

  Numeric vector representing time in hours.

## Value

A numeric vector with the processed time in hours. For input times in
minutes that are part of the set {15, 30, 45, 60}, the time is converted
to hours. For time in hours greater than 1, the original hours are
returned. Otherwise, \`NA_real\_\` is returned for those cases not
matching the conditions.

## Examples

``` r
if (FALSE) { # \dontrun{
service_request_time(30, 120)
} # }
```
