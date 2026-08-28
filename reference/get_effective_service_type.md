# Reduce a service_type data frame to the row effective for each service_type_code as of a given date

\`service_type\` may contain multiple rows per \`service_type_code\`,
distinguished by \`start_date\`, so that rate changes can be scheduled
by adding a row rather than editing code. This collapses the table to
the single row per \`service_type_code\` that is in effect as of
\`as_of_date\`: the row with the latest \`start_date\` that is not after
\`as_of_date\`. A row with \`start_date = NA\` is treated as effective
since the beginning of time and is only used as a fallback when no dated
row applies yet.

## Usage

``` r
get_effective_service_type(service_type, as_of_date)
```

## Arguments

- service_type:

  a dataframe with columns service_type_code, start_date, and other
  service_type columns

- as_of_date:

  the date to resolve the effective rate for

## Value

a dataframe with one row per service_type_code, each the row effective
as of as_of_date

## Examples

``` r
if (FALSE) { # \dontrun{
get_effective_service_type(service_type, redcapcustodian::get_script_run_time())
} # }
```
