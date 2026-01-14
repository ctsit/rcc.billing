# Update Pro Bono Service Request Details

This function processes a dataframe of service requests to update the
billable_rate field

## Usage

``` r
get_probono_service_request_updates(service_requests)
```

## Arguments

- service_requests:

  A dataframe containing service request data from REDCap PID 1414

## Value

Returns a dataframe with updated billable rate

## Examples

``` r
if (FALSE) { # \dontrun{
updates <- get_probono_service_request_updates(service_requests)
} # }
```
