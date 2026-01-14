# Get Service Request Lines

This function processes a dataset of service requests to extract and
transform various service details. It groups response data by record_id,
service_date, and probono status to summarize response data and create a
source dataset for invoice line items and other tasks. It returns one
month of data. By default, the data returned is from the previous month.
You can return the current month by setting \`months_previous = 0\`. Get
earlier months by setting \`months_previous\` to higher number.

## Usage

``` r
get_service_request_lines(
  service_requests,
  return_all_records = F,
  months_previous = 1
)
```

## Arguments

- service_requests:

  A data frame of service requests, REDCap Service Request PID 1414.

- return_all_records:

  A boolean to indicate every record should be returned or just last
  month's records

- months_previous:

  A double indicating how many months back we should look when querying
  the service_requests for service_request_lines. Defaults to 1.

## Value

A data frame with response details to the service request

## Details

To get \_all\_ of the data, set \`return_all_records = T\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# get just last month's records
service_request_lines <- get_service_request_lines(service_requests)

# get all the records
service_request_lines <- get_service_request_lines(service_requests, return_all_records = T)
} # }
```
