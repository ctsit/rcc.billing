# Get details on the projects we need to create invoice line items for.

Get details on the projects we need to create invoice line items for.

## Usage

``` r
get_target_projects_to_invoice(rc_conn)
```

## Arguments

- rc_conn:

  A connection to a REDCap database that uses the project_ownership
  module

## Value

a dataframe of project and owner details

## Examples

``` r
if (FALSE) { # \dontrun{
  get_target_projects_to_invoice(rc_conn)
} # }
```
