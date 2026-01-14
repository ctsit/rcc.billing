# Get new service instances that need to be created given a dataframe of projects_to_invoice and a dataframe of the initial service_instance records.

Get new service instances that need to be created given a dataframe of
projects_to_invoice and a dataframe of the initial service_instance
records.

## Usage

``` r
get_new_project_service_instances(
  projects_to_invoice,
  initial_service_instance
)
```

## Arguments

- projects_to_invoice:

  \- the projects for which we will need to create invoice line items

- initial_service_instance:

  \- a dataframe of the existing service instance table

## Value

a dataframe of new service_instance rows

## Examples

``` r
if (FALSE) { # \dontrun{
  get_new_project_service_instances(projects_to_invoice, initial_service_instance)
} # }
```
