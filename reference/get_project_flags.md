# Get important boolean flags that describe projects

Get important boolean flags that describe projects

## Usage

``` r
get_project_flags(rc_conn)
```

## Arguments

- rc_conn:

  A DBI connection object to a REDCap Database on a system that uses the
  UF extensions to REDCap Project Ownership

## Value

a dataframe of boolean flags for every REDCap project in the
redcap_projects table

## Examples

``` r
if (FALSE) { # \dontrun{
  get_project_flags(rc_conn)
} # }
```
