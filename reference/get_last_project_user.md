# get_last_project_user

Returns the last user to log an event on a project. This function is not
vectorized.

## Usage

``` r
get_last_project_user(con, pid)
```

## Arguments

- con, :

  a DBI connection object to a REDCap database

- pid, :

  the project ID of the project of interest.

## Value

Username of the last user to log an actio against the project

## Examples

``` r
if (FALSE) { # \dontrun{
  get_last_project_user(
    con = redcap_connection,
    pid = project_id
  )
} # }
```
