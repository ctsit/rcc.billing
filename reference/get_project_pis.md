# get_project_pis

Returns a dataframe of all project_PI details in redcap_projects for PIs
with an email address in project_pi_email

## Usage

``` r
get_project_pis(redcap_projects, return_project_ownership_format = FALSE)
```

## Arguments

- redcap_projects, :

  The contents of the REDCap table of the same name.

- return_project_ownership_format, :

  Rename the columns to match the redcap_entity_project_ownership format

## Value

a dataframe of project_PI details from redcap_projects

## Examples

``` r
if (FALSE) { # \dontrun{
get_project_pis(
  redcap_projects = redcap_projects,
  return_project_ownership_format = TRUE
)
} # }
```
