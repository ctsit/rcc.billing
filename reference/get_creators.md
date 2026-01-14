# get_creators

Returns a dataframe of project creator usernames for non-suspended,
non-redcap-staff, with a primary email address. Suspended creators can
be optionally included.

## Usage

``` r
get_creators(
  redcap_projects,
  redcap_user_information,
  redcap_staff_employment_periods,
  include_suspended_users = FALSE,
  return_project_ownership_format = FALSE
)
```

## Arguments

- redcap_projects, :

  The contents of the REDCap table of the same name.

- redcap_user_information, :

  The contents of the REDCap table of the same name.

- redcap_staff_employment_periods, :

  a dataset of redcap usernames and employment intervals with one
  interval per row

- include_suspended_users, :

  Include users whose accounts are suspended

- return_project_ownership_format, :

  Rename the columns to match the redcap_entity_project_ownership format

## Value

a dataframe of project creators from redcap_projects

## Examples

``` r
if (FALSE) { # \dontrun{
unsuspended_creators <- get_creators(
  redcap_projects = redcap_projects,
  redcap_user_information = redcap_user_information,
  redcap_staff_employment_periods = ctsit_staff_employment_periods,
  return_project_ownership_format = T
)

creators <- get_creators(
  redcap_projects = redcap_projects,
  redcap_user_information = redcap_user_information,
  redcap_staff_employment_periods = ctsit_staff_employment_periods,
  include_suspended_users = T,
  return_project_ownership_format = T
)
} # }
```
