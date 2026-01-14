# get_privileged_user

Returns a dataframe of project IDs and usernames of users with design or
user_rights who are non-suspended, non-redcap-staff, with a primary
email address. Optionally include users with any privilege on the
project. Optionally include suspended users.

## Usage

``` r
get_privileged_user(
  redcap_projects,
  redcap_user_information,
  redcap_staff_employment_periods,
  redcap_user_rights,
  redcap_user_roles,
  include_low_privilege_users = FALSE,
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

- redcap_user_rights, :

  The contents of the REDCap table of the same name.

- redcap_user_roles, :

  The contents of the REDCap table of the same name.

- include_low_privilege_users, :

  Include users whose accounts have any permission on a project

- include_suspended_users, :

  Include users whose accounts are suspended

- return_project_ownership_format, :

  Rename the columns to match the redcap_entity_project_ownership format

## Value

a dataframe of privileged, \_user\_ accounts on the projects provided

## Examples

``` r
if (FALSE) { # \dontrun{
unsuspended_high_privilege_user <- get_privileged_user(
  redcap_projects = redcap_projects,
  redcap_user_information = redcap_user_information,
  redcap_staff_employment_periods = ctsit_staff_employment_periods,
  redcap_user_rights = redcap_user_rights,
  redcap_user_roles = redcap_user_roles,
  return_project_ownership_format = T
)

unsuspended_low_privilege_user <- get_privileged_user(
  redcap_projects = redcap_projects,
  redcap_user_information = redcap_user_information,
  redcap_staff_employment_periods = ctsit_staff_employment_periods,
  redcap_user_rights = redcap_user_rights,
  redcap_user_roles = redcap_user_roles,
  include_low_privilege_users = T,
  include_suspended_users = FALSE,
  return_project_ownership_format = T
)

any_low_privilege_user <- get_privileged_user(
  redcap_projects = redcap_projects,
  redcap_user_information = redcap_user_information,
  redcap_staff_employment_periods = ctsit_staff_employment_periods,
  redcap_user_rights = redcap_user_rights,
  redcap_user_roles = redcap_user_roles,
  include_low_privilege_users = T,
  include_suspended_users = T,
  return_project_ownership_format = T
)
} # }
```
