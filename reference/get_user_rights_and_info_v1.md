# get_user_rights_and_info_v1

Get redcap_user_rights combined with roles and user_information

## Usage

``` r
get_user_rights_and_info_v1(
  redcap_user_rights,
  redcap_user_roles,
  redcap_user_information
)
```

## Arguments

- redcap_user_rights, :

  The contents of the REDCap table of the same name.

- redcap_user_roles, :

  The contents of the REDCap table of the same name.

- redcap_user_information, :

  The contents of the REDCap table of the same name.

## Value

a dataframe of combined redcap_user_rights, roles, and user_information

## Examples

``` r
if (FALSE) { # \dontrun{
get_user_rights_and_info_v1(
  redcap_user_rights,
  redcap_user_roles,
  redcap_user_information
)
} # }
```
