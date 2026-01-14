# Get every attribute of every permission entry and userinfo for each user on each permission

Get every attribute of every permission entry and userinfo for each user
on each permission

## Usage

``` r
get_user_rights_and_info(
  rc_conn,
  require_active_account = T,
  require_active_permissions = T
)
```

## Arguments

- rc_conn:

  A DBI connection object to a REDCap Database on a system.

- require_active_account:

  A boolean to indicate if only active accounts are returned. Defaults
  to TRUE.

- require_active_permissions:

  A boolean to indicate if only active permission entries are returned.
  Defaults to TRUE.

## Value

a dataframe of every permission entry and with the matching
user_information data appended

## Examples

``` r
if (FALSE) { # \dontrun{
get_user_rights_and_info(
  rc_conn = rc_conn,
  require_active_account = T,
  require_active_permissions = T
)
} # }
```
