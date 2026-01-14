# get_projects_needing_new_owners

Returns the project IDs of projects that are owned by a REDCap user that
has no primary email address

## Usage

``` r
get_projects_needing_new_owners(
  redcap_entity_project_ownership,
  redcap_user_information
)
```

## Arguments

- redcap_entity_project_ownership, :

  The contents of the REDCap Project Ownership table of the same name.

- redcap_user_information, :

  The contents of the REDCap table of the same name.

## Value

a vector of project IDs

## Examples

``` r
if (FALSE) { # \dontrun{
get_projects_needing_new_owners(
  redcap_entity_project_ownership = redcap_entity_project_ownership,
  redcap_user_information = redcap_user_information
)
} # }
```
