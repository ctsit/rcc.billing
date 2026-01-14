# get_projects_without_owners

Returns the project_ids of projects without owners

## Usage

``` r
get_projects_without_owners(redcap_projects, redcap_entity_project_ownership)
```

## Arguments

- redcap_projects, :

  The contents of the REDCap table of the same name.

- redcap_entity_project_ownership, :

  The contents of the REDCap Project Ownership table of the same name.

## Value

a vector of project IDs

## Examples

``` r
if (FALSE) { # \dontrun{
get_projects_without_owners(
  redcap_projects = redcap_projects,
  redcap_entity_project_ownership = redcap_entity_project_ownership
)
} # }
```
