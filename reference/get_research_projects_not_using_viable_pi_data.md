# get_research_projects_not_using_viable_pi_data

Returns the project_ids of projects that have no viable PI data

## Usage

``` r
get_research_projects_not_using_viable_pi_data(
  redcap_projects,
  redcap_entity_project_ownership,
  redcap_user_information
)
```

## Arguments

- redcap_projects, :

  The contents of the REDCap table of the same name.

- redcap_entity_project_ownership, :

  The contents of the REDCap Project Ownership table of the same name.

- redcap_user_information, :

  The contents of the REDCap table of the same name.

## Value

a vector of project IDs

## Examples

``` r
if (FALSE) { # \dontrun{
get_research_projects_not_using_viable_pi_data(
  redcap_projects = redcap_projects,
  redcap_entity_project_ownership = redcap_entity_project_ownership,
  redcap_user_information = redcap_user_information
)
} # }
```
