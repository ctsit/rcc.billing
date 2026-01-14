# sequester_projects

sequester projects listed in \`project_ids\` that can be sequestered

## Usage

``` r
sequester_projects(
  conn,
  project_id = as.numeric(NA),
  reason = as.character(NA)
)
```

## Arguments

- conn:

  \- a connection to a redcap database

- project_id:

  \- a vector of project IDs to be sequestered

- reason:

  \- a vector of reasons the project IDs were sequestered

## Value

\- a list describing the function activity via these objects

- project_ownership_sync_updates - updates made to project_ownership

- redcap_projects_sync_updates - updates made to redcap_projects

- project_ids_updated - project ids that received updates

## Examples
