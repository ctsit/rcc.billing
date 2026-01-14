# get_orphaned_projects

Return a dataframe of projects that have been orphaned

## Usage

``` r
get_orphaned_projects(rc_conn, rcc_billing_conn, months_previous = 0)
```

## Arguments

- rc_conn:

  \- a connection to a redcap database,
  [`connect_to_redcap_db`](https://ctsit.github.io/redcapcustodian/reference/connect_to_redcap_db.html)

- rcc_billing_conn:

  \- a connection to an rcc_billing database,
  [`connect_to_rcc_billing_db`](https://ctsit.github.io/rcc.billing/reference/connect_to_rcc_billing_db.md)

- months_previous:

  \- the nth month previous to today to consider

## Value

a dataframe describing orphaned projects

- project_id - project_id of the orphaned project

- reason - why this project was selected

- priority - the priority of the reason

## Examples

``` r
if (FALSE) { # \dontrun{
get_orphaned_projects(
  rc_conn = rc_conn,
  rcc_billing_conn = rcc_billing_conn,
  months_previous = 0
)
} # }
```
