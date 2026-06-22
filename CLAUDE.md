# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project overview

`rcc.billing` is an R package that implements an automated, data-driven
billing system for a REDCap instance. It was built by the University of
Florida’s CTS-IT to charge an annual per-project fee and to bill for
support work logged in a REDCap-based service request system. The
package ships with ETL scripts, cron job definitions, and reporting
scripts — all deployed together in a Docker container derived from the
`redcapcustodian` base image.

## Commands

### Run all tests

``` r

devtools::test()
```

### Run a single test file

``` r

devtools::test(filter = "get_billable_candidates")
```

### Lint

``` r

lintr::lint_package()
```

Configured in `.lintr`: max line length 300, max object name length 45.

### Build the R package tarball and install it

``` r

devtools::install()
```

### Build and tag the Docker image

``` sh
./build.sh
```

### Build and deploy cron/env files to the host

``` sh
./build.sh -d
```

### Run an ETL script inside the container

``` sh
docker run -v /rcc/rcc.billing:/root --rm \
  --env-file /rcc/default.env --env-file /rcc/rcc.billing/prod.env \
  rcc.billing Rscript etl/run_etl.R etl/<script_name>.R
```

`run_etl.R` is a thin wrapper that calls `rscript()` and emails a
failure log on error.

## Architecture

### Two databases

All ETL scripts connect to two databases via DBI:

| Connection var | Database | Contains |
|----|----|----|
| `rc_conn` | REDCap’s own MariaDB | `redcap_projects`, `redcap_entity_project_ownership`, `redcap_user_information`, `redcap_user_rights`, `redcap_log_event*`, etc. |
| `rcc_billing_conn` | rcc_billing MariaDB | `invoice_line_item`, `service_instance`, `invoice_line_item_communications`, `person_org`, `org_hierarchies`, `banned_owners`, `free_support_time_remaining`, `service_type` |

[`connect_to_rcc_billing_db()`](https://ctsit.github.io/rcc.billing/reference/connect_to_rcc_billing_db.md)
(in `R/rcc.billing.R`) reads `RCCBILLING_*` environment variables.
REDCap DB credentials come from
[`redcapcustodian::connect_to_redcap_db()`](https://ctsit.github.io/redcapcustodian/reference/connect_to_redcap_db.html)
which reads `REDCAP_DB_*` vars.

### Layer responsibilities

- **`R/`** — exported package functions (the reusable library layer).
  Functions `get_*` query and transform data; functions `transform_*`
  reshape data for specific consumers (e.g. CSBT invoice system).
- **`etl/`** — runnable R scripts that wire together library functions,
  write results back to the DB, send emails, and log outcomes via
  [`redcapcustodian::log_job_success()`](https://ctsit.github.io/redcapcustodian/reference/log_job_success.html).
- **`cron/`** — one file per ETL, containing the cron schedule line that
  runs the ETL inside the container via `docker run`.
- **`report/`** — R and Quarto scripts for internal reporting (billable
  candidates, revenue projections).
- **`inst/schema/`** — SQL DDL for all rcc_billing tables and
  incremental upgrade scripts.
- **`data-raw/` / `data/`** — bundled reference datasets:
  `fiscal_years`, `ctsit_staff`, `csbt_column_names`,
  `log_event_tables`.

### Key business concepts

- **Billable candidate**: a REDCap project marked `billable = 1` in
  `redcap_entity_project_ownership`, at least one year old, with a known
  owner email.
- **Service instance**: one row per (project or service-request, service
  type) pair — the thing being billed. `service_type_code = 1` is the
  annual per-project fee; other codes are support services.
- **Invoice line item**: one charge row per service instance per billing
  period (fiscal year + month). Status lifecycle:
  `new → sent → paid / cancelled`.
- **Sequestration**: projects that are orphaned or have unpaid bills are
  moved to REDCap’s *completed* state and flagged `sequestered = 1` in
  the ownership table; a UI hack (external module) re-labels these for
  users.
- **Fiscal year**: stored in `data/fiscal_years.rda`; label format
  matches the CSBT invoicing system (e.g. `"FY2025"`).

### ETL script pattern

Every ETL follows this skeleton (see
`etl/create_and_send_new_invoice_line_items.R` as the canonical
example):

1.  `init_etl("script_name")` — sets script name and run time in
    `redcapcustodian` package scope.
2.  Connect to both DBs.
3.  Snapshot initial state of affected tables.
4.  Compute desired state using library functions.
5.  Use
    [`redcapcustodian::dataset_diff()`](https://ctsit.github.io/redcapcustodian/reference/dataset_diff.html) +
    [`redcapcustodian::sync_table()`](https://ctsit.github.io/redcapcustodian/reference/sync_table.html)
    to apply only the delta.
6.  Build an `activity_log` list and call
    `log_job_success(jsonlite::toJSON(activity_log))`.
7.  Disconnect both DBs.

### Testing pattern

Tests use in-memory **duckdb** databases populated from `.rds` fixtures
in `tests/testthat/<test-directory>/`. Helper functions
`create_a_table_from_rds_test_data()` and `create_a_table_from_rds()`
load these fixtures into DBI connections. The `setup.R` file initialises
`redcapcustodian` package-scope variables (`set_script_run_time()`,
`set_script_name()`). All PII in test fixtures is hashed with a
session-scoped salt (see `helper-common.R`).

### Environment / configuration

Scripts read all secrets from environment variables. The
`examples/testing.env` file documents all required variables (database
credentials, SMTP settings, email addresses, REDCap URI). In production,
env files live under `/rcc/rcc.billing/` on the host and are mounted
into the container at `/root`.
