# Changelog

## rcc.billing 1.53.3 (released 2026-08-14)

- Convert ENUM columns to VARCHAR when reconciling
  redcap_projects_history ([@pbchase](https://github.com/pbchase))
  Comparing COLUMN_TYPE literals between source and target still failed
  with “Data truncated for column ‘survey_auth_min_fields’ \[1265\]”
  even though schema_drift reported zero rows: both sides already
  declared enum(‘1’,‘2’,‘3’), but REDCap’s live redcap_projects table
  already holds a legacy “” value for that row (predating strict mode)
  that isn’t a member of the enum at all. No amount of widening the
  ENUM’s member list fixes that, and REDCap will keep adding legal
  values over time regardless.

Add desired_column_type(), which maps any ENUM source type to
varchar(255) instead of mirroring it verbatim. get_column_type_drift()
now flags an ENUM-typed target column as drifted even when its literal
type matches the source, so reconciliation permanently migrates these
columns off ENUM rather than chasing each new or pre-existing
out-of-range value with another ALTER TABLE.

Assisted-by: Claude:claude-sonnet-5

- Stop tightening nullability when reconciling redcap_projects_history
  ([@pbchase](https://github.com/pbchase)) Reconciling a column’s
  nullability to match the live REDCap source (added in 8f103b0) failed
  with “Invalid use of NULL value \[1138\]” on
  allow_delete_record_from_log: the history table has legitimate legacy
  NULLs from before REDCap made that column NOT NULL, and MariaDB
  refuses to tighten a column to NOT NULL against existing NULL data.

get_column_type_drift() now only flags drift on COLUMN_TYPE, and
build_column_alter_statements() preserves the target’s own current
nullability instead of adopting the source’s, so reconciliation only
ever widens types (fixing truncation) and never fights existing rows.

Assisted-by: Claude:claude-sonnet-5

- Reconcile redcap_projects_history schema drift before sync
  ([@pbchase](https://github.com/pbchase)) redcap_projects_history was
  created once as a frozen snapshot of REDCap’s redcap_projects DDL, so
  upstream REDCap upgrades that widen columns or add ENUM values
  (e.g. survey_auth_min_fields) caused sync_redcap_projects_to_history.R
  to fail with “Data truncated for column” errors. Add
  reconcile_redcap_projects_history_schema(), which diffs
  information_schema.columns between source and target and issues ALTER
  TABLE MODIFY COLUMN statements to heal drift before each sync runs.
  Also switch the create-if-absent branch to copy REDCap’s live DDL via
  SHOW CREATE TABLE instead of dbWriteTable’s R-inferred types, so
  freshly created tables start from real column definitions.

Also includes incidental R CMD check build-warning fixes (RoxygenNote,
minimum R version, .Rbuildignore entries, tibble dependency).

Assisted-by: Claude:claude-sonnet-5

## rcc.billing 1.53.2 (released 2026-08-13)

- Fix pivot_wider dropping status columns in revenue report
  ([@pbchase](https://github.com/pbchase)) pivot_wider(names_from =
  status) only emits a column for statuses actually present in a given
  slice, so coalesce(invoiced, 0) errored with “object ‘invoiced’ not
  found” whenever no row in that slice had status == “invoiced”. Cast
  status to a factor with the full fixed level set and add names_expand
  = TRUE, values_fill = 0 so every expected status column always exists.

## rcc.billing 1.53.1 (released 2026-06-22)

- Update update_invoice_line_items_with_invoicing_details.R
  ([@pbchase](https://github.com/pbchase),
  [\#291](https://github.com/ctsit/rcc.billing/issues/291))
  - Add a test for bad data from CSBT; output an error file and issue a
    warning.
  - Fix correctable flaws in the CSBT input data file; add a patch file;
    filter out bad rows.
  - Test for rows in csbt_billable_details before attempting to do
    anything.
- Address Docker build warnings about platform and CMD parameters
  ([@pbchase](https://github.com/pbchase))
- Use ghcr.io/ctsit/redcapcustodian:latest in CI
  ([@pbchase](https://github.com/pbchase))
- Replace .zenodo.json with CITATION.cff
  ([@pbchase](https://github.com/pbchase))
- Add CLAUDE.md with codebase guidance for Claude Code
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.53.0 (released 2026-01-14)

- Update revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase),
  [\#289](https://github.com/ctsit/rcc.billing/issues/289))
  - Split free Contractual work from free Support work.
  - Add ‘Figure 5: Pro Bono costs in last FY as portion of revenue by
    service type’.
  - Add paragraph to explain the role of the new Figure 5 in annual rate
    review.
- Echo latest_payment_file_info at the end of
  update_invoice_line_items_with_invoicing_details.R
  ([@pbchase](https://github.com/pbchase))
- Fix path to tmp_invoice_file in
  create_and_send_new_invoice_line_items.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.52.3 (released 2025-09-30)

- Remove fix\_\* functions ([@pbchase](https://github.com/pbchase),
  [\#284](https://github.com/ctsit/rcc.billing/issues/284))

## rcc.billing 1.52.2 (released 2025-09-29)

- Move more test data to test scope
  ([@pbchase](https://github.com/pbchase),
  [\#283](https://github.com/ctsit/rcc.billing/issues/283))
  - Reconstruct make_test_data.R.
  - Convert rda to rds as we move them to test scope.
  - Delete 3 more man pages.

## rcc.billing 1.52.1 (released 2025-09-29)

- Move test data out of package space and remove tests of schema files
  ([@pbchase](https://github.com/pbchase),
  [\#282](https://github.com/ctsit/rcc.billing/issues/282))
  - Move test data from package scope to test scope.
  - Remove 9 man pages for test data that movwed to test scope.
  - Remove tests that were only testing schema files.
  - Remove helpers used only by the removed tests.
  - Fix tests that needed the rescoped test data.

## rcc.billing 1.52.0 (released 2025-09-09)

- Update update_invoice_line_items_with_invoicing_details.R
  ([@pbchase](https://github.com/pbchase),
  [\#281](https://github.com/ctsit/rcc.billing/issues/281)) Refine input
  file detection. Remove redundant fields from the CSBT we don’t listen
  to. Remove debug code. Fix date_of_pmt format

- Update transform_invoice_line_items_for_ctsit()
  ([@pbchase](https://github.com/pbchase),
  [\#281](https://github.com/ctsit/rcc.billing/issues/281)) Add tests of
  the input file from the CSBT.

## rcc.billing 1.51.1 (released 2025-08-29)

- Update update_invoice_line_items_with_invoicing_details.R
  ([@pbchase](https://github.com/pbchase))
  - Accept multiple date forms for date_of_pmt.

## rcc.billing 1.51.0 (released 2025-06-30)

- Add support for the IRB number in invoice line items
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [\#280](https://github.com/ctsit/rcc.billing/issues/280),
  [\#277](https://github.com/ctsit/rcc.billing/issues/277),
  [\#278](https://github.com/ctsit/rcc.billing/issues/278),
  [\#276](https://github.com/ctsit/rcc.billing/issues/276),
  [\#275](https://github.com/ctsit/rcc.billing/issues/275))
- Replaces direct call to S3 method from bit64
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [\#280](https://github.com/ctsit/rcc.billing/issues/280),
  [\#267](https://github.com/ctsit/rcc.billing/issues/267))
- Re-add VERSION ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.50.1 (released 2025-06-23)

- Remove VERSION file ([@pbchase](https://github.com/pbchase))
  - This is no longer needed because we are using gfrc to manage
    releases.
  - See
    <https://gist.github.com/pbchase/83a7a156b6deac90b5833246e1edb305>
- Relabel ‘CTSIT ID’ to ‘Other System Billing ID’ in csbt_column_names.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.50.0 (released 2025-06-09)

- Update create_and_send_new_invoice_line_items.R to send service
  request fiscal details. ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.49.0 (released 2025-06-02)

- Describe probono work
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [@pbchase](https://github.com/pbchase),
  [\#269](https://github.com/ctsit/rcc.billing/issues/269),
  [\#270](https://github.com/ctsit/rcc.billing/issues/270),
  [\#272](https://github.com/ctsit/rcc.billing/issues/272))
  - Add new pro bono section and related figures to
    revenue_status_and_projections.qmd
    ([@saipavan10-git](https://github.com/saipavan10-git),
    [@pbchase](https://github.com/pbchase),
    [\#269](https://github.com/ctsit/rcc.billing/issues/269),
    [\#272](https://github.com/ctsit/rcc.billing/issues/272))
  - ETL script and cron for daily sync to history table
    ([@saipavan10-git](https://github.com/saipavan10-git),
    [@pbchase](https://github.com/pbchase),
    [\#269](https://github.com/ctsit/rcc.billing/issues/269),
    [\#272](https://github.com/ctsit/rcc.billing/issues/272))
  - Create backfill_redcap_history_table.R
    ([@saipavan10-git](https://github.com/saipavan10-git),
    [@pbchase](https://github.com/pbchase),
    [\#269](https://github.com/ctsit/rcc.billing/issues/269),
    [\#272](https://github.com/ctsit/rcc.billing/issues/272))
- Remove Tracy Blair from the addressees
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.48.0 (released 2025-04-16)

- Add unsequester_erroneously_sequestered_projects.R and run it
  regularly ([@pbchase](https://github.com/pbchase))
- Update revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase),
  [\#271](https://github.com/ctsit/rcc.billing/issues/271))
  - Add contractual work concept.
  - Refocus report on three distinct services: : Annual Project Billing,
    Support Billing, and Contractual work
  - Remove income estimates.
  - Let plot y-axis float.
- Update WORKDIR in docker env (<saipavankamma@ufl.edu>,
  [\#266](https://github.com/ctsit/rcc.billing/issues/266))

## rcc.billing 1.47.0 (released 2025-03-12)

- Add REDCapServiceRequest.xml ([@pbchase](https://github.com/pbchase))
- Update get_service_request_lines() and
  get_service_request_line_items() for a simplified project design
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.46.5 (released 2025-03-05)

- Update create_and_send_new_invoice_line_items.R
  ([@pbchase](https://github.com/pbchase))
- Fix bug in service request time.
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.46.4 (released 2025-02-03)

- Disable write_uf_fiscal_orgs\* jobs
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.46.3 (released 2025-02-03)

- Remove reference to rcc.ctsit from
  request_correction_of_bad_ownership_data.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.46.2 (released 2025-01-28)

- Update sequester_orphans.R ([@pbchase](https://github.com/pbchase),
  [\#263](https://github.com/ctsit/rcc.billing/issues/263),
  [\#264](https://github.com/ctsit/rcc.billing/issues/264))

## rcc.billing 1.46.1 (released 2025-01-22)

- Handle an empty update in request_correction_of_bad_ownership_data.R
  ([@pbchase](https://github.com/pbchase))
- use get_user_rights to find project designers
  ([@ljwoodley](https://github.com/ljwoodley),
  [\#248](https://github.com/ctsit/rcc.billing/issues/248),
  [\#262](https://github.com/ctsit/rcc.billing/issues/262))

## rcc.billing 1.46.0 (released 2025-01-15)

- Add run_etl and use it ([@ljwoodley](https://github.com/ljwoodley),
  [\#260](https://github.com/ctsit/rcc.billing/issues/260),
  [\#261](https://github.com/ctsit/rcc.billing/issues/261))

## rcc.billing 1.45.0 (released 2024-01-03)

- Fix missing name_of_service_instance in
  get_service_request_line_items() result
  ([@pbchase](https://github.com/pbchase),
  [\#259](https://github.com/ctsit/rcc.billing/issues/259))
- Bill for some support requests without project IDs
  [\#257](https://github.com/ctsit/rcc.billing/issues/257)
  ([@pbchase](https://github.com/pbchase),
  [\#257](https://github.com/ctsit/rcc.billing/issues/257),
  [\#258](https://github.com/ctsit/rcc.billing/issues/258))
- Restyle get_service_request_lines.R
  ([@pbchase](https://github.com/pbchase),
  [\#258](https://github.com/ctsit/rcc.billing/issues/258))
- Add ProjectId to rcc.billing.Rproj
  ([@pbchase](https://github.com/pbchase),
  [\#258](https://github.com/ctsit/rcc.billing/issues/258))

## rcc.billing 1.44.3 (released 2024-12-04)

- Add is_sequestered filter to sequestered_orphans
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [\#255](https://github.com/ctsit/rcc.billing/issues/255),
  [\#256](https://github.com/ctsit/rcc.billing/issues/256))
- Merge pull request
  [\#253](https://github.com/ctsit/rcc.billing/issues/253) from
  pbchase/move_revenue_report_to_typst
  ([@pbchase](https://github.com/pbchase))
- Update revenue_status_and_projections.qmd
  ([@ljwoodley](https://github.com/ljwoodley),
  [@pbchase](https://github.com/pbchase),
  [\#253](https://github.com/ctsit/rcc.billing/issues/253))
  - Embed html resources
  - Adjust figure dimensions
  - Set default format to html
  - Add typst support
- Remove final dependency on rcc.ctsit
  ([@pbchase](https://github.com/pbchase),
  [\#253](https://github.com/ctsit/rcc.billing/issues/253))
- Update render_report.R to redcapcustodian 1.25.0
  ([@pbchase](https://github.com/pbchase),
  [\#253](https://github.com/ctsit/rcc.billing/issues/253))

## rcc.billing 1.44.2 (released 2024-09-09)

- Add source code URL to URL param in DESCRIPTION
  ([@pbchase](https://github.com/pbchase),
  [\#252](https://github.com/ctsit/rcc.billing/issues/252))
- Update run-tests.yml
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [\#252](https://github.com/ctsit/rcc.billing/issues/252))
- Add SIPOC for cleanup_bad_email_addresses
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [@pbchase](https://github.com/pbchase),
  [\#252](https://github.com/ctsit/rcc.billing/issues/252))

## rcc.billing 1.44.1 (released 2024-09-09)

- Update .zenodo.json with references and creator edits
  ([@pbchase](https://github.com/pbchase))
- Add DOI to README.md ([@pbchase](https://github.com/pbchase))
- Move functions and data into test space
  ([@pbchase](https://github.com/pbchase),
  [\#251](https://github.com/ctsit/rcc.billing/issues/251))
- Move cleanup_project_ownership_test_data into test space
  ([@pbchase](https://github.com/pbchase),
  [\#251](https://github.com/ctsit/rcc.billing/issues/251))

## rcc.billing 1.44.0 (released 2024-09-09)

- Update docs for publication ([@pbchase](https://github.com/pbchase))
- Reactivate cron/create_and_send_new_invoice_line_items
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.43.1 (released 2024-09-04)

- Deselect term_date_is_accurate in project functions
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.43.0 (released 2024-09-04)

- Fix doc formatting for service_request_time()
  ([@pbchase](https://github.com/pbchase))
- Add Sai’s ORCID in DESCRIPTION
  ([@pbchase](https://github.com/pbchase))
- Add employees to ctsit_staff\* data frames
  ([@pbchase](https://github.com/pbchase),
  [\#250](https://github.com/ctsit/rcc.billing/issues/250))
- Refactor ctsit_staff.R to make it easier to maintain
  ([@pbchase](https://github.com/pbchase),
  [\#250](https://github.com/ctsit/rcc.billing/issues/250))
- Update revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Add ‘REDCap consulting revenue’ section to
  revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Update vignettes ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.42.1 (released 2024-08-26)

- Remove unsuspended_high_privilege_faculty from
  cleanup_project_ownership_table.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.42.0 (released 2024-08-26)

- Add ORCIDs in package authors ([@pbchase](https://github.com/pbchase),
  [\#249](https://github.com/ctsit/rcc.billing/issues/249))
- Add vignettes for most ETLs and reports
  ([@pbchase](https://github.com/pbchase),
  [@saipavan10-git](https://github.com/saipavan10-git),
  [\#244](https://github.com/ctsit/rcc.billing/issues/244),
  [\#245](https://github.com/ctsit/rcc.billing/issues/245),
  [\#246](https://github.com/ctsit/rcc.billing/issues/246),
  [\#247](https://github.com/ctsit/rcc.billing/issues/247))

## rcc.billing 1.41.4 (released 2024-08-15)

- Update github workflows to allow R to correctly access the PAT
  ([@saipavan10-git](https://github.com/saipavan10-git))
- Update description file to have a REMOTES section for ctsit packages
  ([@saipavan10-git](https://github.com/saipavan10-git))
- Update dependency installation for pkgdown workflow
  ([@saipavan10-git](https://github.com/saipavan10-git))

## rcc.billing 1.41.3 (released 2024-08-14)

- Update github workflows to address missing dependencies
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.41.2 (released 2024-08-14)

- Update image version in run-tests.yaml
  ([@pbchase](https://github.com/pbchase))
- Resequence dependencies in pkgdown.yaml
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.41.1 (released 2024-08-14)

- Add dependencies in pkgdown.yaml
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.41.0 (released 2024-08-13)

- Add first vignette, cleanup_bad_email_addresses
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.40.0 (released 2024-08-13)

- Initialize pkgdown with usethis::use_pkgdown_github_pages()
  ([@pbchase](https://github.com/pbchase))
- Update README.md ([@pbchase](https://github.com/pbchase))
- Add schema upgrade scripts for release 1.39.0
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.39.1 (released 2024-08-13)

- Add return_all_records param to get_service_request_lines()
  ([@pbchase](https://github.com/pbchase),
  [\#239](https://github.com/ctsit/rcc.billing/issues/239))

## rcc.billing 1.39.0 (released 2024-08-13)

- Add fiscal_contact details to invoice_line_item
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [@pbchase](https://github.com/pbchase),
  [\#237](https://github.com/ctsit/rcc.billing/issues/237),
  [\#238](https://github.com/ctsit/rcc.billing/issues/238))

## rcc.billing 1.38.1 (released 2024-08-01)

- Read always_bill in update_probono_service_request_records.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.38.0 (released 2024-08-01)

- Deactivate create_and_send_new_invoice_line_items
  ([@pbchase](https://github.com/pbchase))
- Add support for always_bill in get_probono_service_request_updates()
  ([@pbchase](https://github.com/pbchase),
  [\#236](https://github.com/ctsit/rcc.billing/issues/236))
- Fix duckdb disconnect warnings caused by tests
  ([@pbchase](https://github.com/pbchase),
  [\#235](https://github.com/ctsit/rcc.billing/issues/235))
- Create and send service_request_line_items in
  create_and_send_new_invoice_line_items
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [@pbchase](https://github.com/pbchase),
  [\#235](https://github.com/ctsit/rcc.billing/issues/235))
- Add get_service_request_lines()
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [@pbchase](https://github.com/pbchase),
  [\#233](https://github.com/ctsit/rcc.billing/issues/233))
- Add get_service_request_line_items()
  ([@saipavan10-git](https://github.com/saipavan10-git),
  [@pbchase](https://github.com/pbchase),
  [\#233](https://github.com/ctsit/rcc.billing/issues/233))
- Add tests for get_project_details_for_billing()
  ([@pbchase](https://github.com/pbchase),
  [\#233](https://github.com/ctsit/rcc.billing/issues/233))
- Fix tests for get_target_projects_to_invoice()
  ([@pbchase](https://github.com/pbchase),
  [\#233](https://github.com/ctsit/rcc.billing/issues/233))
- Update docs for get_new_project_invoice_line_items()
  ([@pbchase](https://github.com/pbchase),
  [\#233](https://github.com/ctsit/rcc.billing/issues/233))

## rcc.billing 1.37.2 (released 2024-07-22)

- Refactor create_and_send_new_redcap_prod_per_project_line_items.R
  ([@pbchase](https://github.com/pbchase),
  [\#228](https://github.com/ctsit/rcc.billing/issues/228),
  [\#230](https://github.com/ctsit/rcc.billing/issues/230))
- Add get_new_project_invoice_line_items()
  ([@pbchase](https://github.com/pbchase),
  [\#228](https://github.com/ctsit/rcc.billing/issues/228),
  [\#230](https://github.com/ctsit/rcc.billing/issues/230)).
- Add get_new_project_service_instances()
  ([@pbchase](https://github.com/pbchase),
  [\#228](https://github.com/ctsit/rcc.billing/issues/228),
  [\#230](https://github.com/ctsit/rcc.billing/issues/230)).
- Add get_target_projects_to_invoice()
  ([@pbchase](https://github.com/pbchase),
  [\#228](https://github.com/ctsit/rcc.billing/issues/228),
  [\#230](https://github.com/ctsit/rcc.billing/issues/230)).

## rcc.billing 1.37.1 (released 2024-07-01)

- Fix bugs in revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.37.0 (released 2024-06-26)

- Adjust run times for update_probono_service_request_records
  ([@pbchase](https://github.com/pbchase))
- Add update_free_support_time_remaining.R and a schema file for the
  empty table it needs ([@pbchase](https://github.com/pbchase),
  [\#224](https://github.com/ctsit/rcc.billing/issues/224))
- Add people_with_rights_to_unpaid_invoice_line_items.R,
  get_project_flags(), and get_user_rights_and_info()
  ([@pbchase](https://github.com/pbchase),
  [\#220](https://github.com/ctsit/rcc.billing/issues/220))
- Rename get_user_rights_and_info() to get_user_rights_and_info_v1()
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.36.1 (released 2024-06-20)

- Mount the credentials volume in
  cron/update_probono_service_request_records
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.36.0 (released 2024-06-20)

- Add single-use/backfill_billable_rate_in_service_request_records.R
  ([@pbchase](https://github.com/pbchase))
- Modernize_imports_and_conform_to_tidyselect
  ([@pbchase](https://github.com/pbchase),
  [\#223](https://github.com/ctsit/rcc.billing/issues/223))
- Add get_service_request_lines()
  ([@ljwoodley](https://github.com/ljwoodley),
  [@pbchase](https://github.com/pbchase),
  [\#219](https://github.com/ctsit/rcc.billing/issues/219),
  [\#205](https://github.com/ctsit/rcc.billing/issues/205))
- Add update_invoice_line_items_to_correct_fiscal_year.R a script we
  used once in August 2023 ([@pbchase](https://github.com/pbchase))
- Add get_probono_service_request_records()
  ([@ljwoodley](https://github.com/ljwoodley),
  [@pbchase](https://github.com/pbchase),
  [\#218](https://github.com/ctsit/rcc.billing/issues/218),
  [\#207](https://github.com/ctsit/rcc.billing/issues/207))

## rcc.billing 1.35.0 (released 2024-05-23)

- Set custom CC when running revenue_status_and_projections
  ([@pbchase](https://github.com/pbchase))
- Revise input file search and management in
  update_invoice_line_items_with_invoicing_details.R
  ([@pbchase](https://github.com/pbchase))
- Add get_ctsi_study_id_to_project_id_map
  ([@ljwoodley](https://github.com/ljwoodley),
  [@pbchase](https://github.com/pbchase),
  [@ChemiKyle](https://github.com/ChemiKyle),
  [\#212](https://github.com/ctsit/rcc.billing/issues/212))
- Reflow make_test_data_for_get_billable_candidates.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.34.0 (released 2024-04-26)

- Add draft_reports.qmd ([@pbchase](https://github.com/pbchase))
- Add get_project_details_for_billing
  ([@ljwoodley](https://github.com/ljwoodley))
- Add ‘REDCap consulting’ service_type to test data
  ([@pbchase](https://github.com/pbchase))
- CC REDCAP_BILLING_L in request_correction_of_bad_ownership_data.R
  ([@pbchase](https://github.com/pbchase))
- Update create_and_send_new_redcap_prod_per_project_line_items.R to
  prevent duplicates ([@pbchase](https://github.com/pbchase))
- Update revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
  - Add a note to Figure 3. ‘REDCap APB Revenue by FY with 12 months of
    projected revenue.’  
  - Add more aging brackets.
  - Fix chit-chat about historic payment rate.
  - Remove ‘Possible revenue opportunities’.
  - Remove ‘Projecting income from extant billable projects’ section  
  - Remove ‘Payments Rates and Projections’ section.
  - Remove ‘Report summary’ section.

## rcc.billing 1.33.2 (released 2024-03-15)

- Fix NA in average_portion_paid in revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.33.1 (released 2024-03-12)

- Fix bad collect() syntax ([@pbchase](https://github.com/pbchase))
- Fix crashes in sequester_unpaid_projects.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.33.0 (released 2024-02-27)

- Add fiscal year reporting to revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Add revenue_description to
  red_team_auxiliary_revenue_actuals_redcap_apb.csv output in
  revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Control positioning in revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.32.0 (released 2024-02-14)

- Add owner’s org data to get_billable_candidates() output
  ([@pbchase](https://github.com/pbchase),
  [@ChemiKyle](https://github.com/ChemiKyle))

## rcc.billing 1.31.1 (released 2024-02-12)

- Update update_invoice_line_items_with_invoicing_details.R to fix
  target table in service_instance_update.
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.31.0 (released 2024-02-08)

- Curate and use CTSI Study IDs ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.30.0 (released 2024-01-12)

- Fix typos in text of revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Update update_invoice_line_items_with_invoicing_details to handle do
  not bill reasons ([@ChemiKyle](https://github.com/ChemiKyle))

## rcc.billing 1.29.1 (released 2023-12-08)

- Fix create_and_send_new_redcap_prod_per_project_line_items.R

## rcc.billing 1.29.0 (released 2023-11-21)

- Implement code changes required for the Fall 2023 rate increase
  ([@pbchase](https://github.com/pbchase))
- Update revenue_status_and_projections.qmd adding
  revenue_by_month_received ([@pbchase](https://github.com/pbchase),
  [@ljwoodley](https://github.com/ljwoodley))

## rcc.billing 1.28.0 (released 2023-11-01)

- Replace fig-revenue-by-month with fig-revenue-by-month-invoiced and
  fig-revenue-by-month-received in revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Add bar labels to fig-revenue-by-month-\* figures in
  revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Adjust project revenue to use just the last 12 months and correct for
  the rate increase in revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.27.0 (released 2023-10-31)

- Add delete_abandoned_projects.R
  ([@ljwoodley](https://github.com/ljwoodley))

## rcc.billing 1.26.0 (released 2023-10-24)

- Add export_project_data_with_owner_org.R
  ([@pbchase](https://github.com/pbchase))
- Change annual project price from \$100 to \$130 in warning
  communications ([@ChemiKyle](https://github.com/ChemiKyle))

## rcc.billing 1.25.1 (released 2023-10-16)

- Update broken Update Project Ownership links
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Prevent sequestered projects from receiving billing warnings
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Fix typo in revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Update broken link to document that details project deletion steps
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Update update_invoice_line_items_with_invoicing_details.R
  ([@pbchase](https://github.com/pbchase))
- Remove unused code from warn_owners_of_impending_bill.R
  ([@pbchase](https://github.com/pbchase))
- Update Roxygen version in DESCRIPTION
  ([@pbchase](https://github.com/pbchase))
- Update test-get_billable_candidates.R
  ([@pbchase](https://github.com/pbchase))
- Remove unused code from get_billable_candidates()
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.25.0 (released 2023-08-29)

- Update revenue_status_and_projections
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.24.0 (released 2023-08-23)

- Add write_uf_fiscal_orgs_to_person_org ETL
  ([@ChemiKyle](https://github.com/ChemiKyle))
- add cron job for invoice line item creation
  ([@ljwoodley](https://github.com/ljwoodley))
- add test for df row count ([@ljwoodley](https://github.com/ljwoodley))
- Replace current_fiscal_year with fiscal_year_invoiced concept
  ([@pbchase](https://github.com/pbchase))
- create empty please_fix_log df
  ([@ljwoodley](https://github.com/ljwoodley))

## rcc.billing 1.23.0 (released 2023-08-04)

- Add reason to project sequestration messages
  ([@pbchase](https://github.com/pbchase))
- Add speed improvements to get_orphaned_projects
  ([@pbchase](https://github.com/pbchase))
- Replace NA character with NA string to prevent entire email from
  appearing as NA ([@ChemiKyle](https://github.com/ChemiKyle))
- Refactor SQLite out of get_orphaned_projects and its tests
  ([@pbchase](https://github.com/pbchase))
- Update render report ([@ljwoodley](https://github.com/ljwoodley))

## rcc.billing 1.22.2 (released 2023-07-19)

- Accommodate very long project titles
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.22.1 (released 2023-07-19)

- Accommodate very long project titles
  ([@pbchase](https://github.com/pbchase))
- Update revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Update update_invoice_line_items_with_invoicing_details.R to handle
  non-rccbilling data ([@pbchase](https://github.com/pbchase))
- Update report_on_projects_by_dept.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.22.0 (released 2023-06-23)

- Add cancel_invoice_line_items.R
  ([@pbchase](https://github.com/pbchase))
- Add ban_people_from_ownership.R
  ([@pbchase](https://github.com/pbchase))
- Add report_on_projects_by_dept.R
  ([@pbchase](https://github.com/pbchase))
- Add get_billable_candidates() ([@pbchase](https://github.com/pbchase))
- Silence long path warnings relating to
  request_correction_of_bad_ownership_data.R
  ([@pbchase](https://github.com/pbchase))
- Refactor billable_candidates.R to use get_billable_candidates()
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.21.2 (released 2023-06-07)

- Update revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.21.1 (released 2023-06-07)

- Update cron file for new render_report.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.21.0 (released 2023-06-07)

- Update render_report.R to add Quarto support
  ([@pbchase](https://github.com/pbchase))
- Ban PIs who left UF in
  update_invoice_line_items_with_invoicing_details.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.20.0 (released 2023-06-05)

- Add revenue_status_and_projections.qmd
  ([@pbchase](https://github.com/pbchase))
- Update email templates ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.19.0 (released 2023-05-24)

- Add remind_owners_to_review_ownership.R
  ([@pbchase](https://github.com/pbchase))
- Update sequester_unpaid_projects.R
  ([@pbchase](https://github.com/pbchase))
- Add historic redcap admins to CTS-IT staff
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.18.2 (released 2023-04-03)

- Filter out sequestered and deleted projects in
  sequester_unpaid_projects.R ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.18.1 (released 2023-04-03)

- Fix NEWS.md ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.18.0 (released 2023-04-03)

- Add sequester_unpaid_projects ETL
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Set date_sent when creating invoice line items
  ([@pbchase](https://github.com/pbchase))
- Fix cron for write_uf_fiscal_orgs_to_org_hierarchies.R again
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.17.0 (released 2023-03-17)

- Add ETL to write to org_hierachies from VIVO Add schema for
  org_hierarchies ([@ChemiKyle](https://github.com/ChemiKyle))
- Add cron’d runs of sequester_orphans.R
  ([@pbchase](https://github.com/pbchase))
- Simplify manual sequestration in sequester_orphans.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.16.0 (released 2023-03-03)

- Add banned_owners rule to get_orphaned_projects function Add
  banned_owners schema ([@ChemiKyle](https://github.com/ChemiKyle))
- Embrace subdirectories for db specific tables in testing data
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Rename conn to mem\_conn in get_orphaned_projects test
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Prevent blank emails on new invoice line items
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.15.0 (released 2023-02-27)

- Add unit test for get_orphaned_projects()
  ([@pbchase](https://github.com/pbchase))
- Add unresolvable_ownership_issues to get_orphans function
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Add request_correction_of_bad_ownership_data report
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Add erasure of project ownership identification columns to
  cleanup_project_ownership_table
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Use variable instead of hardcoding in send_alert_email
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Prevent emails RE: unresolvable_ownership_issues in sequester_orphans
  ([@ChemiKyle](https://github.com/ChemiKyle))

## rcc.billing 1.14.0 (released 2023-01-27)

- Add complete_but_non_sequestered rule to get_orphaned_projects
  ([@pbchase](https://github.com/pbchase))
- Add warn_completers_of_impending_sequestration.R
  ([@pbchase](https://github.com/pbchase))
- Update version numbers in NEWS.md to conform to tagging error on
  2022-12-19 ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.13.0 (released 2023-01-24)

- Update get_orphaned_projects reducing the horizon from 12 to 11 months
  ([@pbchase](https://github.com/pbchase))
- Move warn_owners_of_impending_bill.R back to the default dates
  ([@pbchase](https://github.com/pbchase))
- Update update_invoice_line_items_with_invoicing_details.R
  ([@pbchase](https://github.com/pbchase))
- Revert error in sequester_orphans.R
  ([@pbchase](https://github.com/pbchase))
- Sync invoice_line_item table to RC DB during
  update_invoice_line_items_with_invoicing_details
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Mark CTSIT-owned projects as non-billable in
  update_project_billable_attribute.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.12.1 (released 2022-12-19)

- Load rcc.billing library in cleanup_bad_email_addresses.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 1.12.0 (released 2022-12-19)

- Port cleanup_bad_email_addresses from rcc.ctsit
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Create get_bad_emails_from_log
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Update billable_candidates.R ([@pbchase](https://github.com/pbchase))
- Ignore timestamp updates in
  update_invoice_line_items_with_invoicing_details.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.11.1 (released 2022-12-07)

- Temporarily move warn_owners_of_impending_bill to the 3rd and 14th of
  the month ([@pbchase](https://github.com/pbchase))
- Add a comment to guide manual orphan sequestration
  ([@pbchase](https://github.com/pbchase))
- Fix new row IDs in
  create_and_send_new_redcap_prod_per_project_line_items.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.11.0 (released 2022-11-14)

- Set invoice_line_item status conditionally based on date_of_pmt
  presence when loading data from CSBT
  ([@ChemiKyle](https://github.com/ChemiKyle))

## rcc.billing 0.10.0 (released 2022-11-02)

- Include project_irb_number in report/billable_candidates
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Add invoice facts to billable candidates
  ([@pbchase](https://github.com/pbchase))
- Re-enable empty_and_inactive_projects in get_orphaned_projects
  ([@pbchase](https://github.com/pbchase))
- Adjust the id column in new_invoice_line_item_communications to avoid
  collisions ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.9.1 (released 2022-10-28)

- Execute named lists construction with lst
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.9.0 (released 2022-10-28)

- Add transform_invoice_line_items_for_ctsit
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Add update_invoice_line_items_with_invoicing_details
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Add rule inactive_projects_with_no_viable_users to
  get_orphaned_projects() ([@pbchase](https://github.com/pbchase))
- Add orphaned_projects to logged data in sequester_orphans.R
  ([@pbchase](https://github.com/pbchase))
- Show user_lastlogin in billable_candidates.R
  ([@pbchase](https://github.com/pbchase))
- Use full month name instead of abbreviation in
  create_and_send_new_redcap_prod_per_project_line_items
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Associate month_invoiced with project’s birth month rather than script
  run month ([@ChemiKyle](https://github.com/ChemiKyle))

## rcc.billing 0.8.1 (released 2022-10-21)

- Include GITHUB_PAT in docker build step
  ([@pbchase](https://github.com/pbchase))
- Pass project_id vector to sequester_projects
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.8.0 (released 2022-10-21)

- Activate sequester_orphans.R ([@pbchase](https://github.com/pbchase))
- Add filter for no viable users to get_orphaned_projects
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Add get_user_rights_and_info ([@pbchase](https://github.com/pbchase))
- Install rcc.ctsit in Dockerfile using a GitHub PAT
  ([@pbchase](https://github.com/pbchase))
- Make PIs and faculty project owners
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.7.1 (released 2022-10-04)

- Update create_and_send_new_redcap_prod_per_project_line_items.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.7.0 (released 2022-10-04)

- Add get_orphaned_projects() ([@pbchase](https://github.com/pbchase))
- Add sequester_orphans ([@pbchase](https://github.com/pbchase))
- Add sequester_projects() ([@pbchase](https://github.com/pbchase))
- Fix service_type in service_type_test_data
  ([@pbchase](https://github.com/pbchase))
- Update csbt column output names CTSI IT ID -\> CTSIT ID
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Filter out non-sequestered projects in
  create_and_send_new_redcap_prod_per_project_line_items
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Include project_ownership user identifiers in
  create_and_send_new_redcap_prod_per_project_line_items.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.6.1 (released 2022-09-28)

- Run billable_candidates.R weekly
  ([@pbchase](https://github.com/pbchase))
- Fix subject, body, and from in billable_candidates.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.6.0 (released 2022-09-28)

- Create billable_candidates report
  ([@ChemiKyle](https://github.com/ChemiKyle))
- Add deleted projects filter and fix birthday_in_previous_month filter
  when creating invoice line
  items([@pbchase](https://github.com/pbchase))

## rcc.billing 0.5.0 (released 2022-09-22)

- Run warn_owners_of_impending_bill.R on 1st and 23rd of the month
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.4.0 (released 2022-09-07)

- Activate warn_owners_of_impending_bill.R
  ([@pbchase](https://github.com/pbchase))
- Add correct_project_pi_emails
  ([@ChemiKyle](https://github.com/ChemiKyle))

## rcc.billing 0.3.1 (released 2022-09-06)

- Catch and log all errors and successes in
  warn_owners_of_impending_bill.R
  ([@ChemiKyle](https://github.com/ChemiKyle),
  [@pbchase](https://github.com/pbchase))

## rcc.billing 0.3.0 (released 2022-09-01)

- Add warn_owners_of_impending_bill.R
  ([@ChemiKyle](https://github.com/ChemiKyle),
  [@pbchase](https://github.com/pbchase))

## rcc.billing 0.2.0 (released 2022-08-30)

- Add warn_owners_of_impending_bill.R
  ([@ChemiKyle](https://github.com/ChemiKyle))

## rcc.billing 0.1.2 (released 2022-08-26)

- Fix paths in cron files ([@pbchase](https://github.com/pbchase))
- Load rcc.billing in update_project_billable_attribute.R
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.1.1 (released 2022-08-26)

- Build rcc.billing in Dockerfile
  ([@pbchase](https://github.com/pbchase))

## rcc.billing 0.1.0 (released 2022-08-25)

- Add function connect_to_rcc_billing_db
- Add function convert_schema_to_sqlite
- Add function create_and_load_test_table
- Add function create_table
- Add function draft_communication_record_from_line_item
- Add function fix_data_in_invoice_line_item
- Add function fix_data_in_invoice_line_item_communication
- Add function fix_data_in_redcap_log_event
- Add function fix_data_in_redcap_projects
- Add function fix_data_in_redcap_user_information
- Add function get_creators
- Add function get_last_project_user
- Add function get_privileged_user
- Add function get_project_pis
- Add function get_projects_needing_new_owners
- Add function get_projects_without_owners
- Add function get_reassigned_line_items
- Add function get_unpaid_redcap_prod_per_project_line_items
- Add function invoice_line_item_df_from
- Add function mutate_columns_to_posixct
- Add function populate_table
- Add function transform_invoice_line_items_for_csbt
- Add function update_billable_by_ownership
- Add ETL cancel_redcap_prod_per_project_line_item.R
- Add ETL cleanup_project_ownership_table.R
- Add ETL create_and_send_new_redcap_prod_per_project_line_items.R
- Add ETL deploy_initial_rcc_billing_db.R
- Add ETL fix_bad_activity_and_login_dates.R
- Add ETL reassign_redcap_prod_per_project_line_item.R
- Add ETL receive_payments.R
- Add ETL update_ctsi_study_ids.R
- Add ETL update_project_billable_attribute.R
- Add dataset cleanup_project_ownership_test_data
- Add dataset csbt_column_names
- Add dataset ctsit_staff
- Add dataset ctsit_staff_employment_periods
- Add dataset fiscal_years
- Add dataset invoice_line_item_communications_test_data
- Add dataset invoice_line_item_reasons
- Add dataset invoice_line_item_statuses
- Add dataset invoice_line_item_test_data
- Add dataset one_deleted_project_record
- Add dataset projects_table_fragment
- Add dataset redcap_entity_project_ownership_test_data
- Add dataset redcap_log_event_test_data
- Add dataset redcap_projects_test_data
- Add dataset redcap_user_information_test_data
- Add dataset service_instance_test_data
- Add dataset service_type_test_data

## rcc.billing 0.0.0 (released 2022-03-21)

- Initial commit of rcc.billing, an automated, data-driven service
  billing system implemented on REDCap Custodian
  ([@pbchase](https://github.com/pbchase))
