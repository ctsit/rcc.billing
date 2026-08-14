# Package index

## All functions

- [`build_column_alter_statements()`](https://ctsit.github.io/rcc.billing/reference/build_column_alter_statements.md)
  : build_column_alter_statements

- [`connect_to_rcc_billing_db()`](https://ctsit.github.io/rcc.billing/reference/connect_to_rcc_billing_db.md)
  : Connect to the rcc_billing DB

- [`csbt_column_names`](https://ctsit.github.io/rcc.billing/reference/csbt_column_names.md)
  : csbt_column_names

- [`ctsit_staff`](https://ctsit.github.io/rcc.billing/reference/ctsit_staff.md)
  : CTS-IT Staff

- [`ctsit_staff_employment_periods`](https://ctsit.github.io/rcc.billing/reference/ctsit_staff_employment_periods.md)
  : ctsit_staff_employment_periods

- [`desired_column_type()`](https://ctsit.github.io/rcc.billing/reference/desired_column_type.md)
  : desired_column_type

- [`draft_communication_record_from_line_item()`](https://ctsit.github.io/rcc.billing/reference/draft_communication_record_from_line_item.md)
  :

  Adds metadata necessary for sending emails to an invoice_line_item
  dataframe, e.g. `transform_invoice_line_items_for_csbt`

- [`fiscal_years`](https://ctsit.github.io/rcc.billing/reference/fiscal_years.md)
  : fiscal_years

- [`get_bad_emails_from_log()`](https://ctsit.github.io/rcc.billing/reference/get_bad_emails_from_log.md)
  : Get bad email addresses from the rcc.billing log

- [`get_billable_candidates()`](https://ctsit.github.io/rcc.billing/reference/get_billable_candidates.md)
  : get_billable_candidates

- [`get_column_type_drift()`](https://ctsit.github.io/rcc.billing/reference/get_column_type_drift.md)
  : get_column_type_drift

- [`get_creators()`](https://ctsit.github.io/rcc.billing/reference/get_creators.md)
  : get_creators

- [`get_ctsi_study_id_to_project_id_map()`](https://ctsit.github.io/rcc.billing/reference/get_ctsi_study_id_to_project_id_map.md)
  : Map CTSI Study IDs to Project IDs

- [`get_last_project_user()`](https://ctsit.github.io/rcc.billing/reference/get_last_project_user.md)
  : get_last_project_user

- [`get_new_ctsi_study_ids()`](https://ctsit.github.io/rcc.billing/reference/get_new_ctsi_study_ids.md)
  : Find previously unknown CTSI Study IDs in invoice_line_item data

- [`get_new_project_invoice_line_items()`](https://ctsit.github.io/rcc.billing/reference/get_new_project_invoice_line_items.md)
  : Get new project billing invoice line items given a dataframe of
  projects to invoice, the initial state of the invoice_line_item table,
  a connection to the redcap database, and a connection to the rcc
  billing database.

- [`get_new_project_service_instances()`](https://ctsit.github.io/rcc.billing/reference/get_new_project_service_instances.md)
  : Get new service instances that need to be created given a dataframe
  of projects_to_invoice and a dataframe of the initial service_instance
  records.

- [`get_orphaned_projects()`](https://ctsit.github.io/rcc.billing/reference/get_orphaned_projects.md)
  : get_orphaned_projects

- [`get_privileged_user()`](https://ctsit.github.io/rcc.billing/reference/get_privileged_user.md)
  : get_privileged_user

- [`get_probono_service_request_updates()`](https://ctsit.github.io/rcc.billing/reference/get_probono_service_request_updates.md)
  : Update Pro Bono Service Request Details

- [`get_project_details_for_billing()`](https://ctsit.github.io/rcc.billing/reference/get_project_details_for_billing.md)
  : Get Project Details for Billing

- [`get_project_flags()`](https://ctsit.github.io/rcc.billing/reference/get_project_flags.md)
  : Get important boolean flags that describe projects

- [`get_project_pis()`](https://ctsit.github.io/rcc.billing/reference/get_project_pis.md)
  : get_project_pis

- [`get_projects_needing_new_owners()`](https://ctsit.github.io/rcc.billing/reference/get_projects_needing_new_owners.md)
  : get_projects_needing_new_owners

- [`get_projects_without_owners()`](https://ctsit.github.io/rcc.billing/reference/get_projects_without_owners.md)
  : get_projects_without_owners

- [`get_reassigned_line_items()`](https://ctsit.github.io/rcc.billing/reference/get_reassigned_line_items.md)
  : Get a dataframe of reassigned line items

- [`get_research_projects_not_using_viable_pi_data()`](https://ctsit.github.io/rcc.billing/reference/get_research_projects_not_using_viable_pi_data.md)
  : get_research_projects_not_using_viable_pi_data

- [`get_service_request_line_items()`](https://ctsit.github.io/rcc.billing/reference/get_service_request_line_items.md)
  : Assemble line items for service requests billing

- [`get_service_request_lines()`](https://ctsit.github.io/rcc.billing/reference/get_service_request_lines.md)
  : Get Service Request Lines

- [`get_target_projects_to_invoice()`](https://ctsit.github.io/rcc.billing/reference/get_target_projects_to_invoice.md)
  : Get details on the projects we need to create invoice line items
  for.

- [`get_unpaid_redcap_prod_per_project_line_items()`](https://ctsit.github.io/rcc.billing/reference/get_unpaid_redcap_prod_per_project_line_items.md)
  : Create a dataframe of unpaid REDCap production per project line
  items that were sent

- [`get_user_rights_and_info()`](https://ctsit.github.io/rcc.billing/reference/get_user_rights_and_info.md)
  : Get every attribute of every permission entry and userinfo for each
  user on each permission

- [`get_user_rights_and_info_v1()`](https://ctsit.github.io/rcc.billing/reference/get_user_rights_and_info_v1.md)
  : get_user_rights_and_info_v1

- [`invoice_line_item_df_from()`](https://ctsit.github.io/rcc.billing/reference/invoice_line_item_df_from.md)
  : Creates a invoice_line_item data from
  invoice_line_item_communications_data

- [`log_event_tables`](https://ctsit.github.io/rcc.billing/reference/log_event_tables.md)
  : log_event_tables

- [`mutate_columns_to_posixct()`](https://ctsit.github.io/rcc.billing/reference/mutate_columns_to_posixct.md)
  : mutate_columns_to_posixct

- [`next_n_months()`](https://ctsit.github.io/rcc.billing/reference/next_n_months.md)
  : next_n_months

- [`previous_month()`](https://ctsit.github.io/rcc.billing/reference/previous_month.md)
  : previous_month

- [`previous_n_months()`](https://ctsit.github.io/rcc.billing/reference/previous_n_months.md)
  : previous_n_months

- [`reconcile_redcap_projects_history_schema()`](https://ctsit.github.io/rcc.billing/reference/reconcile_redcap_projects_history_schema.md)
  : reconcile_redcap_projects_history_schema

- [`sequester_projects()`](https://ctsit.github.io/rcc.billing/reference/sequester_projects.md)
  : sequester_projects

- [`service_request_time()`](https://ctsit.github.io/rcc.billing/reference/service_request_time.md)
  : Calculate Service Request Time

- [`transform_invoice_line_items_for_csbt()`](https://ctsit.github.io/rcc.billing/reference/transform_invoice_line_items_for_csbt.md)
  : Renames columns of a dataframe from CTSIT format to CSBT format

- [`transform_invoice_line_items_for_ctsit()`](https://ctsit.github.io/rcc.billing/reference/transform_invoice_line_items_for_ctsit.md)
  : Renames columns of a dataframe from CSBT format to CTSIT format

- [`update_billable_by_ownership()`](https://ctsit.github.io/rcc.billing/reference/update_billable_by_ownership.md)
  : Get a dataframe of updated billable status for project ownership
  projects, set all projects as billable except those created by CTS-IT
  staff

- [`update_billable_if_owned_by_ctsit()`](https://ctsit.github.io/rcc.billing/reference/update_billable_if_owned_by_ctsit.md)
  : Get a dataframe of updated billable status for project owned by
  CTS-IT staff
