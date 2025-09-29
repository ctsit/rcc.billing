#' Sample data for invoice_line_item table
#'
#' @format
#' \describe{
#'   \item{\code{id}}{the primary key}
#'   \item{\code{service_identifier}}{either a redcap project ID, or redcap username}
#'   \item{\code{service_type_code}}{a numeric code that uniquely identifies the service_type}
#'   \item{\code{service_instance_id}}{a numeric code that uniquely identifies the service_instance}
#'   \item{\code{ctsi_study_id}}{CSBT's unique identifier for a service}
#'   \item{\code{name_of_service}}{name of the service}
#'   \item{\code{name_of_service_instance}}{name of the study}
#'   \item{\code{other_system_invoicing_comments}}{additional invoice information, either project url, or sponsor and pi}
#'   \item{\code{price_of_service}}{price of the service, in US dollars}
#'   \item{\code{qty_provided}}{quantity provided}
#'   \item{\code{amount_due}}{amount due, in US dollars}
#'   \item{\code{fiscal_year}}{fiscal year of the invoice}
#'   \item{\code{month_invoiced}}{month of the invoice}
#'   \item{\code{pi_last_name}}{last name of the person invoiced}
#'   \item{\code{pi_first_name}}{first name of the person invoiced}
#'   \item{\code{pi_email}}{email of the person invoiced}
#'   \item{\code{gatorlink}}{gatorlink of the person invoiced}
#'   \item{\code{reason}}{reason for the invoice}
#'   \item{\code{status}}{status of the invoice}
#'   \item{\code{sender}}{character: message sender, typically an email address}
#'   \item{\code{recipient}}{character: message recipient, typically an email address}
#'   \item{\code{date_sent}}{POSIXct: date CTSI sent the message}
#'   \item{\code{date_received}}{POSIXct: date CTSIT received the email}
#'   \item{\code{script_name}}{character: the script that created this record}
#'   \item{\code{created}}{created at timestamp}
#'   \item{\code{updated}}{updated at timestamp}
#' }
#'
#' @source \url{https://github.com/ctsit/rcc.billing/issues/3}
"invoice_line_item_test_data"

#' @title CTS-IT Staff
#' @description usernames of CTS-IT staff
#' @format A data frame with 6 rows and 1 variable:
#' \describe{
#'   \item{\code{redcap_username}}{character: a REDCap username. Typically this is Gatorlink ID.}
#' }
"ctsit_staff"

#' @title ctsit_staff_employment_periods
#' @description Usernames and employment periods of CTS-IT staff. This
#' dataset will be used to inform default data ownership and setting billable flags
#' in the REDCap Entity / Project Ownership table. Multiple employment periods are
#' represented as multiple rows
#' @format A data frame with 7 rows and 2 variables:
#' \describe{
#'   \item{\code{redcap_username}}{character a REDCap username. Typically this is Gatorlink ID.}
#'   \item{\code{employment_interval}}{double a lubridate time interval}
#'   \item{\code{term_date_is_accurate}}{boolean indicating the quality of the termination date}
#'}
"ctsit_staff_employment_periods"

#' @title one_deleted_project_record
#' @description A single REDCap project record for a deleted project
#' @format A data frame with 1 rows and 149 variables:
#' \describe{
#'   \item{\code{project_id}}{double}
#'   \item{\code{project_name}}{character}
#'   \item{\code{app_title}}{character}
#'   \item{\code{status}}{integer}
#'   \item{\code{creation_time}}{double}
#'   \item{\code{production_time}}{double}
#'   \item{\code{inactive_time}}{double}
#'   \item{\code{completed_time}}{double}
#'   \item{\code{completed_by}}{character}
#'   \item{\code{data_locked}}{integer}
#'   \item{\code{log_event_table}}{character}
#'   \item{\code{created_by}}{integer}
#'   \item{\code{draft_mode}}{integer}
#'   \item{\code{surveys_enabled}}{integer}
#'   \item{\code{repeatforms}}{integer}
#'   \item{\code{scheduling}}{integer}
#'   \item{\code{purpose}}{integer}
#'   \item{\code{purpose_other}}{character}
#'   \item{\code{show_which_records}}{integer}
#'   \item{\code{__SALT__}}{character}
#'   \item{\code{count_project}}{integer}
#'   \item{\code{investigators}}{character}
#'   \item{\code{project_note}}{character}
#'   \item{\code{online_offline}}{integer}
#'   \item{\code{auth_meth}}{character}
#'   \item{\code{double_data_entry}}{integer}
#'   \item{\code{project_language}}{character}
#'   \item{\code{project_encoding}}{character}
#'   \item{\code{is_child_of}}{character}
#'   \item{\code{date_shift_max}}{integer}
#'   \item{\code{institution}}{character}
#'   \item{\code{site_org_type}}{character}
#'   \item{\code{grant_cite}}{character}
#'   \item{\code{project_contact_name}}{character}
#'   \item{\code{project_contact_email}}{character}
#'   \item{\code{headerlogo}}{character}
#'   \item{\code{auto_inc_set}}{integer}
#'   \item{\code{custom_data_entry_note}}{character}
#'   \item{\code{custom_index_page_note}}{character}
#'   \item{\code{order_id_by}}{character}
#'   \item{\code{custom_reports}}{character}
#'   \item{\code{report_builder}}{character}
#'   \item{\code{disable_data_entry}}{integer}
#'   \item{\code{google_translate_default}}{character}
#'   \item{\code{require_change_reason}}{integer}
#'   \item{\code{dts_enabled}}{integer}
#'   \item{\code{project_pi_firstname}}{character}
#'   \item{\code{project_pi_mi}}{character}
#'   \item{\code{project_pi_lastname}}{character}
#'   \item{\code{project_pi_email}}{character}
#'   \item{\code{project_pi_alias}}{character}
#'   \item{\code{project_pi_username}}{character}
#'   \item{\code{project_pi_pub_exclude}}{integer}
#'   \item{\code{project_pub_matching_institution}}{character}
#'   \item{\code{project_irb_number}}{character}
#'   \item{\code{project_grant_number}}{character}
#'   \item{\code{history_widget_enabled}}{integer}
#'   \item{\code{secondary_pk}}{character}
#'   \item{\code{secondary_pk_display_value}}{integer}
#'   \item{\code{secondary_pk_display_label}}{integer}
#'   \item{\code{custom_record_label}}{character}
#'   \item{\code{display_project_logo_institution}}{integer}
#'   \item{\code{imported_from_rs}}{integer}
#'   \item{\code{display_today_now_button}}{integer}
#'   \item{\code{auto_variable_naming}}{integer}
#'   \item{\code{randomization}}{integer}
#'   \item{\code{enable_participant_identifiers}}{integer}
#'   \item{\code{survey_email_participant_field}}{character}
#'   \item{\code{survey_phone_participant_field}}{character}
#'   \item{\code{data_entry_trigger_url}}{character}
#'   \item{\code{template_id}}{integer}
#'   \item{\code{date_deleted}}{double}
#'   \item{\code{data_resolution_enabled}}{integer}
#'   \item{\code{field_comment_edit_delete}}{integer}
#'   \item{\code{realtime_webservice_enabled}}{integer}
#'   \item{\code{realtime_webservice_type}}{character}
#'   \item{\code{realtime_webservice_offset_days}}{double}
#'   \item{\code{realtime_webservice_offset_plusminus}}{character}
#'   \item{\code{last_logged_event}}{double}
#'   \item{\code{edoc_upload_max}}{integer}
#'   \item{\code{file_attachment_upload_max}}{integer}
#'   \item{\code{survey_queue_custom_text}}{character}
#'   \item{\code{survey_queue_hide}}{integer}
#'   \item{\code{survey_auth_enabled}}{integer}
#'   \item{\code{survey_auth_field1}}{character}
#'   \item{\code{survey_auth_event_id1}}{integer}
#'   \item{\code{survey_auth_field2}}{character}
#'   \item{\code{survey_auth_event_id2}}{integer}
#'   \item{\code{survey_auth_field3}}{character}
#'   \item{\code{survey_auth_event_id3}}{integer}
#'   \item{\code{survey_auth_min_fields}}{character}
#'   \item{\code{survey_auth_apply_all_surveys}}{integer}
#'   \item{\code{survey_auth_custom_message}}{character}
#'   \item{\code{survey_auth_fail_limit}}{integer}
#'   \item{\code{survey_auth_fail_window}}{integer}
#'   \item{\code{twilio_enabled}}{integer}
#'   \item{\code{twilio_modules_enabled}}{character}
#'   \item{\code{twilio_hide_in_project}}{integer}
#'   \item{\code{twilio_account_sid}}{character}
#'   \item{\code{twilio_auth_token}}{character}
#'   \item{\code{twilio_from_number}}{double}
#'   \item{\code{twilio_voice_language}}{character}
#'   \item{\code{twilio_option_voice_initiate}}{integer}
#'   \item{\code{twilio_option_sms_initiate}}{integer}
#'   \item{\code{twilio_option_sms_invite_make_call}}{integer}
#'   \item{\code{twilio_option_sms_invite_receive_call}}{integer}
#'   \item{\code{twilio_option_sms_invite_web}}{integer}
#'   \item{\code{twilio_default_delivery_preference}}{character}
#'   \item{\code{twilio_request_inspector_checked}}{double}
#'   \item{\code{twilio_request_inspector_enabled}}{integer}
#'   \item{\code{twilio_append_response_instructions}}{integer}
#'   \item{\code{twilio_multiple_sms_behavior}}{character}
#'   \item{\code{twilio_delivery_preference_field_map}}{character}
#'   \item{\code{two_factor_exempt_project}}{integer}
#'   \item{\code{two_factor_force_project}}{integer}
#'   \item{\code{disable_autocalcs}}{integer}
#'   \item{\code{custom_public_survey_links}}{character}
#'   \item{\code{pdf_custom_header_text}}{character}
#'   \item{\code{pdf_show_logo_url}}{integer}
#'   \item{\code{pdf_hide_secondary_field}}{integer}
#'   \item{\code{pdf_hide_record_id}}{integer}
#'   \item{\code{shared_library_enabled}}{integer}
#'   \item{\code{allow_delete_record_from_log}}{integer}
#'   \item{\code{delete_file_repository_export_files}}{integer}
#'   \item{\code{custom_project_footer_text}}{character}
#'   \item{\code{custom_project_footer_text_link}}{character}
#'   \item{\code{google_recaptcha_enabled}}{integer}
#'   \item{\code{datamart_allow_repeat_revision}}{integer}
#'   \item{\code{datamart_allow_create_revision}}{integer}
#'   \item{\code{datamart_enabled}}{integer}
#'   \item{\code{break_the_glass_enabled}}{integer}
#'   \item{\code{datamart_cron_enabled}}{integer}
#'   \item{\code{datamart_cron_end_date}}{double}
#'   \item{\code{fhir_include_email_address_project}}{integer}
#'   \item{\code{file_upload_vault_enabled}}{integer}
#'   \item{\code{file_upload_versioning_enabled}}{integer}
#'   \item{\code{missing_data_codes}}{character}
#'   \item{\code{record_locking_pdf_vault_enabled}}{integer}
#'   \item{\code{record_locking_pdf_vault_custom_text}}{character}
#'   \item{\code{fhir_cdp_auto_adjudication_enabled}}{integer}
#'   \item{\code{fhir_cdp_auto_adjudication_cronjob_enabled}}{integer}
#'   \item{\code{project_dashboard_min_data_points}}{integer}
#'   \item{\code{bypass_branching_erase_field_prompt}}{integer}
#'   \item{\code{protected_email_mode}}{integer}
#'   \item{\code{protected_email_mode_custom_text}}{character}
#'   \item{\code{protected_email_mode_trigger}}{character}
#'   \item{\code{protected_email_mode_logo}}{integer}
#'   \item{\code{hide_filled_forms}}{integer}
#'   \item{\code{form_activation_survey_autocontinue}}{integer}
#' }
"one_deleted_project_record"

#' @title projects_table_fragment
#' @description a portion of a REDCap project table used to make test datasets
#' @format A data frame with 4 rows and 149 variables:
#' \describe{
#'   \item{\code{project_id}}{double}
#'   \item{\code{project_name}}{character}
#'   \item{\code{app_title}}{character}
#'   \item{\code{status}}{integer}
#'   \item{\code{creation_time}}{double}
#'   \item{\code{production_time}}{double}
#'   \item{\code{inactive_time}}{double}
#'   \item{\code{completed_time}}{double}
#'   \item{\code{completed_by}}{character}
#'   \item{\code{data_locked}}{integer}
#'   \item{\code{log_event_table}}{character}
#'   \item{\code{created_by}}{integer}
#'   \item{\code{draft_mode}}{integer}
#'   \item{\code{surveys_enabled}}{integer}
#'   \item{\code{repeatforms}}{integer}
#'   \item{\code{scheduling}}{integer}
#'   \item{\code{purpose}}{integer}
#'   \item{\code{purpose_other}}{character}
#'   \item{\code{show_which_records}}{integer}
#'   \item{\code{__SALT__}}{character}
#'   \item{\code{count_project}}{integer}
#'   \item{\code{investigators}}{character}
#'   \item{\code{project_note}}{character}
#'   \item{\code{online_offline}}{integer}
#'   \item{\code{auth_meth}}{character}
#'   \item{\code{double_data_entry}}{integer}
#'   \item{\code{project_language}}{character}
#'   \item{\code{project_encoding}}{character}
#'   \item{\code{is_child_of}}{character}
#'   \item{\code{date_shift_max}}{integer}
#'   \item{\code{institution}}{character}
#'   \item{\code{site_org_type}}{character}
#'   \item{\code{grant_cite}}{character}
#'   \item{\code{project_contact_name}}{character}
#'   \item{\code{project_contact_email}}{character}
#'   \item{\code{headerlogo}}{character}
#'   \item{\code{auto_inc_set}}{integer}
#'   \item{\code{custom_data_entry_note}}{character}
#'   \item{\code{custom_index_page_note}}{character}
#'   \item{\code{order_id_by}}{character}
#'   \item{\code{custom_reports}}{character}
#'   \item{\code{report_builder}}{character}
#'   \item{\code{disable_data_entry}}{integer}
#'   \item{\code{google_translate_default}}{character}
#'   \item{\code{require_change_reason}}{integer}
#'   \item{\code{dts_enabled}}{integer}
#'   \item{\code{project_pi_firstname}}{character}
#'   \item{\code{project_pi_mi}}{character}
#'   \item{\code{project_pi_lastname}}{character}
#'   \item{\code{project_pi_email}}{character}
#'   \item{\code{project_pi_alias}}{character}
#'   \item{\code{project_pi_username}}{character}
#'   \item{\code{project_pi_pub_exclude}}{integer}
#'   \item{\code{project_pub_matching_institution}}{character}
#'   \item{\code{project_irb_number}}{character}
#'   \item{\code{project_grant_number}}{character}
#'   \item{\code{history_widget_enabled}}{integer}
#'   \item{\code{secondary_pk}}{character}
#'   \item{\code{secondary_pk_display_value}}{integer}
#'   \item{\code{secondary_pk_display_label}}{integer}
#'   \item{\code{custom_record_label}}{character}
#'   \item{\code{display_project_logo_institution}}{integer}
#'   \item{\code{imported_from_rs}}{integer}
#'   \item{\code{display_today_now_button}}{integer}
#'   \item{\code{auto_variable_naming}}{integer}
#'   \item{\code{randomization}}{integer}
#'   \item{\code{enable_participant_identifiers}}{integer}
#'   \item{\code{survey_email_participant_field}}{character}
#'   \item{\code{survey_phone_participant_field}}{character}
#'   \item{\code{data_entry_trigger_url}}{character}
#'   \item{\code{template_id}}{integer}
#'   \item{\code{date_deleted}}{double}
#'   \item{\code{data_resolution_enabled}}{integer}
#'   \item{\code{field_comment_edit_delete}}{integer}
#'   \item{\code{realtime_webservice_enabled}}{integer}
#'   \item{\code{realtime_webservice_type}}{character}
#'   \item{\code{realtime_webservice_offset_days}}{double}
#'   \item{\code{realtime_webservice_offset_plusminus}}{character}
#'   \item{\code{last_logged_event}}{double}
#'   \item{\code{edoc_upload_max}}{integer}
#'   \item{\code{file_attachment_upload_max}}{integer}
#'   \item{\code{survey_queue_custom_text}}{character}
#'   \item{\code{survey_queue_hide}}{integer}
#'   \item{\code{survey_auth_enabled}}{integer}
#'   \item{\code{survey_auth_field1}}{character}
#'   \item{\code{survey_auth_event_id1}}{integer}
#'   \item{\code{survey_auth_field2}}{character}
#'   \item{\code{survey_auth_event_id2}}{integer}
#'   \item{\code{survey_auth_field3}}{character}
#'   \item{\code{survey_auth_event_id3}}{integer}
#'   \item{\code{survey_auth_min_fields}}{character}
#'   \item{\code{survey_auth_apply_all_surveys}}{integer}
#'   \item{\code{survey_auth_custom_message}}{character}
#'   \item{\code{survey_auth_fail_limit}}{integer}
#'   \item{\code{survey_auth_fail_window}}{integer}
#'   \item{\code{twilio_enabled}}{integer}
#'   \item{\code{twilio_modules_enabled}}{character}
#'   \item{\code{twilio_hide_in_project}}{integer}
#'   \item{\code{twilio_account_sid}}{character}
#'   \item{\code{twilio_auth_token}}{character}
#'   \item{\code{twilio_from_number}}{double}
#'   \item{\code{twilio_voice_language}}{character}
#'   \item{\code{twilio_option_voice_initiate}}{integer}
#'   \item{\code{twilio_option_sms_initiate}}{integer}
#'   \item{\code{twilio_option_sms_invite_make_call}}{integer}
#'   \item{\code{twilio_option_sms_invite_receive_call}}{integer}
#'   \item{\code{twilio_option_sms_invite_web}}{integer}
#'   \item{\code{twilio_default_delivery_preference}}{character}
#'   \item{\code{twilio_request_inspector_checked}}{double}
#'   \item{\code{twilio_request_inspector_enabled}}{integer}
#'   \item{\code{twilio_append_response_instructions}}{integer}
#'   \item{\code{twilio_multiple_sms_behavior}}{character}
#'   \item{\code{twilio_delivery_preference_field_map}}{character}
#'   \item{\code{two_factor_exempt_project}}{integer}
#'   \item{\code{two_factor_force_project}}{integer}
#'   \item{\code{disable_autocalcs}}{integer}
#'   \item{\code{custom_public_survey_links}}{character}
#'   \item{\code{pdf_custom_header_text}}{character}
#'   \item{\code{pdf_show_logo_url}}{integer}
#'   \item{\code{pdf_hide_secondary_field}}{integer}
#'   \item{\code{pdf_hide_record_id}}{integer}
#'   \item{\code{shared_library_enabled}}{integer}
#'   \item{\code{allow_delete_record_from_log}}{integer}
#'   \item{\code{delete_file_repository_export_files}}{integer}
#'   \item{\code{custom_project_footer_text}}{character}
#'   \item{\code{custom_project_footer_text_link}}{character}
#'   \item{\code{google_recaptcha_enabled}}{integer}
#'   \item{\code{datamart_allow_repeat_revision}}{integer}
#'   \item{\code{datamart_allow_create_revision}}{integer}
#'   \item{\code{datamart_enabled}}{integer}
#'   \item{\code{break_the_glass_enabled}}{integer}
#'   \item{\code{datamart_cron_enabled}}{integer}
#'   \item{\code{datamart_cron_end_date}}{double}
#'   \item{\code{fhir_include_email_address_project}}{integer}
#'   \item{\code{file_upload_vault_enabled}}{integer}
#'   \item{\code{file_upload_versioning_enabled}}{integer}
#'   \item{\code{missing_data_codes}}{character}
#'   \item{\code{record_locking_pdf_vault_enabled}}{integer}
#'   \item{\code{record_locking_pdf_vault_custom_text}}{character}
#'   \item{\code{fhir_cdp_auto_adjudication_enabled}}{integer}
#'   \item{\code{fhir_cdp_auto_adjudication_cronjob_enabled}}{integer}
#'   \item{\code{project_dashboard_min_data_points}}{integer}
#'   \item{\code{bypass_branching_erase_field_prompt}}{integer}
#'   \item{\code{protected_email_mode}}{integer}
#'   \item{\code{protected_email_mode_custom_text}}{character}
#'   \item{\code{protected_email_mode_trigger}}{character}
#'   \item{\code{protected_email_mode_logo}}{integer}
#'   \item{\code{hide_filled_forms}}{integer}
#'   \item{\code{form_activation_survey_autocontinue}}{integer}
#' }
"projects_table_fragment"

#' @title csbt_column_names
#' @description Column names useful for CTSI-IT billing
#' @format A data frame with 12 rows and 2 variables:
#' \describe{
#'   \item{\code{ctsit}}{character: Internal database columns}
#'   \item{\code{csbt}}{character: Billing invoice database column names}
#'}
"csbt_column_names"

#' @title fiscal_years
#' @description A dataframe of fiscal years and labels from 2019-2020 to 2039-2040
#' @format A data frame with 21 rows and 2 variables:
#' \describe{
#'   \item{\code{csbt_label}}{character A string representing the fiscal year range, in the format YYYY-YYYY}
#'   \item{\code{fy_interval}}{double A lubridate interval object from the start of the UF fiscal year (July 1) to the end}
#'}
#' @details DETAILS
"fiscal_years"

#' @title log_event_tables
#' @description A vector of the names of the 9 redcap log event tables
#' @format A vector with 9 elements
#' @details DETAILS
"log_event_tables"
