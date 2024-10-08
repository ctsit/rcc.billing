library(dplyr)

ctsit <- c(
  "service_instance_id",
  "ctsi_study_id",
  "name_of_service",
  "name_of_service_instance",
  "other_system_invoicing_comments",
  "price_of_service",
  "qty_provided",
  "amount_due",
  "fiscal_year",
  "month_invoiced",
  "pi_last_name",
  "pi_first_name",
  "pi_email",
  "fiscal_contact_name",
  "fiscal_contact_email"
)

csbt <- c(
  "CTSIT ID",
  "CTSI Study ID",
  "Name of Service",
  "Study Name",
  "Other System Invoicing Comments",
  "Price of Service",
  "Qty Provided",
  "Amount Due",
  "Fiscal Year",
  "Month Invoiced",
  "PI Last Name",
  "PI First Name",
  "PI Email",
  "Fiscal Contact Name",
  "Fiscal Contact Email"
)

csbt_column_names <- data.frame(ctsit, csbt)

usethis::use_data(csbt_column_names, overwrite = TRUE)
