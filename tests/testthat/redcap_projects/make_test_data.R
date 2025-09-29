library(redcapcustodian)
library(rcc.billing)
library(tidyverse)
library(lubridate)
library(DBI)
library(RMariaDB)
library(dotenv)

invoice_line_item_test_data <- readRDS(
  file = testthat::test_path(
    "invoice_line_item",
    "invoice_line_item.rds"
  )
)

project_table_cols <-
  invoice_line_item_test_data %>%
  filter(service_type_code == 1) %>%
  filter(id >= 4) %>%
  head(4) %>%
  select(
    project_id = service_instance_id,
    app_title = name_of_service_instance,
    project_pi_firstname = pi_first_name,
    project_pi_lastname = pi_last_name,
    project_pi_email = pi_email
  ) %>%
  mutate(project_id = as.numeric(project_id)) %>%
  mutate(project_name = gsub(" ", "_", tolower(app_title))) %>%
  mutate(
    creation_time = ymd("2021-05-15") +
      ddays(c(-3.2, 5.5, -1.7, -7.4)) -
      years(c(0, 3, 2, 1))
  )

# # run this once against a test redcap to extract the part we need
# conn <- connect_to_redcap_db()
# projects <- tbl(conn, "redcap_projects")
#
# projects_table_fragment <- projects %>%
#   filter(project_id >= 15) %>%
#   filter(status == 0) %>%
#   filter(is.na(date_deleted)) %>%
#   head(nrow(project_table_cols)) %>%
#   collect() %>%
#   mutate(across(colnames(project_table_cols), ~ NA))
#
# saveRDS(
#   projects_table_fragment,
#   testthat::test_path("redcap_projects", "projects_table_fragment.rds")
# )
#
# one_deleted_project_record <- projects %>%
#   filter(project_id >= 15) %>%
#   filter(status == 0) %>%
#   filter(is.na(date_deleted)) %>%
#   head(nrow(project_table_cols) + 1) %>%
#   collect() %>%
#   tail(1) %>%
#   mutate(project_id = min(as.numeric(project_table_cols$project_id)) - 2,
#          creation_time = min(project_table_cols$creation_time) - ddays(2),
#          date_deleted = creation_time + ddays(30)
#   )
# 
# saveRDS(
#   one_deleted_project_record,
#   testthat::test_path("redcap_projects", "one_deleted_project_record.rds")
# )

projects_table_fragment <- readRDS(
  file = testthat::test_path("redcap_projects", "projects_table_fragment.rds")
)

one_deleted_project_record <- readRDS(
  file = testthat::test_path("redcap_projects", "one_deleted_project_record.rds")
)

redcap_projects_test_data <-
  bind_cols(
    project_table_cols,
    projects_table_fragment %>% select(-colnames(project_table_cols))
  ) %>%
  bind_rows(one_deleted_project_record) %>%
  select(colnames(projects_table_fragment)) %>%
  mutate(twilio_from_number = as.integer(NA))

saveRDS(
  redcap_projects_test_data,
  file = testthat::test_path("redcap_projects", "redcap_projects_test_data.rds")
)
