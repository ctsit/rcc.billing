library(redcapcustodian)
library(RMariaDB)
library(DBI)
library(tidyverse)
library(dotenv)

conn <- connect_to_redcap_db()

redcap_user_information <- tbl(conn, "redcap_user_information") %>% collect()
redcap_user_rights <- tbl(conn, "redcap_user_rights") %>% collect()
redcap_user_roles <- tbl(conn, "redcap_user_roles") %>% collect()

saveRDS(
  redcap_user_information,
  testthat::test_path(
    "get_user_rights_and_info_v1",
    "redcap_user_information.rds"
  )
)

saveRDS(
  redcap_user_rights,
  testthat::test_path(
    "get_user_rights_and_info_v1",
    "redcap_user_rights.rds"
  )
)

saveRDS(
  redcap_user_roles,
  testthat::test_path(
    "get_user_rights_and_info_v1",
    "redcap_user_roles.rds"
  )
)
