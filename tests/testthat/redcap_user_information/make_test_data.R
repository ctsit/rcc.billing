## Create redcap_user_information from the database of a testing system
library(redcapcustodian)
library(rcc.billing)
library(tidyverse)
library(lubridate)
library(DBI)
library(RMariaDB)
library(dotenv)

my_table <- "redcap_user_information"

# run this once against a test redcap to extract the part we need
conn <- connect_to_redcap_db()
data <- tbl(conn, my_table)

redcap_user_information <- data |>
  dplyr::collect() |>
  dplyr::filter(ui_id >= 3)

# write the test data
saveRDS(
  redcap_user_information,
  testthat::test_path("redcap_user_information", "redcap_user_information.rds")
)

# write the schema
DBI::dbGetQuery(conn, paste("show create table", my_table))$`Create Table` |>
  write(file = paste0("inst/schema/", my_table, ".sql"))

dbDisconnect(conn)
