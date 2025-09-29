# Recommended approach from https://www.rdocumentation.org/packages/usethis/versions/2.1.5/topics/use_data
library(tibble)
library(usethis)

service_type <- tribble(
  ~service_type_code,
  ~service_type,
  ~price,
  ~billing_frequency,
  1, "Annual REDCap Project Maintenance", 130, 12,
  2, 'REDCap consulting', 130, 0
)

saveRDS(
  service_type,
  testthat::test_path(
    "service_type",
    "service_type.rds"
  )
)
