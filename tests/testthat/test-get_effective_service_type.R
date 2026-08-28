test_service_type <- tibble::tribble(
  ~service_type_code, ~service_type, ~price, ~billing_frequency, ~start_date,
  1, "Annual REDCap Project Maintenance", 130, 12, as.Date(NA),
  1, "Annual REDCap Project Maintenance", 150, 12, as.Date("2026-10-01"),
  2, "REDCap consulting", 130, 0, as.Date(NA)
)

testthat::test_that("get_effective_service_type returns the row effective before any dated row applies", {
  result <- get_effective_service_type(test_service_type, as.Date("2020-01-01"))
  code_1 <- dplyr::filter(result, service_type_code == 1)
  testthat::expect_equal(nrow(code_1), 1)
  testthat::expect_equal(code_1$price, 130)
})

testthat::test_that("get_effective_service_type returns the new rate once its start_date is reached", {
  result <- get_effective_service_type(test_service_type, as.Date("2026-10-01"))
  code_1 <- dplyr::filter(result, service_type_code == 1)
  testthat::expect_equal(nrow(code_1), 1)
  testthat::expect_equal(code_1$price, 150)
})

testthat::test_that("get_effective_service_type resolves each service_type_code independently", {
  result <- get_effective_service_type(test_service_type, as.Date("2026-10-01"))
  code_2 <- dplyr::filter(result, service_type_code == 2)
  testthat::expect_equal(nrow(code_2), 1)
  testthat::expect_equal(code_2$price, 130)
})
