testthat::test_that("clean_processed_payment_data_from_email can clean the content in processed payment data emails", {
  testthat::expect_equal(clean_processed_payment_data_from_email(clean_processed_payment_data_from_email_test_data$input),
                         clean_processed_payment_data_from_email_test_data$output)
})
