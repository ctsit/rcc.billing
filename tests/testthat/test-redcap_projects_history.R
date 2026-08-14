testthat::test_that("build_column_alter_statements returns one ALTER statement per drifted column", {
  drift <- tibble::tribble(
    ~column_name, ~source_column_type, ~source_is_nullable, ~target_column_type, ~target_is_nullable,
    "survey_auth_min_fields", "enum('1','2','3','4')", "YES", "enum('1','2','3')", "YES",
    "project_note", "varchar(500)", "NO", "varchar(255)", "NO"
  )

  expected_result <- c(
    "ALTER TABLE redcap_projects_history MODIFY COLUMN `survey_auth_min_fields` enum('1','2','3','4') NULL",
    "ALTER TABLE redcap_projects_history MODIFY COLUMN `project_note` varchar(500) NOT NULL"
  )

  testthat::expect_equal(
    build_column_alter_statements(drift, "redcap_projects_history"),
    expected_result
  )
})

testthat::test_that("build_column_alter_statements returns an empty character vector when there is no drift", {
  drift <- tibble::tribble(
    ~column_name, ~source_column_type, ~source_is_nullable, ~target_column_type, ~target_is_nullable
  )

  testthat::expect_equal(
    build_column_alter_statements(drift, "redcap_projects_history"),
    character(0)
  )
})
