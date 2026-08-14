testthat::test_that("desired_column_type converts ENUM types to varchar(255)", {
  testthat::expect_equal(
    desired_column_type("enum('1','2','3')"),
    "varchar(255)"
  )
  testthat::expect_equal(
    desired_column_type("ENUM('grid','popup')"),
    "varchar(255)"
  )
})

testthat::test_that("desired_column_type leaves non-ENUM types unchanged", {
  testthat::expect_equal(
    desired_column_type(c("varchar(191)", "int(11)", "tinyint(1)")),
    c("varchar(191)", "int(11)", "tinyint(1)")
  )
})

testthat::test_that("build_column_alter_statements returns one ALTER statement per drifted column, preserving the target's existing nullability", {
  drift <- tibble::tribble(
    ~column_name, ~desired_column_type, ~target_is_nullable,
    "survey_auth_min_fields", "varchar(255)", "YES",
    "project_note", "varchar(500)", "NO"
  )

  expected_result <- c(
    "ALTER TABLE redcap_projects_history MODIFY COLUMN `survey_auth_min_fields` varchar(255) NULL",
    "ALTER TABLE redcap_projects_history MODIFY COLUMN `project_note` varchar(500) NOT NULL"
  )

  testthat::expect_equal(
    build_column_alter_statements(drift, "redcap_projects_history"),
    expected_result
  )
})

testthat::test_that("build_column_alter_statements never tightens nullability to match the source", {
  # target has pre-existing NULLs (e.g. older history rows), even though the source
  # column is now NOT NULL; the generated statement must preserve the target's NULL-able
  # state rather than reproducing MariaDB error 1138 (Invalid use of NULL value)
  drift <- tibble::tribble(
    ~column_name, ~desired_column_type, ~target_is_nullable,
    "allow_delete_record_from_log", "tinyint(2)", "YES"
  )

  expected_result <- "ALTER TABLE redcap_projects_history MODIFY COLUMN `allow_delete_record_from_log` tinyint(2) NULL"

  testthat::expect_equal(
    build_column_alter_statements(drift, "redcap_projects_history"),
    expected_result
  )
})

testthat::test_that("build_column_alter_statements returns an empty character vector when there is no drift", {
  drift <- tibble::tribble(
    ~column_name, ~desired_column_type, ~target_is_nullable
  )

  testthat::expect_equal(
    build_column_alter_statements(drift, "redcap_projects_history"),
    character(0)
  )
})
