
test_that("Error if token doesn't exist", {
  expect_error(build_survey(BW_app_path = tempdir()))
  })

