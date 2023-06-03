
test_that("Error if token doesn't exist", {
  expect_error(download_data(Dropbox_App_folder = "survey_responses",
                             BW_app_path = paste0(tempdir(), "/my_app_name_1"))
  )
})
