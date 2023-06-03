

test_that("Error if path doesnt exist", {
  expect_error(activate_dropbox(Dropbox_App_Name = "my_app_name_1",
                                Dropbox_App_Key = "your_app_key_here",
                                Dropbox_App_Secret = "your_app_secret_here",
                                BW_app_path = paste0(tempdir(),"/DONTEXIST")))
})
