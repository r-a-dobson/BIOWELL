
test_that("Error if path doesn't exist", {
  expect_error(create_URL(BW_app_path = paste0(tempdir(),"/my_app_name_noreal"),
                          BW_app_name = "my_app_name_1",
                          Shiny_Server_user = 'your_username_here',
                          Shiny_Server_token = 'your_token_here',
                          Shiny_Server_secret = 'your_secret_here')
  )
})
