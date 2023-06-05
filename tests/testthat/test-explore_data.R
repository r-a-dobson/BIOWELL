data("sample_BW_data")

test_that("Result is list object", {
  results <- explore_data(sample_BW_data,plot = FALSE,var_name = "testing")
  expect_true(inherits(results,"list"))
})

test_that("Result is list object of length two", {
  results <- explore_data(sample_BW_data,plot = FALSE,var_name = "testing")
  expect_equal(length(results),2)
})

test_that("Result element one is list object", {
  results <- explore_data(sample_BW_data,plot = FALSE,var_name = "testing")
  expect_true(inherits(results[[1]],"list"))
})

# Same tests but with column

test_that("Result is list object", {
  results <- explore_data(sample_BW_data,plot = FALSE, column_name = "Gender")
  expect_true(inherits(results,"list"))
})

test_that("Result is list object of length two", {
  results <- explore_data(sample_BW_data,plot = FALSE, column_name = "Gender")
  expect_equal(length(results),2)
})

test_that("Result element one is gg object", {
  results <- explore_data(sample_BW_data,plot = FALSE, column_name = "Gender")
  expect_true(inherits(results[[1]],"gg"))
})

test_that("Result element two is df object", {
  results <- explore_data(sample_BW_data,plot = FALSE, column_name = "Gender")
  expect_true(inherits(results[[2]],"data.frame"))
})


# Same tests with biowell specified

test_that("Result is list object", {
  results <- explore_data(sample_BW_data,plot = FALSE,biowell_scale="physical")
  expect_true(inherits(results,"list"))
})

test_that("Result is list object of length two", {
  results <- explore_data(sample_BW_data,plot = FALSE,biowell_scale="physical")
  expect_equal(length(results),2)
})

test_that("Result element one is list object", {
  results <- explore_data(sample_BW_data,plot = FALSE,biowell_scale="physical")
  expect_true(inherits(results[[1]],"list"))
})

# Same tests but with column

test_that("Result is list object", {
  results <- explore_data(sample_BW_data,plot = FALSE, column_name = "Gender",biowell_scale="physical")
  expect_true(inherits(results,"list"))
})

test_that("Result is list object of length two", {
  results <- explore_data(sample_BW_data,plot = FALSE, column_name = "Gender",biowell_scale="physical")
  expect_equal(length(results),2)
})

test_that("Result element one is gg object", {
  results <- explore_data(sample_BW_data,plot = FALSE, column_name = "Gender",biowell_scale="physical")
  expect_true(inherits(results[[1]],"gg"))
})

test_that("Result element two is df object", {
  results <- explore_data(sample_BW_data,plot = FALSE, column_name = "Gender",biowell_scale="physical")
  expect_true(inherits(results[[2]],"data.frame"))
})

test_that("Works with one row", {
  results <- explore_data(sample_BW_data[1,],plot = FALSE, column_name = "Gender",biowell_scale="physical")
  expect_true(inherits(results[[2]],"data.frame"))
})


# Test for errors


test_that("Expect error if not df", {
  expect_error(explore_data("sample_BW_data",plot = FALSE, column_name = "Gender",biowell_scale="physical"))
})



test_that("Expect error if not BW df", {
  expect_error(explore_data(data.frame(biowell=50),plot = FALSE, column_name = "Gender",biowell_scale="physical"))
})


