data("sample_BW_data")

test_that("Result is list object", {
  results <- evaluate_survey(sample_BW_data)
  expect_true(inherits(results,"list"))
})

test_that("Result is list object of length three", {
  results <- evaluate_survey(sample_BW_data)
  expect_equal(length(results),6)
})

test_that("Result [[1]] is df object", {
  results <- evaluate_survey(sample_BW_data)
  expect_true(inherits(results[[1]],"data.frame"))
})

test_that("Result [[2]] is cronbach alpha object", {
  results <- evaluate_survey(sample_BW_data)
  expect_true(inherits(results[[2]],"cronbachAlpha"))
})

test_that("Result [[3]] is df object", {
  results <- evaluate_survey(sample_BW_data)
  expect_true(inherits(results[[3]],"data.frame"))
})


test_that("Errors if not DF", {
  expect_error(evaluate_survey("sample_BW_data"))
})


test_that("Errors if DF not BW", {
  expect_error(evaluate_survey(data.frame(biowell = 50)))
})


test_that("Errors if DF only one row", {
  expect_error(evaluate_survey(data[1,]))
})

