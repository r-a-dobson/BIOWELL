

test_that("stops if no occ.data provided", {
  expect_error(
    evaluate_survey("x")
  )
})
