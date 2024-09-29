test_that("code returns a data frame with correct variable types for MLE setup", {
  data <- ageplot_prep_SS(ss_mle)
  # Check output is dataframe
  expect_s3_class(data,"data.frame")
  # Check variable types
  expect_type(data$year,"integer")
  expect_type(data$fleet,"integer")
  expect_type(data$bin,"integer")
  expect_type(data$sex,"character")
  # expect_type(data$obs,"double")
  # expect_type(data$exp,"double")
  expect_type(data$scenario,"integer")
  # Check there are no NAs
  expect_false(any(is.na(data$year)))
  expect_false(any(is.na(data$fleet)))
  expect_false(any(is.na(data$bin)))
  expect_false(any(is.na(data$sex)))
  expect_false(any(is.na(data$obs)))
  expect_false(any(is.na(data$exp)))
  expect_false(any(is.na(data$scenario)))
})
