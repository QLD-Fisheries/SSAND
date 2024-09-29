test_that("Recruitment deviations successfully retrieved from all model options", {
  data <- biomassplot_prep_DD(dd_mle)
  # Check output is dataframe
  expect_s3_class(data,"data.frame")
  # Check variable types
  expect_type(data$year,"double")
  expect_type(data$value,"double")
  expect_type(data$lower,"double")
  expect_type(data$upper,"double")
  expect_type(data$scenario,"integer")
  expect_type(data$biomass_type,"character")
  expect_type(data$biomass_definition,"character")
  # Check there are no NAs
  expect_false(any(is.na(data$year)))
  expect_false(any(is.na(data$value)))
  expect_false(any(is.na(data$lower)))
  expect_false(any(is.na(data$upper)))
  expect_false(any(is.na(data$scenario)))
  # Check years output are correct
  expect_true(min(sapply(dd_mle, function(x) x$data$first_year_catch)) == min(data$year))
  expect_true(min(sapply(dd_mle, function(x) x$data$last_year_catch)) == max(data$year))
})