test_that("Creates a data.frame", {
  data <- srplot_prep_DD(dd_mle)
  expect_s3_class(data,"data.frame")
})
test_that("Too many scenarios", {
  expect_error(srplot_prep_DD(dd_mle, scenarios = 1:10))
})
test_that("return columns", {
  expect_equal(names(srplot_prep_DD(dd_mle)),
  c("year","spawn_bio","pred_recr","dev","exp_recr","bias_adjusted","scenario"))
})
