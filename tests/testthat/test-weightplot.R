test_that("ggplot class", {
  data <- weightplot_prep_DD(dd_mle, age_max = 20, age_rec = 4)
  p <- weightplot(data)
  expect_s3_class(p, "ggplot")
})
test_that("Plot layers match expectations",{
  data <- weightplot_prep_DD(dd_mle, age_max = 20, age_rec = 4)
  p <- weightplot(data)
  expect_type(p$layers[[1]], "environment")
  expect_s3_class(p$layers[[1]], "ggproto")
})
test_that("Scale range is NULL",{
  data <- weightplot_prep_DD(dd_mle, age_max = 20, age_rec = 4)
  p <- weightplot(data)
  expect_null(p$scales$range )
})
test_that("Use standard colours",{
  data <- weightplot_prep_DD(dd_mle, age_max = 20, age_rec = 4)
  p <- weightplot(data)
  expect_equal(p$theme$line$colour,"black")
  expect_equal(p$theme$rect$colour,"black")
  expect_equal(p$theme$rect$fill,"white")
})
