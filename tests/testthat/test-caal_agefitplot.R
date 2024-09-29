test_that("plot is produced and displaying data", {
  p <- caal_agefitplot(caal_agefitplot_prep_SS(ss_mle))
  # Test if function produces a ggplot
  expect_true(inherits(p, "gg"))
  # Test if data being plot is not empty
  expect_true(nrow(ggplot2::ggplot_build(p)$data[[1]])>1)
})


