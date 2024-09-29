test_that("plot is produced and displaying data", {
  p <- biomassplot(biomassplot_prep_SS(ss_mle))
  # Test if function produces a ggplot
  expect_true(inherits(p, "gg"))
  # Test if data being plot is not empty
  expect_true(nrow(ggplot2::ggplot_build(p)$data[[1]])>1)

  p <- biomassplot(biomassplot_prep_SS(ss_mle))
  expect_true(inherits(p, "gg"))
  expect_true(nrow(ggplot2::ggplot_build(p)$data[[1]])>1)

  p <- biomassplot(biomassplot_prep_SS(ss_mle,ss_mcmc))
  expect_true(inherits(p, "gg"))
  expect_true(nrow(ggplot2::ggplot_build(p)$data[[1]])>1)
})

test_that("axis limits are as to be expected", {
  data <- biomassplot_prep_SS(ss_mle)
  p <- biomassplot(data)
  expect_equal(ggplot2::layer_scales(p)$x$dimension(), c(min(data$year),max(data$year)))

  data <- biomassplot_prep_DD(dd_mle)
  p <- biomassplot(data, xlim = c(1990,2000), ylim=c(0.2,0.4))
  expect_equal(ggplot2::layer_scales(p)$x$dimension(), c(1990,2000))
  expect_equal(ggplot2::layer_scales(p)$y$dimension(), c(0.2,0.4))
})



