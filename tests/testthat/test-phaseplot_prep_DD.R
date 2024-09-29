test_that("multiplication works", {
  expect_equal(names(phaseplot_prep_DD(dd_mle)),c("year", "Bratio", "F_", "scenario", "B_MSY", "F_max"))
})
