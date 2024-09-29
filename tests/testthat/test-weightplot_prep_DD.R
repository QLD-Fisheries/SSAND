test_that("Empty age_max and age_rec arguments", {
  expect_error(weightplot_prep_DD(dd_mle))
})

test_that("Return data.frame with expected number of rows", {
  age_max = 10
  age_rec = 2
  expect_equal(nrow(weightplot_prep_DD(dd_mle, age_max = age_max, age_rec = age_rec)),(age_max+1)*length(dd_mle))

  age_max = 9.5
  age_rec = 2
  expect_equal(nrow(weightplot_prep_DD(dd_mle, age_max = age_max, age_rec = age_rec)),floor(age_max+1)*length(dd_mle))

  age_max = 1000
  age_rec = 2
  expect_equal(nrow(weightplot_prep_DD(dd_mle, age_max = age_max, age_rec = age_rec)),floor(age_max+1)*length(dd_mle))
})

test_that("SS notation", {
  expect_equal(weightplot(weightplot_prep_DD(dd_mle, age_max = 10, age_rec = 2, sex = 1)),
               weightplot(weightplot_prep_DD(dd_mle, age_max = 10, age_rec = 2, sex = 'female')))
  expect_equal(weightplot(weightplot_prep_DD(dd_mle, age_max = 10, age_rec = 2, sex = 2)),
               weightplot(weightplot_prep_DD(dd_mle, age_max = 10, age_rec = 2, sex = 'male')))
})
