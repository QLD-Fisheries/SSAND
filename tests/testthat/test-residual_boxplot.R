test_that("ggplot class", {

  raw_catch_rates <- data.frame(
    year = seq(5,100,5),   
    effort = c(118,58,42,35,27,25,21,19,18,10,12,15,13,22,8,9,7,16,17,12),
    kilograms = c(75,81,72,69,35,26,21,18,16,13,12,12,11,9,10,11,15,9,8,5))
  model <- glm(kilograms ~ year + effort, data = raw_catch_rates, family = Gamma)
  p <- residual_boxplot(model, log = TRUE, nbreaks = 5)
  expect_s3_class(p, "ggplot")

})
test_that("ggplot class",{

  raw_catch_rates <- data.frame(
    year = seq(5,100,5),   
    effort = c(118,58,42,35,27,25,21,19,18,10,12,15,13,22,8,9,7,16,17,12),
    kilograms = c(75,81,72,69,35,26,21,18,16,13,12,12,11,9,10,11,15,9,8,5))
  model <- glm(kilograms ~ year + effort, data = raw_catch_rates, family = Gamma)
  p <- residual_boxplot(model, log = FALSE, nbreaks = 5)
  expect_s3_class(p, "ggplot")

})

test_that("Use standard colours",{

  raw_catch_rates <- data.frame(
    year = seq(5,100,5),   
    effort = c(118,58,42,35,27,25,21,19,18,10,12,15,13,22,8,9,7,16,17,12),
    kilograms = c(75,81,72,69,35,26,21,18,16,13,12,12,11,9,10,11,15,9,8,5))
  model <- glm(kilograms ~ year + effort, data = raw_catch_rates, family = Gamma)
  expect_warning(residual_boxplot(model, nbreaks = 50), "Few observations per group")
})
