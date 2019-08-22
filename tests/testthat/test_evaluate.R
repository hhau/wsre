context("eval method")

wsre_obj <- wsre(
  model_name = "normal",
  wf_mean = list(1),
  n_mcmc_samples = 500
)
good_test_point <- c(0.1, 0.5)
bad_test_point  <- c(-880, -87123) # substantially too far away.
combined_point <- rbind(good_test_point, bad_test_point) 

result <- evaluate(wsre_obj, combined_point[, 1], combined_point[, 2])

test_that("Evaluate returns a numeric vector, and it's the correct length", {
  expect_true(
    object = is.numeric(result)
  )
  expect_length(
    object = result,
    n = 4
  )
})

test_that("Points that are not estimated well return NaN", {
  expect_identical(
    object = result[2:4],
    expected = rep(NaN, 3)
  )
})

# test that 2D things are evaluateable?

two_dim_wsre_obj <- wsre(
  model_name = "normal", 
  wf_mean = list(c(1, 1)),
  wf_pars = list(wf_sd = c(2, 2), wf_exponent = 1, target_dimension = 2),
  n_mcmc_samples = 500
)