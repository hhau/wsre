context("naive_ratio_estimate method")

futile.logger::flog.threshold(futile.logger::TRACE)

naive_est <- naive_ratio_estimate(
  model_name = "normal",
  wf_pars = list(wf_mean = 0, wf_sd = 2, wf_exponent = 0, target_dimension = 1),
  n_mcmc_samples = 800,
  stan_control_params = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  )
)

test_that("That the function succeeds and returns the expected class", {
  expect_s3_class(
    object = naive_est,
    class = "wsre_sub"
  )  
})

test_that("The estimates are numerically feasible", {
  test_point <- c(0.2, 1.2) # should always be fine this close to the mean w/ 800 
  test_ratio <- naive_est$ratio(
    test_point[1],
    test_point[2]
  )
  expect_false(
    any(is.na(test_ratio), is.nan(test_ratio), is.infinite(test_ratio))
  )
})