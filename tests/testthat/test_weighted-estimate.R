context("weighted ratio estimate")

futile.logger::flog.threshold(futile.logger::TRACE) 

weighted_est <- weighted_ratio_estimate(
  stanmodel = wsre:::stanmodels[["normal"]],
  stan_data = list(),
  wf_pars = list(
    wf_mean = as.array(c(3)),
    wf_sd = as.array(c(2)),
    wf_exponent = 1,
    target_dimension = 1
  ),
  output_properties = list(lower_quantile = 0.25, upper_quantile = 0.75),
  n_mcmc_samples = 800,
  stan_control_params = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  )
)

test_that("That the function succeeds and returns the expected class", {
  expect_s3_class(
    object = weighted_est,
    class = "wsre_sub"
  )  
})

test_that("The estimates are numerically feasible", {
  test_point <- c(1.5, 1.8) # should always be fine this close to the mean w/ 800 
  test_ratio <- weighted_est$ratio(
    test_point[1],
    test_point[2]
  )
  expect_false(
    any(is.na(test_ratio), is.nan(test_ratio), is.infinite(test_ratio))
  )
})

# Do it again, but in 2D

weighted_est_2d <- weighted_ratio_estimate(
  stanmodel = wsre:::stanmodels[["normal"]],
  stan_data = list(),
  wf_pars = list(
    wf_mean = as.array(rep(2, times = 2)),
    wf_sd = as.array(rep(2, times = 2)),
    wf_exponent = 1,
    target_dimension = 2
  ),
  output_properties = list(lower_quantile = 0.25, upper_quantile = 0.75),
  n_mcmc_samples = 800,
  stan_control_params = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  )
)

test_that("The 2D weighted estimates are numerically feasible", {
  test_x_nu <- c(1.5, 1.5)
  test_x_de <- c(1.8, 1.8)
  test_ratio <- weighted_est_2d$ratio(
    test_x_nu,
    test_x_de
  )
  expect_false(
    any(is.na(test_ratio), is.nan(test_ratio), is.infinite(test_ratio))
  )
  expect_true(
    test_ratio > 1 # this should strictly be true.
  )
})
