context("weighted ratio estimate")

futile.logger::flog.threshold(futile.logger::TRACE) 

bandwidth <- bw.SJ(rnorm(800))

dim <- 2

weighted_est <- weighted_ratio_estimate(
  stanmodel = wsre:::.stan_models[["normal"]],
  wf_pars = list(wf_mean = as.array(rep(3, times = dim)), wf_sd = as.array(rep(2, times = dim)), wf_exponent = 1, target_dimension = dim),
  n_mcmc_samples = 800,
  stan_control_params = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  ),
  bandwidth = bandwidth
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