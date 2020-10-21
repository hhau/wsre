context("naive_ratio_estimate method")

futile.logger::flog.threshold(futile.logger::TRACE)

dim <- 2

naive_est <- naive_ratio_estimate(
  stanmodel = wsre:::stanmodels[["normal"]],
  wf_pars = list(
    wf_mean = as.array(rep(0, times = dim)),
    wf_sd = as.array(rep(2, times = dim)),
    wf_exponent = 0,
    target_dimension = dim
  ),
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
  test_x_nu <- c(0.2, 0.2) 
  test_x_de <- c(1.2, 1.2)
  test_ratio <- naive_est$ratio(
    test_x_nu,
    test_x_de
  )
  expect_false(
    any(is.na(test_ratio), is.nan(test_ratio), is.infinite(test_ratio))
  )
  expect_true(
    test_ratio > 1
  )
})
