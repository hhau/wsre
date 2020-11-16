context("evaluate function")

wsre_obj <- wsre(
  model_name = "normal",
  wf_mean = list(1),
  n_mcmc_samples = 2000
)
good_test_point <- c(x_nu = 0.1, x_de = 0.5)
bad_test_point  <- c(x_nu = -880, x_de = -87123) # substantially too far away.

good_result <- evaluate(
  wsre_obj, 
  good_test_point['x_nu'], 
  good_test_point['x_de'],
  mc_cores = 1
)

bad_result <- evaluate(wsre_obj, bad_test_point['x_nu'], bad_test_point['x_de'])

test_that("Evaluate returns a numeric vector, and it's the correct length", {
  expect_true(
    object = is.numeric(good_result)
  )
  expect_length(
    object = good_result,
    n = 1
  )
})

test_that("Points that are not estimated well return NaN", {
  expect_identical(
    object = bad_result,
    expected = rep(NaN, 1)
  )
})

# test that 2D things are evaluateable?
two_dim_wsre_obj <- wsre(
  model_name = "normal", 
  wf_mean = list(array(c(1, 1)), array(c(2, 2)), array(c(3, 3)), array(c(4, 4))),
  wf_pars = list(wf_sd = array(c(2, 2)), wf_exponent = 1, target_dimension = 2),
  n_mcmc_samples = 2000#,
  # flog_threshold = futile.logger::TRACE
)

good_2d_point <- list(
  x_nu = array(c(1, 1)),
  x_de = array(c(2, 2))
)

good_2d_result <- evaluate(
  wsre_obj = two_dim_wsre_obj,
  x_nu = good_2d_point$x_nu,
  x_de = good_2d_point$x_de
)

test_that("2D evaluation works, feasible, sensible", {
  expect_false(
    any(is.na(good_2d_result), is.nan(good_2d_result), is.infinite(good_2d_result))
  )
  expect_true(
    good_2d_result > 1 # (1, 1) should have a higher density value that (2, 2)
  )
})
