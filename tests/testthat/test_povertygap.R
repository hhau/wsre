context("povertygap model can be run without errors")

wsre_est <- wsre(
  model_name = "povertygap",
  stan_data = stan_data,
  wf_mean = seq(from = 0.01, to = 0.2, length.out = 2),
  wf_pars = list(wf_sd = array(0.025), wf_exponent = 1, target_dimension = 1)
)

test_that("wsre can call the povertygap model successfully", {
  expect_s3_class(
    object = wsre_est,
    class = "wsre"
  )
})

x_nu <- 0.1
x_de <- 0.2

test_that("low prob numerator and high prob denominator gives <<1 value", {
  expect_true(evaluate(wsre_est, x_nu, x_de) < 1)
})