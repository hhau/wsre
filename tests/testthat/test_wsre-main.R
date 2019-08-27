context("wsre method")

# defaults are actually fine for testing? 
wsre_obj <- wsre(model_name = "normal", wf_mean = c(1), n_mcmc_samples = 500)
test_point <- c(0.1, 0.5)

test_that("wsre returns object of correct type:", {
  expect_s3_class(
    object = wsre_obj,
    class = "wsre"
  )
})

# this is a slow test, but I care a lot about this behaviour
test_that(
  "writing the wsre object to disk and reading it back preserves closures",
  {
    object_on_disk <- file.path(tempdir(), "wsreobj.rds")
    saveRDS(
      object = wsre_obj,
      file = object_on_disk
    )
    read_obj <- readRDS(object_on_disk)

    expect_s3_class(
      object = read_obj,
      class = "wsre"
    )
    expect_true(
      object = .is_numerically_okay(
        read_obj$estimates[[1]]$ratio(test_point[1], test_point[2])
      )
    )
  }
)

test_that("wrong model_name leads to error", {
  expect_error(
    object = wsre(model_name = "garbage"),
    regexp = "I don't know a model with that model_name"
  )
})


# next test is really slow, but useful?
# I have no way of checking that the correct thing gets added to the target
# (in general - so this is not a good guide)
test_that("Passing in stanmodel, with correct data block, leads to wsre_obj", {
  # this test takes a long time, but I want it to run everywhere that isn't
  # my system, leaving the burden on me to uncomment it from time to time, 
  # before pushing to CI.
  skip_if(
    # condition = system2("whoami", stdout = TRUE) == "amanderson",
    condition = runif(n = 1) < 0.95, # skip 95% of the time
    message = "Locally skipping long test that involves Stan compiler"
  )
  test_stanmodel <- rstan::stan_model(
    model_code = 
    "data {
      int <lower = 1> target_dimension;
      real wf_mean [target_dimension];
      real <lower = 0> wf_sd [target_dimension];
      real <lower = 0> wf_exponent;
    }
    parameters {
      real x [target_dimension];
    }
    model {
      x ~ normal(0, 1);
      target += wf_exponent * normal_lpdf(x | wf_mean, wf_sd);
    }"
  )
  test_res <- wsre(stanmodel = test_stanmodel)

  expect_s3_class(
    object = test_res,
    class = "wsre"
  )
})

## test that 2D things work?
two_dim_wsre_obj <- wsre(
  model_name = "normal", 
  wf_mean = list(c(1, 1)),
  wf_pars = list(wf_sd = c(2, 2), wf_exponent = 1, target_dimension = 2),
  n_mcmc_samples = 500
)

test_that("2D model is sample-able", {
  expect_s3_class(
    object = two_dim_wsre_obj,
    class = "wsre"
  )  
})


test_point <- c(0.1, 0.5)