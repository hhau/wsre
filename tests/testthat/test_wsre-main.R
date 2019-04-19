context("wsre method")

# defaults are actually fine for testing? 
wsre_obj <- wsre(wf_mean = c(1), n_mcmc_samples = 500)
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