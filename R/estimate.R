wsre <- function(
  model_name = c("normal", "binom"),
  wf_mean = c(-3, 3, 5),
  wf_pars = list(
    wf_sd = 2,
    wf_exponent = 1,
    target_dimension = 1
  ),
  n_mcmc_samples = 5000,
  stan_control_params = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  ),
  flog_threshold = futile.logger::INFO
) {

  if (missing(model_name)) {
    model_name <- match.arg(
      arg = model_name,
      choices = names(.stan_models)
    ) 
  }

  # do the naive estimate first, might need it for later calculations
  flog.trace("Obtaining the Naive estimate")
  naive_wf_pars <- .extend_list(
    wf_pars,
    list(wf_mean = 0, wf_exponent = 0)
  )
  naive_estimate <- naive_ratio_estimate(
    model_name = model_name,
    wf_pars = naive_wf_pars,
    n_mcmc_samples = n_mcmc_samples
  )  

  # at the moment I'm just going to assume the wf_mean's are sensible,
  # I could figure this out automatically, but not for right now
  wsre_obj <-

}

naive_ratio_estimate <- function(
  model_name,
  wf_pars,
  n_mcmc_samples
) {

  stanmodel <- .stan_models[[model_name]]
  stanfit <- rstan::sampling(
    object = stanmodel,
    data = wf_pars,
    iter = n_mcmc_samples + 500,
    warmup = 500,
    chains = 1,
    refresh = 0
  )

  # is now a numeric vector
  x_samples <- as.array(stanfit, pars = "x") %>% as.vector()
  bandwidth <- stats::bw.SJ(x_samples)
  
  kde_est <- function(x) {
    gauss_kde(x = x, x_samples = x_samples, bandwidth = bandwidth)
  }

  ratio <- function(x_nu, x_de) {
    exp(log(kde_est(x_nu)) - log(kde_est(x_de)))
  }

  weighting <- function(x_nu, x_de) {
    exp(log(kde_est(x_nu)) + log(kde_est(x_de)))
  }

  ratio_obj <- list(
    wf_pars = wf_pars,
    ratio = ratio,
    weighting = weighting
  )

  class(ratio_obj) <- "wsre_sub"
  return(ratio_obj)

}

weighted_ratio_estimate <- function(
  model_name,
  wf_pars,
  n_mcmc_samples
) {

  stanmodel <- .stan_models[[model_name]]
  stanfit <- rstan::sampling(
    object = stanmodel,
    data = wf_pars,
    iter = n_mcmc_samples + 500,
    warmup = 500,
    chains = 1,
    refresh = 0
  )

  # is now a numeric vector
  x_samples <- as.array(stanfit, pars = "x") %>% as.vector()
  bandwidth <- stats::bw.SJ(x_samples)
  
  direct_kde_est <- function(x) {
    gauss_kde(x = x, x_samples = x_samples, bandwidth = bandwidth)
  }

  weighted_kde_est <- function(x) {
    with(wf_pars, weight_gauss_kde_jones(
      x = x,
      weighted_samples = x_samples,
      wf_mean = wf_mean,
      wf_sd = wf_sd,
      wf_exp = wf_exponent, # TODO: this bad naming, needs fixing
      bandwidth = bandwidth
    ))
  }

  ratio <- function(x_nu, x_de) {
    exp(log(weighted_kde_est(x_nu)) - log(weighted_kde_est(x_de)))
  }

  weighting <- function(x_nu, x_de) {
    exp(log(direct_kde_est(x_nu)) + log(direct_kde_est(x_de)))
  }

  ratio_obj <- list(
    wf_pars = wf_pars,
    ratio = ratio,
    weighting = weighting
  )

  class(ratio_obj) <- "wsre_sub"
  return(ratio_obj)

}