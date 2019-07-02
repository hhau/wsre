#' Weighted Self Ratio Estimation
#' 
#' Estimates the self ratio of a given model using a number of weighting 
#' functions.
#'
#' @param model_name String: Can the name of a \code{.stan} file in the 
#' \code{stan_files} directory, "normal" or "binom" at the moment. If you 
#' pass a compiled model to 
#' @param stanmodel stanmodel: the output from \code{\link[rstan]{stan_model}}.
#' Must follow the naming convention in the vignette.
#' @param wf_mean Numeric Vector: vector of means of weighting functions. May be
#' computed automatically in the future.
#' @param wf_pars Named List: see default for structure; other parameters for
#' the weighting function 
#' @param n_mcmc_samples Integer: number of MCMC samples to draw from each of 
#' the targets.
#' @param stan_control_params Named List: see \code{control} section of
#' \code{\link[rstan]{stan}}.
#' @param flog_threshold Special: See 
#' \code{\link[futile.logger]{futile.logger-package}}. Set to 
#' \code{futile.logger::TRACE} to see informational messages about what is going
#' on.
#'
#' @return A \code{wsre} object for saving / use in other functions
#' @export
wsre <- function(
  model_name = c("normal", "binom"),
  stanmodel = .named_model(model_name),
  wf_mean = c(-3, 3, 5),
  wf_pars = list(wf_sd = 2, wf_exponent = 1, target_dimension = 1),
  n_mcmc_samples = 5000,
  stan_control_params = list(adapt_delta = 0.95, max_treedepth = 12),
  flog_threshold = futile.logger::INFO
) {

  if (missing(model_name) & missing(stanmodel)) {
    stop("Must supply argument for one of model_name or stanmodel") 
  } else if (!missing(model_name) & missing(stanmodel)) {
    if (!(model_name %in% c("normal", "binom"))) {
      stop("I don't know a model with that model_name")
    }
  } 

  futile.logger::flog.threshold(flog_threshold)

  # do the naive estimate first, might need it for later calculations
  futile.logger::flog.trace("Obtaining the Naive estimate")
  naive_wf_pars <- .extend_list(wf_pars, list(wf_mean = 0, wf_exponent = 0))
  naive_estimate <- naive_ratio_estimate(
    stanmodel = stanmodel,
    wf_pars = naive_wf_pars,
    n_mcmc_samples = n_mcmc_samples,
    stan_control_params = stan_control_params
  )  

  # at the moment I'm just going to assume the wf_mean's are sensible,
  # I could figure this out automatically, but not for right now
  # mclapply for cheap speed? parallel logging and dll loading is hard
  wsre_estimates <- lapply(wf_mean, function(a_mean) {
    futile.logger::flog.trace(sprintf("Starting mean: %.2f", a_mean))
    wf_pars <- .extend_list(wf_pars, list(wf_mean = a_mean))
    sub_obj <- weighted_ratio_estimate(
      stanmodel = stanmodel,
      wf_pars = wf_pars,
      n_mcmc_samples = n_mcmc_samples,
      stan_control_params = stan_control_params,
      bandwidth = naive_estimate$naive_bandwidth
    )
    return(sub_obj)
  })
  n_ests <- length(wf_mean) + 1
  wsre_estimates[[n_ests]] <- naive_estimate

  # big object - I don't know what to call this
  # this is going to be written to disk quite a bit, so best to keep the info
  # around about how it was created?
  # need to check that all the closures work okay - write tests?
  output <- list(
    properties = list(
      model_name = stanmodel@model_name,
      n_mcmc_samples = n_mcmc_samples,
      stan_control_params = stan_control_params
    ),
    estimates = wsre_estimates
  )
  
  class(output) <- "wsre"
  return(output)

}

naive_ratio_estimate <- function(
  stanmodel,
  wf_pars,
  n_mcmc_samples,
  stan_control_params
) {

  stanfit <- rstan::sampling(
    stanmodel,
    data = wf_pars,
    iter = n_mcmc_samples + 500,
    warmup = 500,
    chains = 1,
    refresh = 0,
    control = stan_control_params
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
    weighting = weighting,
    naive_bandwidth = bandwidth
  )

  class(ratio_obj) <- "wsre_sub"
  return(ratio_obj)

}

weighted_ratio_estimate <- function(
  stanmodel,
  wf_pars,
  n_mcmc_samples,
  stan_control_params,
  bandwidth
) {

  stanfit <- rstan::sampling(
    object = stanmodel,
    data = wf_pars,
    iter = n_mcmc_samples + 500,
    warmup = 500,
    chains = 1,
    refresh = 0,
    control = stan_control_params
  )

  # is now a numeric vector
  x_samples <- as.array(stanfit, pars = "x") %>% as.vector()
  # bandwidth <- stats::bw.SJ(x_samples)
  
  direct_kde_est <- function(x) {
    gauss_kde(x = x, x_samples = x_samples, bandwidth = bandwidth)
  }

  weighted_kde_est <- function(x) {
    with(wf_pars, weight_gauss_kde_jones(
      x = x,
      weighted_samples = x_samples,
      wf_mean = wf_mean,
      wf_sd = wf_sd,
      wf_exponent = wf_exponent, # TODO: this bad naming, needs fixing
      bandwidth = bandwidth
    ))
  }

  ratio <- function(x_nu, x_de) {
    exp(log(weighted_kde_est(x_nu)) - log(weighted_kde_est(x_de)))
  }

  weighting <- function(x_nu, x_de) {
    exp(log(direct_kde_est(x_nu)) + log(direct_kde_est(x_de)))
  }

  ratio_obj <- list(wf_pars = wf_pars, ratio = ratio, weighting = weighting)

  class(ratio_obj) <- "wsre_sub"
  return(ratio_obj)

}
