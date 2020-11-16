#' Weighted Self Ratio Estimation
#' 
#' Estimates the self ratio of a given model using a number of weighting 
#' functions.
#'
#' @param model_name String: The name of a \code{.stan} file in the 
#' \code{inss/stan} directory. 
#' @param stanmodel stanmodel: output from \code{\link[rstan]{stan_model}}.
#' Must follow the naming convention in the vignette.
#' @param stan_data list: other data to pass to Stan. Not related to weighting
#' function parameters.
#' @param wf_mean List: target-dimension vectors of means of weighting 
#' functions. 
#' @param wf_pars Named List: see default for structure; other parameters for
#' the weighting function. Must have wf_sd as an array, even if univariate.
#' Stan will throw array/vector type errors if you get this wrong.
#' @param n_mcmc_samples Integer: number of MCMC samples to draw from each of 
#' the targets, after a fixed 2000 iteration warmup.
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
  model_name = names(stanmodels),
  stanmodel = .named_model(model_name),
  stan_data = list(),
  wf_mean = list(-3, 3, 5),
  wf_pars = list(wf_sd = as.array(c(2)), wf_exponent = 1, target_dimension = 1),
  n_mcmc_samples = 1200,
  stan_control_params = list(adapt_delta = 0.98, max_treedepth = 13),
  flog_threshold = futile.logger::INFO,
  mc_cores = 1
) {

  if (missing(model_name) & missing(stanmodel)) {
    stop("Must supply argument for one of model_name or stanmodel") 
  } else if (!missing(model_name) & missing(stanmodel)) {
    if (!(model_name %in% names(stanmodels))) {
      stop("I don't know a model with that model_name")
    }
  } 

  futile.logger::flog.threshold(flog_threshold)

  # do the naive estimate first, might need it for later calculations
  futile.logger::flog.trace("Obtaining the Naive estimate")
  naive_wf_pars <- .extend_list(
    wf_pars, 
    list(
      wf_mean = as.array(rep(0, times = wf_pars$target_dimension)),
      wf_exponent = 0
    )
  )
  naive_estimate <- naive_ratio_estimate(
    stanmodel = stanmodel,
    stan_data = stan_data,
    wf_pars = naive_wf_pars,
    n_mcmc_samples = n_mcmc_samples,
    stan_control_params = stan_control_params
  )  

  # at the moment I'm just going to assume the wf_mean's are sensible,
  # I could figure this out automatically, but not for right now
  # mclapply for cheap speed? parallel logging and dll loading is hard
  wsre_estimates <- parallel::mclapply(wf_mean, mc.cores = mc_cores, function(a_mean) {
    futile.logger::flog.trace(sprintf("Starting mean: %.2f", a_mean))
    wf_pars <- .extend_list(wf_pars, list(wf_mean = as.array(c(a_mean))))
    sub_obj <- weighted_ratio_estimate(
      stanmodel = stanmodel,
      stan_data = stan_data,
      wf_pars = wf_pars,
      n_mcmc_samples = n_mcmc_samples,
      stan_control_params = stan_control_params
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
  stan_data,
  wf_pars,
  n_mcmc_samples,
  stan_control_params
) {

  full_data = .extend_list(stan_data, wf_pars)
  stanfit <- rstan::sampling(
    stanmodel,
    data = full_data,
    iter = n_mcmc_samples + 2000,
    warmup = 2000,
    chains = 1,
    refresh = 0,
    control = stan_control_params
  )

  dim <- length(wf_pars$wf_mean)
  # the first `1` drops the chain dim, but the latter vector should keep the 
  # result in an [n_mcmc_samples, dim] array? No, need to mess around for 
  # consistency. Should work as written, the as.array is the generic for the
  # stanfit object.
  x_samples <- as.array(stanfit, pars = "x")[, 1, ] %>% 
    as.vector() %>% 
    array(dim = c(n_mcmc_samples, dim))

  bandwidth_vec <- apply(x_samples, 2, stats::bw.SJ)

  kde_est <- function(x) {
    kde_func_nd(x_val = x, x_sample_mat = x_samples, bw_vec = bandwidth_vec)
  }

  ratio <- function(x_nu, x_de) {
    exp(log(kde_est(x_nu)) - log(kde_est(x_de)))
  }

  weighting <- function(x_nu, x_de) {
    exp(log(kde_est(x_nu)) + log(kde_est(x_de)))
  }

  properties <- list(
    sample_mean = apply(x_samples, 2, mean),
    sample_sd = apply(x_samples, 2, mean),
    sample_min = apply(x_samples, 2, min),
    sample_max = apply(x_samples, 2, max),
    sample_10th = apply(x_samples, 2, quantile, 0.1),
    sample_90th = apply(x_samples, 2, quantile, 0.9),
    bandwidth = bandwidth_vec
  )
   
  ratio_obj <- list(
    wf_pars = wf_pars,
    ratio = ratio,
    weighting = weighting,
    naive_bandwidth = bandwidth_vec,
    properties = properties
  )

  class(ratio_obj) <- "wsre_sub"
  return(ratio_obj)

}

weighted_ratio_estimate <- function(
  stanmodel,
  stan_data,
  wf_pars,
  n_mcmc_samples,
  stan_control_params
) {

  full_data = .extend_list(stan_data, wf_pars)
  stanfit <- rstan::sampling(
    object = stanmodel,
    data = full_data,
    iter = n_mcmc_samples + 2000,
    warmup = 2000,
    chains = 1,
    refresh = 0,
    control = stan_control_params
  )

  dim <- length(wf_pars$wf_mean)
  x_samples <- as.array(stanfit, pars = "x")[, 1, ] %>% 
    as.vector() %>% 
    array(dim = c(n_mcmc_samples, dim))

  bandwidth_vec <- apply(x_samples, 2, stats::bw.SJ)
  
  direct_kde_est <- function(x) {
    kde_func_nd(x_val = x, x_sample_mat = x_samples, bw_vec = bandwidth_vec)
  }

  weighted_kde_est <- function(x) {
    with(wf_pars, weight_gauss_kde_jones_nd(
      x_val = x,
      weighted_samples = x_samples,
      wf_mean = wf_mean,
      wf_sd = wf_sd,
      wf_exponent = wf_exponent,
      bw_vec = bandwidth_vec
    ))
  }

  ratio <- function(x_nu, x_de) {
    exp(log(weighted_kde_est(x_nu)) - log(weighted_kde_est(x_de)))
  }

  weighting <- function(x_nu, x_de) {
    exp(log(direct_kde_est(x_nu)) + log(direct_kde_est(x_de)))
  }

  properties <- list(
    sample_mean = apply(x_samples, 2, mean),
    sample_sd = apply(x_samples, 2, mean),
    sample_min = apply(x_samples, 2, min),
    sample_max = apply(x_samples, 2, max),
    sample_10th = apply(x_samples, 2, quantile, 0.1),
    sample_90th = apply(x_samples, 2, quantile, 0.9),
    bandwidth = bandwidth_vec
  )

  ratio_obj <- list(
    wf_pars = wf_pars, 
    ratio = ratio, 
    weighting = weighting,
    properties = properties
  )

  class(ratio_obj) <- "wsre_sub"
  return(ratio_obj)

}
