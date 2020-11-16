#' Evaluate a \code{wsre} object
#'
#' Evaluate a weighted self ratio estimate at a set of numerator and denominator
#' points
#' 
#' NOTE: This differs from the earlier version, everything is now pointwise due
#' to the requirement to `work` in N-dimensions.
#' 
#' @param wsre_obj \code{wsre} object: From \code{\link{wsre}}
#' @param x_nu Numeric Vector: vector of length one or more; numerator 
#' location to evaluate the estimate at.
#' @param x_de Numeric Vector: vector of length one or more; denominator 
#' location to evaluate the estimate at.
#' @param mc_cores Integer: number of cores to use to evaluate the constituent
#' estimates in parallel. Passed to \code{mclapply}. Suggest leaving at 1
#' unless each sub estimate is made up of a very large number of samples (more
#' than 10,000).
#'
#' @return Double: self ratio value
#' @export
evaluate <- function(
  wsre_obj,
  x_nu,
  x_de,
  mc_cores = 1
) {
  
  stopifnot(
    length(wsre_obj$estimates[[1]]$wf_pars$wf_mean) == length(x_nu),
    length(x_nu) == length(x_de)
  )

  n_estimates <- length(wsre_obj$estimates)
  result <- c(NaN)

  # we should only have to iterate over n_estimates here, not over the 
  # large space formed by (x_nu ⊗ x_de)
  # vectorising the ratio and weighting operations is key.
  # hardcoded to my machine at the moment because I don't think anyone else will
  # use this
  res <- parallel::mclapply(1 : n_estimates, mc.cores = mc_cores, function(estimate_index) {
    with(wsre_obj$estimates[[estimate_index]], {
      # set the contributions to the ratio and norm_conts vectors to their
      # sensible values - NA's for numerically garbage things
      ratio_value <- c(NA)
      weighting_value <- c(NA)

      # get the ratio at the point set
      ratio_value <- ratio(
        x_nu = x_nu,
        x_de = x_de
      )

      # check if the results are numerically sensible ? I don't do anything 
      # special here if they aren't
      if (!.is_numerically_okay(ratio_value)) {
        return(list(
          weights = NA,
          w_ratios = NA
        ))
      }

      # get the weighting at the sensible points
      weighting_value <- weighting(
        x_nu = x_nu,
        x_de = x_de 
      )
      weighted_ratio_value <- exp(log(weighting_value) + log(ratio_value))
      # return a list of ratios / weightings?
      return(list(weight = weighting_value, w_ratio = weighted_ratio_value))
    })
  })
  # avoid importing dplyr just for these two statements?
  weights_matrix <- do.call(rbind, lapply(res, function(a_list) {
    a_list$weight
  }))
  ratios_matrix <- do.call(rbind, lapply(res, function(a_list) {
    a_list$w_ratio
  }))
  
  # colSums doesn't drop NA only columns, thank god!
  norm_const <- colSums(weights_matrix, na.rm = TRUE)
  ratio_value <- colSums(ratios_matrix, na.rm = TRUE)
  return((1 / norm_const) * ratio_value)

}

