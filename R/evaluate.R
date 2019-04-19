#' Evaluate a \code{wsre} object
#'
#' Evaluate a weighted self ratio estimate at a set of numerator and denominator
#' points
#' 
#' @param wsre_obj \code{wsre} object: From \code{\link{wsre}}
#' @param x_nu Numeric Vector: vector of numerator locations to evaluate the
#' estimate at.
#' @param x_de Numeric Vector: vector of denominator locations to evaluate the
#' estimate at.
#'
#' @return Numeric Vector: self ratio values
#' @export
evaluate <- function(
  wsre_obj,
  x_nu,
  x_de
) {

  n_estimates <- length(wsre_obj$estimates)
  full_point_set <- expand.grid(x_nu = x_nu, x_de = x_de)
  full_point_set$ratio <- NA
  n_points <- nrow(full_point_set)

  # we should only have to iterate over n_estimates here, not over the 
  # large space formed by (x_nu ⊗ x_de)
  # vectorising the ratio and weighting operations is key.
  # hardcoded to my machine at the moment because I don't think anyone else will
  # use this
  res <- parallel::mclapply(1 : n_estimates, mc.cores = 6, function(estimate_index) {
    with(wsre_obj$estimates[[estimate_index]], {
      # set the contributions to the ratio and norm_conts vectors to their
      # sensible values - NA's for numerically garbage things
      ratio_values <- rep(NA, length = n_points)
      weighting_values <- rep(NA, length = n_points)

      # get the ratio at the point set
      tmp_ratio_values <- ratio(
        x_nu = full_point_set$x_nu,
        x_de = full_point_set$x_de
      )

      # check if the results are numerically sensible
      subset_vec <- .is_numerically_okay(tmp_ratio_values)
      ratio_values[subset_vec] <- tmp_ratio_values[subset_vec]

      # get the weighting at the sensible points
      tmp_weighting_values <- weighting(
        x_nu = full_point_set$x_nu[subset_vec],
        x_de = full_point_set$x_de[subset_vec] 
      )
      weighting_values[subset_vec] <- tmp_weighting_values
      
      weighted_ratio_values <- exp(log(weighting_values) + log(ratio_values))
      # return a list of ratios / weightings?
      return(list(weights = weighting_values, w_ratios = weighted_ratio_values))
    })
  })
  # avoid importing dplyr just for these two statements?
  weights_matrix <- do.call(rbind, lapply(res, function(a_list) {
    a_list$weights
  }))
  ratios_matirx <- do.call(rbind, lapply(res, function(a_list) {
    a_list$w_ratios
  }))
  
  # colSums doesn't drop NA only columns, thank god!
  norm_const <- colSums(weights_matrix, na.rm = TRUE)
  ratio_values <- colSums(ratios_matirx, na.rm = TRUE)
  return((1 / norm_const) * ratio_values)

}

