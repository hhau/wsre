.extend_list <- function(...) {
  lists <- list(...)
  output <- lists[[1]]
  for (value in lists[2 : length(lists)]) {
    for (name in names(value)) {
      output[[name]] <- value[[name]]
    }
  }
  return(output)
}

.is_numerically_okay <- function(x) {
  !(is.infinite(x) | is.nan(x) | is.na(x) | abs(x) < .Machine$double.eps)
}

.named_model <- function(name) {
  a_name <- match.arg(arg = name, choices = names(stanmodels))
  stanmodels[[a_name]]
}

#' Null wsre values
#'
#' Null wsre parameter values, for when you want to use the included stan models
#' but not do wsre.
#'
#' @export
null_wsre_data <- list(
  target_dimension = 1,
  wf_mean = array(data = 0, dim = 1),
  wf_sd = array(data = 0, dim = 1),
  wf_exponent = 0
)