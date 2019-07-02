#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

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
  name = match.arg(arg = name, choices = c("normal", "binom"))
  .stan_models[[name]]
}