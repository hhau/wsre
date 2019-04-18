#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector weighting_function(
  NumericVector x,
  double wf_mean,
  double wf_sd,
  double wf_exponent // this needs to be one to precisely control the product mean
) {
  NumericVector result(x.size()); 
  result = pow(dnorm(x, wf_mean, wf_sd), wf_exponent);
  return(result);
}

// This is one of those rare cases where the C++ version is only slightly faster
// as it can (mostly) avoid garbage collection.

// [[Rcpp::export]]
NumericVector gauss_kde(
  NumericVector x,
  NumericVector x_samples,
  double bandwidth
) {
  int n_vals = x.size();
  int n_samples = x_samples.size();
  NumericVector result(n_vals);

  for (int ii = 0; ii < n_vals; ++ii) {
    result[ii] = sum(dnorm((x[ii] - x_samples) / bandwidth, 0.0, 1.0));
  }
  
  return(result / (n_samples * bandwidth)); 

}

// [[Rcpp::export]]
NumericVector weight_gauss_kde_jones(
  NumericVector x,
  NumericVector weighted_samples,
  double wf_mean,
  double wf_sd,
  double wf_exponent,
  double bandwidth
) {
  int n_vals = x.size();
  int n_samples = weighted_samples.size();
  NumericVector result(n_vals);
  NumericVector wf_inv_vals = pow(weighting_function(
    weighted_samples,
    wf_mean,
    wf_sd,
    wf_exponent
  ), -1.0);
 
  for (int ii = 0; ii < n_vals; ++ii) {
    result[ii] = sum(
      dnorm((x[ii] - weighted_samples) / bandwidth, 0.0, 1.0) * wf_inv_vals
    );
  }

  return(result / (n_samples * bandwidth));

}

// [[Rcpp::export]]
NumericVector weight_gauss_kde_bhatt(
  NumericVector x,
  NumericVector weighted_samples,
  double wf_mean,
  double wf_sd,
  double wf_exponent,
  double bandwidth
) {
  int n_vals = x.size();
  int n_samples = weighted_samples.size();
  NumericVector result(n_vals);
  NumericVector wf_inv_vals = pow(weighting_function(
    x,
    wf_mean,
    wf_sd,
    wf_exponent
  ), -1.0);

  for (int ii = 0; ii < n_vals; ++ii) {
    result[ii] = sum(
      dnorm((x[ii] - weighted_samples) / bandwidth, 0.0, 1.0)
    );
  }

  return((result * wf_inv_vals) / (n_samples * bandwidth));

}