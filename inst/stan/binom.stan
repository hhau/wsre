data {
  // wsre args
  int <lower = 0> target_dimension;
  real wf_mean [target_dimension];
  real <lower = 0> wf_sd [target_dimension];
  real <lower = 0> wf_exponent;

}

parameters {
  real <lower = 0, upper = 1> P_x;
  real <lower = 0> N_x;

  // thing we actually care about - has to be called `x`
  real <lower = 0, upper = N_x> x [target_dimension];
}

model {
  // priors
  P_x ~ beta(864.0, 648.0);
  N_x ~ lognormal(4.93, 0.012);

  // continuous binomial
  for (dim in 1:target_dimension) {
    target += 
    lgamma(N_x - 1.0) - 
    (lgamma(x[dim] - 1.0) + lgamma(N_x - x[dim] - 1.0)) + 
    lmultiply(x[dim], P_x) + 
    (N_x - x[dim]) * log1m(P_x); // ideally combine with lmultiply, but not possible  
  }
  
  // weighting function - normal again
  target += wf_exponent * normal_lpdf(x | wf_mean, wf_sd);
}
