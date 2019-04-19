data {
  // actual data - zilch

  // weighting function arguments
  real wf_mean;
  real <lower = 0> wf_sd;
  real <lower = 0> wf_exp;

  // in this example this will be zero, but needed for consistency with other
  // model signatures
  int <lower = 0> target_dimension;
}

parameters {
  real <lower = 0, upper = 1> P_x;
  real <lower = 0> N_x;

  // thing we actually care about - has to be called `x`
  real <lower = 0, upper = N_x> x;
}

model {
  // priors
  P_x ~ beta(864.0, 648.0);
  N_x ~ lognormal(4.93, 0.012);

  // continuous binomial
  target += 
    lgamma(N_x - 1) - 
    (lgamma(x - 1) + lgamma(N_x - x - 1)) + 
    lmultiply(x, P_x) + 
    (N_x - x) * log1m(P_x); // ideally combine with lmultiply, but not possible

  // weighting function - normal again
  target += wf_exp * normal_lpdf(x | wf_mean, wf_sd);
}
