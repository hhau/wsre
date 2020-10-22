data {
  // wsre args
  int <lower = 0> target_dimension;
  real wf_mean [target_dimension];
  real <lower = 0> wf_sd [target_dimension];
  real <lower = 0> wf_exponent;
}

parameters {
  real x [target_dimension];
}

model {
  // standard normal target
  target += std_normal_lpdf(x);

  // weighting function
  target += (wf_exponent * normal_lpdf(x | wf_mean, wf_sd));
}
