data {
  // how many dimensions should the target be - commonly 1
  int <lower = 1> target_dimension;
 
  // weighting function params
  // wf := weighting_function
  real wf_mean;
  real <lower = 0> wf_sd;
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
