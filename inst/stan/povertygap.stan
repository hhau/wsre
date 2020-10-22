// generated with brms 2.13.3
// Modified by Andrew Manderson on 2020-09-30T14:24:48+01:00
data {
  int <lower = 1> N;  // number of observations
  vector [N] Y;  // response variable
  int <lower = 1> K;  // number of population-level effects
  matrix [N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int <lower = 1> N_1;  // number of grouping levels
  int <lower = 1> M_1;  // number of coefficients per level
  int <lower = 1> J_1 [N];  // grouping indicator per observation
  // group-level predictor values
  vector [N] Z_1_1;

  int prior_only;  // should the likelihood be ignored?

  // needed to compute weighted sum + phi
  vector [N_1] obs_per_group; 
  real <lower = 0> log1p_poverty_threshold;

  // wsre args
  int <lower = 0> target_dimension;
  real wf_mean [target_dimension];
  real <lower = 0> wf_sd [target_dimension];
  real <lower = 0> wf_exponent;
}

transformed data {
  int Kc = K - 1;
  matrix [N, Kc] Xc;  // centered version of X without an intercept
  vector [Kc] means_X;  // column means of X before centering
  for (i in 2 : K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}

parameters {
  vector [Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  real <lower = 0> sigma;  // residual SD
  vector <lower = 0> [M_1] sd_1;  // group-level standard deviations
  vector [N_1] z_1 [M_1];  // standardized group-level effects

  vector [N] latent_std_normal;
}

transformed parameters {
  // initialize linear predictor term
  vector [N] mu = Intercept + Xc * b;
  // actual group-level effects
  vector [N_1] r_1_1 = (sd_1[1] * (z_1[1]));  
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // phi / SAE terms
  vector [N_1] phi_area_poverty_gap = rep_vector(0.0, N_1);
  real x [target_dimension];
  vector [N] post_pred_y = mu + sigma * latent_std_normal;

  for (n in 1:N) {
    // add more terms to the linear predictor
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
  }

  for (ii in 1 : N) {
    phi_area_poverty_gap[J_1[ii]] += 
      ((log1p_poverty_threshold - post_pred_y[ii]) / log1p_poverty_threshold) * 
        (post_pred_y[ii] < log1p_poverty_threshold);
  }

  phi_area_poverty_gap = phi_area_poverty_gap ./ obs_per_group;
  x[1] = sum(phi_area_poverty_gap .* obs_per_group) / N;
}

model {
  // priors including all constants
  target += normal_lpdf(b | 0, 1.0);
  target += normal_lpdf(Intercept | 6, 1);
  target += normal_lpdf(sigma | 0, 0.25);
  target += inv_gamma_lpdf(sd_1 | 8.918 * 2, 8.644);
  target += normal_lpdf(z_1[1] | 0, 0.5);

  // this needs to happen again, will never get the variance otherwise
  target += std_normal_lpdf(latent_std_normal);

  // weighting function
  if (wf_exponent != 0) {
    target += (wf_exponent * normal_lpdf(x | wf_mean, wf_sd));  
  }

  if (!prior_only) {
    target += normal_lpdf(Y | mu, sigma);
  }
}

generated quantities {
  vector [N] predictive_y = to_vector(
    exp(normal_rng(b_Intercept + Xc * b, sigma))
  ); 
}
