// -----------------------------------------------
// simulation_mrp.stan
// -----------------------------------------------
data {
  int<lower=1> M;	 // # states
  int<lower=1> N;	 // # obs
  int<lower=1> K;        // # 2nd-level predictors
  int y[N];
  int total[N];
  int state[N];
  vector[N] t;
  matrix[M,K] z;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma_alpha_state;
  vector<multiplier=sigma_alpha_state>[M] alpha_state;
  real<lower=0> sigma_beta_state;
  vector<multiplier=sigma_beta_state>[M] beta_state;
  real<lower=0> sigma_gamma;
  vector<multiplier=sigma_gamma>[K] gamma;
}
model {
  // likelihood
  y ~ binomial_logit(total,
		     alpha
		     + alpha_state[state]
		     + beta * t
		     + beta_state[state] .* t
		     + z[state,] * gamma);
  // priors: coefficients
  alpha ~ normal(0,2);
  beta ~ normal(0,2);
  alpha_state ~ normal(0,sigma_alpha_state);
  beta_state ~ normal(0,sigma_beta_state);
  gamma ~ normal(0,sigma_gamma);
  sigma_alpha_state ~ normal(0,1);
  sigma_beta_state ~ normal(0,1);
  sigma_gamma ~ normal(0,1);
}
