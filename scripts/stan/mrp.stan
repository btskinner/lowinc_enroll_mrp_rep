// -----------------------------------------------
// mrp_cov
// -----------------------------------------------
data {
  int<lower=1> M;	 // # states
  int<lower=1> N;	 // # obs
  int<lower=1> J;        // # raceetj groups
  int<lower=1> K;        // # 2nd-level predictors
  int<lower=1> R;        // # regionss
  int st[N];
  int rg[N];
  int college[N];
  int total[N];
  vector[N] ge;
  vector[N] lo;
  int ra[N];
  matrix[M,K] z;
}
parameters {
  // overall
  real a;
  real<lower=0> s_a_rg;
  vector<multiplier=s_a_rg>[R] a_rg;
  real<lower=0> s_a_st;
  vector<multiplier=s_a_st>[M] a_st;
  // first-level primary terms
  real b_ge;
  real b_lo;
  real<lower=0> s_a_ra;
  vector<multiplier=s_a_ra>[J] a_ra;
  // interaction between primary effect and st
  real<lower=0> s_a_st_lo;
  vector<multiplier=s_a_st_lo>[M] a_st_lo;
  // second-level covariate matrix
  real<lower=0> s_g;
  vector<multiplier=s_g>[K] g;
}
model {
  // -------------------------
  // likelihood
  // -------------------------
  college ~ binomial_logit(total,
			   a
			   + a_rg[rg]
			   + a_st[st]
			   + b_lo * lo
			   + b_ge * ge
			   + a_ra[ra]
			   + a_st_lo[st] .* lo
			   + z[st,] * g);
  // -------------------------
  // priors: coefficients
  // -------------------------
  a ~ normal(0,2);
  a_rg ~ normal(0,s_a_rg);
  a_st ~ normal(0,s_a_st);
  b_lo ~ normal(0,2);
  b_ge ~ normal(0,2);
  a_ra ~ normal(0,s_a_ra);
  a_st_lo ~ normal(0,s_a_st_lo);
  g ~ normal(0,s_g);
  // -------------------------
  // priors: variance
  // -------------------------
  s_a_rg ~ normal(0,1);
  s_a_st ~ normal(0,1);
  s_a_ra ~ normal(0,1);
  s_a_st_lo ~ normal(0,1);
  s_g ~ normal(0,1);
}
