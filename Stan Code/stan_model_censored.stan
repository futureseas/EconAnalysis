data {
  int<lower = 0> N;   // number of observations.
  int<lower = 1> J;   // Number of species.
  int<lower = 0> L;   // number of ports.
  int<lower = 0> Y;   // number of years.
  int<lower=1,upper=L> portID[N]; //
  int yearID[N,1]; //
  real sdm_psdn[N];
  real sdm_msqd[N]; 
  vector[J] q[N]; // q is an N x J matrix. Landings by J species. 
  int<lower=0, upper=1> q_cen[N,J]; // ?is left-censored : +1, 0
}

transformed data {
  int<lower=0> n_censored = 0;
  for (n in 1:N) n_censored = n_censored + sum(q_cen[n]);
}

parameters {
  real<lower=0> prob_psdn[L];        // Parameters matrix
  real<lower=0> prob_msqd[L];        // Parameters matrix
  vector<lower=0>[J] alpha[L]; // Parameters matrix
  vector[J] beta_year[Y];
  real<upper=0> q_miss[n_censored];
  corr_matrix[J] Omega;
  vector<lower=0>[J] tau;
  real<lower=0,upper=2> eta;
  real<lower=0> sigma_msqd;
  real<lower=0> sigma_psdn;
}

transformed parameters{
  vector[J] mu[N];
  for (n in 1:N) {
      mu[n,1] = alpha[portID[n],1] + prob_psdn[portID[n]] * sdm_psdn[n] + beta_year[yearID[n,1],1];
      mu[n,2] = alpha[portID[n],2] + prob_msqd[portID[n]] * sdm_msqd[n] + beta_year[yearID[n,1],2];
  }
}

model {
  int pos = 1;  
  vector[J] q_c[N];
  matrix[J, J] Sigma;
  Sigma = quad_form_diag(Omega, tau);
  tau ~ cauchy(0, 2.5);
  Omega ~ lkj_corr(eta);
  for (l in 1:L) {
   prob_msqd[l] ~ lognormal(0, sigma_msqd);
   prob_psdn[l] ~ lognormal(0, sigma_psdn);
  }

  for(n in 1:N) for (j in 1:J) {
    if (q_cen[n,j] == 0) q_c[n,j] = q[n,j];
    else {
      q_c[n,j] = q_miss[pos];
      pos = pos + 1;
    }
  }

  q_c ~ multi_normal(mu, Sigma);
}

