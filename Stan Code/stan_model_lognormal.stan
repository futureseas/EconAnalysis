data {
  int<lower = 0> N; // number of observations.
  int<lower = 1> J; // Number of species.
  int<lower = 0> L; // number of ports.
  int<lower = 0> Y; // number of years.
  int<lower=1,upper=L> portID[N]; 
  int yearID[N,1];  
  real sdm_psdn[N];
  real sdm_msqd[N]; 
  vector[J] q[N]; // q is an N x J matrix. Landings by J species. 
  int<lower=0, upper=1> q_probit[N,J]; // 1 if landings are positive.
}

transformed data {
  vector[J] log_q[N];
  log_q = log(q);
}

parameters {
  real<lower=0> prob_psdn[L]; 
  real<lower=0> prob_msqd[L]; 
  vector<lower=0>[J] alpha[L];
  vector[J] beta_year[Y];
  real<lower=0> sigma_msqd;
  real<lower=0> sigma_psdn;
  corr_matrix[J] Omega;
  vector<lower=0>[J] tau;
  // real<lower=0,upper=2> eta;
}


transformed parameters{
  vector[J] mu[N];
  for (n in 1:N) {
      mu[n,1] = alpha[portID[n],1] + prob_psdn[portID[n]] * sdm_psdn[n] + beta_year[yearID[n,1],1];
      mu[n,2] = alpha[portID[n],2] + prob_msqd[portID[n]] * sdm_msqd[n] + beta_year[yearID[n,1],2];
  }
}

model {
  matrix[J,J] Sigma; 
  Sigma = quad_form_diag(Omega, tau);
  Omega ~ lkj_corr(1);
  tau ~ cauchy(0, 2.5);
  for (l in 1:L) {
   prob_msqd[l] ~ lognormal(0, sigma_msqd);
   prob_psdn[l] ~ lognormal(0, sigma_psdn);
  }
  
  for (n in 1:N) {
    if (q_probit[n, 1] && q_probit[n, 2]) {
      log_q[n] ~ multi_normal(mu, Sigma);
      for (j in 1:J)
        target += -log_q[j];
    }
    
    else if (q_probit[n, 1]) {
      log_q[n, 1] ~ normal(mu[1], sqrt(Sigma[1, 1]));
      target += -log_q[1];
    }
    
    else if (q_probit[n, 2]) {
      log_q[n, 2] ~ normal(mu[2], sqrt(Sigma[2, 2]));
      target += -log_q[2];
    }
  }
}

