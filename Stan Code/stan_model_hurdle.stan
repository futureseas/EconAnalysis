data {
  int<lower = 0> N; // number of observations.
  int<lower = 1> J; // Number of species.
  int<lower = 0> L; // number of ports.
  int<lower = 0> Y; // number of years.
  int<lower=1,upper=L> portID[N]; 
  int yearID[N];  
  real<lower=0,upper=1> sdm_psdn[N];
  real<lower=0,upper=1> sdm_msqd[N]; 
  real<lower=0> qPSDN[N]; // q is an N x J matrix. Landings by J species. 
  real<lower=0> qMSQD[N]; // q is an N x J matrix. Landings by J species. 
}

parameters {
  real<lower=0> prob_psdn[L]; 
  real<lower=0> prob_msqd[L]; 
  real<lower=0> const_msqd[L];
  real<lower=0> const_psdn[L];  
  vector[J] year_psdn[Y];
  real<lower=0> sd_probMSQD;
  real<lower=0> sd_probPSDN;
  real<lower=0> sd_qPSDN;
  real<lower=0,upper=1> theta_psdn;
}

transformed parameters{
  real mu_qPSDN[N];
  real mu_qMSQD[N];
  for (n in 1:N) {
      mu_qPSDN[n] = const_psdn[portID[n]] + prob_psdn[portID[n]] * sdm_psdn[n] + year_psdn[yearID[n]];
      mu_qMSQD[n] = const_msqd[portID[n]] + prob_msqd[portID[n]] * sdm_msqd[n] + year_msqd[yearID[n]];
  }
}

model {
// Priors
  for (l in 1:L) {
   prob_msqd[l] ~ lognormal(0, sd_probMSQD);
   prob_psdn[l] ~ lognormal(0, sd_probPSDN);
  }

  sd_probMSQD ~ cauchy(0, 2.5);
  sd_probPSDN ~ cauchy(0, 2.5);
  sd_qPSDN ~ cauchy(0, 2.5);
  

// Posterior
  for (n in 1:N) {
    if (qPSDN[n] == 0)
      1 ~ bernoulli(theta_psdn);
    else {
      0 ~ bernoulli(theta_psdn);
      qPSDN[n] ~ lognormal(mu_qPSDN[n], sd_qPSDN) T[000.1, ];
    }
  }
}



