functions {
  int observed_12(int[,] a) {
    int i_12 = 0;
    for (n in 1:size(a)) {
      if (a[n, 1] == 1 && a[n, 2] == 1) {
        i_12 += 1;
      }
    }
    return i_12;
  }

  int observed_1(int[,] a) {
    int i_1 = 0;
    for (n in 1:size(a)) {
      if (a[n, 1] == 1 && a[n, 2] == 0) {
        i_1 += 1;
      }
    }
    return i_1;
  }

  int observed_2(int[,] a) {
    int i_2 = 0;
    for (n in 1:size(a)) {
      if (a[n, 1] == 0 && a[n, 2] == 1) {
        i_2 += 1;
      }
    }
    return i_2;
  }  
}

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
  int<lower=0, upper=1> q_probit[N, J]; // 1 if landings are positive.
}

transformed data {
  vector[J] log_q[N];
  int ns12[observed_12(q_probit)];
  int ns1[observed_1(q_probit)];
  int ns2[observed_2(q_probit)];
  int n12;
  int n1;
  int n2;

  log_q = log(q);

  // Obtain position where different sampling situations 
  n12 = 1;
  n1 = 1;
  n2 = 1;

  for (n in 1:N) {
    if (q_probit[n, 1] && q_probit[n, 2]) {
      ns12[n12] = n;
      n12 += 1;
    } 
    else if (q_probit[n, 1]) {
      ns1[n1] = n;
      n1 += 1;
    }
    else if (q_probit[n, 2]) {
      ns2[n2] = n;
      n2 += 1;
    }
  }
}

parameters {
  real<lower=0> prob_psdn[L]; 
  real<lower=0> prob_msqd[L]; 
  vector<lower=0>[J] alpha[L];
  // vector[J] beta_year[Y];
  real<lower=0> sigma_msqd;
  real<lower=0> sigma_psdn;
  corr_matrix[J] Omega;
  vector<lower=0>[J] tau;
  real<lower=0,upper=2> eta;
}

transformed parameters{
  vector[J] mu[N];
  for (n in 1:N) {
      mu[n,1] = alpha[portID[n],1] + prob_psdn[portID[n]] * sdm_psdn[n]; // + beta_year[yearID[n,1],1];
      mu[n,2] = alpha[portID[n],2] + prob_msqd[portID[n]] * sdm_msqd[n]; // + beta_year[yearID[n,1],2];
  }
}

model {
  matrix[J,J] Sigma; 
  Sigma = quad_form_diag(Omega, tau);
  Omega ~ lkj_corr(eta);
  tau ~ cauchy(0, 2.5);

  for (l in 1:L) {
   prob_msqd[l] ~ lognormal(0, sigma_msqd);
   prob_psdn[l] ~ lognormal(0, sigma_psdn);
  }
  
  log_q[ns12] ~ multi_normal(mu[ns12], Sigma);
  log_q[ns1,1] ~ normal(mu[ns1,1], sqrt(Sigma[1, 1]));
  log_q[ns2,2] ~ normal(mu[ns1,2], sqrt(Sigma[2, 2]));

  for (n in 1:N) {
    if (q_probit[n, 1] && q_probit[n, 2]) {
      for (j in 1:J) {
        target += -log_q[n,j];
      }
    }
    else if (q_probit[n, 1]) {
      target += -log_q[n, 1];
    }
    else if (q_probit[n, 2]) {
      target += -log_q[n, 2];
    }
  }
}

