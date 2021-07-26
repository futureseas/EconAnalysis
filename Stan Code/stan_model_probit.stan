functions {
  int sum2d(int[,] a) {
    int s = 0;
    for (i in 1:size(a))
      s += sum(a[i]);
    return s;
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
  int<lower=0, upper=1> q_probit[N,J]; // 1 if landings are positive.
}

transformed data {
  int<lower=0> N_pos;
  int<lower=1,upper=N> n_pos[sum2d(q_probit)];
  int<lower=1,upper=J> j_pos[size(n_pos)];
  int<lower=0> N_neg;
  int<lower=1,upper=N> n_neg[(N * J) - size(n_pos)];
  int<lower=1,upper=J> j_neg[size(n_neg)];
  int i;
  int k;

  // Transformed data
  N_pos = size(n_pos);
  N_neg = size(n_neg);

  // Obtain position where landing happen in matrix q
  i = 1;
  k = 1;
  for (n in 1:N) {
    for (j in 1:J) {
      if (q_probit[n,j] == 1) {
        n_pos[i] = n;
        j_pos[i] = j;
        i += 1;
      }
      else {
        n_neg[k] = n;
        j_neg[k] = j;
        k += 1;
      }
    }
  }
}

parameters {
  real<lower=0> prob_psdn[L];  
  real<lower=0> prob_msqd[L];  
  vector<lower=0>[J] alpha[L]; 
  vector[J] beta_year[Y];
  real<lower=0> sigma_msqd;
  real<lower=0> sigma_psdn;
  corr_matrix[J] Omega;
  real<lower=0,upper=2> eta;
  vector<lower=0>[N_pos] z_pos;
  vector<upper=0>[N_neg] z_neg;
}

transformed parameters {
  vector[J] z[N];
  vector[J] mu[N];
  for (n in 1:N_pos)
    z[n_pos[n], j_pos[n]] = z_pos[n];
  for (n in 1:N_neg)
    z[n_neg[n], j_neg[n]] = z_neg[n];
  for (n in 1:N) {
      mu[n,1] = alpha[portID[n],1] + prob_psdn[portID[n]] * sdm_psdn[n] + beta_year[yearID[n,1],1];
      mu[n,2] = alpha[portID[n],2] + prob_msqd[portID[n]] * sdm_msqd[n] + beta_year[yearID[n,1],2];
  }
}

model {
  Omega ~ lkj_corr(eta);
  for (l in 1:L) {
   prob_msqd[l] ~ lognormal(0, sigma_msqd);
   prob_psdn[l] ~ lognormal(0, sigma_psdn);
  }

  z ~ multi_normal(mu, Omega);
}


