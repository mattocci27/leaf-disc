data{
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> J;
  array[N] int<lower=1, upper=J> sp;  // species
  vector[N] log_y; // whole-leaf LMA / disc LMA
  vector[N] log_lma_disc; // whole-leaf LMA / disc LMA
  matrix[N, K] x; // predictors
}

parameters{
  vector[K] beta;
  vector[K] gamma;
  real<lower=0> omega;
  vector[N] log_sigma;
  vector<lower=0,upper=pi()/2>[2] tau_unif;
  vector[J] phi1_raw;
  vector[J] phi2_raw;
}

transformed parameters {
  vector[J] phi1;
  vector[J] phi2;
  vector[2] tau;
  for (i in 1:2) tau[i] = 2.5 * tan(tau_unif[i]);
  phi1 = phi1_raw * tau[1];
  phi2 = phi2_raw * tau[2];
}

model{
  vector[N] log_mu;
  vector[N] sigma;
  sigma = exp(log_sigma);
  log_mu = x * beta + log_lma_disc + phi1[sp] ;
  beta ~ normal(0, 5);
  gamma ~ normal(0, 5);
  log_sigma ~ normal(x * gamma + phi2[sp], omega);
  omega ~ cauchy(0, 5);
  log_y ~ normal(log_mu, sigma);
}

// for model selection
generated quantities {
  vector[N] log_lik;
  vector[N] log_mu;
  vector[N] sigma;
  sigma = exp(log_sigma);
  log_mu = x * beta + log_lma_disc ;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(log_y[n] | log_mu[n], sigma[n]);
  }
}
