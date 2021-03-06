data{
  int<lower=0> N;
  int<lower=0> K;
  vector[N] log_y; // whole-leaf LMA / disc LMA
  matrix[N, K] x; // predictors
}

parameters{
  vector[K] beta;
  vector[K] gamma;
  real<lower=0,upper=pi()/2> omega_unif;
  vector[N] z;
}

transformed parameters {
  real<lower=0> omega;
  vector[N] log_sigma;
  omega = 2.5 * tan(omega_unif);
  log_sigma = x * gamma + z * omega;
}

model{
  vector[N] log_mu;
  vector[N] sigma;
  sigma = exp(log_sigma);
  log_mu = x * beta;
  z ~ std_normal();
  beta ~ normal(0, 5);
  gamma ~ normal(0, 5);
  log_y ~ normal(log_mu, sigma);
}

// for model selection
generated quantities {
  vector[N] log_lik;
  vector[N] log_mu;
  vector[N] sigma;
  sigma = exp(log_sigma);
  log_mu = x * beta;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(log_y[n] | log_mu[n], sigma[n]);
  }
}
