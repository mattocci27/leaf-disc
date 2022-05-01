data{
  int<lower=0> N;
  int<lower=0> K;
  vector[N] log_y; // whole-leaf LMA / disc LMA
  vector[N] log_lma_disc; // whole-leaf LMA / disc LMA
  matrix[N, K] x; // predictors
}

parameters {
  vector[K] beta;
  vector[K] gamma;
  real<lower=0,upper=pi()/2> sigma_x_unif;
  real<lower=0,upper=pi()/2> omega_unif;
  vector[N] z;
  vector[N] z2;
}

transformed parameters {
  vector[N] log_lma_disc_true;
  vector[N] log_sigma;
  real<lower=0> sigma_x;
  real<lower=0> omega;
  sigma_x = 2.5 * tan(sigma_x_unif);
  omega = 2.5 * tan(omega_unif);
  log_lma_disc_true = log_lma_disc + z * sigma_x;
  log_sigma = x * gamma + z2 * omega;
}

model {
  vector[N] log_mu;
  vector[N] sigma;
  sigma = exp(log_sigma);
  z ~ std_normal();
  z2 ~ std_normal();
  beta ~ normal(0, 5);
  gamma ~ normal(0, 5);
  log_mu = x * beta + log_lma_disc_true;
  log_y ~ normal(log_mu, sigma);
}

// for model selection
generated quantities {
  vector[N] log_lik;
  vector[N] log_mu;
  vector[N] sigma;
  sigma = exp(log_sigma);
  log_mu = x * beta + log_lma_disc_true;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(log_y[n] | log_mu[n], sigma[n]);
  }
}
