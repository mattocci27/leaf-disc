data{
  int<lower=0> N;
  int<lower=0> K;
  vector[N] log_y; // whole-leaf LMA / disc LMA
  vector[N] log_lma_disc; // whole-leaf LMA / disc LMA
  matrix[N, K] x; // predictors
}

transformed data{
  vector[N] log_lma_disc_s; // scaled
  int<lower=0> K2;
  K2 = K + 1;
  log_lma_disc_s = (log_lma_disc - mean(log_lma_disc)) / sd(log_lma_disc);
}

parameters{
  vector[K2] beta;
  vector[K2] gamma;
  real<lower=0> omega;
  vector[N] log_sigma;
  real<lower=0,upper=pi()/2> sigma_x_unif;
  vector[N] z;
}

transformed parameters {
  vector[N] log_lma_disc_true;
  real<lower=0> sigma_x;
  matrix[N, K2] x2; // predictors
  sigma_x = 2.5 * tan(sigma_x_unif);
  log_lma_disc_true = log_lma_disc_s + z * sigma_x_unif;
  // (int, la, lt, lma_disc)
  x2 = append_col(x, log_lma_disc_true);
}

model{
  vector[N] log_mu;
  vector[N] sigma;
  sigma = exp(log_sigma);
  z ~ std_normal();
  beta ~ normal(0, 5);
  gamma ~ normal(0, 5);
  omega ~ cauchy(0, 5);
  log_sigma ~ normal(x2 * gamma, omega);
  log_mu = x2 * beta;
  log_y ~ normal(log_mu, sigma);
}

// for model selection
generated quantities {
  vector[N] log_lik;
  vector[N] log_mu;
  vector[N] sigma;
  sigma = exp(log_sigma);
  log_mu = x2 * beta;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(log_y[n] | log_mu[n], sigma[n]);
  }
}
