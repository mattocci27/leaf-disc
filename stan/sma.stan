data{
  int<lower=0> N;
  int<lower=0> K;
  vector[N] log_y; // whole-leaf LMA / disc LMA
  vector[N] log_lma_disc; // whole-leaf LMA / disc LMA
  matrix[N, K] x; // predictors
}

// parameters {
//   vector[2] beta;
//   real<lower=0> sigma;
//   real mu_x;
//   real<lower=0> sigma_x;
//   real<lower=0> omega;
//   vector[N] log_lma_disc_true; // whole-leaf LMA / disc LMA
// }

// model {
//   vector[N] log_mu;
//   beta ~ normal(0, 5);
//   mu_x ~ normal(0, 5);
//   log_lma_disc_true ~ normal(mu_x, sigma_x);
//   log_lma_disc ~ normal(log_lma_disc_true, omega);
//   log_mu = beta[1] + beta[2] * log_lma_disc_true;
//   log_y ~ normal(log_mu, sigma);
//}

// transformed data {
//   vector[N] log_lma_disc_s;
//   log_lma_disc_s = (log_lma_disc - mean(log_lma_disc)) / sd(log_lma_disc);
// }

parameters {
  vector[2] beta;
  real<lower=0> sigma;
  real<lower=0,upper=pi()/2> sigma_x_unif;
  real log_lma_disc_hat;
  vector[N] z;
}

transformed parameters {
  vector[N] log_lma_disc_true;
  real<lower=0> sigma_x;
  sigma_x = 2.5 * tan(sigma_x_unif);
  log_lma_disc_true = log_lma_disc_hat + z * sigma_x;
}


model {
  vector[N] log_mu;
  z ~ std_normal();
  log_lma_disc_hat ~ normal(0, 5);
  beta ~ normal(0, 5);
  log_mu = beta[1] + beta[2] * log_lma_disc_true;
  log_y ~ normal(log_mu, sigma);
}

// for model selection
generated quantities {
  vector[N] log_lik;
  vector[N] log_mu;
  log_mu = beta[1] + beta[2] * log_lma_disc_true;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(log_y[n] | log_mu[n], sigma);
  }
}
