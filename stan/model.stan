data{
  int<lower=0> N;
  int<lower=0> K;
  vector[N] log_y; // whole-leaf lma
  vector[N] log_lma_disc; //
  matrix[N, K] x; // predictors
}

parameters{
  vector[2] beta;
  vector[K] gamma;
  //vector[N] log_sigma;
  real<lower=0> tau;
  vector[N] log_sigma;
}

model{
  // vector[N] log_sigma;
  // log_sigma = x * gamma;
  vector[N] log_sigma_hat;
  log_sigma_hat = x * gamma;
  beta ~ normal(0, 5);
  gamma ~ normal(0, 5);
  tau ~ cauchy(0, 5);
  log_sigma ~ normal(log_sigma_hat, tau);
  log_y ~ normal(beta[1] + beta[2] * log_lma_disc, exp(log_sigma));
  //log_y ~ normal(log_lma_disc, exp(log_sigma));
}

// for model selection
generated quantities {
  vector[N] log_lik;
  // vector[N] log_sigma;
  // log_sigma = x * gamma;
  for (n in 1:N) {
    //log_lik[n] = normal_lpdf(log_y[n] | log_lma_disc[n],
    log_lik[n] = normal_lpdf(log_y[n] | beta[1] + beta[2] * log_lma_disc[n],
                                        exp(log_sigma[n]));
  }
}
