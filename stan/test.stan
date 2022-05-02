data{
  int<lower=0> N;
  vector[N] y; // whole-leaf LMA / disc LMA
  vector[N] x; // whole-leaf LMA / disc LMA
}

parameters {
  vector[2] beta;
  real<lower=0> sigma;
  real<lower=0> sigma_x;
  real x_hat;
  real<lower=0,upper=pi()/2> omega_unif;
  vector[N] z;
}

transformed parameters {
  vector[N] x_true;
  real<lower=0> omega;
  omega = 2.5 * tan(omega_unif);
  x_true = x_hat + z * omega;
}

model {
  z ~ std_normal();
  x_hat ~ normal(0, 2.5);
  beta ~ normal(0, 5);
  sigma_x ~ cauchy(0, 2.5);
  x ~ normal(x_true, sigma_x);
  y ~ normal(beta[1] + beta[2] * x_true, sigma);
}