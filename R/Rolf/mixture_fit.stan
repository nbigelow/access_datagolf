data {
  int<lower = 0> N; // Number of observations
  real y[N]; // Observations
  real mu_1; // Prior parameter for lambda_1
  real<lower=0> sigma_1; // Prior parameter for lambda_1
  real mu_2; // Prior parameter for lambda_2
  real<lower=0> sigma_2; // Prior parameter for lambda_2
  real<lower=0,upper=1> theta_0; // Prior for theta
  int<lower=0,upper=1> lambda[N];
}

parameters {
  real mu[2]; // Mixture components
  real<lower=0> sigma[2];
  real<lower=0, upper=1> theta; // Mixing proportion
}

model {
  // priors
  sigma[1] ~ inv_gamma(1,1); // Prior for lambda_1
  sigma[2] ~ inv_gamma(1,1); // Prior for lambda_2
  mu[1] ~ normal(mu_1, sigma_1); // Prior for lambda_1
  mu[2] ~ normal(mu_2, sigma_2); // Prior for lambda_1
  theta ~ beta(theta_0*25, (1-theta_0)*25); // Prior for mixing proportion

  // likelihood
  for (n in 1:N){
    lambda[n] ~ bernoulli(theta);
    if(lambda[n]==1){
      target += normal_lpdf(y[n] | mu[1],sigma[1]);
    } else {
      target += normal_lpdf(y[n] | mu[2],sigma[2]);
    }
  }
}

generated quantities {
  int<lower=0,upper=1> lambda_new; // New value for lambda
  real y_new; // New observation

  // Sample lambda from the posterior distribution
  lambda_new = bernoulli_rng(theta);

  // Sample a new observation from the selected normal distribution
  if (lambda_new == 1) {
    y_new = normal_rng(mu[1],sigma[1]);
  } else {
    y_new = normal_rng(mu[2],sigma[2]);
  }
}
