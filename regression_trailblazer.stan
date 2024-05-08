data {
  int<lower=0> N;  // Number of observations
  int<lower=0> K;  // Number of covariates
  matrix[N, K] X;  // Matrix of covariates
  vector[N] y;     // Vector of response variable
}

parameters {
  vector[K] beta;  // Coefficients for the covariates
  real<lower=0> sigma; // Standard deviation of the error term
}

model {
  // Priors
  beta ~ normal(0, 500);   
  sigma ~ cauchy(0, 50);  
  
  // Likelihood
  y ~ normal(X * beta, sigma);  // Linear regression model
}