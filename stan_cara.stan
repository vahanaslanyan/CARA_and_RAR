
  data {
    int<lower=0> N;  // number of observations
    int<lower=1> K;  // number of treatments
    int<lower=0, upper=1> x[N];  // binary covariate
    matrix[N,K] treatment;  // categorical treatment variable dummified
    vector[N] y;  // outcome variable
}

  parameters {
    real beta_x;  // coefficient for binary covariate
    vector[K] beta_t;  // coefficients for treatment levels
    real<lower=0> sigma;  // error standard deviation
}

  model {
    // priors
    beta_t ~ normal(0, 500);
    beta_x ~ normal(0, 500);
    sigma ~ cauchy(0, 50);
  
    // model
    for (i in 1:N) {
    y[i] ~ normal(treatment[i]*beta_t+beta_x*x[i], sigma);
  }
}

