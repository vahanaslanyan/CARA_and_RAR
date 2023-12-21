
  data {
    int<lower=0> N;  // number of observations
    int<lower=1> K;  // number of treatments
    matrix[N,K] treatment;  // categorical treatment variable dummified
    vector[N] y;  // outcome variable
}

  parameters {
    vector[K] beta_t;  // coefficients for treatment levels
    real<lower=0> sigma;  // error standard deviation
}

  model {
    // priors
    beta_t ~ normal(0, 500);
    sigma ~ cauchy(0, 50);
  
    // model
    for (i in 1:N) {
    y[i] ~ normal(treatment[i]*beta_t, sigma);
  }
}

