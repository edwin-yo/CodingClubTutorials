data { int N; vector[N] y; }
parameters { real mu; real<lower=0> sigma; }
model { y ~ normal(mu, sigma); }
