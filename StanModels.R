# Set working directory
# Stan models will be saved to this directory
setwd("C:/Users/koen-/Desktop/PhD/Project/Writing/BayesianAssumptionCheckForNormality/BayesianNormalityCheck")

stanmodelH0 <- '
     data {
     int<lower=1> n; // number of observations
     vector[n] X; // observations
     int choice1;
     int choice2;
     real mu_normal_value1;
     real mu_normal_value2;
     real sigma_normal_value1;
     real sigma_normal_value2;
     }
     parameters{
     real mu;
     real<lower=0> sigma;
     }
     model {
     if(choice1 == 1){
        mu ~ normal(mu_normal_value1, mu_normal_value2);
     } else if (choice1 == 2){
        mu ~ cauchy(mu_normal_value1, mu_normal_value2);
     } else if (choice1 == 3){
        mu ~ gamma(mu_normal_value1, mu_normal_value2);
     }
     if(choice1 == 1){
        sigma ~ normal(sigma_normal_value1, sigma_normal_value2);
     } else if (choice1 == 2){
        sigma ~ cauchy(sigma_normal_value1, sigma_normal_value2);
     } else if (choice1 == 3){
        sigma ~ gamma(sigma_normal_value1, sigma_normal_value2);
     }
     for(i in 1:n){
     target += normal_lpdf(X[i] | mu, sigma);
     }
     }'

stanNormal <- rstan::stan_model(model_code = stanmodelH0, model_name="H0",auto_write = TRUE)
saveRDS(stanNormal, file = "stanNormal.rds")

stanmodelH1 <- "
data{
    int n;
    vector[n] X;
    int choice1;
    int choice2;
    int choice3;
    real lambda_mixture_value1;
    real lambda_mixture_value2;
    real sigma_mixture_value1;
    real sigma_mixture_value2;
    real theta_mixture_value1;
    real theta_mixture_value2;
}
parameters {
real lambda1;
real lambda2;
real<lower=0,upper=1> theta;
real<lower=0> sigma;
}
model {
    if(choice1 == 1){
        lambda1 ~ normal(lambda_mixture_value1, lambda_mixture_value2);
        lambda2 ~ normal(lambda_mixture_value1, lambda_mixture_value2);
    } else if (choice1 == 2){
        lambda1 ~ cauchy(lambda_mixture_value1, lambda_mixture_value2);
        lambda2 ~ cauchy(lambda_mixture_value1, lambda_mixture_value2);
    } else if (choice1 == 3){
        lambda1 ~ gamma(lambda_mixture_value1, lambda_mixture_value2);
        lambda2 ~ gamma(lambda_mixture_value1, lambda_mixture_value2);
    }
    if(choice2 == 1){
        sigma ~ normal(sigma_mixture_value1, sigma_mixture_value2);
    } else if (choice2 == 2){
        sigma ~ cauchy(sigma_mixture_value1, sigma_mixture_value2);
    } else if (choice2 == 3){
        sigma ~ gamma(sigma_mixture_value1, sigma_mixture_value2);
    }
    if(choice3 == 1){
        theta ~ normal(theta_mixture_value1, theta_mixture_value2);
    } else if (choice2 == 2){
        theta ~ cauchy(theta_mixture_value1, theta_mixture_value2);
    } else if (choice2 == 3){
        theta ~ gamma(theta_mixture_value1, theta_mixture_value2);
    }
    for(i in 1:n){
        target += log_mix(theta, normal_lpdf(X[i] | lambda1, sigma), normal_lpdf(X[i] | lambda2, sigma));
    }
} 
"

stanMixture <- rstan::stan_model(model_code = stanmodelH1, model_name="H1",auto_write = TRUE)
saveRDS(stanMixture, file = "stanMixture.rds")

stanmodelH2 <- '
data {
    int<lower=0> n;
    vector[n] X;
    int choice1;
    int choice2;
    int choice3;
    real mu_skew_value1;
    real mu_skew_value2;
    real sigma_skew_value1;
    real sigma_skew_value2;
    real alpha_skew_value1;
    real alpha_skew_value2;
}
parameters {
    real xi;
    real<lower=0> omega;
    real alpha;
}
model {
    if(choice1 == 1){
        xi ~ normal(mu_skew_value1,mu_skew_value2);
    } else if (choice1 == 2){
        xi ~ cauchy(mu_skew_value1, mu_skew_value2);
    } else if (choice1 == 3){
        xi ~ gamma(mu_skew_value1, mu_skew_value2);
    }
    if(choice2 == 1){
        omega ~ normal(sigma_skew_value1, sigma_skew_value2);
    } else if (choice2 == 2){
        omega ~ cauchy(sigma_skew_value1, sigma_skew_value2);
    } else if (choice2 == 3){
        omega ~ gamma(sigma_skew_value1, sigma_skew_value2);
    }
    if(choice3 == 1){
        alpha ~ normal(alpha_skew_value1, alpha_skew_value2);
    } else if (choice3 == 2){
        alpha ~ cauchy(alpha_skew_value1, alpha_skew_value2);
    } else if (choice3 == 3){
        alpha ~ gamma(alpha_skew_value1, alpha_skew_value2);
    }

    for(i in 1:n){
        target += skew_normal_lpdf(X[i] | xi, omega, alpha);
    }
}
'

stanSkew <- rstan::stan_model(model_code = stanmodelH2, model_name="H2", auto_write = TRUE)
saveRDS(stanSkew, file = "stanSkew.rds")
