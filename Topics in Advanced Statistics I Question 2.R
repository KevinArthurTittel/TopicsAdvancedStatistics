# Topics in Advanced Statistics - Assignment 1 - Question 2

Q2 <- read.csv("~/Downloads/Q2.csv")

x_Q2 = Q2[,2]
y_Q2 = Q2[,3]


# Question 2.1
ols <- lm(y_Q2~1+x_Q2)
beta <- coef(ols)
beta_hat0 <- beta[1]
beta_hat1 <- beta[2]

se_beta <- summary(ols)$coefficients[,2]
se_beta_hat0 <- se_beta[1]
se_beta_hat1 <- se_beta[2]

res <- residuals(ols)

# Question 2.2
set.seed(123)
epsilon_star <- sample(res, size=500, replace=T)
x_parboot <- x_Q2
y_parboot <- rep(NA, 500)
for (i in 1:500){
  y_parboot[i] <- beta_hat0 + beta_hat1*x_parboot[i] + epsilon_star[i]
}

# Question 2.3
ols_parboot <- lm(y_parboot~1+x_parboot)
beta_parboot <- coef(ols_parboot)
beta_hat0_parboot <- beta_parboot[1]
beta_hat1_parboot <- beta_parboot[2]

se_beta_parboot <- summary(ols_parboot)$coefficients[,2]
se_beta_hat0_parboot <- se_beta_parboot[1]
se_beta_hat1_parboot <- se_beta_parboot[2]

# Question 2.4
set.seed(123)
y_parboot_Q2.4 <- matrix(NA, nrow = 500, ncol = 100)
x_parboot_Q2.4 <- matrix(NA, nrow = 500, ncol = 100)
ols_parboot_Q2.4 <- rep(NA, 100)
beta0_parboot_Q2.4 <- rep(NA, 100)
beta1_parboot_Q2.4 <- rep(NA, 100)

for (j in 1:100){
  epsilon_star <- sample(res,size=500,replace=T)
  x_parboot_Q2.4 <- x_Q2
  for (i in 1:500){
    y_parboot_Q2.4[i,j] <- beta_hat0 + beta_hat1*x_parboot_Q2.4[i] + epsilon_star[i]
  }
ols_parboot_Q2.4 <- lm(y_parboot_Q2.4[,j]~1+x_parboot_Q2.4)
beta_parboot_Q2.4 <- coef(ols_parboot_Q2.4)
beta0_parboot_Q2.4[j] <- beta_parboot_Q2.4[1]
beta1_parboot_Q2.4[j] <- beta_parboot_Q2.4[2]
}

se_beta0_hat_Q2.4 <- sd(beta0_parboot_Q2.4)
se_beta1_hat_Q2.4 <- sd(beta1_parboot_Q2.4)

# Question 2.5
set.seed(123)
se_beta0_hat_Q2.4 <- rep(NA, 100)
se_beta1_hat_Q2.4 <- rep(NA, 100)

for (k in 1:100){
y_parboot_Q2.4 <- matrix(NA, nrow = 500, ncol = 100)
x_parboot_Q2.4 <- matrix(NA, nrow = 500, ncol = 100)
ols_parboot_Q2.4 <- rep(NA, 100)
beta0_parboot_Q2.4 <- rep(NA, 100)
beta1_parboot_Q2.4 <- rep(NA, 100)

for (j in 1:100){
  epsilon_star <- sample(res,size=500,replace=T)
  x_parboot_Q2.4 <- x_Q2
  for (i in 1:500){
    y_parboot_Q2.4[i,j] <- beta_hat0 + beta_hat1*x_parboot_Q2.4[i] + epsilon_star[i]
  }
  ols_parboot_Q2.4 <- lm(y_parboot_Q2.4[,j]~1+x_parboot_Q2.4)
  beta_parboot_Q2.4 <- coef(ols_parboot_Q2.4)
  beta0_parboot_Q2.4[j] <- beta_parboot_Q2.4[1]
  beta1_parboot_Q2.4[j] <- beta_parboot_Q2.4[2]
}

se_beta0_hat_Q2.4[k] <- sd(beta0_parboot_Q2.4)
se_beta1_hat_Q2.4[k] <- sd(beta1_parboot_Q2.4)
}

hist(se_beta0_hat_Q2.4)
hist(se_beta1_hat_Q2.4)


