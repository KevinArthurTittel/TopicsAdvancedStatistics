# Topics in Advanced Statistics - Assignment 1 - Question 1

Q1 <- read.csv("~/Downloads/Q1.csv")

x_Q1 = Q1[,2]
y_Q1 = Q1[,3]

# Question 1.2
set.seed(123)
x_Q1.2_bootstrap <- matrix(NA, nrow = 5, ncol = 10)
for (i in 1:5){
  x_Q1.2_bootstrap[i,] <- sample(x_Q1,replace=T)
}
print(x_Q1.2_bootstrap)

# Question 1.4a
set.seed(123) 
B <- 200

theta_hat_star <- rep(NA,B)
for (b in 1:B){
  x_star <- sample(x_Q1,replace=T)
  y_star <- sample(y_Q1,replace=T)
  theta_hat_star[b] <- mean(x_star)/mean(y_star)
}

# Question 1.4b
hist(theta_hat_star)

# Question 1.4c
bias_theta_hat_Q1.4 <- mean(theta_hat_star) - (mean(x_Q1)/mean(y_Q1))
st_error_theta_hat_Q1.4 <- sd(theta_hat_star)


# Question 1.5
set.seed(123)
h <- 100
bias_theta_hat_Q1.5 <- rep(NA,h)
st_error_theta_hat_Q1.5 <- rep(NA,h)

for (i in 1:h){
  x_Q1.5_bootstrap <- sample(x_Q1,replace=T)
  y_Q1.5_bootstrap <- sample(y_Q1,replace=T)
  
  B <- 200
  
  theta_hat_star <- rep(NA,B)
  for (b in 1:B){
    x_star <- sample(x_Q1,replace=T)
    y_star <- sample(y_Q1,replace=T)
    theta_hat_star[b] <- mean(x_star)/mean(y_star)
  }

  bias_theta_hat_Q1.5[i] <- mean(theta_hat_star) - (mean(x_Q1)/mean(y_Q1))
  st_error_theta_hat_Q1.5[i] <- sd(theta_hat_star)
}

#Question1.5a
hist(bias_theta_hat_Q1.5)
hist(st_error_theta_hat_Q1.5)

mean(bias_theta_hat_Q1.5)
mean(st_error_theta_hat_Q1.5)






