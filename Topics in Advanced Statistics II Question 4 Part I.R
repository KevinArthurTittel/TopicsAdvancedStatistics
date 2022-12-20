# QUESTION 4 PART I ------------------------------------------------------------------

# Import the data
Q4 <- read.csv("~/Downloads/Q4-2.csv")
x1 <- Q4[,3]
x2 <- Q4[,4]
x <- Q4[,3:4]
y <- Q4[,2]

# Set the seed
set.seed(123)

# Question 4 Part I. (1)

  # Visualize the dataset
  shape <- c()
  shape[y == 0] = 1
  shape[y == 1] = 4
  plot(x1, x2, pch=shape,cex=.8,col="black", xlim=c(-4,4),ylim=c(-3,4), main="Plot of data set")

# Question 4 Part I. (2)
  
  # Estimate parameters mu_0 and Sigma_0 for all observations such that y == 0
  X0 <- x[(y == 0),]
  n <- nrow(X0)
  nll0 <- function(mu1, mu2, sigma11, sigma12, sigma22) {
    mu <- cbind(mu1, mu2)
    Sigma <- as.matrix(cbind(c(sigma11, sigma12), c(sigma12, sigma22)))
    prob <- 0
    for (i in 1:n) {
      prob <- prob - log(2*pi) - 0.5*log(det(Sigma)) - 0.5*((as.matrix(X0[i,]-mu)) %*% solve(Sigma) %*% (t(as.matrix(X0[i,] - mu))))
    }
    return(-1*prob)
  }
  MLEfit <- mle(minuslog=nll0, start = list(mu1=0, mu2=0, sigma11=0.5, sigma12=0.1, sigma22=0.5), lower = c(-Inf, -Inf, 0, -Inf, 0), method = "L-BFGS-B")
  mu0 <- c(coef(MLEfit)[1], coef(MLEfit)[2]) 
  Sigma0 <- cbind(c(coef(MLEfit)[3], coef(MLEfit)[4]), c(coef(MLEfit)[4], coef(MLEfit)[5]))
  
  # Estimate parameters mu_1 and Sigma_1 for all observations such that y == 1
  X1 <- x[y==1]
  n <- nrow(X1)
  nll0 <- function(mu1, mu2, sigma11, sigma12, sigma22) {
    mu <- cbind(mu1, mu2)
    Sigma <- as.matrix(cbind(c(sigma11, sigma12), c(sigma12, sigma22)))
    prob <- 0
    for (i in 1:n) {
      prob <- prob - log(2*pi) - 0.5*log(det(Sigma)) - 0.5*((as.matrix(X1[i,]-mu)) %*% solve(Sigma) %*% (t(as.matrix(X1[i,] - mu))))
    }
    return(-1*prob)
  }
  MLEfit <- mle(minuslog=nll0, start = list(mu1=0, mu2=0, sigma11=0.5, sigma12=0.1, sigma22=0.5), lower = c(-Inf, -Inf, 0, -Inf, 0), method = "L-BFGS-B")
  mu1 <- c(coef(MLEfit)[1], coef(MLEfit)[2]) 
  Sigma1 <- cbind(c(coef(MLEfit)[3], coef(MLEfit)[4]), c(coef(MLEfit)[4], coef(MLEfit)[5]))
  
# Question 4 Part I. (3)
  
  # Define normal density
  density <- function(x, mu, Sigma) {
    return(((1/(2*pi))^(0.5))*((det(Sigma))^(0.5))*(exp(-0.5*((as.matrix(x - mu))) %*% solve(as.matrix(Sigma)) %*% (t(as.matrix(x - mu))))))
  }
  
  # Predict
  predictions <- matrix(data = 0, nrow = length(x), ncol = 1)
  for (i in 1:nrow(x)) {
    if (density(x[i,],mu1,Sigma1) > density(x[i,],mu0,Sigma0)) {
      predictions[i] <- 1
    } else {
      predictions[i] <- 0
    }
  }
  
  # Visualize the predictions
  colour <- c()
  colour[predictions == 1] = "red"
  colour[predictions == 0] = "blue"
  plot(x1, x2, pch=shape,cex=.8,col=colour, xlim=c(-4,4),ylim=c(-3,4),main="Plot of predictions for data set")
  
# Question 4 Part I. (4)
  amount_misclassified <- sum(y != predictions)
  
  
  
  
  
  
  
  
  
  
