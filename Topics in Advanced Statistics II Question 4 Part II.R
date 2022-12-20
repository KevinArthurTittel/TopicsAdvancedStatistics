# QUESTION 4 PART II ------------------------------------------------------------------

# Import the data
Q4 <- read.csv("~/Downloads/Q4-2.csv")
X <- Q4[,3:4]
x1 <- Q4[,3]
x2 <- Q4[,4]
y <- Q4[,2]

# Set the seed
set.seed(123)

# Question 4 Part II. (1)
  
  # Initialize the parameters
  H <- cbind(c(0.5, 0), c(0, 0.5))

  # Define the function
  K <- function(u) {
    return((1/sqrt(2*pi))*exp(-0.5*(u)^2))
  }
  
  # Estimate the value hat(f)_{0}(1,1)
  x <- c(1,1)
  f_0_11 <- 0
  X0 <- X[(y == 0),]
  for (i in 1:nrow(X0)) {
    u <- solve(H)%*%(t(x - X0[i,]))
    f_0_11 <- f_0_11 + (1/det(H))*K(u[1])*K(u[2])
  }
  f_0_11 <- f_0_11/nrow(X0)
  
  # Estimate the value hat(f)_{0}(1,1)
  x <- c(0,1)
  f_1_01 <- 0
  X1 <- X[(y == 1),]
  for (i in 1:nrow(X1)) {
    u <- solve(H)%*%t(x - X1[i,])
    f_1_01 <- f_1_01 + (1/det(H))*K(u[1])*K(u[2])
  }
  f_1_01 <- f_1_01/nrow(X1)
  
# Question 4 Part II. (2)

  # Predict
  predictions <- matrix(data = 0, nrow = length(x), ncol = 1)
  for (i in 1:nrow(X)) {
    f_0 <- 0
    f_1 <- 0
    for (j in 1:nrow(X0)) {
      u <- solve(H)%*%t(X[i,] - X0[j,])
      f_0 <- f_0 + (1/det(H))*K(u[1])*K(u[2])
    }
    f_0 <- f_0/nrow(X0)
    for (j in 1:nrow(X1)) {
      u <- solve(H)%*%t(X[i,] - X1[j,])
      f_1 <- f_1 + (1/det(H))*K(u[1])*K(u[2])
    }
    f_1 <- f_1/nrow(X1)
    if (f_1 > f_0) {
      predictions[i] <- 1
    } else {
      predictions[i] <- 0
    }
  }

  # Visualize the predictions
  colour <- c()
  colour[predictions == 1] = "red"
  colour[predictions == 0] = "blue"
  plot(x1, x2, pch=shape,cex=.8,col=colour, xlim=c(-4,4),ylim=c(-3,4), main="Plot of predictions for data set")

# Question 4 Part I. (4)
  amount_misclassified <- sum(y != predictions)










