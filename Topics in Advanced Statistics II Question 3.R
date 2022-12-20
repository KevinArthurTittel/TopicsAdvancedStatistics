# QUESTION 3 ------------------------------------------------------------------

# Import the data
Q3 <- read.csv("~/Downloads/Q3.csv")
x0 <- Q3[,3]
g0 <- Q3[,4]
y0 <- Q3[,2]

# Set the seed
set.seed(123)

# Import packages
install.packages("splines")

# Question 3 (1)
  
  # Visualize the dataset
  shape <- c()
  shape[g0 == 0] = 1
  shape[g0 == 1] = 4
  plot(x0, y0,cex=.8,col="black", pch=shape, xlim=c(0,10),ylim=c(-11,30), main="Visualization of data set Q3")

# Question 3 (2)

  # Retrieve data points
  x0_subset <- x0[g0 == 0]
  y0_subset <- y0[g0 == 0]
  
  # Initialize the parameters
  n <- length(x0_subset)
  
  # Basis functions of the linear spline
  h1 <- rep(1,n)
  h2 <- x0_subset
  h3 <- x0_subset - 5
  h3[h3 < 0] <- 0
  h4 <- x0_subset - 7
  h4[h4 < 0] <- 0
  dataLinearSpline <- data.frame(y0_subset, h1, h2, h3, h4)
  
  # Estimate coefficients
  fitLinearSpline <- lm(y0_subset ~ 0 + h1 + h2 + h3 + h4, dataLinearSpline)
  betaLinearSpline <- coef(fitLinearSpline)
  
  # Plot of the fitted regression line overlapped with the observed data points
  # and indicate where the knots are (at x = 5 and x = 7)
  plot(x0_subset, y0_subset, pch=16, cex=.8, col='gray',main="Linear spline")
  abline(v=5, lty=2, col='red')
  abline(v=7, lty=2, col='red')
  x <- x0_subset
  curve(betaLinearSpline[1] + betaLinearSpline[2]*x, from = min(x), to = 5, col='red',add=T,lwd=2)
  curve(betaLinearSpline[1] + betaLinearSpline[2]*x + betaLinearSpline[3]*(x - 5), from = 5, to = 7, col='red',add=T,lwd=2)
  curve(betaLinearSpline[1] + betaLinearSpline[2]*x + betaLinearSpline[3]*(x - 5) + betaLinearSpline[4]*(x - 7),from = 7, to = max(x), col='red',add=T,lwd=2)
  
# Question 3 (3)

  # Retrieve data points
  x0_subset <- x0[g0 == 0]
  y0_subset <- y0[g0 == 0]
  
  # Initialize the parameters
  n <- length(x0_subset)
  
  # Basis functions of the cubic spline
  h1 <- rep(1,n)
  h2 <- x0_subset
  h3 <- x0_subset^2
  h4 <- x0_subset^3
  h5 <- (x0_subset - 5)^3
  h5[x0_subset < 5] <- 0
  dataCubicSpline <- data.frame(y0_subset, h1, h2, h3, h4, h5)
  
  # Estimate coefficients
  fitCubicSpline <- lm(y0_subset ~ 0 + h1 + h2 + h3 + h4 + h5, dataCubicSpline)
  betaCubicSpline <- coef(fitCubicSpline)
  
  # Plot of the fitted regression line overlapped with the observed data points
  # and indicate where the knots are (at x = 5 and x = 7)
  plot(x0_subset, y0_subset, pch=16, cex=.8, col='gray', main="Cubic spline")
  abline(v=5, lty=2, col='red')
  x <- x0_subset
  curve(betaCubicSpline[1] + betaCubicSpline[2]*x + betaCubicSpline[3]*x^2 + betaCubicSpline[4]*x^3, from = min(x), to = 5, col='red',add=T,lwd=2)
  curve(betaCubicSpline[1] + betaCubicSpline[2]*x + betaCubicSpline[3]*x^2 + betaCubicSpline[4]*x^3 + betaCubicSpline[5]*(x-5)^3, from = 5, to = max(x), col='red',add=T,lwd=2)
  
# Question 3 (4)

  # Retrieve data points
  x0_subset <- x0[g0 == 0]
  y0_subset <- y0[g0 == 0]
  
  # Fit the three smoothing splines with varying lambda parameter
  fit1 <- smooth.spline(x0_subset, y0_subset, lambda = 10^(-6))
  fit2 <- smooth.spline(x0_subset, y0_subset, lambda = 10^(-4))
  fit3 <- smooth.spline(x0_subset, y0_subset, lambda = 10^(-2))
  
  # Plot the three smoothing splines with varying lambda parameter
  plot(x0_subset,y0_subset,pch=16,cex=.8,col='gray',main='Smoothing spline, lambda=10e-6')
  lines(fit1, col='red', lwd=2)
  
  plot(x0_subset,y0_subset,pch=16,cex=.8,col='gray',main='Smoothing spline, lambda=10e-4')
  lines(fit2, col='red', lwd=2)
  
  plot(x0_subset,y0_subset,pch=16,cex=.8,col='gray',main='Smoothing spline, lambda=10e-2')
  lines(fit3, col='red', lwd=2)
  
# Question 3 (5)
  
  # Data points
  x0 <- Q3[,3]
  g0 <- Q3[,4]
  y0 <- Q3[,2]
  # Initialize the parameters
  n <- length(x0)
  
  # Basis functions of the cubic spline
  h1 <- rep(1,n)
  h2 <- x0
  h3 <- x0^2
  h4 <- x0^3
  h5 <- ((x0 - 5)^3)*(1-g0)
  h5[x0 < 5] <- 0
  h6 <- ((x0 - 5)^3)*g0
  h6[x0 < 5] <- 0
  dataCubicSplineQ3_5 <- data.frame(y0, h1, h2, h3, h4, h5, h6)
  
  # Estimate coefficients
  fitCubicSplinef0 <- lm(y0 ~ 0 + h1 + h2 + h3 + h4 + h5 + h6, dataCubicSplineQ3_5)
  betaCubicSplinef0 <- coef(fitCubicSplinef0)
  
  # Plot
  plot(x0, y0, pch=16, cex=.8, col='gray', main="Cubic spline")
  abline(v=5, lty=2, col='red')
  x <- x0
  curve(betaCubicSplinef0[1] + betaCubicSplinef0[2]*x + betaCubicSplinef0[3]*x^2 + betaCubicSplinef0[4]*x^3, from = min(x), to = 5, col='red',add=T,lwd=2)
  curve(betaCubicSplinef0[1] + betaCubicSplinef0[2]*x + betaCubicSplinef0[3]*x^2 + betaCubicSplinef0[4]*x^3 + betaCubicSplinef0[5]*(x-5)^3, from = 5, to = max(x), col='red',add=T,lwd=2)
  curve(betaCubicSplinef0[1] + betaCubicSplinef0[2]*x + betaCubicSplinef0[3]*x^2 + betaCubicSplinef0[4]*x^3 + betaCubicSplinef0[6]*(x-5)^3, from = 5, to = max(x), col='blue',add=T,lwd=2)
  
  
  