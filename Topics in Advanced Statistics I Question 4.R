# Topics in Advanced Statistics - Assignment 1 - Question 4

# Import data

data <- read.csv("~/Downloads/Q4.csv")

# QUESTION 4) A
n <- nrow(data)   # Number of data points
s <- sqrt(var(data[,2]))  # Sample standard deviation of data points
h_opt <- 3.5*s*(n^(-1/3))

# QUESTION 4) B
hist_lower <- min(data[,2]) - 0.1
hist_upper <- max(data[,2]) + 0.1

  # Take binwidth = h_opt
number_bins <- ceiling((hist_upper-hist_lower)/h_opt)
hist(data[,2],breaks = hist_lower + (0:number_bins)*h_opt)

  # Take binwidth = h_opt/2
h1 <- h_opt/2
number_bins <- ceiling((hist_upper-hist_lower)/h1)
hist(data[,2],breaks = hist_lower + (0:number_bins)*h1)

  # Take binwidth = h_opt*2
h2 <- h_opt*2
number_bins <- ceiling((hist_upper-hist_lower)/h2)
hist(data[,2],breaks = hist_lower + (0:number_bins)*h2)

# QUESTION 4) D
h_opt <- 1.06*s*(n^(-1/5))
plot(density(data[,2],bw=h_opt,kernel='gaussian'),main='Gaussian kernel', ylim=c(0,0.5),xlim=c(-4,4))
par(new=TRUE)
lines(density(data[,2]), lty=2)

# QUESTION 4) F
  # Program the CV function
CV <- function(h) {
  firstsum <- 0
  for (i in 1:n) {
    firstsum <- firstsum + sum(((1/(sqrt(2)*sqrt(2*pi)))*exp(-0.25*((data[i,2]-data[,2])/h)^2)))
  } 
    
  secondsum <- 0
  for (i in 1:n) {
    secondsum <- secondsum + sum(((1/sqrt(2*pi))*(1/h)*exp(-0.5*((data[i,2]-data[-i,2])/h)^2)))
  }
  return((1/(h*(n^2)))*firstsum - (2/(n*(n-1)))*secondsum)
}

h_list <- seq(0.01,0.11,by=0.005)
CV_list <- c()
for (h in h_list) {
  CV_list <- c(CV_list, CV(h))
}
plot(h_list, CV_list, xlab="h", ylab="CV(h)", main="Cross-validation scores for different bandwidths")

h_cv<- h_list[which.min(CV_list)] 
plot(density(data[,2],bw=h_cv,kernel='gaussian'),main='kernel density estimate')


