# Topics in Advanced Statistics regarding Density Estimation

For the MSc Econometrics & Management Science, specializing in Business Analytics & Quantitative Marketing [BAQM], at the Erasmus School of Economics [ESE] in Rotterdam, I have written two assignments for the course "Topics in Advanced Statistics" awarded with a 9.8 on average, concerning various topics on density estimation.

# Replication Files

This repository contains the following files for the first assignment:
- Topics in Advanced Statistics I.pdf: Presents descriptions and results of the assignments concerning density estimation, optimal bandwidth selection for histogram plots, and bootstrapping on real and synthetic datasets.
- Topics in Advanced Statistics I Question 1.R: Uses bootstrapping on a small synthetic data set in producing a histrogram with a bandwidth that makes the plot informative, examining bias in pointwise estimates and standard errors.
- Topics in Advanced Statistics I Question 2.R: Uses parametric bootstrap, and a self-proposed improved way of bootstrapping, to estimate linear regression coefficients and standard errors, plotted in a histogram.
- Topics in Advanced Statistics I Question 4.R: Density estimation of an unknown distribution on a large dataset using the i) histogram pdf estimator and ii) Gaussian kernel density estimation using various bandwidths (e.g. Silverman's rule-of-thumb, or cross-validated, bandwidth).

This repository contains the following files for the second assignment:
- Topics in Advanced Statistics II.pdf: Presents descriptions and results of the assignments concerning various regression models to capture smoothness (e.g. local constant Nadaraya-Watson estimator, local linear regression, smoothing splines models, k-nearest neighbors models, quadratic/cubic/natural cubic spline models).
- Topics in Advanced Statistics II Question 3.R: Visualizes a categorical dataset, and fits a spline regression model (linear, cubic, smoothing splines) on the dataset.
- Topics in Advanced Statistics II Question 4 Part I.R: Visualizes dataset with binary outcome variable and numeric covariates coming from a multivariate normal distribution, fits density on the data using systematic approach, and predicts and assesses misclassification of data points.
- Topics in Advanced Statistics II Question 4 Part II.R: Continued from Part I, a nonparametric approach in terms of multivariate kernel density estimation is used to fit the data and assess fit.
