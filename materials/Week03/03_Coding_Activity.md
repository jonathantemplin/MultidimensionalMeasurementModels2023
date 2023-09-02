# Class Coding Activity

## Simulation and Estimation of a binary IRT model

In this activity, we will simulate data from a binary IRT model and estimate the model using the EM algorithm. We will then compare the estimates to the true values.

### Simulate data

We will simulate data from a binary IRT model with 10 items and 1000 examinees. 

* Using the slope/intercept parameterization:
  * Generate slope parameters from a lognormal distribution with mean 0 and standard deviation 1
  * Generate intercept parameters  from a normal distribution with mean 0 and standard deviation 1
  * Generate no $c$ or $d$ parameters (or all $c_i=0$ and $d_i=1$)

* Generate examinee ability parameters from a bivariate normal distribution with:
  * Both means set to 0 
  * Both variances set to 1
  * Correlation set to 0.75
  
### Estimate the model

Use the R package `mirt` to estimate three models using the EM algorithm:

1. A unidimensional 2PL model
2. A unidimensional 3PL model (with a lower asymptote parameter)
3. The two-dimensional model we just estimated

### Compare the estimates to the true values

Compare the estimated EAP theta values to the following:

1. The true theta values from each dimension
2. The sum of true theta values from each dimension


