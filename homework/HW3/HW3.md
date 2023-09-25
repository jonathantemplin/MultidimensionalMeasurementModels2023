
---
title: "Homework #3 (Due October 10, 2023 by 11:59pm)"
author: "Multidimensional Measurement Models (Fall 2023)" 
format: "docx"
---

The objective of this homework is to give you hands-on practice on some of the technical aspects of psychometrics with respect to estimation.

1. Derive the sample log-likelihood function for tetrachoric correlations

2. Using the result from (1), implement this log-likelihood as a function in R that:
   1. Takes as input the tetrachoric correlation parameter (a single parameter) and a pair of observed binary variables
   2. Returns the log-likelihood value

> HINT/CONSTRAINT: You may not use any built-in functions in R that compute tetrachoric correlations nor may you use any other packages where tetrachoric correlations are estimated. Moreover, do not use any packages but one:  The one package you may use is the `pmvnorm()` function that computes integrals across a multivariate normal distribution from the `mvtnorm` package.

3. Using the result from (2), create a second function that uses the `optim()` function with default options to find the MLE for a tetrachoric correlation

4. Estimate a matrix of tetrachoric correlations across all pairs of observed variables for your in-class data set. Note: If any variables do not have any overlapping observations, you should not have an estimate for that pair of variables (and just use NA for the value). You may need to restrict the input into your function in (3) to determine if this is the case.

Submission Instructions: Upload three files:

1. A file showing the derivation of the log-likelihood function for tetrachoric correlations. This can be in any format, including a picture of your handwritten derivations.
2. Upload one R file (not markdown) that contains the two functions in steps 2 and 3 and also includes the code you used to estimate the tetrachoric correlations for your data set.
3. Upload the data set you used to estimate the tetrachoric correlations. This should be a CSV file.

I will grade this assignment by rerunning your syntax 
