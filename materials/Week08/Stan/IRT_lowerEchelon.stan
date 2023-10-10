data {
  
  // data specifications  =============================================================
  int<lower=0> nObs;                            // number of observations
  int<lower=0> nItems;                          // number of items
  
  // input data  =============================================================
  array[nItems, nObs] int Y; // item responses in an array

  // loading specifications  =============================================================
  int<lower=1> nFactors;                                       // number of loadings in the model
  array[nItems, nFactors] int<lower=0, upper=1> Qmatrix;
  int nLoadings;
  
  // prior specifications =============================================================
  vector[nItems] meanMu;      // prior mean vector for intercepts
  matrix[nItems, nItems] covMu;  // prior covariance matrix for intercepts
  
  vector[nLoadings] meanLambda;         // prior mean vector for discrimination parameters
  matrix[nLoadings, nLoadings] covLambda;  // prior covariance matrix for discrimination parameters

  vector[nFactors] meanTheta;

}

transformed data{
  
  array[nLoadings, 2] int loadingLocation;                     // the row/column positions of each loading
  int loadingNum=1;
  
  for (item in 1:nItems){
    for (factor in 1:nFactors){
      if (Qmatrix[item, factor] == 1){
        loadingLocation[loadingNum, 1] = item;
        loadingLocation[loadingNum, 2] = factor;
        loadingNum = loadingNum + 1;
      }
    }
  }

}

parameters {
  vector[nLoadings] lambda;
  array[nObs] vector[nFactors] theta;  
  vector[nItems] mu; 
}

transformed parameters{
  matrix[nItems, nFactors] lambdaMatrix = rep_matrix(0.0, nItems, nFactors);
  matrix[nObs, nFactors] thetaMatrix;
  
  // build matrix for lambdas to multiply theta matrix
  for (loading in 1:nLoadings){
    lambdaMatrix[loadingLocation[loading,1], loadingLocation[loading,2]] = lambda[loading];
  }
  
  for (factor in 1:nFactors){
    thetaMatrix[,factor] = to_vector(theta[,factor]);
  }
  
}

model {
  
  lambda ~ multi_normal(meanLambda, covLambda); 
  mu ~ multi_normal(meanMu, covMu);

  for (factor in 1:nFactors){
    theta[,factor] ~ std_normal();
  }
  
  for (item in 1:nItems){
    Y[item] ~ bernoulli_logit(mu[item]+thetaMatrix*lambdaMatrix[item,1:nFactors]');
  }
  
  
}

generated quantities{ 
  
  // for model fit
  // for PPMC:
  array[nItems, nObs] int simY;
  
  // for LOO/WAIC:
  vector[nObs] personLike = rep_vector(0.0, nObs);
  
  for (item in 1:nItems){
    for (obs in 1:nObs){
      // generate data based on distribution and model
      simY[item, obs] = bernoulli_logit_rng(mu[item]+thetaMatrix[obs,]*lambdaMatrix[item,1:nFactors]');
      
      // calculate conditional data likelihood for LOO/WAIC
      personLike[obs] = personLike[obs] + 
        bernoulli_logit_lpmf(Y[item, obs] | mu[item]+thetaMatrix[obs,]*lambdaMatrix[item,1:nFactors]');
    }
  }  
  
}
