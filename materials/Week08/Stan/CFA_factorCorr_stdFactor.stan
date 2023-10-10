data {
  
  // data specifications  =============================================================
  int<lower=0> nObs;                            // number of observations
  int<lower=0> nItems;                          // number of items
  
  // input data  =============================================================
  array[nItems, nObs] real Y; // item responses in an array

  // loading specifications  =============================================================
  int<lower=1> nFactors;                                       // number of loadings in the model
  array[nItems, nFactors] int<lower=0, upper=1> Qmatrix;
  
  // prior specifications =============================================================
  vector[nItems] meanMu;      // prior mean vector for intercepts
  matrix[nItems, nItems] covMu;  // prior covariance matrix for intercepts
  
  vector[nItems] meanLambda;         // prior mean vector for discrimination parameters
  matrix[nItems, nItems] covLambda;  // prior covariance matrix for discrimination parameters

  vector[nItems] meanPsi;
  vector[nItems] sdPsi;

  vector[nFactors] meanTheta;

  real thetaCovLKJ; // LKJ prior for theta correlation matrix
}

transformed data{
  int<lower=0> nLoadings = 0;                                      // number of loadings in model
  
  for (factor in 1:nFactors){
    nLoadings = nLoadings + sum(Qmatrix[1:nItems, factor]);
  }

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
  array[nObs] vector[nFactors] theta;  
  vector[nItems] mu; 
  vector<lower=0>[nItems] psi; 
  vector[nLoadings] lambda;            
  cholesky_factor_corr[nFactors] thetaCorrL;
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
  thetaCorrL ~ lkj_corr_cholesky(thetaCovLKJ);
  theta ~ multi_normal_cholesky(meanTheta, thetaCorrL);    

  for (item in 1:nItems){
    psi[item] ~ lognormal(meanPsi[item], sdPsi[item]);
    Y[item] ~ normal(mu[item]+thetaMatrix*lambdaMatrix[item,1:nFactors]', psi[item]);
  }
  
  
}

generated quantities{ 
  
  // for calculating the correlation matrix of theta from the cholesky factorization
  corr_matrix[nFactors] thetaCorr;
  thetaCorr = multiply_lower_tri_self_transpose(thetaCorrL);

  // for model fit
  // for PPMC:
  array[nItems, nObs] real simY;
  
  // for LOO/WAIC:
  vector[nObs] personLike = rep_vector(0.0, nObs);
  
  for (item in 1:nItems){
    for (obs in 1:nObs){
      // generate data based on distribution and model
      simY[item, obs] = normal_rng(mu[item]+thetaMatrix[obs,]*lambdaMatrix[item,1:nFactors]', psi[item]);
      
      // calculate conditional data likelihood for LOO/WAIC
      personLike[obs] = personLike[obs] + 
        normal_lpdf(Y[item, obs] | mu[item]+thetaMatrix[obs,]*lambdaMatrix[item,1:nFactors]', psi[item]);
    }
  }  
  
}
