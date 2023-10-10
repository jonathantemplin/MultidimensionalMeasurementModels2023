data {
  
  // data specifications  =============================================================
  int<lower=0> nObs;                            // number of observations
  int<lower=0> nItems;                          // number of items
  
  // input data  =============================================================
  array[nItems, nObs] real Y; // item responses in an array

  // loading specifications  =============================================================
  int<lower=1> nFactors;                                       // number of loadings in the model
  array[nItems, nFactors] int<lower=0, upper=1> Qmatrix;
  int nLoadings;

  // prior specifications =============================================================
  vector[nItems] meanMu;      // prior mean vector for intercepts
  matrix[nItems, nItems] covMu;  // prior covariance matrix for intercepts
  
  vector[nLoadings] meanLambda;         // prior mean vector for discrimination parameters
  matrix[nLoadings, nLoadings] covLambda;  // prior covariance matrix for discrimination parameters

  vector[nItems] meanPsi;
  vector[nItems] sdPsi;

  vector[nFactors] meanTheta;

  vector[nFactors] meanThetaSD; // prior mean for standard deviation for theta 
  vector[nFactors] sdThetaSD;   // prior mean for standard deviation for theta correlation matrix
  real thetaCovLKJ;           // LKJ prior for theta correlation matrix
}

transformed data{

  array[nFactors] int markerItems;
  array[nLoadings, 2] int loadingLocation;                     // the row/column positions of each loading

  // initialize all marker items to zero  
  for (factor in 1:nFactors){
    markerItems[factor] = 0;
  }

  int loadingNum = 1; // counter for number of loadings below
  for (item in 1:nItems){
    for (factor in 1:nFactors){
      if (Qmatrix[item, factor] == 1){
        if (markerItems[factor] > 0){
          loadingLocation[loadingNum, 1] = item;
          loadingLocation[loadingNum, 2] = factor;
          loadingNum = loadingNum + 1;
        } else {
          markerItems[factor] = item;
        }
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
  vector<lower=0>[nFactors] thetaSD;
}

transformed parameters{

  matrix[nItems, nFactors] lambdaMatrix = rep_matrix(0.0, nItems, nFactors);
  matrix[nObs, nFactors] thetaMatrix;
  
  // build matrix for lambdas to multiply theta matrix
  for (loading in 1:nLoadings){
    lambdaMatrix[loadingLocation[loading,1], loadingLocation[loading,2]] = lambda[loading];
  }

  for (factor in 1:nFactors){
    lambdaMatrix[markerItems[factor], factor] = 1.0;
  }
  
  for (factor in 1:nFactors){
    thetaMatrix[,factor] = to_vector(theta[,factor]);
  }
  
}

model {
  matrix[nFactors, nFactors] thetaCovL;

  // priors for item parameters
  lambda ~ multi_normal(meanLambda, covLambda); 
  mu ~ multi_normal(meanMu, covMu);
  

  // priors for structural model parameters
  for (factor in 1:nFactors){
    thetaSD ~ lognormal(meanThetaSD[factor], sdThetaSD[factor]);
  }
  thetaCorrL ~ lkj_corr_cholesky(thetaCovLKJ);
  thetaCovL = diag_pre_multiply(thetaSD, thetaCorrL);

  // priors for person parameters
  theta ~ multi_normal_cholesky(meanTheta, thetaCovL);    

  for (item in 1:nItems){
    psi[item] ~ lognormal(meanPsi[item], sdPsi[item]);
    Y[item] ~ normal(mu[item]+thetaMatrix*lambdaMatrix[item,1:nFactors]', psi[item]);
  }
  
  
}

generated quantities{ 
  
  // for calculating the correlation matrix of theta from the cholesky factorization
  corr_matrix[nFactors] thetaCorr;
  cov_matrix[nFactors] thetaCov;
  thetaCorr = multiply_lower_tri_self_transpose(thetaCorrL);
  thetaCov = quad_form_diag(thetaCorr, thetaSD);

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
