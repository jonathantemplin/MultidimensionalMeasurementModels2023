# Clear workspace =====================================================================================================
rm(list = ls())

# Load Packages =======================================================================================================

# note: cmdstanr needs installation using the following code:
if (!require(cmdstanr)){
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  check_cmdstan_toolchain()
  install_cmdstan(cores = 2)
}

library(cmdstanr)

if (!require(loo)){
  install.packages("loo")
}

library(loo)

# Set random number seed for consistency in simluation-based methods ==================================================

analysisSeed = 21092023

# Generate Simulated Data =============================================================================================

nItems = 10
nExaminees = 1000
nFactors = 2

Qmatrix = matrix(data = 0, nrow = nItems, ncol = 2)
colnames(Qmatrix) = paste0("theta", 1:nFactors)
rownames(Qmatrix) = paste0("item", 1:nItems)

Qmatrix[1:5,1] = 1
Qmatrix[6:10,2] = 1

Qmatrix 

# generate item intercepts
mu = rnorm(n = nItems, mean = 0, sd = 1)

# generate item slopes
lambda = rlnorm(n = nItems, mean = 0, sd = 1)

# generate item residual variances (for CFA)
psi2 = rlnorm(n = nItems, mean = 0, sd = 1) 

# generate thetas
thetaCorr = diag(nFactors)
thetaCorr[1,2] = thetaCorr[2,1] = .75

thetaCorrU = chol(thetaCorr)
t(thetaCorrU) %*% thetaCorrU

# uncorrelated univariate normal
thetaZ = matrix(data = 0, nrow = nExaminees, ncol = nFactors)
for (factor in 1:nFactors){
    thetaZ[,factor] = rnorm(n = nExaminees, mean = 0, sd = 1)
}
cor(thetaZ)

theta = thetaZ %*% thetaCorrU
cor(theta)

# generate data 
dataMatBinary = as.data.frame(matrix(data = NA, nrow = nExaminees, ncol = nItems))
dataMatContinuous = as.data.frame(matrix(data = NA, nrow = nExaminees, ncol = nItems))
names(dataMatBinary) = paste0("item", 1:nItems)
names(dataMatContinuous) = paste0("item", 1:nItems)
for (item in 1:nItems){
    logit = mu[item] + Qmatrix[item,1] * lambda[item] * theta[,1] + Qmatrix[item,2] * lambda[item] * theta[,2]
    prob = exp(logit) / (1 + exp(logit))
    dataMatBinary[,item] = rbinom(n = nExaminees, size = 1, prob = prob)
    dataMatContinuous[,item] = rnorm(n = nExaminees, mean = logit, sd = sqrt(psi2[item]))
}

# Compile Stan models =================================================================================================

# CFA model with standardized factors and factor correlations
modelCFA_stdFactors = cmdstan_model(stan_file = "Stan/CFA_factorCorr_stdFactor.stan")

# CFA with marker items and factor covariances
modelCFA_markerItems = cmdstan_model(stan_file = "Stan/CFA_factorCorr_markerLoadings.stan")

# CFA model with independent factors (for lower-echelon Q-matrices)
modelCFA_lowerEchelon = cmdstan_model(stan_file = "Stan/CFA_lowerEchelon.stan")

# IRT model with standardized factors and factor correlations
modelIRT_stdFactors = cmdstan_model(stan_file = "Stan/IRT_factorCorr_stdFactor.stan")

# IRT model with marker items and factor covariances
modelIRT_markerItems = cmdstan_model(stan_file = "Stan/IRT_factorCorr_markerLoadings.stan")

# IRT model with independent factors (for lower-echelon Q-matrices)
modelIRT_lowerEchelon = cmdstan_model(stan_file = "Stan/IRT_lowerEchelon.stan")

# CFA Models in Stan =================================================================================================

# Model 1: Exploratory 2PL CFA model with Lower-Echelon Q-matrix three dimensions
Qmatrix_lowerEchelon3D = matrix(data = 0, nrow = nItems, ncol = 3)
Qmatrix_lowerEchelon3D[,1] = 1
Qmatrix_lowerEchelon3D[2:nItems,2] = 1
Qmatrix_lowerEchelon3D[3:nItems,3] = 1

nLoadings = sum(apply(X = Qmatrix_lowerEchelon3D, MARGIN = 2, FUN = sum))

modelCFA_lowerEchelon3D_data = list(
  nObs = nrow(dataMatContinuous),
  nItems = ncol(dataMatContinuous),
  Y = t(dataMatContinuous),
  nFactors = ncol(Qmatrix_lowerEchelon3D),
  Qmatrix = Qmatrix_lowerEchelon3D,
  nLoadings = nLoadings,
  meanMu = rep(0, ncol(dataMatContinuous)),
  covMu = 1 * diag(ncol(dataMatContinuous)),
  meanLambda = rep(0, nLoadings),
  covLambda = .5 * diag(nLoadings),
  meanPsi = rep(0, ncol(dataMatContinuous)),
  sdPsi = rep(.5, ncol(dataMatContinuous)), 
  meanTheta = rep(0, ncol(Qmatrix_lowerEchelon3D))
)

# for starting theta in the correct direction:
# create sum scores for each latent variable
sumScores = as.matrix(dataMatContinuous) %*% Qmatrix_lowerEchelon3D

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelCFA_lowerEchelon3D_samples = modelCFA_lowerEchelon$sample(
  data = modelCFA_lowerEchelon3D_data,
  seed = 100920231,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(
    lambda = rnorm(sum(apply(X = Qmatrix_lowerEchelon3D, MARGIN = 2, FUN = sum)), mean = 5, sd = 1), 
    theta = sumScores
    )
)

# save model object
modelCFA_lowerEchelon3D_samples$save_object(file = "modelCFA_lowerEchelon3D_samples.RDS")

# checking convergence
max(modelCFA_lowerEchelon3D_samples$summary(variables = c("lambda", "mu", "psi", "theta"))$rhat, na.rm = TRUE)

# model parameter results
print(modelCFA_lowerEchelon3D_samples$summary(variables = c("lambda", "mu", "psi")) ,n = Inf)


# Model 2: Exploratory 2PL CFA model with Lower-Echelon Q-matrix two dimensions
Qmatrix_lowerEchelon2D = matrix(data = 0, nrow = nItems, ncol = 2)
Qmatrix_lowerEchelon2D[,1] = 1
Qmatrix_lowerEchelon2D[2:nItems,2] = 1

nLoadings = sum(apply(X = Qmatrix_lowerEchelon2D, MARGIN = 2, FUN = sum))

modelCFA_lowerEchelon2D_data = list(
  nObs = nrow(dataMatContinuous),
  nItems = ncol(dataMatContinuous),
  Y = t(dataMatContinuous),
  nFactors = ncol(Qmatrix_lowerEchelon2D),
  Qmatrix = Qmatrix_lowerEchelon2D,
  nLoadings = nLoadings,
  meanMu = rep(0, ncol(dataMatContinuous)),
  covMu = .5 * diag(ncol(dataMatContinuous)),
  meanLambda = rep(0, nLoadings),
  covLambda = .5 * diag(nLoadings),
  meanPsi = rep(0, ncol(dataMatContinuous)),
  sdPsi = rep(.5, ncol(dataMatContinuous)), 
  meanTheta = rep(0, ncol(Qmatrix_lowerEchelon2D))
)

# for starting theta in the correct direction:
# create sum scores for each latent variable
sumScores = as.matrix(dataMatContinuous) %*% Qmatrix_lowerEchelon2D

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelCFA_lowerEchelon2D_samples = modelCFA_lowerEchelon$sample(
  data = modelCFA_lowerEchelon2D_data,
  seed = 100920232,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(
    lambda = rnorm(sum(apply(X = Qmatrix_lowerEchelon2D, MARGIN = 2, FUN = sum)), mean = 5, sd = 1), 
    theta = sumScores
    )
)

# save model object
modelCFA_lowerEchelon2D_samples$save_object(file = "modelCFA_lowerEchelon2D_samples.RDS")

# checking convergence
max(modelCFA_lowerEchelon2D_samples$summary(variables = c("lambda", "mu", "psi", "theta"))$rhat, na.rm = TRUE)

# model parameter results
print(modelCFA_lowerEchelon2D_samples$summary(variables = c("lambda", "mu", "psi")) ,n = Inf)

# comparing model fit with previous models
loo_compare(
  list(
    model3D = modelCFA_lowerEchelon3D_samples$loo(variables = "personLike"),
    model2D = modelCFA_lowerEchelon2D_samples$loo(variables = "personLike")
  )
)

# Model 3: CFA model with correct Q-matrix, standardized factors, and factor correlations

modelCFA_stdFactors_data = list(
  nObs = nrow(dataMatContinuous),
  nItems = ncol(dataMatContinuous),
  Y = t(dataMatContinuous),
  nFactors = ncol(Qmatrix),
  Qmatrix = Qmatrix,
  meanMu = rep(0, ncol(dataMatContinuous)),
  covMu = 1 * diag(ncol(dataMatContinuous)),
  meanLambda = rep(0, ncol(dataMatContinuous)),
  covLambda = 1* diag(ncol(dataMatContinuous)),
  meanPsi = rep(0, ncol(dataMatContinuous)),
  sdPsi = rep(1, ncol(dataMatContinuous)), 
  meanTheta = rep(0, ncol(Qmatrix)),
  thetaCovLKJ = 1
)

# for starting theta in the correct direction:
# create sum scores for each latent variable
sumScores = as.matrix(dataMatContinuous) %*% Qmatrix

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelCFA_stdFactors_samples = modelCFA_stdFactors$sample(
  data = modelCFA_stdFactors_data,
  seed = 100920233,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(lambda = rnorm(nItems, mean = 5, sd = 1), theta = sumScores)
)

# save model object
modelCFA_stdFactors_samples$save_object(file = "modelCFA_stdFactors_samples.RDS")

# checking convergence
max(modelCFA_stdFactors_samples$summary(variables = c("lambda", "mu", "psi", "thetaCorr", "theta"))$rhat, na.rm = TRUE)

# model parameter results
print(modelCFA_stdFactors_samples$summary(variables = c("lambda", "mu", "psi", "thetaCorr")) ,n = Inf)

# comparing model fit with previous models
loo_compare(
  list(
    model3D = modelCFA_lowerEchelon3D_samples$loo(variables = "personLike"),
    model2D = modelCFA_lowerEchelon2D_samples$loo(variables = "personLike"),
    modelCorrect2D = modelCFA_stdFactors_samples$loo(variables = "personLike")
  )
)

# Model 4: CFA model with correct Q-matrix, marker items, and factor covariances
modelCFA_makerItems_data = list(
  nObs = nrow(dataMatContinuous),
  nItems = ncol(dataMatContinuous),
  Y = t(dataMatContinuous),
  nFactors = ncol(Qmatrix),
  Qmatrix = Qmatrix,
  nLoadings = sum(apply(X = Qmatrix, MARGIN = 2, FUN = sum)) - ncol(Qmatrix),
  meanMu = rep(0, ncol(dataMatContinuous)),
  covMu = 1 * diag(ncol(dataMatContinuous)),
  meanLambda = rep(0, ncol(dataMatContinuous) - ncol(Qmatrix)),
  covLambda = 1 * diag(ncol(dataMatContinuous) - ncol(Qmatrix)),
  meanPsi = rep(0, ncol(dataMatContinuous)),
  sdPsi = rep(1, ncol(dataMatContinuous)), 
  meanTheta = rep(0, ncol(Qmatrix)),
  meanThetaSD = rep(0, ncol(Qmatrix)),
  sdThetaSD = rep(1, ncol(Qmatrix)),
  thetaCovLKJ = 1
)

# for starting theta in the correct direction:
# create sum scores for each latent variable
sumScores = as.matrix(dataMatContinuous) %*% Qmatrix

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelCFA_makerItems_samples = modelCFA_markerItems$sample(
  data = modelCFA_makerItems_data,
  seed = 100920234,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(
    lambda = rnorm(sum(apply(X = Qmatrix, MARGIN = 2, FUN = sum)) - ncol(Qmatrix), mean = 5, sd = 1), 
    theta = sumScores
    )
)

# save model object
modelCFA_makerItems_samples$save_object(file = "modelCFA_makerItems_samples.RDS")

# checking convergence
max(
  modelCFA_makerItems_samples$summary(
    variables = c("lambda", "mu", "psi", "thetaCorr", "thetaSD", "theta"))$rhat, 
    na.rm = TRUE
)

# model parameter results
print(
  modelCFA_makerItems_samples$summary(
    variables = c("lambda", "mu", "psi", "thetaCorr", "thetaSD")
    ),
  n = Inf
)

# comparing model fit with previous models
loo_compare(
  list(
    model3D = modelCFA_lowerEchelon3D_samples$loo(variables = "personLike"),
    model2D = modelCFA_lowerEchelon2D_samples$loo(variables = "personLike"),
    modelCorrect2D = modelCFA_stdFactors_samples$loo(variables = "personLike"),
    modelMarkerItems = modelCFA_makerItems_samples$loo(variables = "personLike")
  )
)

# Model 5: CFA model with one dimension (standardized factor)

Qmatrix_oneFactor = matrix(data = 0, nrow = nItems, ncol = 1)
Qmatrix_oneFactor[,1] = 1

nLoadings = sum(apply(X = Qmatrix_oneFactor, MARGIN = 2, FUN = sum))

modelCFA_oneFactor_data = list(
  nObs = nrow(dataMatContinuous),
  nItems = ncol(dataMatContinuous),
  Y = t(dataMatContinuous),
  nFactors = ncol(Qmatrix_oneFactor),
  Qmatrix = Qmatrix_oneFactor,
  nLoadings = nLoadings,
  meanMu = rep(0, ncol(dataMatContinuous)),
  covMu = 10 * diag(ncol(dataMatContinuous)),
  meanLambda = rep(0, nLoadings),
  covLambda = 1 * diag(nLoadings),
  meanPsi = rep(0, ncol(dataMatContinuous)),
  sdPsi = rep(1, ncol(dataMatContinuous)), 
  meanTheta = rep(0, ncol(Qmatrix_oneFactor)),
  thetaCovLKJ = 1
)

# for starting theta in the correct direction:
# create sum scores for each latent variable
sumScores = as.matrix(dataMatContinuous) %*% Qmatrix_oneFactor

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelCFA_oneFactor_samples = modelCFA_stdFactors$sample(
  data = modelCFA_oneFactor_data,
  seed = 100920235,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(
    lambda = rnorm(sum(apply(X = Qmatrix_oneFactor, MARGIN = 2, FUN = sum)), mean = 5, sd = 1), 
    theta = sumScores
    )
)

# save model object
modelCFA_oneFactor_samples$save_object(file = "modelCFA_oneFactor_samples.RDS")

# checking convergence
max(modelCFA_oneFactor_samples$summary(variables = c("lambda", "mu", "psi", "theta"))$rhat, na.rm = TRUE)

# model parameter results
print(modelCFA_oneFactor_samples$summary(variables = c("lambda", "mu", "psi")) ,n = Inf)

# comparing model fit with previous models  
loo_compare(
  list(
    model3D = modelCFA_lowerEchelon3D_samples$loo(variables = "personLike"),
    model2D = modelCFA_lowerEchelon2D_samples$loo(variables = "personLike"),
    modelCorrect2D = modelCFA_stdFactors_samples$loo(variables = "personLike"),
    modelMarkerItems = modelCFA_makerItems_samples$loo(variables = "personLike"),
    modelOneFactor = modelCFA_oneFactor_samples$loo(variables = "personLike")
  )
)

# IRT Models in Stan =================================================================================================

# Model 1: Exploratory 2PL IRT model with Lower-Echelon Q-matrix three dimensions
Qmatrix_lowerEchelon3D = matrix(data = 0, nrow = nItems, ncol = 3)
Qmatrix_lowerEchelon3D[,1] = 1
Qmatrix_lowerEchelon3D[2:nItems,2] = 1
Qmatrix_lowerEchelon3D[3:nItems,3] = 1

nLoadings = sum(apply(X = Qmatrix_lowerEchelon3D, MARGIN = 2, FUN = sum))

modelIRT_lowerEchelon3D_data = list(
  nObs = nrow(dataMatBinary),
  nItems = ncol(dataMatBinary),
  Y = t(dataMatBinary),
  nFactors = ncol(Qmatrix_lowerEchelon3D),
  Qmatrix = Qmatrix_lowerEchelon3D,
  nLoadings = nLoadings,
  meanMu = rep(0, ncol(dataMatBinary)),
  covMu = 10 * diag(ncol(dataMatBinary)),
  meanLambda = rep(0, nLoadings),
  covLambda = 1 * diag(nLoadings),
  meanTheta = rep(0, ncol(Qmatrix_lowerEchelon3D))
)

# for starting theta in the correct direction:
# create sum scores for each latent variable
sumScores = as.matrix(dataMatBinary) %*% Qmatrix_lowerEchelon3D

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelIRT_lowerEchelon3D_samples = modelIRT_lowerEchelon$sample(
  data = modelIRT_lowerEchelon3D_data,
  seed = 100920236,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(
    lambda = rnorm(sum(apply(X = Qmatrix_lowerEchelon3D, MARGIN = 2, FUN = sum)), mean = 5, sd = 1), 
    theta = sumScores
    )
)

# save model object
modelIRT_lowerEchelon3D_samples$save_object(file = "modelIRT_lowerEchelon3D_samples.RDS")

# checking convergence
max(modelIRT_lowerEchelon3D_samples$summary(variables = c("lambda", "mu", "theta"))$rhat, na.rm = TRUE)

# model parameter results
print(modelIRT_lowerEchelon3D_samples$summary(variables = c("lambda", "mu")) ,n = Inf)

# Model 2: Exploratory 2PL IRT model with Lower-Echelon Q-matrix two dimensions

Qmatrix_lowerEchelon2D = matrix(data = 0, nrow = nItems, ncol = 2)
Qmatrix_lowerEchelon2D[,1] = 1
Qmatrix_lowerEchelon2D[2:nItems,2] = 1

nLoadings = sum(apply(X = Qmatrix_lowerEchelon2D, MARGIN = 2, FUN = sum))

modelIRT_lowerEchelon2D_data = list(
  nObs = nrow(dataMatBinary),
  nItems = ncol(dataMatBinary),
  Y = t(dataMatBinary),
  nFactors = ncol(Qmatrix_lowerEchelon2D),
  Qmatrix = Qmatrix_lowerEchelon2D,
  nLoadings = nLoadings,
  meanMu = rep(0, ncol(dataMatBinary)),
  covMu = 10 * diag(ncol(dataMatBinary)),
  meanLambda = rep(0, nLoadings),
  covLambda = 1 * diag(nLoadings),
  meanTheta = rep(0, ncol(Qmatrix_lowerEchelon2D))
)

# for starting theta in the correct direction:
# create sum scores for each latent variable

sumScores = as.matrix(dataMatBinary) %*% Qmatrix_lowerEchelon2D

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelIRT_lowerEchelon2D_samples = modelIRT_lowerEchelon$sample(
  data = modelIRT_lowerEchelon2D_data,
  seed = 100920237,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(
    lambda = rnorm(sum(apply(X = Qmatrix_lowerEchelon2D, MARGIN = 2, FUN = sum)), mean = 5, sd = 1), 
    theta = sumScores
    )
)

# save model object
modelIRT_lowerEchelon2D_samples$save_object(file = "modelIRT_lowerEchelon2D_samples.RDS")

# checking convergence
max(modelIRT_lowerEchelon2D_samples$summary(variables = c("lambda", "mu", "theta"))$rhat, na.rm = TRUE)

# model parameter results
print(modelIRT_lowerEchelon2D_samples$summary(variables = c("lambda", "mu")) ,n = Inf)

# comparing model fit with previous models

loo_compare(
  list(
    model3D = modelIRT_lowerEchelon3D_samples$loo(variables = "personLike"),
    model2D = modelIRT_lowerEchelon2D_samples$loo(variables = "personLike")
  )
)

# Model 3: IRT model with correct Q-matrix, standardized factors, and factor correlations

modelIRT_stdFactors_data = list(
  nObs = nrow(dataMatBinary),
  nItems = ncol(dataMatBinary),
  Y = t(dataMatBinary),
  nFactors = ncol(Qmatrix),
  Qmatrix = Qmatrix,
  meanMu = rep(0, ncol(dataMatBinary)),
  covMu = 10 * diag(ncol(dataMatBinary)),
  meanLambda = rep(0, ncol(dataMatBinary)),
  covLambda = 1 * diag(ncol(dataMatBinary)),
  meanTheta = rep(0, ncol(Qmatrix)),
  thetaCovLKJ = 1
)

# for starting theta in the correct direction:
# create sum scores for each latent variable
sumScores = as.matrix(dataMatBinary) %*% Qmatrix

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelIRT_stdFactors_samples = modelIRT_stdFactors$sample(
  data = modelIRT_stdFactors_data,
  seed = 100920238,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(lambda = rnorm(nItems, mean = 5, sd = 1), theta = sumScores)
)

# save model object
modelIRT_stdFactors_samples$save_object(file = "modelIRT_stdFactors_samples.RDS")

# checking convergence
max(modelIRT_stdFactors_samples$summary(variables = c("lambda", "mu", "thetaCorr", "theta"))$rhat, na.rm = TRUE)

# model parameter results
print(modelIRT_stdFactors_samples$summary(variables = c("lambda", "mu", "thetaCorr")) ,n = Inf)

# comparing model fit with previous models
loo_compare(
  list(
    model3D = modelIRT_lowerEchelon3D_samples$loo(variables = "personLike"),
    model2D = modelIRT_lowerEchelon2D_samples$loo(variables = "personLike"),
    modelCorrect2D = modelIRT_stdFactors_samples$loo(variables = "personLike")
  )
) 

# Model 4: IRT model with correct Q-matrix, marker items, and factor covariances
modelIRT_makerItems_data = list(
  nObs = nrow(dataMatBinary),
  nItems = ncol(dataMatBinary),
  Y = t(dataMatBinary),
  nFactors = ncol(Qmatrix),
  Qmatrix = Qmatrix,
  nLoadings = sum(apply(X = Qmatrix, MARGIN = 2, FUN = sum)) - ncol(Qmatrix),
  meanMu = rep(0, ncol(dataMatBinary)),
  covMu = 10 * diag(ncol(dataMatBinary)),
  meanLambda = rep(0, ncol(dataMatBinary) - ncol(Qmatrix)),
  covLambda = 1 * diag(ncol(dataMatBinary) - ncol(Qmatrix)),
  meanTheta = rep(0, ncol(Qmatrix)),
  meanThetaSD = rep(0, ncol(Qmatrix)),
  sdThetaSD = rep(1, ncol(Qmatrix)),
  thetaCovLKJ = 1
)

# for starting theta in the correct direction:
# create sum scores for each latent variable
sumScores = as.matrix(dataMatBinary) %*% Qmatrix

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelIRT_makerItems_samples = modelIRT_markerItems$sample(
  data = modelIRT_makerItems_data,
  seed = 100920239,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(
    lambda = rnorm(sum(apply(X = Qmatrix, MARGIN = 2, FUN = sum)) - ncol(Qmatrix), mean = 5, sd = 1), 
    theta = sumScores
    )
)

# save model object
modelIRT_makerItems_samples$save_object(file = "modelIRT_makerItems_samples.RDS")

# checking convergence
max(
  modelIRT_makerItems_samples$summary(
    variables = c("lambda", "mu", "thetaCorr", "thetaSD", "theta"))$rhat, 
    na.rm = TRUE
) 

# model parameter results
print(
  modelIRT_makerItems_samples$summary(
    variables = c("lambda", "mu", "thetaCorr", "thetaSD")
    ),
  n = Inf
)

# comparing model fit with previous models

loo_compare(
  list(
    model3D = modelIRT_lowerEchelon3D_samples$loo(variables = "personLike"),
    model2D = modelIRT_lowerEchelon2D_samples$loo(variables = "personLike"),
    modelCorrect2D = modelIRT_stdFactors_samples$loo(variables = "personLike"),
    modelMarkerItems = modelIRT_makerItems_samples$loo(variables = "personLike")
  )
) 

# Model 5: IRT model with one dimension (standardized factor)

Qmatrix_oneFactor = matrix(data = 0, nrow = nItems, ncol = 1)
Qmatrix_oneFactor[,1] = 1

nLoadings = sum(apply(X = Qmatrix_oneFactor, MARGIN = 2, FUN = sum))

modelIRT_oneFactor_data = list(
  nObs = nrow(dataMatBinary),
  nItems = ncol(dataMatBinary),
  Y = t(dataMatBinary),
  nFactors = ncol(Qmatrix_oneFactor),
  Qmatrix = Qmatrix_oneFactor,
  nLoadings = nLoadings,
  meanMu = rep(0, ncol(dataMatBinary)),
  covMu = 10 * diag(ncol(dataMatBinary)),
  meanLambda = rep(0, nLoadings),
  covLambda = 1 * diag(nLoadings),
  meanTheta = rep(0, ncol(Qmatrix_oneFactor)),
  thetaCovLKJ = 1
)

# for starting theta in the correct direction:

# create sum scores for each latent variable
sumScores = as.matrix(dataMatBinary) %*% Qmatrix_oneFactor

# standardize each latent variable sum score
sumScores = scale(sumScores)

modelIRT_oneFactor_samples = modelIRT_stdFactors$sample(
  data = modelIRT_oneFactor_data,
  seed = 100920240,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init = function() list(
    lambda = rnorm(sum(apply(X = Qmatrix_oneFactor, MARGIN = 2, FUN = sum)), mean = 5, sd = 1), 
    theta = sumScores
    )
)

# save model object
modelIRT_oneFactor_samples$save_object(file = "modelIRT_oneFactor_samples.RDS")

# checking convergence
max(modelIRT_oneFactor_samples$summary(variables = c("lambda", "mu", "theta"))$rhat, na.rm = TRUE)

# model parameter results
print(modelIRT_oneFactor_samples$summary(variables = c("lambda", "mu", "theta")) ,n = Inf)

# comparing model fit with previous models
loo_compare(
  list(
    model3D = modelIRT_lowerEchelon3D_samples$loo(variables = "personLike"),
    model2D = modelIRT_lowerEchelon2D_samples$loo(variables = "personLike"),
    modelCorrect2D = modelIRT_stdFactors_samples$loo(variables = "personLike"),
    modelMarkerItems = modelIRT_makerItems_samples$loo(variables = "personLike"),
    modelOneFactor = modelIRT_oneFactor_samples$loo(variables = "personLike")
  )
) 
