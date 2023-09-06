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

# generate thetas
thetaCorr = diag(nFactors)
thetaCorr[1,2] = thetaCorr[2,1] = .75

thetaCorrU = chol(thetaCorr)
t(thetaCorrU) %*% thetaCorrU

# uncorrelated univariate normal
thetaZ = matrix(data = 0, nrow = nExaminees, ncol = nFactors)
for (factor in 1:nFactors){
    thetaZ[,factor] = rnorm(n = nExaminees, mean=0, sd = 1)
}
cor(thetaZ)

theta = thetaZ %*% thetaCorrU
cor(theta)

# generate data 
dataMat = matrix(data = NA, nrow = nExaminees, ncol = nItems)
colnames(dataMat) = paste0("item", 1:nItems)
for (item in 1:nItems){
    logit = mu[item] + Qmatrix[item,1]*lambda[item]*theta[,1] + Qmatrix[item,2]*lambda[item]*theta[,2]
    prob = exp(logit)/(1+exp(logit))
    dataMat[,item] = rbinom(n = nExaminees, size = 1, prob = prob)
}



if (!require(mirt)) install.packages("mirt")
library(mirt)

U2PL = mirt(dataMat, 1)
coef(U2PL, IRTpars = TRUE)
