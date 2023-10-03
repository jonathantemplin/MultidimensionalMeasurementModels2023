# Load Packages =======================================================================================================

if (!require(mirt)) install.packages("mirt")
library(mirt)

# Set random number seed for consistency in simluation-based methods ==================================================
set.seed(28092023)


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
    thetaZ[,factor] = rnorm(n = nExaminees, mean=0, sd = 1)
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

# Build Q-matrices for analysis =======================================================================================

Qmatrix_lowerEchelon_3D = matrix(data = 0, nrow = nItems, ncol = 3)
Qmatrix_lowerEchelon_3D[1:nItems,1] = 1
Qmatrix_lowerEchelon_3D[2:nItems,2] = 1
Qmatrix_lowerEchelon_3D[3:nItems,3] = 1

# for MIRT: which covariances should be estimated
estCov3D_lowerEchelon = matrix(data = FALSE, nrow = 3, ncol = 3)

Qmatrix_lowerEchelon_2D = matrix(data = 0, nrow = nItems, ncol = 2)
Qmatrix_lowerEchelon_2D[1:nItems,1] = 1
Qmatrix_lowerEchelon_2D[2:nItems,2] = 1

estCov2D_lowerEchelon = matrix(data = FALSE, nrow = 2, ncol = 2)

Qmatrix_correct = Qmatrix

estCov2D_correct = matrix(data = TRUE, nrow = 2, ncol = 2)
estCov2D_correct[1,1] = estCov2D_correct[2,2] = FALSE

Qmatrix_unidimensional = matrix(data = 1, nrow = nItems, ncol = 1)


# NO CFA Models in MIRT
# IRT Models in mirt using MML via EM =================================================================================

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix three dimensions
IRT_lowerEchelon3D = mirt(data = dataMatBinary, 
                          model = mirt.model(Qmatrix_lowerEchelon_3D, COV = estCov3D_lowerEchelon), 
                          SE = TRUE, 
                          method = "EM", 
                          quadpts = 15, 
                          technical = list(NCYCLES = 10000))

summary(IRT_lowerEchelon3D)
coef(IRT_lowerEchelon3D, printSE = TRUE)

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix two dimensions
IRT_lowerEchelon2D = mirt(data = dataMatBinary, 
                          model = mirt.model(Qmatrix_lowerEchelon_2D, COV = estCov2D_lowerEchelon), 
                          SE = TRUE, 
                          method = "EM", 
                          quadpts = 15,
                          technical = list(NCYCLES = 10000))

summary(IRT_lowerEchelon2D)
coef(IRT_lowerEchelon2D, printSE = TRUE)

# Confirmatory 2PL IRT model with correct Q-matrix
IRT_correct = mirt(data = dataMatBinary, 
                   model = mirt.model(Qmatrix_correct, COV = estCov2D_correct), 
                   SE = TRUE, 
                   method = "EM", 
                   quadpts = 15,
                   technical = list(NCYCLES = 10000))
summary(IRT_correct)
coef(IRT_correct, printSE = TRUE)

# Confirmatory 1PL IRT model with correct Q-matrix
IRT_unidimensional = mirt(data = dataMatBinary, 
                          model = mirt.model(Qmatrix_unidimensional), 
                          SE = TRUE, 
                          method = "EM", 
                          quadpts = 15,
                          technical = list(NCYCLES = 10000))
summary(IRT_unidimensional)
coef(IRT_unidimensional, printSE = TRUE)

# model comparisons WTF?
anova(IRT_lowerEchelon3D, IRT_lowerEchelon2D, IRT_correct, IRT_unidimensional)

LRT = -2*(IRT_lowerEchelon2D@Fit$logLik - IRT_lowerEchelon3D@Fit$logLik)
df = IRT_lowerEchelon2D@Fit$df - IRT_lowerEchelon3D@Fit$df
pvalue = pchisq(LRT, df, lower.tail = FALSE)
cat(paste0("Exploratory 3D vs Exploratory 2D\n", "LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

LRT = -2*(IRT_correct@Fit$logLik - IRT_lowerEchelon2D@Fit$logLik)
df = IRT_correct@Fit$df - IRT_lowerEchelon2D@Fit$df
pvalue = pchisq(LRT, df, lower.tail = FALSE)
cat(paste0("Exploratory 2D vs Correct 2D\n", "LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

LRT = -2*(IRT_unidimensional@Fit$logLik - IRT_correct@Fit$logLik)
df = IRT_unidimensional@Fit$df - IRT_correct@Fit$df
pvalue = pchisq(LRT, df, lower.tail = FALSE)
cat(paste0("Correct 2D vs Correct 1D\n", "LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

# IRT Models in mirt using MML via Quasi Monte Carlo Integration ======================================================

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix three dimensions
IRT_lowerEchelon3D_QMC = mirt(data = dataMatBinary, 
                          model = mirt.model(Qmatrix_lowerEchelon_3D, COV = estCov3D_lowerEchelon), 
                          SE = TRUE, 
                          method = "QMCEM", 
                          technical = list(NCYCLES = 10000))

summary(IRT_lowerEchelon3D)
coef(IRT_lowerEchelon3D, printSE = TRUE)

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix two dimensions
IRT_lowerEchelon2D = mirt(data = dataMatBinary, 
                          model = mirt.model(Qmatrix_lowerEchelon_2D, COV = estCov2D_lowerEchelon), 
                          SE = TRUE, 
                          method = "QMCEM", 
                          technical = list(NCYCLES = 10000))

summary(IRT_lowerEchelon2D)
coef(IRT_lowerEchelon2D, printSE = TRUE)

# Confirmatory 2PL IRT model with correct Q-matrix
IRT_correct = mirt(data = dataMatBinary, 
                   model = mirt.model(Qmatrix_correct, COV = estCov2D_correct), 
                   SE = TRUE, 
                   method = "QMCEM", 
                   technical = list(NCYCLES = 10000))
summary(IRT_correct)
coef(IRT_correct, printSE = TRUE)

# Confirmatory 1PL IRT model with correct Q-matrix
IRT_unidimensional = mirt(data = dataMatBinary, 
                          model = mirt.model(Qmatrix_unidimensional), 
                          SE = TRUE, 
                          method = "QMCEM", 
                          technical = list(NCYCLES = 10000))
summary(IRT_unidimensional)
coef(IRT_unidimensional, printSE = TRUE)

# model comparisons WTF?
anova(IRT_lowerEchelon3D, IRT_lowerEchelon2D, IRT_correct, IRT_unidimensional)

LRT = -2*(IRT_lowerEchelon2D@Fit$logLik - IRT_lowerEchelon3D@Fit$logLik)
df = IRT_lowerEchelon2D@Fit$df - IRT_lowerEchelon3D@Fit$df
pvalue = pchisq(LRT, df, lower.tail = FALSE)
cat(paste0("Exploratory 3D vs Exploratory 2D\n", "LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

LRT = -2*(IRT_correct@Fit$logLik - IRT_lowerEchelon2D@Fit$logLik)
df = IRT_correct@Fit$df - IRT_lowerEchelon2D@Fit$df
pvalue = pchisq(LRT, df, lower.tail = FALSE)
cat(paste0("Exploratory 2D vs Correct 2D\n", "LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

LRT = -2*(IRT_unidimensional@Fit$logLik - IRT_correct@Fit$logLik)
df = IRT_unidimensional@Fit$df - IRT_correct@Fit$df
pvalue = pchisq(LRT, df, lower.tail = FALSE)
cat(paste0("Correct 2D vs Correct 1D\n", "LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

# IRT Models in mirt using MML via MHRM ===============================================================================

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix three dimensions
IRT_lowerEchelon3D_QMC = mirt(data = dataMatBinary, 
                          model = mirt.model(Qmatrix_lowerEchelon_3D, COV = estCov3D_lowerEchelon), 
                          SE = TRUE, 
                          method = "MHRM", 
                          technical = list(NCYCLES = 10000))

summary(IRT_lowerEchelon3D)
coef(IRT_lowerEchelon3D, printSE = TRUE)

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix two dimensions
IRT_lowerEchelon2D = mirt(data = dataMatBinary, 
                          model = mirt.model(Qmatrix_lowerEchelon_2D, COV = estCov2D_lowerEchelon), 
                          SE = TRUE, 
                          method = "QMCEM", 
                          quadpts = 15,
                          technical = list(NCYCLES = 10000))

summary(IRT_lowerEchelon2D)
coef(IRT_lowerEchelon2D, printSE = TRUE)

# Confirmatory 2PL IRT model with correct Q-matrix
IRT_correct = mirt(data = dataMatBinary, 
                   model = mirt.model(Qmatrix_correct, COV = estCov2D_correct), 
                   SE = TRUE, 
                   method = "QMCEM", 
                   quadpts = 15,
                   technical = list(NCYCLES = 10000))
summary(IRT_correct)
coef(IRT_correct, printSE = TRUE)

# Confirmatory 1PL IRT model with correct Q-matrix
IRT_unidimensional = mirt(data = dataMatBinary, 
                          model = mirt.model(Qmatrix_unidimensional), 
                          SE = TRUE, 
                          method = "QMCEM", 
                          quadpts = 15,
                          technical = list(NCYCLES = 10000))
summary(IRT_unidimensional)
coef(IRT_unidimensional, printSE = TRUE)

# model comparisons WTF?
anova(IRT_lowerEchelon3D, IRT_lowerEchelon2D, IRT_correct, IRT_unidimensional)

LRT = -2*(IRT_lowerEchelon2D@Fit$logLik - IRT_lowerEchelon3D@Fit$logLik)
df = IRT_lowerEchelon2D@Fit$df - IRT_lowerEchelon3D@Fit$df
pvalue = pchisq(LRT, df, lower.tail = FALSE)
cat(paste0("Exploratory 3D vs Exploratory 2D\n", "LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

LRT = -2*(IRT_correct@Fit$logLik - IRT_lowerEchelon2D@Fit$logLik)
df = IRT_correct@Fit$df - IRT_lowerEchelon2D@Fit$df
pvalue = pchisq(LRT, df, lower.tail = FALSE)
cat(paste0("Exploratory 2D vs Correct 2D\n", "LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

LRT = -2*(IRT_unidimensional@Fit$logLik - IRT_correct@Fit$logLik)
df = IRT_unidimensional@Fit$df - IRT_correct@Fit$df
pvalue = pchisq(LRT, df, lower.tail = FALSE)
cat(paste0("Correct 2D vs Correct 1D\n", "LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

