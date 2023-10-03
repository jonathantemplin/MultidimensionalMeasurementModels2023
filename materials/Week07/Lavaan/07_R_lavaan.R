
# Load Packages =======================================================================================================

if (!require(lavaan)) install.packages("lavaan")
library(lavaan)

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


# CFA Models in lavaan =================================================================================================

# Confirmatory Factor Analysis (CFA) with Lower-Echelon Q-matrix three dimensions
CFA_lowerEchelon_3D.syntax = "

# measurement model specification ----------------------------------------------------

# latent variable definitions
theta1 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10
theta2 =~ item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10
theta3 =~ item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10

# residual variances
item1 ~~ item1; item2 ~~ item2; item3 ~~ item3; item4 ~~ item4; item5 ~~ item5;
item6 ~~ item6; item7 ~~ item7; item8 ~~ item8; item9 ~~ item9; item10 ~~ item10;

# item intercepts
item1 ~ 1; item2 ~ 1; item3 ~ 1; item4 ~ 1; item5 ~ 1;
item6 ~ 1; item7 ~ 1; item8 ~ 1; item9 ~ 1; item10 ~ 1;

# structural model specification -----------------------------------------------------

# factor means
theta1 ~ 0; theta2 ~ 0; theta3 ~ 0;

# factor variances
theta1 ~~ 1*theta1; theta2 ~~ 1*theta2; theta3 ~~ 1*theta3;

# factor covariances (all set to zero)
theta1 ~~ 0*theta2; theta1 ~~ 0*theta3; theta2 ~~ 0*theta3;
"

CFA_lowerEchelon3D.lavaan = lavaan(
    model = CFA_lowerEchelon_3D.syntax, 
    data = dataMatContinuous, 
    estimator = "MLR", 
    mimic = "mplus"
)

summary(CFA_lowerEchelon3D.lavaan, fit.measures = TRUE, standardized = TRUE)

# Exploratory CFA Lower-Echelon Q-matrix with Two Dimensions

CFA_lowerEchelon_2D.syntax = "

# measurement model specification ----------------------------------------------------

# latent variable definitions
theta1 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10
theta2 =~ item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10

# residual variances
item1 ~~ item1; item2 ~~ item2; item3 ~~ item3; item4 ~~ item4; item5 ~~ item5;
item6 ~~ item6; item7 ~~ item7; item8 ~~ item8; item9 ~~ item9; item10 ~~ item10;

# item intercepts
item1 ~ 1; item2 ~ 1; item3 ~ 1; item4 ~ 1; item5 ~ 1;
item6 ~ 1; item7 ~ 1; item8 ~ 1; item9 ~ 1; item10 ~ 1;

# structural model specification -----------------------------------------------------

# factor means
theta1 ~ 0; theta2 ~ 0; 

# factor variances
theta1 ~~ 1*theta1; theta2 ~~ 1*theta2; 

# factor covariances
theta1 ~~ 0*theta2; 
"

CFA_lowerEchelon2D.lavaan = lavaan(
    model = CFA_lowerEchelon_2D.syntax, 
    data = dataMatContinuous, 
    estimator = "MLR", 
    mimic = "mplus"
)

summary(CFA_lowerEchelon2D.lavaan, fit.measures = TRUE, standardized = TRUE)

# Confirmatory Factor Analysis (CFA) with Correct (from simulated data) Q-matrix


CFA_correct_2D.syntax = "

# measurement model specification ----------------------------------------------------

# latent variable definitions
theta1 =~ item1 + item2 + item3 + item4 + item5
theta2 =~ item6 + item7 + item8 + item9 + item10

# residual variances
item1 ~~ item1; item2 ~~ item2; item3 ~~ item3; item4 ~~ item4; item5 ~~ item5;
item6 ~~ item6; item7 ~~ item7; item8 ~~ item8; item9 ~~ item9; item10 ~~ item10;

# item intercepts
item1 ~ 1; item2 ~ 1; item3 ~ 1; item4 ~ 1; item5 ~ 1;
item6 ~ 1; item7 ~ 1; item8 ~ 1; item9 ~ 1; item10 ~ 1;

# structural model specification -----------------------------------------------------

# factor means
theta1 ~ 0; theta2 ~ 0;

# factor variances
theta1 ~~ 1*theta1; theta2 ~~ 1*theta2;

# factor covariances
theta1 ~~ theta2;

"

CFA_correct_2D.lavaan = lavaan(
    model = CFA_correct_2D.syntax, 
    data = dataMatContinuous, 
    estimator = "MLR", 
    mimic = "mplus"
)

summary(CFA_correct_2D.lavaan, fit.measures = TRUE, standardized = TRUE)

# Unidimensional CFA model
CFA_unidimeionsal.syntax = "

# measurement model specification ----------------------------------------------------

# latent variable definitions
theta1 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10

# residual variances
item1 ~~ item1; item2 ~~ item2; item3 ~~ item3; item4 ~~ item4; item5 ~~ item5;
item6 ~~ item6; item7 ~~ item7; item8 ~~ item8; item9 ~~ item9; item10 ~~ item10;

# item intercepts
item1 ~ 1; item2 ~ 1; item3 ~ 1; item4 ~ 1; item5 ~ 1;
item6 ~ 1; item7 ~ 1; item8 ~ 1; item9 ~ 1; item10 ~ 1;

# structural model specification -----------------------------------------------------

# factor means
theta1 ~ 0; 

# factor variances
theta1 ~~ 1*theta1;

"

CFA_unidimensional.lavaan = lavaan(
    model = CFA_unidimeionsal.syntax, 
    data = dataMatContinuous, 
    estimator = "MLR", 
    mimic = "mplus"
)
summary(CFA_unidimensional.lavaan, fit.measures = TRUE, standardized = TRUE)

# Relative model fit
anova(CFA_lowerEchelon3D.lavaan, CFA_lowerEchelon2D.lavaan, CFA_correct_2D.lavaan, CFA_unidimensional.lavaan)

# Exploratory Factor Analysis
# lavaan Syntax (can be R character string or text file)

EFAmodel3D_continuousData = efa(data = dataMatContinuous, nfactors = 3, rotation="none")
summary(EFAmodel3D_continuousData)

EFAmodel2D_continuousData = efa(data = dataMatContinuous, nfactors = 2, rotation="none")
summary(EFAmodel2D_continuousData)

EFAmodel1D_continuousData = efa(data = dataMatContinuous, nfactors = 1, rotation="none")
summary(EFAmodel1D_continuousData)

# IRT Models in lavaan using limited information (WLSMV) ==============================================================

# IRT model with Lower-Echelon Q-matrix three dimensions
IRT_lowerEchelon_3D.syntax = "

# measurement model specification ----------------------------------------------------

# latent variable definitions
theta1 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10
theta2 =~ item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10
theta3 =~ item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10

# item intercepts
item1 ~ 1; item2 ~ 1; item3 ~ 1; item4 ~ 1; item5 ~ 1;
item6 ~ 1; item7 ~ 1; item8 ~ 1; item9 ~ 1; item10 ~ 1;

# structural model specification -----------------------------------------------------

# factor means
theta1 ~ 0; theta2 ~ 0; theta3 ~ 0;

# factor variances
theta1 ~~ 1*theta1; theta2 ~~ 1*theta2; theta3 ~~ 1*theta3;

# factor covariances (all set to zero)
theta1 ~~ 0*theta2; theta1 ~~ 0*theta3; theta2 ~~ 0*theta3;
"

IRT_lowerEchelon3D.lavaan = lavaan(
    model = IRT_lowerEchelon_3D.syntax, 
    data = dataMatBinary, 
    ordered = paste0("item", 1:nItems),
    parameterization = "Theta"
)

summary(IRT_lowerEchelon3D.lavaan, fit.measures = TRUE, standardized = TRUE)

# IRT model with Lower-Echelon Q-matrix two dimensions
IRT_lowerEchelon_2D.syntax = "

# measurement model specification ----------------------------------------------------

# latent variable definitions
theta1 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10
theta2 =~ item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10

# item intercepts
item1 ~ 1; item2 ~ 1; item3 ~ 1; item4 ~ 1; item5 ~ 1;
item6 ~ 1; item7 ~ 1; item8 ~ 1; item9 ~ 1; item10 ~ 1;

# structural model specification -----------------------------------------------------

# factor means
theta1 ~ 0; theta2 ~ 0; 

# factor variances
theta1 ~~ 1*theta1; theta2 ~~ 1*theta2; 

# factor covariances
theta1 ~~ 0*theta2; 
"

IRT_lowerEchelon2D.lavaan = lavaan(
    model = IRT_lowerEchelon_2D.syntax, 
    data = dataMatBinary,
    ordered = TRUE,
    parameterization = "Theta"
)

summary(IRT_lowerEchelon2D.lavaan, fit.measures = TRUE, standardized = TRUE)

# IRT model with Correct (from simulated data) Q-matrix

IRT_correct_2D.syntax = "

# measurement model specification ----------------------------------------------------

# latent variable definitions
theta1 =~ item1 + item2 + item3 + item4 + item5
theta2 =~ item6 + item7 + item8 + item9 + item10

# item intercepts
item1 ~ 1; item2 ~ 1; item3 ~ 1; item4 ~ 1; item5 ~ 1;
item6 ~ 1; item7 ~ 1; item8 ~ 1; item9 ~ 1; item10 ~ 1;

# structural model specification -----------------------------------------------------

# factor means
theta1 ~ 0; theta2 ~ 0;

# factor variances
theta1 ~~ 1*theta1; theta2 ~~ 1*theta2;

# factor covariances
theta1 ~~ theta2;

"


IRT_correct_2D.lavaan = lavaan(
    model = IRT_correct_2D.syntax, 
    data = dataMatBinary, 
    ordered = TRUE,
    parameterization = "Theta"
)

summary(IRT_correct_2D.lavaan, fit.measures = TRUE, standardized = TRUE)


# IRT model with Unidimensional Q-matrix
IRT_unidimeionsal.syntax = "

# measurement model specification ----------------------------------------------------

# latent variable definitions
theta1 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10

# item intercepts
item1 ~ 1; item2 ~ 1; item3 ~ 1; item4 ~ 1; item5 ~ 1;
item6 ~ 1; item7 ~ 1; item8 ~ 1; item9 ~ 1; item10 ~ 1;

# structural model specification -----------------------------------------------------

# factor means
theta1 ~ 0; 

# factor variances
theta1 ~~ 1*theta1;

"

IRT_unidimensional.lavaan = lavaan(
    model = IRT_unidimeionsal.syntax, 
    data = dataMatBinary, 
    ordered = TRUE,
    parameterization = "Theta"
)

summary(IRT_unidimensional.lavaan, fit.measures = TRUE, standardized = TRUE)

# Relative model fit
anova(IRT_lowerEchelon2D.lavaan, IRT_correct_2D.lavaan, IRT_unidimensional.lavaan)
