# Load Packages =======================================================================================================

if (!require(MplusAutomation)) install.packages("MplusAutomation")
library(MplusAutomation)

# Functions ===========================================================================================================

LRT_MLR = function(model1, model2){

    model1pars = model1$summaries$Parameters
    model2pars = model2$summaries$Parameters

    ll1 = model1$summaries$LL
    ll1cf = model1$summaries$LLCorrectionFactor

    ll2 = model2$summaries$LL
    ll2cf = model2$summaries$LLCorrectionFactor
    
    if (model2pars > model1pars){
        # model12 is H1 model1 is H0
        correction = abs(model1pars*ll1cf - model2pars*ll2cf)/(model2pars - model1pars)
        LRT = -2*(ll1 - ll2)/correction
        df = model2pars - model1pars
        modelH0 = model1$summaries$Filename
        modelH1 = model2$summaries$Filename
    } else {
        correction = abs(model2pars*ll2cf - model1pars*ll1cf)/(model1pars - model2pars)
        LRT = -2*(ll2 - ll1)/correction
        df = model1pars - model2pars
        modelH0 = model2$summaries$Filename
        modelH1 = model1$summaries$Filename
    }
    cat(paste0("Null Model: ", modelH0, "\n"))
    cat(paste0("Alternative Model: ", modelH1, "\n"))
    cat(paste0("LRT = ", LRT, "\n"))
    cat(paste0("df = ", df, "\n"))

    pvalue = pchisq(LRT, df = df, lower.tail = FALSE)
    cat(paste0("p-value = ", pvalue, "\n"))

    return(data.frame(LRT = LRT, df = df, pvalue = pvalue, modelH0 = modelH0, modelH1 = modelH1))
}

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


# CFA Models in Mplus =================================================================================================

# output data in Mplus format (using MplusAutomation package)
mplusData = prepareMplusData(df = dataMatContinuous, filename = "cfaData.dat", keepCols = paste0("item", 1:nItems))

# Confirmatory Factor Analysis (CFA) with Lower-Echelon Q-matrix three dimensions
# Mplus Syntax File: CFA_lowerEchelon3D_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = runModels(target = "CFA_lowerEchelon3D_Qmatrix.inp", recursive = FALSE)

# import Mplus output
CFAmodel_E_3D = readModels(target = "CFA_lowerEchelon3D_Qmatrix.out", recursive = FALSE)

# Confirmatory Factor Analysis (CFA) with Lower-Echelon Q-matrix two dimensions
# Mplus Syntax File: CFA_lowerEchelon2D_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = runModels(target = "CFA_lowerEchelon2D_Qmatrix.inp", recursive = FALSE)

# import Mplus output
CFAmodel_E_2D = readModels(target = "CFA_lowerEchelon2D_Qmatrix.out", recursive = FALSE)

# Confirmatory Factor Analysis (CFA) with Correct (from simulated data) Q-matrix
# Mplus Syntax File: CFA_correct_Qmatrix.inp

# run Mplus
mplusRun = runModels(target = "CFA_correct_Qmatrix.inp", recursive = FALSE)
# import Mplus output
CFAmodel_C_2D = readModels(target = "CFA_correct_Qmatrix.out", recursive = FALSE)

# Unidimensional CFA model
mplusRun = runModels(target = "CFA_unidimensional_Qmatrix.inp", recursive = FALSE)
CFAmodel_C_1D = readModels(target = "CFA_unidimensional_Qmatrix.out", recursive = FALSE)

# Relative model fit
LRT_MLR(model1 = CFAmodel_E_3D, model2 = CFAmodel_E_2D)
LRT_MLR(model1 = CFAmodel_E_2D, model2 = CFAmodel_C_2D)
LRT_MLR(model1 = CFAmodel_C_2D, model2 = CFAmodel_C_1D)

# Exploratory Factor Analysis (using Mplus defaults)
# Mplus Syntax File: EFA.inp

# run Mplus (data file is same as previous)
mplusRun = runModels(target = "EFA.inp", recursive = FALSE)

# import Mplus output
EFAmodel = readModels(target = "EFA.out", recursive = FALSE)


# IRT Models in Mplus using MML =======================================================================================

# output data in Mplus format (using MplusAutomation package)
mplusData = prepareMplusData(df = dataMatBinary, filename = "irtData.dat", keepCols = paste0("item", 1:nItems))

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix three dimensions
# Mplus Syntax File: IRT_MML_lowerEchelon3D_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_MML_lowerEchelon3D_Qmatrix.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_E_3D = 
  readModels(target = "IRT_MML_lowerEchelon3D_Qmatrix.out", recursive = FALSE, quiet = FALSE)

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix two dimensions
# Mplus Syntax File: IRT_MML_lowerEchelon2D_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_MML_lowerEchelon2D_Qmatrix.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_E_2D = 
  readModels(target = "IRT_MML_lowerEchelon2D_Qmatrix.out", recursive = FALSE, quiet = FALSE)

# Exploratory 2PL IRT model with Correct (from simulated data) Q-matrix
# Mplus Syntax File: IRT_MML_correct_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_MML_correct_Qmatrix.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_C_2D = 
  readModels(target = "IRT_MML_correct_Qmatrix.out", recursive = FALSE, quiet = FALSE)

# Unidimensional 2PL IRT model
# Mplus Syntax File: IRT_MML_unidimensional_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_MML_unidimensional_Qmatrix.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_C_1D = 
  readModels(target = "IRT_MML_unidimensional_Qmatrix.out", recursive = FALSE, quiet = FALSE)

# Relative model fit (note: my function won't work for IRT as MplusAutomation does not read in the LLCorrectionFactor)
# all numbers hand-entered from Mplus output files
llH1 = -5843.771
scfH1 = 1.0254
paramsH1 = 37

llH0 = -5855.200
scfH0 = 1.0651
paramsH0 = 29

correctionFactor = abs(paramsH0*scfH0 - paramsH1*scfH1)/(paramsH1 - paramsH0)
LRT = -2*(llH0 - llH1)/correctionFactor
df = paramsH1 - paramsH0
pvalue = pchisq(LRT, df = df, lower.tail = FALSE)
cat(paste0("LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

# comparing 2D exploratory Q with 2D correct Q
llH1 = -5855.200
scfH1 = 1.0651
paramsH1 = 29

llH0 = -5860.517
scfH0 = 1.0128
paramsH0 = 21

correctionFactor = abs(paramsH0*scfH0 - paramsH1*scfH1)/(paramsH1 - paramsH0)
LRT = -2*(llH0 - llH1)/correctionFactor
df = paramsH1 - paramsH0
pvalue = pchisq(LRT, df = df, lower.tail = FALSE)
cat(paste0("LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

# comparing 2D correct Q with 1D correct Q
llH1 = -5860.517
scfH1 = 1.0128
paramsH1 = 21

llH0 = -5867.844
scfH0 = 1.0080
paramsH0 = 20

correctionFactor = abs(paramsH0*scfH0 - paramsH1*scfH1)/(paramsH1 - paramsH0)
LRT = -2*(llH0 - llH1)/correctionFactor
df = paramsH1 - paramsH0
pvalue = pchisq(LRT, df = df, lower.tail = FALSE)
cat(paste0("LRT = ", LRT, "\n", "df = ", df, "\n", "p-value = ", pvalue, "\n"))

# Using Mplus' built-in EFA to estimate the IRT model
# Mplus Syntax File: IRT_MML_EFA.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_MML_EFA.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_EFA = 
  readModels(target = "IRT_MML_EFA.out", recursive = FALSE, quiet = FALSE)

# IRT Models in Mplus using MML and Monte Carlo Integration ===========================================================

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix three dimensions
# Mplus Syntax File: IRT_MML_lowerEchelon3D_Qmatrix_NI.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_MML_lowerEchelon3D_Qmatrix.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_E_3D_NI = 
  readModels(target = "IRT_MML_lowerEchelon3D_Qmatrix.out", recursive = FALSE, quiet = FALSE)

# compare output with that from MML with quadrature

## IRT Models in Mplus using limited information (WLSMV) ===============================================================

# Exploratory 2PL IRT model with Lower-Echelon Q-matrix three dimensions
# Mplus Syntax File: IRT_LI_lowerEchelon3D_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_LI_lowerEchelon3D_Qmatrix.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_E_3D_LI = 
  readModels(target = "IRT_LI_lowerEchelon3D_Qmatrix.out", recursive = FALSE, quiet = FALSE)


# Exploratory 2PL IRT model with Lower-Echelon Q-matrix two dimensions
# Mplus Syntax File: IRT_LI_lowerEchelon2D_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_LI_lowerEchelon2D_Qmatrix.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_E_2D_LI = 
  readModels(target = "IRT_LI_lowerEchelon2D_Qmatrix.out", recursive = FALSE, quiet = FALSE)

# Exploratory 2PL IRT model with Correct (from simulated data) Q-matrix
# Mplus Syntax File: IRT_LI_correct_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_LI_correct_Qmatrix.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_C_2D_LI = 
  readModels(target = "IRT_LI_correct_Qmatrix.out", recursive = FALSE, quiet = FALSE)

# Unidimensional 2PL IRT model
# Mplus Syntax File: IRT_LI_unidimensional_Qmatrix.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_LI_unidimensional_Qmatrix.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
IRTmodel_C_1D_LI = 
  readModels(target = "IRT_LI_unidimensional_Qmatrix.out", recursive = FALSE, quiet = FALSE)


# Exploratory Factor Analysis (using Mplus defaults)
# Mplus Syntax File: IRT_LI_EFA.inp

# run Mplus (data file is same as previous)
mplusRun = 
  runModels(target = "IRT_LI_EFA.inp", recursive = FALSE, quiet = FALSE, showOutput = TRUE)

# import Mplus output
EFAmodel_LI = 
  readModels(target = "IRT_LI_EFA.out", recursive = FALSE, quiet = FALSE)
  
