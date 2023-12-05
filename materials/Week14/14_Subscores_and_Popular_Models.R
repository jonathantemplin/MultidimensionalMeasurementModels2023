# clear workspace =====================================================================================================
rm(list = ls())

# load libraries ======================================================================================================
if (!require("lavaan")) {
  install.packages("lavaan")
  library(lavaan)
}

if (!require("semPlot")) {
  install.packages("semPlot")
  library(semPlot)
}

if (!require("mirtCAT")) {
  install.packages("mirtCAT")
  library(mirtCAT)
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("reshape2")) {
  install.packages("reshape2")
  library(reshape2)
}


# load data ===========================================================================================================
load("9-10th_grade_q2.RData")
responseMatrix = temp$responseMatrix
Qmatrix = temp$Qmatrix

# create Q-matrix that only has Algebra standards
reducedQ = Qmatrix[which(rownames(Qmatrix) %in% paste0("item", 11:30)), ]
reducedQ = reducedQ[, which(colSums(reducedQ) > 0)]


# using only Algebra items (11:30) ====================================================================================
dataMat = responseMatrix[paste0("item", 11:30)]

# remove cases that are all NA ========================================================================================
dataMat = dataMat[apply(dataMat, 1, function(x) !all(is.na(x))), ]

# create lavaan model for unidimensional model ========================================================================

names(dataMat)

unidimensionalModel_syntax = "

theta =~ item11 + item12 + item13 + item14 + item15 + 
         item16 + item17 + item18 + item19 + item20 + 
         item21 + item22 + item23 + item24 + item25 + 
         item26 + item27 + item28 + item29 + item30

"

unidimensionalModel = cfa(
  model = unidimensionalModel_syntax, 
  data = dataMat, 
  estimator = "WLSMV", 
  ordered = paste0("item", 11:30), 
  std.lv = TRUE, 
  parameterization = "theta"
)

summary(unidimensionalModel, fit.measures = TRUE, standardized = TRUE)

unidimensionalScores = lavPredict(unidimensionalModel)

semPlot::semPaths(unidimensionalModel, sizeMan = 2.5, style = "mx", intercepts = FALSE, thresholds = FALSE)

# subscores with irt model estimates ===================================================================================

# source("subscoreSame.R") # Note: this file take some time to run

# subscores with unidimensional model ==================================================================================
# type 2: sum-score based subscores


sumScores = as.matrix(dataMat) %*% as.matrix(reducedQ)

# problem with sumscores: missing data 
head(sumScores)

# resolution: make an average score for each student
averageScores = nItems = matrix(0, nrow = nrow(sumScores), ncol = ncol(sumScores))

for (person in 1:nrow(dataMat)){
  for (col in 1:ncol(reducedQ)){
    for (row in 1:nrow(reducedQ)){
      if (reducedQ[row, col] == 1 && !is.na(dataMat[person, row])){
        averageScores[person, col] = averageScores[person, col] + dataMat[person, row]
        nItems[person, col] = nItems[person, col] + 1
      }   
    }
    averageScores[person, col] = averageScores[person, col] / nItems[person, col]
  }
}

head(averageScores)

# using lavaan to mirror parallel items sumscore-based subscores =======================================================


parallelItems_syntax = "

# constrain all loadings to be equal within each subscore
theta1 =~ L1*item11 + L1*item12 + L1*item13 + L1*item14 + L1*item15
theta2 =~ L2*item16 + L2*item17 + L2*item18 + L2*item19 + L2*item20
theta3 =~ L3*item21 + L3*item22 + L3*item23 + L3*item24 + L3*item25
theta4 =~ L4*item26 + L4*item27 + L4*item28 + L4*item29 + L4*item30

# set the unique variances to be equal within each subscore
item11 ~~ U1*item11; item12 ~~ U1*item12; item13 ~~ U1*item13; item14 ~~ U1*item14; item15 ~~ U1*item15;
item16 ~~ U2*item16; item17 ~~ U2*item17; item18 ~~ U2*item18; item19 ~~ U2*item19; item20 ~~ U2*item20;
item21 ~~ U3*item21; item22 ~~ U3*item22; item23 ~~ U3*item23; item24 ~~ U3*item24; item25 ~~ U3*item25;
item26 ~~ U4*item26; item27 ~~ U4*item27; item28 ~~ U4*item28; item29 ~~ U4*item29; item30 ~~ U4*item30;

# set the item intercepts to be equal within each subscore
item11 ~ I1*1; item12 ~ I1*1; item13 ~ I1*1; item14 ~ I1*1; item15 ~ I1*1;
item16 ~ I2*1; item17 ~ I2*1; item18 ~ I2*1; item19 ~ I2*1; item20 ~ I2*1;
item21 ~ I3*1; item22 ~ I3*1; item23 ~ I3*1; item24 ~ I3*1; item25 ~ I3*1;
item26 ~ I4*1; item27 ~ I4*1; item28 ~ I4*1; item29 ~ I4*1; item30 ~ I4*1;

# set all covariances to zero
theta1 ~~ 0*theta2
theta1 ~~ 0*theta3
theta1 ~~ 0*theta4
theta2 ~~ 0*theta3
theta2 ~~ 0*theta4
theta3 ~~ 0*theta4


"

parallelItemsModel = cfa(
  model = parallelItems_syntax, 
  data = dataMat, 
  estimator = "MLR",
  std.lv = TRUE, 
)

summary(parallelItemsModel, fit.measures = TRUE, standardized = TRUE)

parallelItemsScores = lavPredict(parallelItemsModel)

# plotting model scores with sumscores
par(mfrow = c(2,2))
for (standard in 1:ncol(reducedQ)){
  plot(x = parallelItemsScores[, standard],  y = sumScores[, standard], 
      main = "Parallel Items Model Score vs. Sum Score", 
      xlab = paste0("Parallel Items Model Score: ", colnames(reducedQ)[standard]), 
      ylab = paste0("Sum Score: ", colnames(reducedQ)[standard])
  )
}
par(mfrow = c(1,1))

# create lavaan model for multidimensional model =======================================================================


multidimensionalModel_syntax = "

theta1 =~ item11 + item12 + item13 + item14 + item15
theta2 =~ item16 + item17 + item18 + item19 + item20
theta3 =~ item21 + item22 + item23 + item24 + item25
theta4 =~ item26 + item27 + item28 + item29 + item30

"


multidimensionalModel = cfa(
  model = multidimensionalModel_syntax, 
  data = responseMatrix, 
  estimator = "WLSMV", 
  ordered = paste0("item", 11:30), 
  std.lv = TRUE, 
  parameterization = "theta"
)

summary(multidimensionalModel, fit.measures = TRUE, standardized = TRUE)

semPlot::semPaths(multidimensionalModel, sizeMan = 2.5, style = "mx", intercepts = FALSE, thresholds = FALSE)




anova(unidimensionalModel, multidimensionalModel)

# comparing estimated factor scores between models =====================================================================

totalMIRTScores = rowSums(multidimensionalScores)
plot(x = unidimensionalScores, y = totalMIRTScores, 
     main = "Unidimensional Score vs. Sum of Multidimensional Scores")
cor(x = unidimensionalScores, y = totalMIRTScores)


# creating model for sum of multidimensional scores ====================================================================

summedMIRT_syntax = "

# constrain all loadings to be equal
total =~ L1*theta1 + L1*theta2 + L1*theta3 + L1*theta4

# set the unique variances to be equal
theta1 ~~ U1*theta1; theta2 ~~ U1*theta2; theta3 ~~ U1*theta3; theta4 ~~ U1*theta4;

# set the item intercepts to be equal
theta1 ~ I1*1; theta2 ~ I1*1; theta3 ~ I1*1; theta4 ~ I1*1;

"

summedMIRTModel = cfa(
  model = summedMIRT_syntax, 
  data = multidimensionalScores, 
  estimator = "MLR",
  std.lv = TRUE, 
)

summary(summedMIRTModel, fit.measures = TRUE, standardized = TRUE)

summedMIRTScores = lavPredict(summedMIRTModel)
plot(x = summedMIRTScores, y = totalMIRTScores, 
     main = "Summed MIRT Score vs. Sum of Multidimensional Scores")

# create lavaan model for bifactor model ===============================================================================


bifactorModel_syntax = "

g =~ item11 + item12 + item13 + item14 + item15 + 
     item16 + item17 + item18 + item19 + item20 + 
     item21 + item22 + item23 + item24 + item25 + 
     item26 + item27 + item28 + item29 + item30
         
theta1 =~ item11 + item12 + item13 + item14 + item15
theta2 =~ item16 + item17 + item18 + item19 + item20
theta3 =~ item21 + item22 + item23 + item24 + item25
theta4 =~ item26 + item27 + item28 + item29 + item30

g ~~ 0*theta1
g ~~ 0*theta2
g ~~ 0*theta3
g ~~ 0*theta4


"

bifactorModel = cfa(
  model = bifactorModel_syntax, 
  data = responseMatrix, 
  estimator = "WLSMV", 
  ordered = paste0("item", 11:30), 
  std.lv = TRUE, 
  parameterization = "theta"
)



summary(bifactorModel, fit.measures = TRUE, standardized = TRUE)

anova(unidimensionalModel, multidimensionalModel, bifactorModel)

semPlot::semPaths(bifactorModel, sizeMan = 2.5, style = "mx", intercepts = FALSE, thresholds = FALSE)

# score comparisons with bifactor model and other models ===============================================================

bifactorScores = lavPredict(bifactorModel)

plot(x = unidimensionalScores, y = bifactorScores[, "g"], 
     main = "Unidimensional Score vs. Bifactor Score")

par(mfrow = c(2,2))
plot(x = multidimensionalScores[, 1], y = bifactorScores[, "theta1"],
     main = "Multidimensional Score vs. Bifactor Score for Theta 1")
plot(x = multidimensionalScores[, 2], y = bifactorScores[, "theta2"],
     main = "Multidimensional Score vs. Bifactor Score for Theta 1")
plot(x = multidimensionalScores[, 3], y = bifactorScores[, "theta3"],
     main = "Multidimensional Score vs. Bifactor Score for Theta 1")
plot(x = multidimensionalScores[, 4], y = bifactorScores[, "theta4"],
     main = "Multidimensional Score vs. Bifactor Score for Theta 1")
par(mfrow = c(1,1))





# create lavaan model for higher order model ===========================================================================


higherOrderModel_syntax = "

theta1 =~ item11 + item12 + item13 + item14 + item15
theta2 =~ item16 + item17 + item18 + item19 + item20
theta3 =~ item21 + item22 + item23 + item24 + item25
theta4 =~ item26 + item27 + item28 + item29 + item30

algebra =~ theta1 + theta2 + theta3 + theta4

"

higherOrderModel = cfa(
  model = higherOrderModel_syntax, 
  data = responseMatrix, 
  estimator = "WLSMV", 
  ordered = paste0("item", 11:30), 
  std.lv = TRUE, 
  parameterization = "theta"
)

summary(higherOrderModel, fit.measures = TRUE, standardized = TRUE)

anova(unidimensionalModel, multidimensionalModel, higherOrderModel)

semPlot::semPaths(higherOrderModel, sizeMan = 2.5, style = "mx", intercepts = FALSE, thresholds = FALSE)

# score comparisons with higher order model and other models ===========================================================
higherOrderScores = lavPredict(higherOrderModel)

plot(x = unidimensionalScores, y = higherOrderScores[, "algebra"], 
     main = "Unidimensional Score vs. Higher Order Score")

par(mfrow = c(2,2))

plot(x = multidimensionalScores[, 1], y = higherOrderScores[, "theta1"],
     main = "Multidimensional Score vs. Higher Order Score for Theta 1")
plot(x = multidimensionalScores[, 2], y = higherOrderScores[, "theta2"],
      main = "Multidimensional Score vs. Higher Order Score for Theta 2")
plot(x = multidimensionalScores[, 3], y = higherOrderScores[, "theta3"],
      main = "Multidimensional Score vs. Higher Order Score for Theta 3")
plot(x = multidimensionalScores[, 4], y = higherOrderScores[, "theta4"],
      main = "Multidimensional Score vs. Higher Order Score for Theta 4")

par(mfrow = c(1,1))

# improving the higher order model =====================================================================================

modificationIndices(higherOrderModel)


higherOrderModel_syntax2 = "

theta1 =~ item11 + item12 + item13 + item14 + item15
theta2 =~ item16 + item17 + item18 + item19 + item20
theta3 =~ item21 + item22 + item23 + item24 + item25
theta4 =~ item26 + item27 + item28 + item29 + item30

algebra =~ theta1 + theta2 + theta3 + theta4

theta1 ~~ theta3

"

higherOrderModel2 = cfa(
  model = higherOrderModel_syntax2, 
  data = responseMatrix, 
  estimator = "WLSMV", 
  ordered = paste0("item", 11:30), 
  std.lv = TRUE, 
  parameterization = "theta"
)

summary(higherOrderModel, fit.measures = TRUE, standardized = TRUE)

anova(unidimensionalModel, multidimensionalModel, higherOrderModel)