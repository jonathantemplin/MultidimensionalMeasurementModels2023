
if (!require(psych)) install.packages("psych")
library(psych)


structuralModel = read.csv(file = "structuralModelEstimates.csv")

# summarizing via tetrachoric correlations:

tetrachoric(x = structuralModel[,paste0("Att", 1:4)], weight = structuralModel$Prob)

tetrachoricResults = tetrachoric(x = structuralModel[,paste0("Att", 1:4)], weight = structuralModel$Prob)

# showing the proportion for each attribute
1-pnorm(tetrachoricResults$tau)

# other structural models

# loglinear model--need counts to show in R:
structuralModel$ClassN = round(990*structuralModel$Prob)
summary(glm(formula = ClassN ~ 0+ Att1*Att2*Att3*Att4, data = structuralModel, family = poisson))
