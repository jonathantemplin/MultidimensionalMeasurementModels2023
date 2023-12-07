if (!require(remotes)) install.packages("remotes")
library(remotes)

# install blatent from JT's github
remotes::install_github(repo = "https://github.com/jonathantemplin/blatent/tree/blatent-0.1.2-RC-1")
library(blatent)

#ANALYZE EXAMPLE DATA FILE ------------------------------------------------------------------------------

#read in original data and create id variable
data = read.table(file = "dtmrdemo.dat")
paste("X", rep(1:27), sep="")
colnames(data) = c(paste("X", rep(1:27), sep=""), "TrueClass")
data$id = 1:dim(data)[1]

#read in qmatrix file
qmatrix = read.csv("qmatrix.csv", header = FALSE)
qmatrix = qmatrix[,2:5]
rownames(qmatrix) = paste0("X", 1:27)

cat(QmatrixToBlatentSyntax(Qmatrix = qmatrix))

modelEstimates = blatentEstimate(
  dataMat = data,
  modelText = QmatrixToBlatentSyntax(Qmatrix = qmatrix), 
  options = blatentControl(
    nBurnin = 1000,
    nChains = 4,
    nCores = 4, 
    nSampled = 1000,
    nThin = 1,
    parallel = TRUE
  )
)

modelEstimates$summary()

save(modelEstimates, file = "blatentModel.RData")
save.image("dump.RData")
