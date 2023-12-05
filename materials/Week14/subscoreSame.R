library(mirtCAT)
library(ggplot2)
library(reshape2)

#generate data
ICC = function(theta, a, b, c){
  prob = c + (1-c)*exp(a*(theta-b))/(1+exp(a*(theta-b)))
  return(prob)
}

IRTdataN1 = function(theta, a, b, c){
  prob = ICC(theta = theta, a = a, b = b, c = c)
  data = matrix(rbinom(n = length(a), size = 1, prob = prob),nrow=1)
  return(data)
}

IRTdataALL = function(theta, a, b, c){
  tdata = sapply(X = theta, FUN = IRTdataN1, simplify = TRUE, a, b, c)
  data = t(tdata)
  return(data)
}

IRTtestInfo = function(theta, a, b, c){
  #one theta and all a,b,c
  prob = ICC(theta = theta, a = a, b = b, c = c)
  iteminfo = (a^2)*((prob-c)^2/(1-c)^2)*(1-prob)/prob
  testinfo = sum(iteminfo)
  return(testinfo)
}


nScores = 4

minItemsPerSub = 3
maxItemsPerSub = 50
ItemsStep = 1

steps = seq(minItemsPerSub, maxItemsPerSub, ItemsStep)

nSamplesPerStep = 1
sampledThetas = c(1, -1)

subscoreItemDiffMethod = "random" #eventually use ordered
samplingError = FALSE #estimate parameters?
modelError = FALSE #generate from different model?

itemMeasurementError = FALSE #FALSE == generate so that parameters have very high discrimination values and zero guessing
step = 100
thetaEstimates = NULL
step = 3
for (step in steps){
  print(step)
  nItemsScore = rep(step, nScores)
  nItemsTotal = sum(nItemsScore)
  
  # step 1: generate item parameters
  if (!itemMeasurementError){
    aMax = 10
    aMin = 10
    cMax = 0
    cMin = 0
    uMax = 1
    uMin = 1
  }
  
  if (subscoreItemDiffMethod == "random"){
    bMax = 2.5
    bMin = -2.5
  } else if (subscoreItemDiffMethod == "ordered"){
    bRange = 2 - -2
    bStep = bRange/(nScores)
    bMin = -2
  }
  
  
  itemParams = vector("list", nScores+1)
  mirtParams = vector("list", nScores+1)
  sub = 1
  
  #estimate theta values
  resNitems = nItemsScore
  resTrueTheta1 = sampledThetas[1]
  resTrueTheta2 = sampledThetas[2]
  
  
  
  for (sub in 1:nScores){
    
    itemParams[[sub]]$a = runif(n = nItemsScore[sub], min = aMin, max = aMax)
    
    if (subscoreItemDiffMethod == "random"){
      itemParams[[sub]]$b = runif(n = nItemsScore[sub], min = bMin, max = bMax)  
    } else if (subscoreItemDiffMethod == "ordered"){
      bMinSub = bMin+(bStep*(sub-1))
      bMaxSub = bMin+(bStep*(sub))
      itemParams[[sub]]$b = runif(n = nItemsScore[sub], min = bMinSub, max = bMaxSub)
    }
    
    itemParams[[sub]]$c = runif(n = nItemsScore[sub], min = cMin, max = cMax)
    itemParams[[sub]]$u = runif(n = nItemsScore[sub], min = uMin, max = uMax)
    itemParams[[sub]]$mirt = data.frame(a1=itemParams[[sub]]$a, d = -1*itemParams[[sub]]$a*itemParams[[sub]]$b,
                                        g = itemParams[[sub]]$c, u = itemParams[[sub]]$u)
    
    mirtParams[[sub]] = generate.mirt_object(parameters = itemParams[[sub]]$mirt, itemtype = "3PL")
    
    itemParams[[nScores+1]]$a = c(itemParams[[nScores+1]]$a, itemParams[[sub]]$a)
    itemParams[[nScores+1]]$b = c(itemParams[[nScores+1]]$b, itemParams[[sub]]$b)
    itemParams[[nScores+1]]$c = c(itemParams[[nScores+1]]$c, itemParams[[sub]]$c)
    itemParams[[nScores+1]]$u = c(itemParams[[nScores+1]]$u, itemParams[[sub]]$u)
    
  }
  
  itemParams[[nScores+1]]$mirt = data.frame(a1=itemParams[[nScores+1]]$a, d = -1*itemParams[[nScores+1]]$a*itemParams[[nScores+1]]$b,
                                            g = itemParams[[nScores+1]]$c, u = itemParams[[sub]]$u)
  mirtParams[[nScores+1]] = generate.mirt_object(parameters = itemParams[[nScores+1]]$mirt , itemtype = "3PL")
  
  #generate data
  for (i in 1:nSamplesPerStep){
    dat = IRTdataALL(theta = sampledThetas, a = itemParams[[nScores+1]]$a, b = itemParams[[nScores+1]]$b, itemParams[[nScores+1]]$c)
    sub=1
    estThetas = rep(NA, (nScores+1)*length(sampledThetas))
    estSEs = rep(NA, (nScores+1)*length(sampledThetas))
    estMAXCI = rep(NA, (nScores+1)*length(sampledThetas))
    estMINCI = rep(NA, (nScores+1)*length(sampledThetas))
    for (sub in 1:(nScores+1)){
      if (sub != (nScores+1)){
        dataStart = nItemsScore[sub]*(sub-1)+1
        dataEnd = nItemsScore[sub]*(sub-1)+nItemsScore[sub]  
      } else {
        dataStart = 1
        dataEnd = ncol(dat)
      }
      
      mirtEst = data.frame(fscores(object = mirtParams[[sub]], response.pattern = dat[,dataStart:dataEnd]))
      
      
      for (samp in 1:length(sampledThetas)){
        
        estThetas[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = mirtEst$F1[samp]
        estSEs[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = mirtEst$SE_F1[samp]
        estMAXCI[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = mirtEst$F1[samp]+1.96*mirtEst$SE_F1[samp]
        estMINCI[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = mirtEst$F1[samp]-1.96*mirtEst$SE_F1[samp]
        if (sub != (nScores+1)){
          names(estThetas)[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = paste0("THETA", samp, "subscore", sub)
          names(estSEs)[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = paste0("THETA", samp, "subscoreSE", sub)
          names(estMAXCI)[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = paste0("THETA", samp, "subscoreMAXCI", sub)
          names(estMINCI)[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = paste0("THETA", samp, "subscoreMINCI", sub)
        } else {
          names(estThetas)[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = paste0("THETA", samp, "overall")
          names(estSEs)[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = paste0("THETA", samp, "overallSE")
          names(estMAXCI)[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = paste0("THETA", samp, "overallMAXCI")
          names(estMINCI)[length(sampledThetas)*sub-(length(sampledThetas)-samp)] = paste0("THETA", samp, "overallMINCI")
        }
        
      }
      
    }
    
    thetaEstimates = rbind(thetaEstimates, data.frame(resNitems = resNitems[1], resTrueTheta1 = resTrueTheta1, resTrueTheta2 = resTrueTheta2, 
                                                      data.frame(t(estThetas)), data.frame(t(estSEs)), data.frame(t(estMAXCI)), data.frame(t(estMINCI))))
    
  }
  
  
}

temp = thetaEstimates[c("resNitems", "THETA1subscore1", "THETA1subscore2", "THETA1subscore3", "THETA1subscore4")]
names(temp) = c("resNitems", "Score1", "Score2", "Score3", "Score4")
plotdata = melt(temp, id.var = "resNitems")



p1 = ggplot(plotdata, aes(x = resNitems, y = value, color = variable, linetype = variable, group = variable)) +
  geom_smooth(se = FALSE, size=5, method = "loess") + geom_point(size = 2) +
  scale_color_manual(values=c("navyblue", "red4", "gold4", "darkviolet"), name = "Subtest Scores", labels = c("Subtest Score 1", "Subtest Score 2", "Subtest Score 3", "Subtest Score 4")) + 
  scale_linetype_discrete(labels = c("Subtest Score 1", "Subtest Score 2", "Subtest Score 3", "Subtest Score 4"), 
                          name = "Subtest Scores") + 
  labs(title = "True Score: 1", x = "Items Per Subtest", y = "Estimated Score") + geom_hline(yintercept = 1, size = 1, lty = 2) +
  theme(legend.position = "bottom")

temp2 = thetaEstimates[c("resNitems", "THETA2subscore1", "THETA2subscore2", "THETA2subscore3", "THETA2subscore4")]
names(temp2) = c("resNitems", "Score1", "Score2", "Score3", "Score4")
plotdata2 = melt(temp2, id.var = "resNitems")

p2 = ggplot(plotdata2, aes(x = resNitems, y = value, color = variable, linetype = variable, group = variable)) +
  geom_smooth(se = FALSE, size=5, method = "loess", show.legend = TRUE) + geom_point(size = 2, show.legend = TRUE) +
  scale_color_manual(values=c("navyblue", "red4", "gold4", "darkviolet"), name = "Subtest Scores", labels = c("Subtest Score 1", "Subtest Score 2", "Subtest Score 3", "Subtest Score 4")) + 
  scale_linetype_discrete(labels = c("Subtest Score 1", "Subtest Score 2", "Subtest Score 3", "Subtest Score 4"), 
                          name = "Subtest Scores") + 
  labs(title = "True Score: -1", x = "Items Per Subtest", y = "Estimated Score") + geom_hline(yintercept = -1, size = 1, lty = 2) + 
  theme(legend.position = "bottom")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

save.image("subscoreSame.RData")
png("subscoreSame.png", width = 1000, height = 500)
multiplot(p1, p2, cols = 2)
dev.off()
