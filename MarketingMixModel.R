# http://datafeedtoolbox.com/marketing-mix-model-for-all-using-r-for-mmm/
# Load libraries #
library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)

#  Load data files  #
fileName = "oneChannelData.csv"
myData = read.csv(file = fileName, header=TRUE, sep=",")


#  Plot data  #
channelName = as.character(myData$Channel[1])
maxX = 1.05*max(myData$Spend)
maxY = 1.05*max(myData$Return)

myPlotDataDF = data.frame(Return = myData$Return, Spend = myData$Spend)

simpleScatterPlot <- ggplot(myPlotDataDF, aes(x = Spend, y = Return)) +
  geom_point(color="black") +
  theme(panel.background = element_rect(fill = 'grey85'),
        panel.grid.major = element_line(colour = "white")) +
  coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = comma) + 
  ggtitle(paste(channelName))

simpleScatterPlot


# To align with how the nlminb() function operates, we need to create a custom function
# that returns the total observed error based on the ADBUDG model

Ufun<-function(x, Spend, Return) {
  predictedReturn = x[2] + (x[1] - x[2])*((Spend^x[3])/(x[4] + (Spend^x[3])))
  errorSq = (predictedReturn - Return)^2
  sumSqError = sum(errorSq)
  return(sumSqError)
}

startValVec = c(25000,100,1.5,100000)
minValVec = c(0,0,1.01,0)
maxValVec = c(500000, 500000, 2, 10000000)

optim.parms<-nlminb(objective=Ufun,start=startValVec,
                    lower=minValVec,
                    upper=maxValVec,
                    control=list(iter.max=100000,eval.max=2000),
                    Spend = myData$Spend,
                    Return = myData$Return)

optim.parms

# A convergence code of 0 indicates successful convergence.  
# We can extract our parameter values by taking a look at optim.parms$par. 

a = optim.parms$par[1]
b = optim.parms$par[2]
c = optim.parms$par[3]
d = optim.parms$par[4]

curveDFx = seq(from=0, to=max(myData$Spend)*2, length.out=10000)
curveDFy = b+(a-b)*((curveDFx^c)/(d+(curveDFx^c)))
curveDF = data.frame(Spend = curveDFx, Return = curveDFy)

maxX = 1.05*max(curveDFx, max(myData$Spend))
maxY = 1.05*max(curveDFy, max(myData$Return))

myPlotDataDF = data.frame(Return = myData$Return, Spend = myData$Spend)
optimLineDF = data.frame(Spend = curveDFx, Return = curveDFy)

scatterPlotPlusFit <- ggplot(myPlotDataDF, aes(x = Spend, y = Return)) +
  geom_point(color="black", shape = 16) +
  theme(panel.background = element_rect(fill = 'grey85'),
        panel.grid.major = element_line(colour = "white")) +
  geom_line(data = optimLineDF, aes(x = Spend, y = Return, color = "darkgreen"))  +
  scale_color_manual(labels = "Optimized ADBUDG Fit",values=c('darkgreen')) +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = comma) + 
  ggtitle(paste(channelName, "Data & Model Fit", sep = " "))

scatterPlotPlusFit


adbudgReturn = function(a,b,c,d,Spend){
  adbudgReturn = sum(b+(a-b)*((Spend^c)/(d+(Spend^c))))
  return(adbudgReturn)
}

returnGoal = 600000
increment = 1000
oldSpendVec = myData$Spend
oldReturn = adbudgReturn(a,b,c,d,oldSpendVec)
newSpendVec = oldSpendVec

totalSpend = sum(oldSpendVec)
totalReturn = oldReturn


while(totalReturn < returnGoal){
  incReturns = NULL
  for(i in 1:length(oldSpendVec)){
    oldSpendTemp = newSpendVec[i]
    newSpendTemp = newSpendVec[i] + increment
    
    oldReturnTemp = b+(a-b)*((oldSpendTemp^c)/(d+(oldSpendTemp^c)))
    newReturnTemp = b+(a-b)*((newSpendTemp^c)/(d+(newSpendTemp^c)))
    
    incReturns[i] = newReturnTemp - oldReturnTemp
    
  }
  
  winner = which.max(incReturns)
  newSpendVec[winner] = newSpendVec[winner] + increment
  
  totalSpend = totalSpend + increment
  totalReturn = adbudgReturn(a,b,c,d,newSpendVec)
  
}


newReturnVec = b+(a-b)*((newSpendVec^c)/(d+(newSpendVec^c)))
myRecommendedData = data.frame(Campaign = myData$Campaign,
                               Channel = myData$Channel,
                               Return = newReturnVec,
                               Spend = newSpendVec)

sum(myRecommendedData$Spend) # Recommended Spend
sum(myRecommendedData$Return)  # Estimated Return from Recommended Spend
sum(myRecommendedData$Spend)/sum(myData$Spend) - 1  # % Increase in Spend to get $600K





#  Graph current spend vs recommended spend  #
compareDF = data.frame(Campaign = rep(myData$Campaign,2), spendType = rep(c("Actual Spend","Recommended Spend"), each=dim(myData)[1]), Spend = c(myData$Spend, myRecommendedData$Spend))

barChart <- ggplot(data=compareDF, aes(x=Campaign, y=Spend, fill=spendType)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  scale_fill_manual(values=c('darkred','darkblue'),
                    name = "") +
  scale_y_continuous(name="Spend", labels = dollar) +
  theme(axis.text.x = element_text(angle = 45, hjust = .75)) +
  ggtitle("Breakdown of Spend by Campaign")

barChart


# A table format would be another simple, but useful visualization.  
# For this representation of the data, I will add a percentage difference 
# measure to show how spend changed between my original and recommended spend amounts:

percDiff = (myRecommendedData$Spend - myData$Spend)/myData$Spend
summaryDF = data.frame(Campaign = myRecommendedData$Campaign, 
                       Channel = myRecommendedData$Channel, 
                       actualSpend = dollar_format()(myData$Spend), 
                       recommendedSpend = dollar_format()(myRecommendedData$Spend),
                       percDiff = percent((myRecommendedData$Spend - myData$Spend)/myData$Spend))

summaryDF
