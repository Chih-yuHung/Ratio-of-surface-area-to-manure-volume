#To plot the ditribution of plausible MCFs in Canada
#The data is from 10000 bootstrapped calibrated temperature 
#in 3403 locations in 2019. so there is 34,030,000 data
library(ggplot2)
#Obtain data, the data were separate to 5 rda (MCF2019-1~5.rda) 
load("MCF2019-1.rda")
MCF2019<-MCF.2019.p
rm(MCF.2019.p)
for (i in 2:5) {
  load(paste("MCF2019-",i,".rda",sep=""))
  MCF2019<-c(MCF2019,MCF.2019.p)
}

#Turn list data to vector
MCF2019.vector<-as.numeric(MCF2019[c(1:length(MCF2019))])
summary(MCF2019.vector) #min =0.110, median=0.204,mean 0.203
                        #max = 0.322
MCF2019.low<-quantile(MCF2019.vector,prob=0.025,names=FALSE) #0.145
MCF2019.high<-quantile(MCF2019.vector,prob=0.975,names=FALSE) #0.275
MCF2019.density<-density(MCF2019.vector)

par(mar=c(4,5,1,4))
hist(MCF2019.vector,freq=FALSE,main=NA,
     xlab="Methane conversion factor (MCF)",
     ylab="Probability density (%)",
     xlim=c(0.1,0.35),ylim=c(0,15),cex.lab=1.5,
     col=NA)
lines(MCF2019.density,lwd=2,col="black")
text(0.1,15,"(b)",pos=1,cex=1.5)
