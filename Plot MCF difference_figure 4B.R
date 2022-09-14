#To plot the ditribution of plausible MCFs in Canada
#The data is from 10000 bootstrapped calibrated temperature 
#in 3403 locations in 2019. so there is 34,030,000 data

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
summary(MCF2019.vector)
hist(MCF2019.vector,freq=FALSE,)

aa<-as.numeric(MCF.2019.p[c(1:length(MCF.2019.p))])
summary(aa)
