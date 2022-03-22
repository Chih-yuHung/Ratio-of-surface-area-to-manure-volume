#This file is to calculate boostrapped confidence interval for the average 
#ratio of manure surface to volume

library(boot)

ratio<-read.csv("Manure Linear Regression.csv")
ratio<-ratio[-6,] #Look likes an outlier
#do the bootstrap
set.seed(2022)
SV.n<-30 #number of samples we want to boostrap everytime
B<-10000 #bootstrap sample size
#Obtain the boostrap samples
Boot.SV<-matrix(sample(ratio$SV,size=B*SV.n,
                replace=TRUE),ncol=B,nrow=SV.n)
#Obtain the mean of each bootstraps
Boot.SV.mean<-colMeans(Boot.SV)
 
#Obtain the percentile
Boot.low<-quantile(Boot.SV.mean,prob=0.025,names=FALSE) #0.3903
Boot.up<-quantile(Boot.SV.mean,prob=0.975,names=FALSE) #0.4173

#We're going to apply this to the linear regression 
#and obtain 95% CI for the temperature difference
temp.lm<-lm(dif~SV,data=ratio)
new<-data.frame(SV=c(Boot.low,Boot.up))
temp.predict<-predict.lm(temp.lm,new,interval="confidence")
#obtain possible difference of the temperature
temp.dif<-c(temp.predict[1,2],temp.predict[2,3])#it's the lowest dif and highest dif


#Now we can use the MCF calculator to see the change of MCF


