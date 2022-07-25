#This file is to calculate boostrapped confidence interval for the average 
#ratio of manure surface to volume

library(boot)

ratio<-read.csv("Manure Linear Regression.csv")
ratio<-ratio[c(-6,-9,-10:-13),] #Look likes an outlier
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
#Boot.low<-quantile(Boot.SV.mean,prob=0.025,names=FALSE) #0.3903
#Boot.up<-quantile(Boot.SV.mean,prob=0.975,names=FALSE) #0.4173

#We're going to apply this to the linear regression 
#and obtain 95% CI for the temperature difference
temp.lm<-lm(dif~SV,data=ratio)
new<-data.frame(SV=Boot.SV.mean)
temp.predict<-predict.lm(temp.lm,new,interval="confidence")
#obtain possible difference of the temperature
low<-quantile(temp.predict[,2],prob=0.025,names=FALSE) #-3.398
up<-quantile(temp.predict[,3],prob=0.975,names=FALSE) #0.720
temp.dif<-c(low,up)#it's the lowest dif and highest dif


#Now we can use the MCF calculator to see the change of MCF

T.avg.m<-read.table("monthly avg T.txt",header=TRUE) # monthly average

#This is an R procedure example how to calculate MCF for different locations. 
library(weathermetrics) #convert C to K

#Required input, which don't influence MCF
VS_Yr<-1200  #kg/yr
VS_LQD<-100  #100%
#constants, these inputs are fixed input in the 2019 IPCC Refinement
f_T1<-308.16 #K
f_Ea<-19347  #cal/mol
f_R<-1.987   #cal/K.mol
B0<-0.24     #dimensionless
#Optional inputs, which influence MCF
f_Tmin<-1.0  #degree C
f_T2d<-3.0   #T2 damping, degree C
E_eff<-95    #95%


#Calculate the MCF for the input locations
#We're runing two scenarios, low (-3.398) and high (+0.720) 
#under the two removal scenario, early spring-early fall
#Obtain removal scenarios from my STOTEN study
removal<-read.csv("removal month.csv",header=TRUE)

#Calculate the MCF for the input locations
#We're runing two scenarios, low (-3.398) and high (+0.720) 
#under the two removal scenario, early spring-early fall
max.month<-apply(T.avg.m,2,which.max)
T.avg.low<-T.avg.m
T.avg.high<-T.avg.m

#Apply the temperature difference to the max three months
for(i in length(T.avg.m)) {
T.avg.low[c(max.month[i]-1,max.month[i],max.month[i]+1),]<-T.avg.m[c(max.month[i]-1,max.month[i],max.month[i]+1),]+temp.dif[1]
T.avg.high[c(max.month[i]-1,max.month[i],max.month[i]+1),]<-T.avg.m[c(max.month[i]-1,max.month[i],max.month[i]+1),]+temp.dif[2]
}
# #Apply the temperature difference to the max months only
# for(i in length(T.avg.m)) {
#   T.avg.low[max.month[i],]<-T.avg.m[max.month[i],]+temp.dif[1]
#   T.avg.high[max.month[i],]<-T.avg.m[max.month[i],]+temp.dif[2]
# }


#MCF.low is the low temperature difference
MCF.low<-c()
for (k in 1:3403){
  rm<-rep(0,12)
  rm[c(removal[k,2],removal[k,4])]<-1
  M.rm<-rm
  Manure.rm<-rep(M.rm,3) # for stabilization 3 yr
  T.sel<-T.avg.low[,k]
  print(paste("Station sequence",k))
  source("MCF calculator_single.R",echo = F)
  MCF.low[k]<-MCF
}

summary(MCF.low) #min:0.1, mean:0.167, max 0.26

#MCF.high is the high temperature difference
MCF.high<-c()
for (k in 1:3403){
  rm<-rep(0,12)
  rm[c(removal[k,2],removal[k,4])]<-1
  M.rm<-rm
  Manure.rm<-rep(M.rm,3) # for stabilization 3 yr
  T.sel<-T.avg.high[,k]
  print(paste("Station sequence",k))
  source("MCF calculator_single.R",echo = F)
  MCF.high[k]<-MCF
}

summary(MCF.high) #min:0.11, mean:0.214, max 0.33

#Use the unmodified temperature to calculate MCF 
MCF.2<-c()
for (k in 1:3403){
  rm<-rep(0,12)
  rm[c(removal[k,2],removal[k,4])]<-1
  M.rm<-rm
  Manure.rm<-rep(M.rm,3) # for stabilization 3 yr
  T.sel<-T.avg.m[,k]
  print(paste("Station sequence",k))
  source("MCF calculator_single.R",echo = F)
  MCF.2[k]<-MCF
}
summary(MCF.2) #min:0.11, mean:0.204, max 0.32

#Put the results in a table
results<-as.data.frame(rbind(summary(MCF.high),summary(MCF.low),summary(MCF.2)))
results$diff<-c(results[1,4]/results[3,4],results[2,4]/results[3,4],1)
row.names(results)<-c("MCF.high","MCF.low","MCF.2")
write.csv(results,"Adjusted MCF estimate.csv",col.names = TRUE,row.names = TRUE)
