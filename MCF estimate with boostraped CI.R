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
<<<<<<< HEAD
<<<<<<< HEAD
T.avg.m<-read.table("monthly avg T.txt",header=TRUE) # monthly average

#This is an R procedure example how to calculate MCF for different locations. 
library(weathermetrics) #convert C to K

=======
=======
>>>>>>> 8bcda19e17e6e5164b6f108ab1605de549f2cbde
library(weathermetrics) #convert C to K

T.avg.m<-read.table("monthly avg T.txt",header=TRUE) # monthly average

<<<<<<< HEAD
>>>>>>> 8bcda19e17e6e5164b6f108ab1605de549f2cbde
=======
>>>>>>> 8bcda19e17e6e5164b6f108ab1605de549f2cbde
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

<<<<<<< HEAD
<<<<<<< HEAD
M.rm<-ifelse(T.avg.m$Removal.two=="Y",1,0)#convert it for calculation
#Calculate the MCF for the input locations
#We're runing two scenarios, low (-3.398) and high (+0.720) 
#under the two removal scenario, early spring-early fall

#MCF.low is the low temperature difference
MCF.low<-c()
for (k in 1:3){
  M.rm<-ifelse(T.avg.m[,3+k]=="Y",1,0)#convert it for calculation
  Manure.rm<-rep(M.rm,3) # for stabilization 3 yr
  T.sel<-T.avg.m[,3] #Use Atlantic Canada for example
=======
=======
>>>>>>> 8bcda19e17e6e5164b6f108ab1605de549f2cbde
#Obtain removal scenarios from my STOTEN study
removal<-read.csv("removal month.csv",header=TRUE)

#Calculate the MCF for the input locations
#We're runing two scenarios, low (-3.398) and high (+0.720) 
#under the two removal scenario, early spring-early fall
max.month<-apply(T.avg.m,2,which.max)
T.avg.low<-T.avg.m
T.avg.high<-T.avg.m

for(i in length(T.avg.m)) {
T.avg.low[max.month[i],]<-T.avg.m[max.month[i],]+temp.dif[1]
T.avg.high[max.month[i],]<-T.avg.m[max.month[i],]+temp.dif[2]
}


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

print(MCF.2)#0.35 for one removal, 0.24 for two removal, 0.18 for three removal

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

#Use the unmodified temperature to calculate MCF 
MCF.2<-c()
for (k in 1:3403){
  rm<-rep(0,12)
  rm[c(removal[k,2],removal[k,4])]<-1
  M.rm<-rm
  Manure.rm<-rep(M.rm,3) # for stabilization 3 yr
  T.sel<-T.avg.m[,k]
<<<<<<< HEAD
>>>>>>> 8bcda19e17e6e5164b6f108ab1605de549f2cbde
=======
>>>>>>> 8bcda19e17e6e5164b6f108ab1605de549f2cbde
  print(paste("Station sequence",k))
  source("MCF calculator_single.R",echo = F)
  MCF.2[k]<-MCF
}
<<<<<<< HEAD
<<<<<<< HEAD
print(MCF.2)#0.35 for one removal, 0.24 for two removal, 0.18 for three removal

#MCF.high is the high temperature difference

=======
=======
>>>>>>> 8bcda19e17e6e5164b6f108ab1605de549f2cbde

#Put the results in a table
results<-rbind(summary(MCF.high),summary(MCF.low),summary(MCF.2))
row.names(results)<-c("MCF.high","MCF.low","MCF.2")
write.csv(results,"Adjusted MCF estimate.csv",col.names = TRUE,row.names = TRUE)
<<<<<<< HEAD
>>>>>>> 8bcda19e17e6e5164b6f108ab1605de549f2cbde
=======
>>>>>>> 8bcda19e17e6e5164b6f108ab1605de549f2cbde
