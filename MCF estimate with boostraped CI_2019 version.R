#This file is to calculate boostrapped confidence interval for the average 
#ratio of manure surface to volume
#By using the monthy avg. T from 1990-2019, similar to NIR
library(boot)
library(tidyverse)
library(data.table)
library(weathermetrics) #convert C to K
library(parallel)

source("MCF estimator.R",echo = F,local=TRUE) #the MCF calculator function.

ratio<-read.csv("Manure Linear Regression.csv")
ratio<-ratio[c(-6,-9),] #9 is from Sweden
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
# Boot.low<-quantile(Boot.SV.mean,prob=0.025,names=FALSE) #0.3903
# Boot.up<-quantile(Boot.SV.mean,prob=0.975,names=FALSE) #0.4173
# Boot.low<-quantile(Boot.SV.mean,prob=0.01,names=FALSE) #0.388
# Boot.up<-quantile(Boot.SV.mean,prob=0.99,names=FALSE) #0.419

#We're going to apply this to the linear regression 
#and obtain 95% CI for the temperature difference
temp.lm<-lm(dif~SV,data=ratio)#y = 42.3x-18.4
new<-data.frame(SV=Boot.SV.mean)
#The possible differecne of the 10000 bootstrap
temp.predict<-predict.lm(temp.lm,new,interval="confidence")
#obtain possible difference of the temperature
low<-quantile(temp.predict[,2],prob=0.025,names=FALSE) #-3.398
up<-quantile(temp.predict[,3],prob=0.975,names=FALSE) #0.720
temp.dif<-c(low,up)#it's the lowest dif and highest dif

#Now we can use the MCF calculator to see the change of MCF
#Read weather data
T.avg<-as.data.frame(fread("1990-2019 monthly avg T.txt"))[,-1] #gigantic table for avg. T for 3403 locations and 30years

#obtain temperature data from 2019 only 
#since I have 30 years, so I ask it to retrieve data every 30y
T.2019<-data.frame()
T.2019<-T.avg[,seq(30,3403*30,by=30)] 

#Prepare temperature data after adjustment of Tdiff
max.month<-apply(T.avg,2,which.max)
#Because 10000 times bootstrap is too large, I split to 5 sets of 2000 
T.avg.dif<-rep(T.2019,2000)
#Apply the temperature difference to the max three months
#manually change paraters, m = 3403*1, 3403*2000, 3403*4000, 3403*6000, 3403*8000
#the i from 1:2000, 2001:4000, 4001:6000, 6001:8000, 8001:1000
m<-3403*1
for (i in 1:2000) {
for(j in 1:length(T.2019)) {
n<-3403*(i-1)+j
T.avg.dif[[n-m]][c(max.month[j]-1,max.month[j],max.month[j]+1)]<-
  T.2019[c(max.month[j]-1,max.month[j],max.month[j]+1),j]+temp.predict[i,1]
print(round((n-m)/(3403*2000)*100,3)) #show runing progress
}
}

save(T.avg.dif,file = "T.avg.diff-1.rda") # to save data. need to know that 
#the data name will be T.avg.dif when you load this file

#Calculate the MCF for the input locations
#under the two removal scenario, early spring-early fall
#removal scenario was set on May and October
#Need to change the "1" to 2,3,4,5 for all results.
load(file = "T.avg.diff-1.rda")
cl <- makeCluster(8)
MCF.2019.p<-c()
time.s<-Sys.time()
MCF.2019.p<-parLapply(cl,T.avg.dif,MCF.est)
print(Sys.time()-time.s)#17 mins with 6806000 
stopCluster(cl)

summary(MCF.2019) #min:0.101, mean:0.189, max 0.339
save(MCF.2019.p,file = "MCF2019-1.rda")


#I save all data to rda data and draw my figure 4B
#See "Plot MCF difference_figure 4B.R" 