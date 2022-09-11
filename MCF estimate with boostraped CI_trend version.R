#This file is to calculate boostrapped confidence interval for the average 
#ratio of manure surface to volume
#By using the monthy avg. T from 1990-2019, similar to NIR
library(boot)
library(tidyverse)

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
temp.lm<-lm(dif~SV,data=ratio)
new<-data.frame(SV=Boot.SV.mean)
temp.predict<-predict.lm(temp.lm,new,interval="confidence")
#obtain possible difference of the temperature
low<-quantile(temp.predict[,2],prob=0.025,names=FALSE) #-3.398
up<-quantile(temp.predict[,3],prob=0.975,names=FALSE) #0.720
temp.dif<-c(low,up)#it's the lowest dif and highest dif


#Now we can use the MCF calculator to see the change of MCF

#T.avg.m<-read.table("monthly avg T.txt",header=TRUE) # monthly average
T.avg<-read.table("1990-2019 monthly avg T.txt",header=TRUE) #gigantic table for avg. T for 3403 locations and 30years

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
max.month<-apply(T.avg,2,which.max)
T.avg.low<-T.avg
T.avg.high<-T.avg

#Apply the temperature difference to the max three months
for(i in length(T.avg)) {
T.avg.low[c(max.month[i]-1,max.month[i],max.month[i]+1),]<-T.avg[c(max.month[i]-1,max.month[i],max.month[i]+1),]+temp.dif[1]
T.avg.high[c(max.month[i]-1,max.month[i],max.month[i]+1),]<-T.avg[c(max.month[i]-1,max.month[i],max.month[i]+1),]+temp.dif[2]
}


#MCF.low is the low temperature difference
MCF.low<-c()
for (k in 1:3403){
  rm<-rep(0,12)
  rm[c(removal[k,2],removal[k,4])]<-1
  M.rm<-rm
  Manure.rm<-rep(M.rm,3) # for stabilization 3 yr
  for (l in 1:30){
  n<-30*(k-1)+l
  T.sel<-T.avg.low[,n]
  print(paste(round(n/102090*100,2),"%"))
  source("MCF calculator_single.R",echo = F)
  MCF.low[n]<-MCF
  }
}

summary(MCF.low) #min:0.091, mean:0.176, max 0.330

#MCF.high is the high temperature difference
MCF.high<-c()
for (k in 1:3403){
  rm<-rep(0,12)
  rm[c(removal[k,2],removal[k,4])]<-1
  M.rm<-rm
  Manure.rm<-rep(M.rm,3) # for stabilization 3 yr
  for (l in 1:30){
  n<-30*(k-1)+l
  T.sel<-T.avg.high[,n]
  print(paste(round(n/102090*100,2),"%"))
  source("MCF calculator_single.R",echo = F)
  MCF.high[n]<-MCF
  }
}
# about 14 mins
summary(MCF.high) #min:0.103, mean:0.2159, max 0.409

#Use the unmodified temperature to calculate MCF 
MCF.2<-c()
for (k in 1:3403){
  rm<-rep(0,12)
  rm[c(removal[k,2],removal[k,4])]<-1
  M.rm<-rm
  Manure.rm<-rep(M.rm,3) # for stabilization 3 yr
  for (l in 1:30){
  n<-30*(k-1)+l
  T.sel<-T.avg[,n]
  print(paste(round(n/102090*100,2),"%"))
  source("MCF calculator_single.R",echo = F)
  MCF.2[n]<-MCF
  }
}
summary(MCF.2) #min:0.10, mean:0.208, max 0.39

#Obtain station ID
name.l<-read.table("monthly avg T.txt",header=T)
name.list<-rep(colnames(name.l),each=30)
yr<-rep(1990:2019,3403)
MCF.2<-MCF.2 %>% 
  cbind(name.list,yr) %>%
  `colnames<-`(c("MCF.2","ID","year")) %>%
   as.data.frame()
MCF.h<-MCF.high %>% 
  cbind(name.list,yr) %>%
  `colnames<-`(c("MCF.h","ID","year")) %>%
  as.data.frame()
MCF.l<-MCF.low %>% 
  cbind(name.list,yr) %>%
  `colnames<-`(c("MCF.l","ID","year")) %>%
  as.data.frame()

# write.csv(MCF.2,"MCF.2.csv",row.names = FALSE)
# write.csv(MCF.h,"MCF.high.csv",row.names = FALSE)
# write.csv(MCF.l,"MCF.low.csv",row.names = FALSE)

MCF.2<-read.csv("MCF.2.csv",header=TRUE)
MCF.h<-read.csv("MCF.high.csv",header=TRUE) 
MCF.l<-read.csv("MCF.low.csv",header=TRUE) 

MCF<-MCF.2 %>%
    merge(MCF.h) %>%
    merge(MCF.l)

MCF$range.h<-(MCF$MCF.h/MCF$MCF.2-1)*100
MCF$range.l<-(MCF$MCF.l/MCF$MCF.2-1)*100

summary(MCF$range.h)
summary(MCF$range.l)

#To obtain natioal average MCF for the high, regular and low scenarios. 
MCF.avg<-data.frame(year=seq(1990,2019),
                    MCF2=rep(0,30),
                    MCFH=rep(0,30),
                    MCFL=rep(0,30))
for(i in 3:5) { 
MCF.avg[,i-1]<-tapply(MCF[,i],MCF$year,mean)
}

#Plot the results
plot(MCF.avg$year,MCF.avg$MCF2,
     ylim=c(0.14,0.28),xlab="Year",
     ylab="Methane Conversion Factor (MCF)",
     yaxs="i",las=1)
arrows(MCF.avg$year,MCF.avg$MCFH,MCF.avg$year,MCF.avg$MCFL,
       length = 0.05,angle=90,code=3)
text(1990,0.28,"(B)",pos=1)
legend(1990,0.27,
       c("Average natioanl MCF",
         "95% Confidence Interval"),
       bty="n",pch=c(1,NA),lty=c(NA,1))

#Put the results in a table
results<-as.data.frame(rbind(summary(MCF.high),summary(MCF.low),summary(MCF.2)))
results$diff<-c(results[1,4]/results[3,4],results[2,4]/results[3,4],1)
row.names(results)<-c("MCF.high","MCF.low","MCF.2")
write.csv(results,"Adjusted MCF estimate.csv",col.names = TRUE,row.names = TRUE)
