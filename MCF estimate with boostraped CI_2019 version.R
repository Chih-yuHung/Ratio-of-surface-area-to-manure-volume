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

#obtain weather data from 2019 only 
#since I have 30 years, so I ask it to retrieve data every 30y
T.2019<-data.frame()
T.2019<-T.avg[,seq(30,3403*30,by=30)] 

#This is an R procedure example how to calculate MCF for different locations. 


#Calculate the MCF for the input locations
#We're runing two scenarios, low (-3.398) and high (+0.720) 
#under the two removal scenario, early spring-early fall
#Obtain removal scenarios from my STOTEN study
#removal<-read.csv("removal month.csv",header=TRUE)

#Calculate the MCF for the input locations
#We're runing two scenarios, low (-3.398) and high (+0.720) 
#under the two removal scenario, early spring-early fall
max.month<-apply(T.avg,2,which.max)
T.avg.dif<-rep(T.2019,2000)
rm(T.avg.dif)
#Apply the temperature difference to the max three months
m<-3403*4000
for (i in 4001:10000) {
for(j in 1:length(T.2019)) {
n<-3403*(i-1)+j
T.avg.dif[[n-m]][c(max.month[j]-1,max.month[j],max.month[j]+1)]<-
  T.2019[c(max.month[j]-1,max.month[j],max.month[j]+1),j]+temp.predict[i,1]
print(round((n-m)/(3403*2000)*100,3)) #show runing progress
}
}

save(T.avg.dif,file = "T.avg.diff-5.rda")
load(file = "T.avg.diff-3.rda")
#Use the unmodified temperature to calculate MCF 
cl <- makeCluster(8)
MCF.2019.p<-c()
time.s<-Sys.time()
MCF.2019.p<-parLapply(cl,T.avg.dif,MCF.est)
print(Sys.time()-time.s)#20 mins with 3403000 
stopCluster(cl)

summary(MCF.2019) #min:0.101, mean:0.189, max 0.339
save(MCF.2019.p,file = "MCF2019-3.rda")

load(file = "T.avg.diff-1.rda")
MCF.2019.4<-MCF.2019.p

#Obtain station ID
name.l<-read.table("monthly avg T.txt",header=T)
name.list<-rep(colnames(name.l),each=30)
yr<-rep(1990:2019,3403)
MCF.2<-MCF.2 %>% 
  cbind(name.list,yr) %>%
  `colnames<-`(c("MCF.2","ID","year")) %>%
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
