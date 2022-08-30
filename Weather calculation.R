#Target : calculate monthly average temperature for years 1990-2019
# Total sites: 3403
# Total days from 1990-2019: 365*30 +7 leap year=  10957
library(data.table)
library(epitools)
library(lubridate)

setwd("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 1_MCF/3_results/Weather data/SLC_Weather_WRH")
site.name<-list.files(pattern="*.txt",recursive=TRUE)
#print(object.size(a),units="Mb")

n.site<-3403
day.t<-10957

#find the station ID list
name.list<-strsplit(site.name,split = "/")
#remove the first two years, ie. 1979-1990, only 30 years to use (1990-2019)
yr <- data.frame(start=c(12L,12L+41L*(1L:3402L)), len=rep(30,3403))
yrs<-sequence(yr$len) + rep(yr$start-1, yr$len)
#obtain stations' ID
name.l<-vector()
for (i in yrs){
  name.l[i]<-name.list[[i]][1]
}
name.l<-unique(name.l)[2:3404]
####

#Create list to store temperature data
temp<- vector("list", length(site.name))
####

#read data
 time.s<-Sys.time()
 for (i in yrs) {
  temp[[i]] <- fread(site.name[i],select=c(1:3)) # read data from txt
  print(paste("progress = ",(139523-i)/139523*100,"%"))
  }
 time.f<-Sys.time()
 time.f-time.s
 #for all files, it was 13.8 mins
DF<-rbindlist(temp)
setDF(DF) #To convert to data.frame
colnames(DF)<-c("date","maxT","minT")
####

#replace Julian day to date and obtain month
dates<-rep(seq(as.Date("1990-1-1"), as.Date("2019-12-31"), by = "days"), n.site)
aa<-format(dates,format="%m-%d")
DF[,1]<-dates
DF[,4]<-month(as.POSIXlt(DF[,1],format="%Y-%m-%d"))
DF[,5]<-year(as.POSIXlt(DF[,1],format="%Y-%m-%d"))
DF[,6]<-aa 

#calculate daily mean temperature
DF[,1]<-(DF[,2]+DF[,3])/2
DF<-subset(DF,select=-c(maxT,minT))

#assign station ID to the DF
DF[,5]<-rep(name.l,each=day.t)
colnames(DF)<-c("avgT","month","year","monthdate","ID")

#calculate monthly average temperature
T.avg<-vector()
T.avg<-tapply(DF$avgT,list(DF$month,DF$year,DF$ID),mean)

#calculate daily average temperature
#T.avg.d<-tapply(DF$avgT,list(DF$monthdate,DF$ID),mean)

#export data for the uncertaity calculation
setwd("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 7_surface area ratio/2_mehtods/R/Ratio-of-surface-area-to-manure-volume")
write.table(T.avg,file="1990-2019 monthly avg T.txt", sep="\t")
#write.table(T.avg.d,file="C:/AAFC/Project 1_MCF/3_results/Weather data/daily avg T.txt", sep="\t")

