#"temp.no.daily" or "swedendata" manure temperatures for warmest months

month.mean<-tapply(swedendata$temp.avg,swedendata$Month,mean)
list(month.mean)

# June: 17.206099 July: 16.053723 August: 15.617404

#sweden 2 Air Temperature for warmest months

head(swedendata2)

AirT.mean<-tapply(swedendata2$'Ave AirTmax 1 and AirTmin1', swedendata2$Month, mean) 
list(AirT.mean)

# June: 16.293333 , July: 14.783871  , August: 16.338710  

#Manure Depth Average for June, July, August

head(swedendata)
depth.mean<-tapply(swedendata$Depth,swedendata$Month,mean)
list(depth.mean)

# June = 0.6114575 , July = 0.8159436 , August = 1.1245895










