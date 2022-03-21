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


#Mean AT calculation with R 


swedendata2$MeanAT<-(swedendata2$AirTmax1+swedendata2$AirTmin1)/2
ATmean<-tapply(swedendata2$MeanAT, swedendata2$Month, mean) 
list(ATmean)

# June =  16.293333 , July = 14.783871 , August = 16.338710

# Calculation of Manure Volume -  radius(r) = 10 meters

#June
volumejune = pi * 100 * 0.6114575
volumejuly = pi * 100 * 0.8159436
volumeaugust = pi * 100 * 1.1245895
list(volumejune, volumejuly, volumeaugust)



