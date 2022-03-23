#My first code March 10th

Articles<-read.csv("Manure Linear Regression.csv")

#March 11
plot(Articles$SV, Articles$dif)

model.SV <- lm(dif~SV,data=Articles)

ID<-data.frame(SV=seq(-17,17,length=50))

#plot simple linear regression model

plot(Articles$SV, Articles$dif)

lines(ID$SV,predict(model.SV,newdata=ID))

# Calculations for swedendata
# Warmest manure temperature = June: 17.206099
# Warmest Air Temperature = August : 16.338710
17.206099-16.338710
# dif =  0.867389 = 0.87

#Surface area = (20/2)^2*PI

swedensurfacearea = pi * 100
list(swedensurfacearea)

#Surface/ Manure Volume(warmest manure's volume) Ratio

# Volume in July : 256.3362 m3

SVratio = 314.1593/256.3362
list(SVratio)
# SVratio = 1.225575

#New row adding

newRow<-data.frame(ID= "22", 
                    Latitude = "NA",
                    Longitude = "NA",
                    Year = "NA",
                    Surface.area..m2. = "NA",
                    Manure.volume..m3. = "NA",
                    SV= "NA",
                    Highest.air.temperature = "NA",
                    Highest.manure.temperature = "NA",
                    dif = "NA",
                    Lowest.air.temperature = "NA",
                    Lowest.manure.temperature = "NA",
                    dif_low = "NA",
                    doi = "NA")
print(newRow)                    
#rbind

Articles<- rbind(Articles, newRow)
print(Articles)
