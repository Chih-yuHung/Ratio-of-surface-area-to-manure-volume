#My first code March 10th

Articles<-read.csv("Manure Linear Regression.csv")


# Calculations for swedendata
# Warmest manure temperature = June: 17.206099 but we'll use July 
# because June is semi-data. In this case:
# Warmest manure temperature = July: 16.053723
# Warmest Air Temperature = August : 16.338710
16.053723-16.338710
# dif =  -0.284987 = -0.28

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
                    Year = "2020-2021",
                    Surface.area..m2. = "314.16",
                    Manure.volume..m3. = "256.34",
                    SV= "1.23",
                    Highest.air.temperature = "NA",
                    Highest.manure.temperature = "NA",
                    dif = "-0.28",
                    Lowest.air.temperature = "NA",
                    Lowest.manure.temperature = "NA",
                    dif_low = "NA",
                    doi = "sweden data")
print(newRow)                    

#rbind

Articles<- rbind(Articles, newRow)
print(Articles)

#plot

plot(Articles$SV, Articles$dif)

model.SV <- lm(dif~SV,data=Articles)

prediction<-data.frame(SV=seq(-1,1.5, length=50))
#plot simple linear regression model

plot(Articles$SV, Articles$dif)


lines(prediction$SV, predict(model.SV,newdata = prediction))

View(Articles)

