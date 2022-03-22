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

df<-Articles
row_to_add <- df[9,]
row_to_add
df<- rbind(df, row_to_add)
tail(df)

new_row<- data.frame(NA,NA,NA,NA,314.16,256.34,1.23,NA,NA,0.87,NA,NA,NA,'sweden', row.names = "9")
new_row


names(new_row) <- names(df)
df<-rbind(df, new_row)
tail(df)

