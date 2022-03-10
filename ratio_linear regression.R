#This is to fit a linear regression between the ratio and the difference. 


#My first code March 10th

data<-read.csv("Manure Linear Regression.csv")




plot(data$SV, data$dif)

model.SV <- lm(dif~SV,data=data)

deneme<-data.frame(SV=seq(-17,17,length=50))

#plot simple linear regression model

plot(data$SV, data$dif)

lines(deneme$SV,predict(model.SV,newdata=deneme))




