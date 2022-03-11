#My first code March 10th

Articles<-read.csv("Manure Linear Regression.csv")

#March 11
plot(Articles$SV, Articles$dif)

model.SV <- lm(dif~SV,data=Articles)

ID<-data.frame(SV=seq(-17,17,length=50))

#plot simple linear regression model

plot(Articles$SV, Articles$dif)

lines(ID$SV,predict(model.SV,newdata=ID))







