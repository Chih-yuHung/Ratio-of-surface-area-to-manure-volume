library(ggplot2)
library(ggpubr)

Temp<-read.csv("Manure Linear Regression.csv")
#I received information that 9 removed manure just before summer,
#the removal significantly decreased the manure volume and influence the result
#Latest data has 3 set data from Fraser farm from 2017-2019

#Linear regression and plot
Temp.lm<-lm(dif~SV,data=Temp)
a<-round(Temp.lm$coefficients[1],2)       #obtain the coeffiecients,-3.92
b<-round(Temp.lm$coefficients[2],1)       #8.4
R2<-round(summary(Temp.lm)$r.squared,3)   #0.303
p.value<-round(summary(Temp.lm)$coefficients[2,4],3) #0.064
Temp.cor<-cor.test(Temp$SV,Temp$dif)
R<-round(Temp.cor$estimate,3) #0.55
p.value<-round(Temp.cor$p.value,3) #0.064

#Obtain the bootstrapped results
# I run this before "MCF estimate with bootstraped CI.R" to obtain bootstrap result
new<-data.frame(SV=seq(0.33,0.68,length.out=10000))
temp.predict<-predict.lm(Temp.lm,new,interval="confidence")

#Replot because I want to combine the two figures together
#The other figure is the simulation results.
#800x600
par(mar=c(4,5,4,4))
plot(0,
     xlab=expression(paste("Surface area / manure volume (",m^-1,")")),
     ylab=expression(paste("T"["diff"]~"(",degree,"C)")),
     las=1,xaxs="i",yaxs="i",
     xlim=c(0.32,0.70),
     ylim=c(-5.0,4),
     cex.lab=1.3)
polygon(c(rev(new[,1]), new[,1]), c(rev(temp.predict[ ,3]), temp.predict[ ,2]), col = 'grey90', border = NA)
lines(new[,1],temp.predict[,2],lty = 'dashed', col = 'red')
lines(new[,1],temp.predict[,3],lty = 'dashed', col = 'red')
points(Temp$SV[Temp$type=="Tank"],Temp$dif[Temp$type=="Tank"],pch=17,cex=1.3)
points(Temp$SV[Temp$type=="Earthen storage"],Temp$dif[Temp$type=="Earthen storage"],pch=16,cex=1.3)
abline(lm(Temp$dif~Temp$SV))
text(0.50,-3,expression(paste("r = 0.55, ",italic(P)," = 0.06"))
                       ,pos=4,cex=1.2)
text(0.32,3.6,"(b)",pos=4,cex=1.3)
legend(0.50,-3,pch=c(17,16),c("Tank","Earthen Storage"),
       bty="n",cex=1.3)
legend(0.497,-3.9,fill="grey90","95% confidence interval",
       bty="n",cex=1.3)
text(Temp$SV+0.009,Temp$dif+0.02,labels = Temp$TableID,cex=1.3)                          
                           
                           