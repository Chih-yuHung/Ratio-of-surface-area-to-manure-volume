#To create a figure to demonstrate the influence of temperature on MCF
#The data is from the sheet:MCF sheet_Fig 1. 
#The manure temperature increase from 3 months. 
Tm<-c(1.0,1.0,1.0,1.0,5.0,12.0,17.3,20.5,17.9,15.7,8.2,1.2)
Tm.2<-c(1.0,1.0,1.0,1.0,5.0,12.0,19.3,22.5,19.9,15.7,8.2,1.2)
CH4  <-c(2,3,3,1,2,6,14,22,4,6,3,2)/2
CH4.2<-c(2,3,3,1,2,6,17,28,5,6,3,2)/2
#Output 800 x 600
par(mar=c(4,5,1,4))
plot(0,type="n",xlim=c(0,12),ylim=c(0,30),
     xlab="Month",ylab="Manure temperature (°C)"
     ,las=1,xaxt="n",xaxs="i",yaxs="i"
     ,cex.lab=1.5)
axis(4,at=seq(0,30,5),labels=seq(0,60,10),
     las=1)
mtext(expression("Methane production ("~m^3~")"),
      side=4,line=3,cex=1.5)
rect(xleft = (c(0:11)+0.1), ybottom = 0,
     xright = (c(0:11)+0.5), ytop=CH4, col = "grey",
     density =  135)
rect(xleft = (c(0:11)+0.5), ybottom = 0,
     xright = (c(0:11)+0.9), ytop=CH4.2, col = "blue",
     density = 20)
lines(c(-1:12),c(0,Tm,0),type="s")
lines(c(-1:12),c(0,Tm.2,0),type="s",col="blue",lty = 2)
legend(0,29,c("original","+ 2°C in the warmest months")
       ,col=c("black","blue"),lty=c(1,2),
       bty="n",title="Manure temperature")
legend(0.3,23,c("original","+ 2°C in the warmest months")
       ,fill=c("grey","blue"),density=c(135,20)
       ,bty="n",title="Methane production")
text(8.2,15,"MCF = 0.26",col="blue")
text(6.6,11.5,"MCF = 0.23")
axis(1,at=c(0.5:11.5),labels=month.abb)
#arrows(8,22.5,8,20.5,length=0.03, angle=90)


