#To create a figure to demonstrate the influence of temperature on MCF
#The data is from the sheet:MCF sheet_Fig 1. 
#The manure temperature increase from 3 months. 
Tm<-c(1.0,1.0,1.0,1.0,5.0,12.0,17.3,20.5,17.9,15.7,8.2,1.2)
Tm.2<-c(1.0,1.0,1.0,1.0,5.0,12.0,19.3,22.5,19.9,15.7,8.2,1.2)
CH4  <-c(2,3,3,1,2,6,14,22,4,6,3,2)/2
CH4.2<-c(2,3,3,1,2,6,17,28,5,6,3,2)/2
#Output 800 x 600
par(mar=c(4,5,1,4))
plot(0,type="n",xlim=c(1,12),ylim=c(0,30),
     xlab="Month",ylab="Manure temperature (°C)"
     ,las=1,xaxt="n",xaxs="i",yaxs="i"
     ,cex.lab=1.5)
axis(1,at=c(1:12),labels=month.abb)
axis(4,at=seq(0,30,5),labels=seq(0,60,10),
     las=1)
mtext(expression("Methane production ("~m^3~")"),
      side=4,line=3,cex=1.5)
polygon(c(c(1:12),c(12:1)),c(CH4.2,rep(0,12))
        ,col="blue"
        ,density=10,angle=45)
polygon(c(c(1:12),c(12:1)),c(CH4,rep(0,12))
        ,col="grey",density = 50, angle =135)
lines(c(1:12),Tm,type="s")
lines(c(1:12),Tm.2,type="s",col="blue")
legend(1,29,c("original","+ 2°C in the warmest months")
       ,col=c("black","blue"),lty=c(1,1),
       bty="n",title="Manure temperature")
legend(1.3,23,c("original","+ 2°C in the warmest months")
       ,fill=c("grey","blue"),density=c(135,20)
       ,bty="n",title="Methane production")
text(8.2,23.5,"MCF = 0.26",col="blue")
text(8.2,16.5,"MCF = 0.23")
#arrows(8,22.5,8,20.5,length=0.03, angle=90)


