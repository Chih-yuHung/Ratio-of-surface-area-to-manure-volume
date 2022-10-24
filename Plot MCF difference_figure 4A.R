#To create a figure to demonstrate the influence of temperature on MCF
#The data is from the sheet:MCF sheet_Fig 4A. 
#The manure temperature increase for 3 months. 
Tm<-c(1.0,1.0,1.0,1.0,5.0,12.0,17.3,20.5,17.9,15.7,8.2,1.2)
Tm.H<-c(1.0,1.0,1.0,1.0,5.0,12.0,18.4,21.8,19.2,15.7,8.2,1.2)
Tm.L<-c(1.0,1.0,1.0,1.0,5.0,12.0,16.2,19.4,16.8,15.7,8.2,1.2)
CH4  <-c(1.8,2.3,2.7,0.6,1.8,6,13.7,22.0,16.6,3.4,2.4,1.4)/2
CH4.H<-c(2.3,2.7,3,0.6,1.8,6,15.6,25.0,18.6,3.4,2.4,1.4)/2
CH4.L  <-c(1.9,2.3,2.7,0.6,1.8,6.0,12.1,19.7,15.2,3.5,2.4,1.4)/2
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
polygon(c(c(1:12),c(12:1)),c(CH4.H,rep(0,12))
        ,col="blue"
        ,density=10,angle=135)
polygon(c(c(1:12),c(12:1)),c(CH4,rep(0,12))
        ,col="white")
polygon(c(c(1:12),c(12:1)),c(CH4.L,rep(0,12))
        ,col="grey"
        ,density=10,aggle=45)
lines(c(1:12),Tm,type="b")
lines(c(1:12),Tm.H,type="b",col="blue")
lines(c(1:12),Tm.L,type="b",col="grey")
legend(1,28,c("original, MCF = 0.26",
              "Rs:v = 0.63, MCF = 0.28",
              "Rs:v = 0.34, MCF = 0.24")
       ,col=c("black","blue","grey"),lty=c(1,1,1),lwd=2
       ,bty="n",title="Manure temperature")
legend(1.3,22,c("original","Rs:v = 0.63","Rs:v = 0.34")
       ,fill=c("white","blue","grey")
       ,density=c(100,20,20),angle=c(0,135,45)
       ,bty="n",title="Methane production")
text(1.3,30,"(a)",pos=1,cex=1.5)

# text(8.2,23,"MCF = 0.24")
# text(8.2,17,"MCF = 0.23")
# text(8.2,14,"MCF = 0.19")
#arrows(8,22.5,8,20.5,length=0.03, angle=90)


