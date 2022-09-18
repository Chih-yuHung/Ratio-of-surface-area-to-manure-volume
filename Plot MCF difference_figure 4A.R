#To create a figure to demonstrate the influence of temperature on MCF
#The data is from the sheet:MCF sheet_Fig 4A. 
#The manure temperature increase for 3 months. 
Tm<-c(1.0,1.0,1.0,1.0,5.0,12.0,17.3,20.5,17.9,15.7,8.2,1.2)
Tm.H<-c(1.0,1.0,1.0,1.0,5.0,12.0,18.02,21.22,18.62,15.7,8.2,1.2)
Tm.L<-c(1.0,1.0,1.0,1.0,5.0,12.0,13.96,17.16,14.56,15.7,8.2,1.2)
CH4  <-c(2.2,2.6,3,0.6,1.8,6,13.8,22.2,4.4,5.7,3.2,1.8)/2
CH4.H<-c(2.2,2.6,3,0.6,1.8,6,15.0,23.7,4.7,5.7,3.2,1.7)/2
CH4.L  <-c(2.2,2.6,3.1,0.6,1.8,6,9.4,15.7,3.0,6.0,3.4,1.8)/2
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
legend(1,28,c("original, MCF = 0.23",
              "Rs:v = 0.417, MCF = 0.24",
              "Rs:v = 0.390, MCF = 0.19")
       ,col=c("black","blue","grey"),lty=c(1,1,1),
       bty="n",title="Manure temperature")
legend(1.3,22,c("original","Rs:v = 0.417","Rs:v = 0.390")
       ,fill=c("white","blue","grey")
       ,density=c(100,20,20),angle=c(0,135,45)
       ,bty="n",title="Methane production")
text(1.3,30,"(A)",pos=1,cex=1.5)

# text(8.2,23,"MCF = 0.24")
# text(8.2,17,"MCF = 0.23")
# text(8.2,14,"MCF = 0.19")
#arrows(8,22.5,8,20.5,length=0.03, angle=90)


