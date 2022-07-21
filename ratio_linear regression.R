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



#See below for CY's code
library(ggplot2)
library(ggpubr)

Temp<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 7_surface area ratio/2_mehtods/R/Ratio-of-surface-area-to-manure-volume/Manure Linear Regression.csv")
#remove the 6 and 9, the 6 is an outlier 
#I received information that 9 removed manure just before summer,
#the removal significantly decreased the manure volume and influence the result
#CY, March 27, 2022
Temp<-Temp[c(-6,-9),]

#Linear regression and plot
#Temp.lm<-lm(dif~SV,data=Temp)
# a<-round(Temp.lm$coefficients[1],2)       #obtain the coeffiecients
# b<-round(Temp.lm$coefficients[2],1)
# R2<-round(summary(Temp.lm)$r.squared,3)   #0.631
# p.value<-round(summary(Temp.lm)$coefficients[2,4],3) #0.033
Temp.cor<-cor.test(Temp$SV,Temp$dif)
R<-round(Temp.cor$estimate,3) #0.795
p.value<-round(Temp.cor$p.value,3) #0.033
#scatter plot and linear model
#Output 800 x 600
ggplot(aes(x=SV,y=dif),data=Temp)+
  geom_point(aes(shape=type),size=4)+
  theme_classic()+                                               # remove gray background
  xlim(0.32,0.50)+                                           #set xy limit
  ylim(-5,4)+
  theme(legend.position=c(0.2,0.80),                        #legend position
        axis.title.y = element_text(margin = margin(r = 7), #y lab position and size
                                    size=14),
        axis.title.x = element_text(margin = margin(t = 7), #x lab
                                    size=14),
        axis.text = element_text(size=12),                  #font size of x,y 
        legend.text = element_text(size=12),                #font size of legend
        legend.title = element_blank())+                    #remove legend title
  xlab(expression("Surface area / manure volume ("~m^-1~")"))+                 # set xy label
  ylab(expression("T"["diff"]~"(°C)"))+
  geom_smooth(method='lm', se=FALSE,color="black")+         #add linear regression
  #annotate("text", label = paste("y = ", a ," + ",
  #                      b,"x", sep = ""),x = 0.33,y=0.5, size = 5) +
  annotate("text", label = "atop(R == 0.795,italic(P) == 0.033)" ,x = 0.33,y=-0.2, size = 5,parse=TRUE)+
  annotate("text",x=0.32,y=3.6,label="(b)")

#Replot because I want to combine the two figures together
#The other figure is the simulation results.
#800x600
plot(0,
     xlab=expression(paste("Surface area / manure volume (",m^-1,")")),
     ylab=expression(paste("T"["diff"]~"(",degree,"C)")),
     las=1,xaxs="i",yaxs="i",
     xlim=c(0.32,0.50),
     ylim=c(-5.0,4),
     cex.lab=1.3)
points(Temp$SV[Temp$type=="Tank"],Temp$dif[Temp$type=="Tank"],pch=17,cex=1.3)
points(Temp$SV[Temp$type=="Earthen storage"],Temp$dif[Temp$type=="Earthen storage"],pch=16,cex=1.3)
abline(lm(Temp$dif~Temp$SV))
text(0.43,-0.8,expression(paste("r = 0.795, ",italic(P),"< 0.033"))
                       ,pos=4,cex=1.2)
text(0.32,3.6,"(b)",pos=4,cex=1.3)
legend(0.33,3,pch=c(17,16),c("Tank","Earthen Storage"),
       bty="n",cex=1.3)
text(Temp$SV+0.005,Temp$dif+0.02,labels = 1:7,cex=1.3)                          
                           
                           