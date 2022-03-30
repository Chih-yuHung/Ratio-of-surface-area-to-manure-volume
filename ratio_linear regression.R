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
Temp<-read.csv("Manure Linear Regression.csv")
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
  geom_point(aes(color=type),size=3)+
  scale_color_manual(values=c("#999999", "#E69F00"))+ # color change
  theme_classic()+                                               # remove gray background
  xlim(0.3,0.46)+                                           #set xy limit
  ylim(-5,3)+
  theme(legend.position=c(0.2,0.88),                        #legend position
        axis.title.y = element_text(margin = margin(r = 7), #y lab position and size
                                    size=14),
        axis.title.x = element_text(margin = margin(t = 7), #x lab
                                    size=14),
        axis.text = element_text(size=12),                  #font size of x,y 
        legend.text = element_text(size=12),                #font size of legend
        legend.title = element_blank())+                    #remove legend title
  xlab("Ratio of surface area to manure volume")+           # set xy label
  ylab("Temperature difference between manure and air (°C)")+
  geom_smooth(method='lm', se=FALSE,color="black")+         #add linear regression
  #annotate("text", label = paste("y = ", a ," + ",
  #                      b,"x", sep = ""),x = 0.33,y=0.5, size = 5) +
  annotate("text", label = "atop(R == 0.795,italic(P) == 0.033)" ,x = 0.33,y=-0.2, size = 5,parse=TRUE)
                           
                           
                           