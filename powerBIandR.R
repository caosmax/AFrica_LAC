
setwd("C:/Users/CEGONZALEZ/Documents/CSA/copy/")
md<-read.csv(file = "C:/Users/CEGONZALEZ/Documents/CSA/copy/SystemProductionBangla.csv",header=T)
 
library(ggplot2)
ban<- ggplot(data = md, aes(x = year, y = Val, colour=scenario))+
        geom_line(aes(group=scenario))+
        geom_point()+ facet_grid(impactparameter~productiontype)+
        ylab("Value ") + ggtitle("Maize Yields & Area")+
        xlab("Years") +
        theme(axis.text.x=element_text(size=12, angle=90))+
        guides(color=guide_legend("Scenarios"))
     

 plot(ban)
# 
# md_a<- md[which(md$impactparameter=="AreaXAgg -- Area"),]
# head(md_a)
# 
# dataset<-write.csv(md_a, "areaBangla.csv")
# dataset2<- read.csv("C:/Users/CEGONZALEZ/Documents/CSA/copy/areaBangla.csv", header = T)
# md<-read.csv(file = "SystemProductionBangla.csv",header=T)
# md2<-write.csv(md,"Bangla.csv")
# md2<- read.csv("C:/Users/CEGONZALEZ/Documents/CSA/copy/Bangla.csv", header = T)
# 
# plot(x=dataset$year, y=dataset$Val, pch=19,xlab = "year", ylab = "Value", main = "Maize Area Bangladesh",type = "p")
# 
