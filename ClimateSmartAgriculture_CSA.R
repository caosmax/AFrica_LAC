# Codigos para el procesamiento y organizacion de los graficos para el CSA. Climate Smart Acgriculture
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)


#Definir directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.2/OutputFiles/Aggregation/")
#Dirreción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/CSA/"

#Cargar marco de datos principal
md<-read.csv("CSA.csv",header=T)


#Hacer un subconjunto que sólo contenga las variables de mi interés
mdsub<-subset(md,md$impactparameter=="QSXAgg -- Total Production" | 
                md$impactparameter=="TAreaXAgg -- Total Area" |
                md$impactparameter== "QNXAgg -- Net Trade" | 
                md$impactparameter== "QDXAgg -- Total Demand" |
                md$impactparameter== "TYldXAgg -- Total Yield")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QDXAgg -- Total Demand"="Total Demand",
                                                        "QNXAgg -- Net Trade"="Net Trade",
                                                        "QSXAgg -- Total Production"="Total Production",
                                                        "TAreaXAgg -- Total Area"="Total Area",
                                                        "TYldXAgg -- Total Yield"= "Yield"))

#Hacer un subconjunto que sólo contenga los cinco cultivos analizados
mdsubcrop<-subset(mdsub,mdsub$commodity=="jbean"| mdsub$commodity=="cbean" |
                    mdsub$commodity=="jmaize" | mdsub$commodity=="cmaiz" |
                    mdsub$commodity=="jrice" | mdsub$commodity=="crice" | 
                    mdsub$commodity=="jsoyb" | mdsub$commodity=="csoyb" |
                    mdsub$commodity=="jwhea" | mdsub$commodity=="cwhea")



#Hacer un subconjunto de md que sólo contenga los paises de LAC
zonas_reg<-levels(mdsubcrop$region)[c(163, 121, 166, 151, 122, 119, 118, 19, 126,140,144 )]

mdsubcrop_lac<-mdsubcrop[which(mdsubcrop$region %in% zonas_reg),]

mdsubcrop_lac<-droplevels(mdsubcrop_lac)

mdwide <- reshape(mdsubcrop_lac, v.names = "Val", idvar = 
                    c("scenario","commodity","region","productiontype",
                      "impactparameter"),timevar = "year", direction = "wide")

mdwide$commodity<-revalue(mdwide$commodity, c("cbean"= "Bean",
                                              "cmaiz"="Maize",
                                              "crice"="Rice",
                                              "csoyb"="Soybean",
                                              "cwhea"="Wheat",
                                              "jbean"="Bean",
                                              "jmaize"="Maize",
                                              "jrice"="Rice",
                                              "jsoyb"="Soybean",
                                              "jwhea"="Wheat"))

mdwide$region<-revalue(mdwide$region,c("EAP-Philippines"="Philippines",
                                       "SAS-Bangladesh"="Bangladesh",
                                       "SAS-Bhutan" ="Bhutan",
                                       "SAS-Nepal"="Nepal",
                                       "SAS-Pakistan"="Pakistan",
                                       "SSA-Benin"="Benin",
                                       "SSA-Mozambique"="Mozambique",
                                       "SSA-Tanzania" = "Tanzania",
                                       "SSA-Zambia" ="Zambia",
                                       "SSA-Gambia"= "Gambia",
                                       "SSA-Ivory Coast"= "Ivory Coast"))



mdwide<-data.frame(mdwide,"Cat"=ifelse(mdwide$scenario=="NoCC","NoCC","CC"))

rend_all<-mdwide[,-c(5:20)]
rend_all$Percentage_Change<-((rend_all$Val.2050-rend_all$Val.2020)/rend_all$Val.2020)*100

#Mediana de los cambios porcentuales por categorias.

anal_datag<-aggregate(rend_all[,"Percentage_Change"],
                      by=list(rend_all$region,rend_all$impactparameter,
                              rend_all$commodity,rend_all$Cat),
                      FUN=median)

ggplot(data=droplevels(subset(anal_datag,anal_datag$Group.3 ==levels(anal_datag$Group.3)[i]
                              & anal_datag$Group.2 !="Net Trade" 
                              & anal_datag$Group.4 != "NoCC")),aes(x=Group.1,y=x))  +
  facet_wrap( ~ Group.2,ncol=4)+
  geom_bar(stat="identity")


#Realizar proceso para graficar todas las variables por regiones.

pl<-NULL
for (i in 1:length(levels(anal_datag$Group.1))) {
  
  tiff(filename=paste(grd,levels(anal_datag$Group.1)[i],"_CSA.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  pl[[i]]<-print(ggplot(data=droplevels(subset(anal_datag,anal_datag$Group.1 ==levels(anal_datag$Group.1)[i]
                                               & anal_datag$Group.2 !="Net Trade" 
                                               & anal_datag$Group.4 != "NoCC")),aes(x=Group.3,y=x
                                                                                    ,fill=Group.2))  +
                   facet_wrap( ~ Group.2,ncol=4)+
                   geom_bar(stat="identity")+
                   geom_point(aes(shape=Group.4),data=droplevels(subset(anal_datag,anal_datag$Group.1 ==
                                                                          levels(anal_datag$Group.1)[i]
                                                                        & anal_datag$Group.2 !="Net Trade" 
                                                                        & anal_datag$Group.4 == "NoCC")),
                              alpha = 0.4,size=4) +
                   guides(fill=FALSE)+
                   theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.title=element_blank())+
                   theme(legend.position="bottom")+
                   labs(y="Percentage Change",x="")
  )
  
  dev.off()
  print(i)
}              

#Evolution of Net Trade by crop and by region
datos<-droplevels(subset(rend_all,rend_all$impactparameter==levels(rend_all$impactparameter)[2]))

datmin<-aggregate(datos[,paste("Val.",2020:2050,sep="")],by=list(datos$region,datos$commodity),FUN=min)

datmin<-reshape(datmin,varying = paste("Val.",2020:2050,sep=""),idvar = c("Group.1","Group.2"),
                direction="long",v.names = "datmin",times=2020:2050)
names(datmin)<-c("Region","Crop","time","datmin")


datmed<-aggregate(datos[,paste("Val.",2020:2050,sep="")],by=list(datos$region,datos$commodity),FUN=median)

datmed<-reshape(datmed,varying = paste("Val.",2020:2050,sep=""),idvar = c("Group.1","Group.2"),
                direction="long",v.names = "datmed",times=2020:2050)
names(datmed)<-c("Region","Crop","time","datmed")


datmax<-aggregate(datos[,paste("Val.",2020:2050,sep="")],by=list(datos$region,datos$commodity),FUN=max)

datmax<-reshape(datmax,varying = paste("Val.",2020:2050,sep=""),idvar = c("Group.1","Group.2"),
                direction="long",v.names = "datmax",times=2020:2050)
names(datmax)<-c("Region","Crop","time","datmax")

extremos<-merge(datmin,datmax)

datost<-merge(extremos,datmed)

py<-NULL

for (i in 1:length(levels(datost$Region))) {
  
  tiff(filename=paste(grd,levels(datost$Region)[i],"_net_trade.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  py[[i]]<-print(ggplot(data=droplevels(subset(datost,datost$Region==levels(datost$Region)[i])),
                        aes(time,datmed,group=Crop,color=Crop)) + 
                   geom_line(linetype="dashed",size=1)+
                   geom_ribbon(aes(ymin=datmin,ymax=datmax,fill=Crop,colour=Crop,linetype=NA),
                               alpha=0.1) +
                   labs(y="Net trade (000 mt)",x="Year")+
                   theme(legend.position="bottom")
                 
  )
  
  
  dev.off()
  print(i)
}  
