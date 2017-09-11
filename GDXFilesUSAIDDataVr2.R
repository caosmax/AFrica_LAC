#Yields
#directories-------
setwd("C:/Users/CEGONZALEZ/Documents/GFSF/CassavaGDX")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/Graphs/")

#librerias------------
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(tidyr)
library(lattice)
library(latticeExtra)
library(dplyr)
library(RColorBrewer)

#################################################### YldXAgg ###########################################
#YldXAgg irrigated and Rainfed load data-------
data<- read.csv("YldXAgg.CSV",header = F)
#adjust labels 
names(data)[1]<-"Scenarios"
names(data)[2]<-"Commodity"
names(data)[3]<- "Regions"   
names(data)[4]<-"Sys"    
names(data)[5]<-"Year"
names(data)[6]<-"Val"

# corregir tipo de variable
data$Scenarios<- as.character(data$Scenarios)
data$Commodity<- as.character(data$Commodity)
data$Regions<- as.character(data$Regions)
data$Sys<- as.character(data$Sys)
#data$Year<-as.numeric(data$Year)

# list crops, esc and Regions
zone<- unique(data$Regions)
# alc<- data[grep(pattern = "LAC",x = data$Regions, ignore.case = T),]
# sas<- data[grep(pattern = "SAS",x = data$Regions, ignore.case = T),]
# eap<- data[grep(pattern = "EAP",x = data$Regions, ignore.case = T),]
# eur<- data[grep(pattern = "EUR",x = data$Regions, ignore.case = T),]
# fsu<- data[grep(pattern = "FSU",x = data$Regions, ignore.case = T),]
# men<- data[grep(pattern = "MEN",x = data$Regions, ignore.case = T),]
# nam<- data[grep(pattern = "NAM",x = data$Regions, ignore.case = T),]
# ssa<- data[grep(pattern = "SSA",x = data$Regions, ignore.case = T),]


#lista de base de datos y vector con las regiones
pots<- list(alc,sas,eap,eur,fsu,men,nam,ssa)
r<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA")



#scenarios IMPACT model y grupo de tuberculos
sce<- unique(data$Scenarios)
sce2<- c("SSP2-NoCC","SSP2-HGEM2","SSP2-IPSL2" )
crops<- unique(data$Commodity)
roots<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Other Roots","R&T-Cassava") 

# data con solo roots and tubers
for(i in 1:length(roots)){
    data_file<- data[which(data$Commodity==roots[i]),]
    rownames(data_file)<- 1:nrow(data_file)
    write.csv(data_file,paste(copy,roots[[i]],".csv",sep = ""))
    
}

#cassava
cassava<-read.csv(paste(copy,"R&T-Cassava.csv",sep = ""))
# corregir tipo de variable
cassava$Scenarios<- as.character(cassava$Scenarios)
cassava$Commodity<- as.character(cassava$Commodity)
cassava$Regions<- as.character(cassava$Regions)
cassava$Sys<- as.character(cassava$Sys)
cassava$X<-NULL
#reshape
require(plyr)
require(tidyr)
cassava<- cassava %>% spread(Year, Val)


#grandes regiones
q<-c("Africa", "Americas", "Asia", "Europe","Oceania")
fao2<- c("Australia and New Zealand","Caribbean","Central America","Central Asia","Eastern Africa",
       "Eastern Asia","Eastern Europe","Melanesia","Middle Africa","Northern Africa","Northern America",
       "Northern Europe","South America","South-Eastern Asia","Southern Africa","Southern Asia",
       "Southern Europe","Western Africa","Western Asia","Western Europe")

#Regions FAO1
cas<- cassava
cas<- cassava[which(cassava$Regions==q),]
rownames(cas)<- 1:nrow(cas)
cas$change<- ((cas$`2050`- cas$`2020`)/cas$`2020`)*100
casstrends<- cas
casstrends$change<- NULL

#Regions FAO2
cassden<- cassava
##LAC
cassden_alc<- cassden[grep(pattern = "LAC",x = cassden$Regions, ignore.case = T),]
rownames(cassden_alc)<- 1:nrow(cassden_alc)
cassden_alc$change<- ((cassden_alc$`2050`- cassden_alc$`2020`)/cassden_alc$`2020`)*100
cassden_alc<- cassden_alc[,-c(5:14)]
##AFRICA SSA
cassden_africa<- cassden[grep(pattern = "SSA",x = cassden$Regions, ignore.case = T),]
rownames(cassden_africa)<- 1:nrow(cassden_africa)
cassden_africa$change<- ((cassden_africa$`2050`- cassden_africa$`2020`)/cassden_africa$`2020`)*100
cassden_africa<- cassden_africa[,-c(5:14)]
##AFRICA SAS
cassden_sas<- cassden[grep(pattern = "SAS",x = cassden$Regions, ignore.case = T),]
rownames(cassden_sas)<- 1:nrow(cassden_sas)
cassden_sas$change<- ((cassden_sas$`2050`- cassden_sas$`2020`)/cassden_sas$`2020`)*100
cassden_sas<- cassden_sas[,-c(5:14)]
##ASIA EAP
cassden_eap<- cassden[grep(pattern = "EAP",x = cassden$Regions, ignore.case = T),]
rownames(cassden_eap)<- 1:nrow(cassden_eap)
cassden_eap$change<- ((cassden_eap$`2050`- cassden_eap$`2020`)/cassden_eap$`2020`)*100
cassden_eap<- cassden_eap[,-c(5:14)]

#reshape trends
require(plyr)
require(tidyr)
casstrends<- casstrends %>% gather(Year, Val, 5:14,na.rm = T)

#graph1 General Trend------
png(filename=paste(copy,"Regions","_","Cassava_Yield_by_SystemProductionBAR.png",sep=""), 
     width = 14, height = 12, units = 'in', res = 300)
n<- ggplot(data=casstrends,aes(x=Year,y=Val,colour=Scenarios))+ 
    geom_line(aes(group=Scenarios))+
    geom_point()+ facet_grid(Sys~Regions)+
    ylab("Yield ") + ggtitle("R&T-Cassava")+
    xlab("Years") +
    theme(axis.title.x=element_text(size=12, face='bold'))+
    theme(axis.title.y=element_text(size=12, face='bold'))+ 
    theme(plot.title=element_text(size=15, face = 'bold'))+
    ylab('Yields') + xlab("Year")+
    theme(strip.text.x = element_text(size = 14))+
    theme(strip.text.y = element_text(size = 14, angle = 270))+
    theme(axis.text.x=element_text(size=12, angle=90))+
    theme(legend.title = element_text(size=12,face="bold"))+
    theme(legend.text = element_text(size=9))+
    guides(color=guide_legend("Scenarios"))
plot(n)
dev.off()    

#graph2 ALC-----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"ALC","_","HeapMap_ChangesPercentage.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = cassden_alc, aes(Scenarios,Regions)) + 
        geom_tile(aes(fill = change), colour = "white")+ 
        labs(x=NULL, y=NULL, title="R&T-Cassava Yield \n Percentage Change 2005-2050") +
        scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
        scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        facet_grid(.~Sys)
    
plot(n)
dev.off()  

#graph2 SSA----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"SSA","_","HeapMap_ChangesPercentage.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = cassden_africa, aes(Scenarios,Regions)) + 
    geom_tile(aes(fill = change), colour = "white")+ 
    labs(x=NULL, y=NULL, title="R&T-Cassava Yield \n Percentage Change 2005-2050") +
    scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(.~Sys)
n
plot(n)
dev.off()  

#graph2 SAS----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"SAS","_","HeapMap_ChangesPercentage.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = cassden_sas, aes(Scenarios,Regions)) + 
    geom_tile(aes(fill = change), colour = "white")+ 
    labs(x=NULL, y=NULL, title="R&T-Cassava Yield \n Percentage Change 2005-2050") +
    scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(.~Sys)
n
plot(n)
dev.off()  

#graph2 EAP----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"EAP","_","HeapMap_ChangesPercentage.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = cassden_eap, aes(Scenarios,Regions)) + 
    geom_tile(aes(fill = change), colour = "white")+ 
    labs(x=NULL, y=NULL, title="R&T-Cassava Yield \n Percentage Change 2005-2050") +
    scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(.~Sys)
n
plot(n)
dev.off()  


#################################################### YldXAgg ###########################################
#YldXAgg irrigated and Rainfed load data-------
data<- read.csv("TYldXAgg.CSV",header = F)
#adjust labels 
names(data)[1]<-"Scenarios"
names(data)[2]<-"Commodity"
names(data)[3]<- "Regions"   
names(data)[4]<-"Year"
names(data)[5]<-"Val"

# corregir tipo de variable
data$Scenarios<- as.character(data$Scenarios)
data$Commodity<- as.character(data$Commodity)
data$Regions<- as.character(data$Regions)

sce2<- c("SSP2-NoCC","SSP2-HGEM2","SSP2-IPSL2" )
crops<- unique(data$Commodity)
roots<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Other Roots","R&T-Cassava") 

# sub sets by crops
rt<-subset(data,data$Commodity %in% roots )
rownames(rt)<- 1:nrow(rt)
rt<-subset(rt,rt$Scenarios %in% sce2 )
rownames(rt)<- 1:nrow(rt)

# Regions
#America latina
alc<- rt[grep(pattern = "LAC",x = rt$Regions, ignore.case = T),]
rownames(alc)<- 1:nrow(alc)
alc<- unique(alc$Regions)

#Asia
eap<- rt[grep(pattern = "EAP",x = rt$Regions, ignore.case = T),]
rownames(eap)<- 1:nrow(eap)
eap<- unique(eap$Regions)

#AFrica
ssa<- rt[grep(pattern = "SSA",x = rt$Regions, ignore.case = T),]
rownames(ssa)<- 1:nrow(ssa)
ssa<- unique(ssa$Regions)
# sas<- data[grep(pattern = "SAS",x = data$Regions, ignore.case = T),]
# eap<- data[grep(pattern = "EAP",x = data$Regions, ignore.case = T),]
# eur<- data[grep(pattern = "EUR",x = data$Regions, ignore.case = T),]
# fsu<- data[grep(pattern = "FSU",x = data$Regions, ignore.case = T),]
# men<- data[grep(pattern = "MEN",x = data$Regions, ignore.case = T),]
# nam<- data[grep(pattern = "NAM",x = data$Regions, ignore.case = T),]
# ssa<- data[grep(pattern = "SSA",x = data$Regions, ignore.case = T),]

#subset only LAC
rt_alc<- subset(rt, rt$Regions %in% alc)
rownames(rt_alc)<- 1:nrow(rt_alc)
rt_alc<- subset(rt_alc, rt_alc$Scenarios %in% sce2)
#rt_alc<- subset(rt_alc, rt_alc$Commodity %in% crop)

rownames(rt_alc)<- 1:nrow(rt_alc)
#reshape
require(plyr)
require(tidyr)
rt_alc<- rt_alc %>% spread(Year, Val)
rt_alc$change<- ((rt_alc$`2050`- rt_alc$`2020`)/rt_alc$`2020`)*100
rt_alc<- rt_alc[,-c(4:13)]



#graph ALC Yield Total-----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"ALC","_","HeapMap_ChangesPercentageYieldTotal.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = rt_alc, aes(Scenarios,Regions)) + 
    geom_tile(aes(fill = change), colour = "white")+ 
    labs(x=NULL, y=NULL, title="R&T-Cassava Yield  \n Percentage Change 2005-2050") +
    scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(.~Commodity)

plot(n)
dev.off()  

#subset only EAP
rt_eap<- subset(rt, rt$Regions %in% eap)
rownames(rt_eap)<- 1:nrow(rt_eap)
rt_eap<- subset(rt_eap, rt_eap$Scenarios %in% sce2)
#rt_alc<- subset(rt_alc, rt_alc$Commodity %in% crop)

rownames(rt_eap)<- 1:nrow(rt_eap)
#reshape
require(plyr)
require(tidyr)
rt_eap<- rt_eap %>% spread(Year, Val)
rt_eap$change<- ((rt_eap$`2050`- rt_eap$`2020`)/rt_eap$`2020`)*100
rt_eap<- rt_eap[,-c(4:13)]

#graph EAP Yield Total-----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"EAP","_","HeapMap_ChangesPercentageYieldTotal.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = rt_eap, aes(Scenarios,Regions)) + 
    geom_tile(aes(fill = change), colour = "white")+ 
    labs(x=NULL, y=NULL, title="R&T-Cassava Yield  \n Percentage Change 2005-2050") +
    scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(.~Commodity)

plot(n)
dev.off()  



#subset only SSA
rt_ssa<- subset(rt, rt$Regions %in% ssa)
rownames(rt_ssa)<- 1:nrow(rt_ssa)
rt_eap<- subset(rt_ssa, rt_ssa$Scenarios %in% sce2)
#rt_alc<- subset(rt_alc, rt_alc$Commodity %in% crop)

rownames(rt_ssa)<- 1:nrow(rt_ssa)
#reshape
require(plyr)
require(tidyr)
rt_ssa<- rt_ssa %>% spread(Year, Val)
rt_ssa$change<- ((rt_ssa$`2050`- rt_ssa$`2020`)/rt_ssa$`2020`)*100
rt_ssa<- rt_ssa[,-c(4:13)]

#graph EAP Yield Total-----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"SSA","_","HeapMap_ChangesPercentageYieldTotal.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = rt_ssa, aes(Scenarios,Regions)) + 
    geom_tile(aes(fill = change), colour = "white")+ 
    labs(x=NULL, y=NULL, title="R&T-Cassava Yield  \n Percentage Change 2005-2050") +
    scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(.~Commodity)

plot(n)
dev.off()  


#################################################### QNXAgg ###########################################
# manejo de digitos
options(digits=3) 
options(scipen = 999)
#YldXAgg irrigated and Rainfed load data-------
data<- read.csv("QNXAgg.CSV",header = F)
#adjust labels 
names(data)[1]<-"Scenarios"
names(data)[2]<-"Commodity"
names(data)[3]<- "Regions"   
names(data)[4]<-"Year"
names(data)[5]<-"Val"

# corregir tipo de variable
data$Scenarios<- as.character(data$Scenarios)
data$Commodity<- as.character(data$Commodity)
data$Regions<- as.character(data$Regions)

sce2<- c("SSP2-NoCC","SSP2-HGEM2","SSP2-IPSL2" )
crops<- unique(data$Commodity)
roots<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Other Roots","R&T-Cassava") 

# sub sets by crops
rt<-subset(data,data$Commodity %in% roots )
rownames(rt)<- 1:nrow(rt)
rt<-subset(rt,rt$Scenarios %in% sce2 )
rownames(rt)<- 1:nrow(rt)

# Regions
#America latina
alc<- rt[grep(pattern = "LAC",x = rt$Regions, ignore.case = T),]
rownames(alc)<- 1:nrow(alc)
alc<- unique(alc$Regions)

#Asia
eap<- rt[grep(pattern = "EAP",x = rt$Regions, ignore.case = T),]
rownames(eap)<- 1:nrow(eap)
eap<- unique(eap$Regions)

#AFrica
ssa<- rt[grep(pattern = "SSA",x = rt$Regions, ignore.case = T),]
rownames(ssa)<- 1:nrow(ssa)
ssa<- unique(ssa$Regions)
# sas<- data[grep(pattern = "SAS",x = data$Regions, ignore.case = T),]
# eap<- data[grep(pattern = "EAP",x = data$Regions, ignore.case = T),]
# eur<- data[grep(pattern = "EUR",x = data$Regions, ignore.case = T),]
# fsu<- data[grep(pattern = "FSU",x = data$Regions, ignore.case = T),]
# men<- data[grep(pattern = "MEN",x = data$Regions, ignore.case = T),]
# nam<- data[grep(pattern = "NAM",x = data$Regions, ignore.case = T),]
# ssa<- data[grep(pattern = "SSA",x = data$Regions, ignore.case = T),]

#subset only LAC
rt_alc<- subset(rt, rt$Regions %in% alc)
rownames(rt_alc)<- 1:nrow(rt_alc)
rt_alc<- subset(rt_alc, rt_alc$Scenarios %in% sce2)
rt_alc<- subset(rt_alc, rt_alc$Commodity %in% crop)
rownames(rt_alc)<- 1:nrow(rt_alc)

rownames(rt_alc)<- 1:nrow(rt_alc)
#reshape
require(plyr)
require(tidyr)
rt_alc<- rt_alc %>% spread(Year, Val)

## logica de los valores 
nn<-  which(rt_alc$`2050`<0 & rt_alc$`2005`<0) # net trade negativo  importador neto
pn<-  which(rt_alc$`2050`>0 & rt_alc$`2005`<0) # impacto positivo inicia como importador termina como exportador
np<-  which(rt_alc$`2050`<0 & rt_alc$`2005`>0) # impacto negativo inicia como exportador termina como importador
pp<-  which(rt_alc$`2050`>0 & rt_alc$`2005`>0) # net trade positivo  exportador neto

# desempeño 
export<-   c(pp)
import <-  c(nn)
tran_XtoM<- c(np) # inicia exportador termina importador
tran_MtoX<- c(pn) # inicia importador termina exportador


# # copia ojo con el orden
rt_alcNET<- rt_alc
rt_alcNET$trend <- NA
rt_alcNET$trend[tran_MtoX]<- "Transition M to X"
rt_alcNET$trend[export] <- "Positive"
rt_alcNET$trend[tran_XtoM]<- "Transition X to M"
rt_alcNET$trend[import] <- "Negative"

#reshape trends
require(plyr)
require(tidyr)
rt_alcNET<- rt_alcNET %>% gather(Year, Val, 4:13,na.rm = T)

#graph1 General Trend------
png(filename=paste(copy,"ALC","_","Cassava_NET_Trade.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)
n<- ggplot(data=rt_alcNET,aes(x=Year,y=Val,colour=Scenarios))+ 
    geom_line(aes(group=Regions))+
    geom_point()+ facet_grid(.~trend)+
    ylab("Yield ") + ggtitle("R&T-Cassava")+
    xlab("Years") +
    theme(axis.title.x=element_text(size=12, face='bold'))+
    theme(axis.title.y=element_text(size=12, face='bold'))+ 
    theme(plot.title=element_text(size=15, face = 'bold'))+
    ylab('Yields') + xlab("Year")+
    theme(strip.text.x = element_text(size = 14))+
    theme(strip.text.y = element_text(size = 14, angle = 270))+
    theme(axis.text.x=element_text(size=12, angle=90))+
    theme(legend.title = element_text(size=12,face="bold"))+
    theme(legend.text = element_text(size=9))+
    guides(color=guide_legend("Scenarios"))
plot(n)
dev.off()    


#graph ALC Yield Total-----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"ALC","_","HeapMap_ChangesPercentageNETTrade.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = rt_alcNET, aes(Scenarios,Regions)) + 
    geom_tile(aes(fill = change), colour = "white")+ 
    labs(x=NULL, y=NULL, title="R&T-Cassava Net Trade  \n Percentage Change 2005-2050") +
    scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") +
    coord_equal()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(.~trend)

plot(n)
dev.off()  

#subset only EAP
rt_eap<- subset(rt, rt$Regions %in% eap)
rownames(rt_eap)<- 1:nrow(rt_eap)
rt_eap<- subset(rt_eap, rt_eap$Scenarios %in% sce2)
#rt_alc<- subset(rt_alc, rt_alc$Commodity %in% crop)

rownames(rt_eap)<- 1:nrow(rt_eap)
#reshape
require(plyr)
require(tidyr)
rt_eap<- rt_eap %>% spread(Year, Val)
rt_eap$change<- ((rt_eap$`2050`- rt_eap$`2020`)/rt_eap$`2020`)*100
rt_eap<- rt_eap[,-c(4:13)]

#graph EAP Yield Total-----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"EAP","_","HeapMap_ChangesPercentageYieldTotal.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = rt_eap, aes(Scenarios,Regions)) + 
    geom_tile(aes(fill = change), colour = "white")+ 
    labs(x=NULL, y=NULL, title="R&T-Cassava Yield  \n Percentage Change 2005-2050") +
    scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(.~Commodity)

plot(n)
dev.off()  



#subset only SSA
rt_ssa<- subset(rt, rt$Regions %in% ssa)
rownames(rt_ssa)<- 1:nrow(rt_ssa)
rt_eap<- subset(rt_ssa, rt_ssa$Scenarios %in% sce2)
#rt_alc<- subset(rt_alc, rt_alc$Commodity %in% crop)

rownames(rt_ssa)<- 1:nrow(rt_ssa)
#reshape
require(plyr)
require(tidyr)
rt_ssa<- rt_ssa %>% spread(Year, Val)
rt_ssa$change<- ((rt_ssa$`2050`- rt_ssa$`2020`)/rt_ssa$`2020`)*100
rt_ssa<- rt_ssa[,-c(4:13)]

#graph EAP Yield Total-----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(copy,"SSA","_","HeapMap_ChangesPercentageYieldTotal.png",sep=""), 
    width = 14, height = 12, units = 'in', res = 300)

n<- ggplot(data = rt_ssa, aes(Scenarios,Regions)) + 
    geom_tile(aes(fill = change), colour = "white")+ 
    labs(x=NULL, y=NULL, title="R&T-Cassava Yield  \n Percentage Change 2005-2050") +
    scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_grid(.~Commodity)

plot(n)
dev.off()  


#################################################### QNXAgg ###########################################
#YldXAgg irrigated and Rainfed load data


##### End Code
g=gc
rm(list = ls())
