# camboya exploring data 

g=gc;rm(list = ls())

# librerias------------
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse)) 
suppressMessages(library(modelr)) 
suppressMessages(library(purrr)) 
suppressMessages(library(broom)) 
suppressMessages(library(tidyr)) 
suppressMessages(library(corrplot)) 
suppressMessages(library(FactoMineR)) 
suppressMessages(library(factoextra)) 
suppressMessages(library(cluster)) 
suppressMessages(library(RCurl)) 
suppressMessages(library(ggthemes)) 
suppressMessages(library(tidyquant))
suppressMessages(library(devtools))
suppressMessages(library(mvoutlier))
suppressMessages(library(R.utils))
suppressMessages(library(RColorBrewer))



# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/factoextra")
# 

abbreviate("percentage") 
options(warn = -1); options(scipen = 999) 
options(digits=3) 



############################################################# BIG Regions ####################################################################

# rdsFiles<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")
rdsFiles<-c("/mnt/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")

# Big Regions[
r<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA")


s<- c("SSP2-HGEM-HiYld2","SSP2-HGEM-RegYld2","SSP2-HGEM-HiNARS2", "SSP2-HGEM-MMEFF2","SSP2-HGEM2")

# Parametro 2 All Countries.
r2<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA", "Africa","Americas","DVD", "DVG","WLD")
r3<- c("Africa","Americas", "Asia","Europe", "Oceania")
r4<- c("Australia and New Zealand","Caribbean","Central America", "Central Asia","Eastern Africa","Eastern Asia","Eastern Europe","Melanesia",
       "Middle Africa","Northern Africa","Northern America","Northern Europe","South America","South-Eastern Asia","Southern Africa","Southern Asia",
       "Southern Europe","Western Africa","Western Asia", "Western Europe", "Western and Central Asia")
r5<- c("MENg","EAPg")
rall<- c(r2,r3,r4, r5)
jrtb<- c("jbana","jcass", "jpota", "jswpt","jyams","jorat")


t<- c(2010, 2030,2050)
# Vector con los cultivos para RTB incluyendo Bananas
rtb<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Cassava","F&V-Banana", "F&V-Plantain") #"R&T-Other Roots"
rtb2<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Cassava","F&V-Banana", "F&V-Plantain","R&T-Other Roots")

# cargo datos de area y rendimiento
cfiles<-list.files(path = rdsFiles, pattern = "datatotal.rds",full.names = T)
cfiles<- lapply(cfiles, readRDS)
cdata<-cfiles
cdata<- do.call(rbind,cdata)

# quitar comillas
cdata$Scenarios<-  gsub("'",'',cdata$Scenarios)
cdata$Commodity<-  gsub("'", '',cdata$Commodity)
cdata$Regions<-  gsub("'", '',cdata$Regions)
cdata$Year<-  gsub("'",'',cdata$Year)

cdata$parameter<- as.character( cdata$parameter)
cdata$Scenarios<- as.character( cdata$Scenarios)
cdata$Commodity<- as.character( cdata$Commodity)
cdata$Regions<- as.character( cdata$Regions) 

cdata<- filter(cdata, Scenarios %in% s)


##### Graph area, rendimiento----------------
yfiled<- cdata
# yfiled<- filter(yfiled, Regions %in% r) 
yfiled<- yfiled[grep(pattern = "EAP",x = yfiled$Regions, ignore.case = T),]
yfiled<- filter(yfiled, Commodity=="R&T-Cassava")

 
# yfiled<- filter(yfiled, Year==2050)
yfiled<- yfiled %>% spread(Scenarios, Val)

yfiled<- filter(yfiled, Regions!="EAP")
yfiled<- filter(yfiled, Regions!="EAPg")
yfiled<- filter(yfiled, Regions!="EAP-Other Pacific Ocean")
yfiled<- filter(yfiled, Regions!="EAP-Other Southeast Asia")


colnames(yfiled)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )

yfiled<- yfiled %>% gather(Scenarios, Val, 5:ncol(yfiled)) %>% spread(Year, Val)   
yfiled$change<- ((yfiled$`2050`-yfiled$`2020`)/yfiled$`2020`) *100
      
# yfiled$HIGH.NARSdif<- ((yfiled$`HIGH+NARS`- yfiled$REF)/yfiled$REF)*100
# yfiled$HIGHdif<- ((yfiled$HIGH- yfiled$REF)/yfiled$REF)*100
# yfiled$RMMdif<- ((yfiled$RMM- yfiled$REF)/yfiled$REF)*100
# yfiled$REGIONdif<- ((yfiled$REGION- yfiled$REF)/yfiled$REF)*100
yfiled<- yfiled[,-c(5:14)]
# yfiled$Year<- NULL
# colnames(yfiled)<- c("Commodity", "Regions","parameter", "HIGH+NARS","HIGH","RMM", "REGION")

# yfiled<- yfiled %>% gather(Scenarios, Percentage, 4:ncol(yfiled))
# yfiled<- as.data.frame(yfiled)

yfiled$Commodity<-  gsub("R&T-", '',yfiled$Commodity)
yfiled$Regions<-  gsub("EAP-", '',yfiled$Regions)

yfiled$parameter<- plyr::revalue(yfiled$parameter, c("QSupXAgg"="Supply","TAreaXAgg"="Area","TYldXAgg"="Yield"))
yfiled$parameter <- factor(yfiled$parameter, levels = c("Yield","Area","Supply"))

breaks <- seq(from=min(range(yfiled$change)), to=max(range(yfiled$change)), length.out=100)
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab') 

y<- filter(yfiled, parameter=="Supply")
y$Scenarios <- factor(y$Scenarios, levels = c("HIGH","HIGH+NARS","REGION","RMM","REF" ))

write.csv(x = y,file = paste(rdsFiles,"Camboya_HeapMapTestLinux.csv",sep=""))

png(filename=paste(rdsFiles,"Camboya_HeapMapTestLINUX.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)

n<- ggplot(data =y, aes(Scenarios, Regions)) + 
      geom_tile(aes(fill = change), colour = "white")+  facet_grid(.~parameter,drop = T)+
      labs(x=NULL, y=NULL) +
      scale_fill_gradientn(colours = hm.palette(100))+ # limits=c(-20,115)
      #scale_fill_gradient2(low = "blue", high = "red", midpoint =0)+
      coord_equal()+ 
     # theme(legend.text = "AAA")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 
      


plot(n)
dev.off()  



# nuevo grafico

png(filename=paste(rdsFiles,"Camboya_PolarTest1.png",sep=""), 
    width = 20, height = 10, units = 'in', res = 300)

p<- ggplot(y, aes(x=Regions, y=change, fill=change)) +
      geom_bar(stat="identity", position = "dodge") +theme_light()  + facet_grid(.~Scenarios)+
      scale_fill_gradient2("Percentage\nChange(%)",low = "black", mid = "white", high = "red", limits=c(-20,120))+
      theme(axis.title.y=element_text(angle=0))+ theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))+
      labs(x = "",y = "", color = "Legend Title\n")+  
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      theme() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+ coord_equal()+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 

      
p<-p + coord_polar()+ aes(x=reorder(Regions, change)) +
      theme(axis.text.x = element_text(angle=0))+labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      theme() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 
     
    
plot(p)
dev.off()  


##

png(filename=paste(rdsFiles,"Camboya_PolarTest2.png",sep=""), 
    width = 24, height = 10, units = 'in', res = 300)

p1<- ggplot(y, aes(x=Regions, y=change, fill=change)) +
      geom_bar(stat="identity") +theme_light()  + facet_grid(.~Scenarios)+
      scale_fill_gradient2("Percentage\nChange(%)",low = "black", mid = "white", high = "red", limits=c(-20,120))+
      theme(axis.title.y=element_text(angle=0))+ theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))+
      labs(x = "",y = "", color = "Legend Title\n")+  
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      theme_gray() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+ coord_equal()+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


p1<-p1 + coord_polar()+ aes(x=reorder(Regions, change)) +
      theme(axis.text.x = element_text(angle=0))+labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      theme() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 12))+
      theme(axis.text.y = element_text(hjust = 1, size = 12))+
      theme(strip.text.x = element_text(size = 12, face = "bold.italic"))+
      theme(strip.text=element_text(size=10))+
      theme(strip.text.y = element_text(angle = 0,size = 12)) 


plot(p1)
dev.off()  

      
#### net trade
cfiles<- readRDS(file = paste(rdsFiles,"TradeFood.rds", sep = ""))

#Cambiar la categoria de las variables  & corregir single quotes importadas de .csv
cfiles$Scenarios<- as.character(cfiles$Scenarios);cfiles$Commodity<- as.character(cfiles$Commodity)
cfiles$Regions<- as.character(cfiles$Regions) 

cfiles$Scenarios<-  gsub("'", '',cfiles$Scenarios)
cfiles$Commodity<-  gsub("'", '',cfiles$Commodity)
cfiles$Regions<-  gsub("'", '',cfiles$Regions)
cfiles$Year<-  gsub("'", '',cfiles$Year)


#List crops, creación de subgrupos por regiones usando patrones
zone<- unique(cfiles$Regions)
eap<- cfiles[grep(pattern = "EAP",x = cfiles$Regions, ignore.case = T),]
eap<- filter(eap,Commodity=="R&T-Cassava")
# Eliminacion de los escenarios de cambio climatico. NoCC
sceNoCC<-  c("SSP2-NoCC", "SSP2-NoCC-SWHC2", "SSP2-NoCC-IRREXP-WUE2", "SSP2-NoCC-Pangloss2", "SSP2-NoCC-IRREXP2")
eap<- filter(eap, Scenarios %in% s)
eap<- eap %>% spread(Scenarios, Val)
sce<- unique(eap$Scenarios)
colnames(eap)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION","REF")
eap<- eap %>% gather(Scenarios, Val, 5:ncol(eap))
eap<- filter(eap, Regions!="EAP")
eap<- filter(eap, Regions!="EAPg")
eap<- filter(eap, Regions!="EAP-Other Pacific Ocean")


# Quitar la palabra SSP2.
eap$Commodity<-  gsub("R&T-", "",eap$Commodity)
# Quitar la palabar LAC.
eap$Regions<-  gsub("^EAP-", "",eap$Regions)

tznet<- eap[which(eap$parameter=="QNXAgg"),]
tznet<- filter(tznet, Year!=2005)
tznet<- tznet %>% spread(Year, Val)

row.names(tznet)<- 1:nrow(tznet)
crops<- c(unique(tznet$Commodity))
sce<- c(unique(tznet$Scenarios))
fpu<- c(unique(tznet$Regions))

tznet$Scenarios <- factor(tznet$Scenarios, levels = c("HIGH","HIGH+NARS","REGION","RMM","REF" ))

# i=j=f=2
# grafico 1. 
for(f in 1:length(fpu)){ 
      z <- tznet[which(tznet$Regions==fpu[f]),]
      rownames(z)<-1:nrow(z)
      
      z<- z[,c("Scenarios", "Commodity", "Regions", "parameter", "2010","2015","2020",
               "2025", "2030","2035","2040",  "2045", "2050")]
      
      z<- z %>% gather("Year", "Val", 5:ncol(z))
      
      png(filename=paste(rdsFiles,"Comabodia_NetTrade",fpu[f],".png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      c<-ggplot(data=z, aes(Year,Val,group=Scenarios,color=Scenarios)) +
            geom_line(linetype="dashed",size=1)+ facet_grid(.~Commodity) +
            labs(title=paste("Net Trade by ",fpu[f], " ,cassava\n All scenarios",sep = ""),y="Net trade (000 mt)",x="Year")+
            theme(strip.text.y = element_text(angle = 0,size = 12)) +  geom_hline(aes(yintercept=0)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme_grey() + labs(x = "",y = "")+
            theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) 
      
      plot(c)
      dev.off()  
      cat(paste( ' running in ', fpu[f], ' It has been complete\n', sep=''))
}


# grafico 2

png(filename=paste(rdsFiles,"RTB_crops_NetTrade",fpu[f],".png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data = tanz, aes(Scenarios,Regions)) + 
      geom_tile(aes(fill = Trend), colour = "white")+  facet_grid(~Commodity, scales = "free")+
      labs(x=NULL, y=NULL, title=("R&T crops, parameter= Net Trade, by all scenarios \n  Trends (2005-2050)")) +
      theme_grey() + labs(x = "",y = "") + 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))


plot(n)
dev.off()  


