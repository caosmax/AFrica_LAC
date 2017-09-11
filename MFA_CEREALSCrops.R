#MFA Cereals
### PCA for RTB crops 
### Carlos Eduardo Gonzalez R. 
### RTB Analysis
g=gc;rm(list = ls())

# librerias------------
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse)) 
# suppressMessages(library(modelr)) 
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



# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/factoextra")
# 

abbreviate("percentage") 
options(warn = -1); options(scipen = 999) 
options(digits=3) 



############################################################# BIG Regions ####################################################################

rdsFiles<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")
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
jrtb<- c("jbarl","jmaiz", "jmill", "jrice","jsorg","jwhea", "jocer")



t<- c(2010, 2030,2050)
# Vector con los cultivos para RTB incluyendo Bananas
rtb<- c("CER-Barley","CER-Maize","CER-Millet","CER-Rice","CER-Sorghum","CER-Wheat","CER-Other Cereals") 

cfiles<-list.files(path = rdsFiles, pattern = "Blue.rds|dataagg.rds|datatotal.rds|precios.rds|TradeFood.rds",full.names = T)
cfiles<- lapply(cfiles, readRDS)
cdata<-cfiles
# primer grupo de variables------------ 

for(i in 1:length(cdata)){
      cdata[[i]]$Scenarios<-  gsub("'",'',cdata[[i]]$Scenarios)
      cdata[[i]]$Commodity<-  gsub("'", '',cdata[[i]]$Commodity)
      cdata[[i]]$Regions<-  gsub("'", '',cdata[[i]]$Regions)
      cdata[[i]]$Year<-  gsub("'",'',cdata[[i]]$Year)
      
      cdata[[i]]$Scenarios<- as.character( cdata[[i]]$Scenarios)
      cdata[[i]]$Commodity<- as.character( cdata[[i]]$Commodity)
      cdata[[i]]$Regions<- as.character( cdata[[i]]$Regions) 
      
      cdata[[i]]<- filter(cdata[[i]], Scenarios %in% s)
      cdata[[i]]<- filter(cdata[[i]], !Regions %in% rall) 
      
      
      cdata[[i]]<- filter(cdata[[i]], Commodity %in% rtb)
      
      cdata[[i]]<-  cdata[[i]]%>% spread(Year, Val) 
      cdata[[i]]$change<- ((cdata[[i]]$`2050`-cdata[[i]]$`2010`)/cdata[[i]]$`2010`)*100
      cdata[[i]]<-  cdata[[i]][,c("Scenarios", "Commodity", "Regions",  "parameter", "change" )]
      
      cdata[[i]]<-  cdata[[i]]%>% spread(Scenarios, change) 
      colnames(cdata[[i]])<- c("Commodity", "Regions","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
      
      print(i)
}

crbind<- do.call(rbind, cdata)


# Por sistema -------- 
jfiles<-list.files(path = rdsFiles, pattern = "datasys.rds|green.rds",full.names = T) #|shock.rds|ipr.rds
jfiles<- lapply(jfiles, readRDS)
jdata<-jfiles

countries <- read.csv(file = paste(rdsFiles,"IPRsLabelsRegions.csv", sep=""), header = T)
colnames(countries)<- c("Regions", "IMPACT.Name")

#i=3
for(i in 1:length(jdata)){
      
      jdata[[i]]$Scenarios<-  gsub("'",'',jdata[[i]]$Scenarios)
      jdata[[i]]$Commodity<-  gsub("'", '',jdata[[i]]$Commodity)
      jdata[[i]]$Regions<-  gsub("'", '',jdata[[i]]$Regions)
      jdata[[i]]$Year<-  gsub("'",'',jdata[[i]]$Year)
      jdata[[i]]$Sys<-  gsub("'",'',jdata[[i]]$Sys)
      jdata[[i]]$parameter<-  gsub("'",'',jdata[[i]]$parameter)
      jdata[[i]]<- filter(jdata[[i]], Scenarios %in% s)
      
      jdata[[i]]<- filter(jdata[[i]], !Regions %in% rall) 
      jdata[[i]]<- filter(jdata[[i]], Commodity %in% rtb)
      jdata[[i]]<- jdata[[i]][,c("Scenarios", "Commodity","Regions","parameter", "Sys", "Year","Val") ]
      jdata[[i]]<-  jdata[[i]]%>% spread(Year, Val) 
      jdata[[i]]$change<- ((jdata[[i]]$`2050`-jdata[[i]]$`2010`)/jdata[[i]]$`2010`)*100
      jdata[[i]]<-  jdata[[i]][,c("Scenarios", "Commodity", "Regions",  "parameter","Sys", "change" )]
      
      jdata[[i]]<-  jdata[[i]]%>% spread(Scenarios, change) 
      colnames(jdata[[i]])<- c("Commodity", "Regions","parameter","Sys", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
      
      print(i)
}


jrbind<- do.call(rbind, jdata)


# Economic Variables1--------------
efiles<-list.files(path = rdsFiles, pattern = "EcoFood.rds",full.names = T)
efiles<- lapply(efiles, readRDS)
erbind<- do.call(rbind, efiles)

#ajuste y corregir asuntos de texto
erbind$Scenarios<-  gsub("'",'',erbind$Scenarios)
erbind$Regions<-  gsub("'", '',erbind$Regions)
erbind$Year<-  gsub("'",'',erbind$Year)
erbind$Scenarios<- as.character( erbind$Scenarios)
erbind$Regions<- as.character(erbind$Regions) 


erbind<-   filter(erbind, Scenarios %in% s) %>% filter(., !Regions %in% rall) %>% spread(Year, Val) 
erbind$change<- ((erbind$`2050`-erbind$`2010`)/erbind$`2010`)*100
erbind<- erbind[,c("Scenarios","Regions","parameter","change")]

erbind<-  erbind %>% spread(Scenarios, change) 
colnames(erbind)<- c("Regions","parameter","HIGH+NARS","HIGH","RMM", "REGION", "REF")


# Economic Variables2--------------
efiles2<-list.files(path = rdsFiles, pattern = "EcoFood2.rds",full.names = T)
efiles2<- lapply(efiles2, readRDS)
erbind2<- do.call(rbind, efiles2)

#ajuste y corregir asuntos de texto
erbind2$Scenarios<-  gsub("'",'',erbind2$Scenarios)
erbind2$Regions<-  gsub("'", '',erbind2$Regions)
erbind2$Year<-  gsub("'",'',erbind2$Year)
erbind2$Scenarios<- as.character( erbind2$Scenarios)
erbind2$Regions<- as.character(erbind2$Regions) 


erbind2<- erbind2 %>% filter(Scenarios %in% s) %>% 
      filter(., !Regions %in% rall) %>% 
      spread(Year, Val) 

erbind2$change<- ((log10(erbind2$`2050`)- log10(erbind2$`2010`))/(2050-2010))
erbind2<- erbind2[,c("Scenarios","Regions","parameter","change")]
erbind2<-  erbind2 %>% spread(Scenarios, change) 
colnames(erbind2)<- c("Regions","parameter","HIGH+NARS","HIGH","RMM", "REGION", "REF")

#Economic variables3-------------
efiles3<-list.files(path = rdsFiles, pattern = "EcoFood3.rds",full.names = T)
efiles3<- lapply(efiles3, readRDS)
erbind3<- do.call(rbind, efiles3)

#ajuste y corregir asuntos de texto
erbind3$Scenarios<-  gsub("'",'',erbind3$Scenarios)
erbind3$Regions<-  gsub("'", '',erbind3$Regions)
erbind3$Year<-  gsub("'",'',erbind3$Year)
erbind3$Scenarios<- as.character( erbind3$Scenarios)
erbind3$Regions<- as.character(erbind3$Regions) 


erbind3<- erbind3 %>% filter(Scenarios %in% s) %>% 
      filter(., !Regions %in% rall) %>% 
      spread(Year, Val) 

erbind3$change<- (erbind3$`2050`- erbind3$`2010`)
erbind3<- erbind3[,c("Scenarios","Regions","parameter","change")]
erbind3<-  erbind3 %>% spread(Scenarios, change) 

colnames(erbind3)<- c("Regions","parameter","HIGH+NARS","HIGH","RMM", "REGION", "REF")


############################### Tratamiento y construccion de grupos de variables ######################

#detection and deleted of NA
crbind[is.na(crbind)]<- 0
erbind[is.na(erbind)]<- 0
jrbind[is.na(jrbind)]<- 0
erbind2[is.na(erbind2)]<- 0
erbind3[is.na(erbind3)]<- 0


crbind<- crbind %>% gather(Sce, change, 4:8)
erbind<- erbind %>% gather(Sce, change, 3:7)
erbind2<- erbind2 %>% gather(Sce, change, 3:7)
erbind3<- erbind3 %>% gather(Sce, change, 3:7)
jrbind<- jrbind %>% gather(Sce, change, 5:9)


###################################### Tratamiento de datos economicos #################################
#economic1
ad_erbind<- erbind %>% split(erbind$Sce)
efiles<- list()

for(i in 1:length(ad_erbind)){ 
      efiles[[i]] <- ad_erbind[[i]] %>% spread(parameter, change) %>% 
            gather(Variable, Summary, -(Regions:Sce)) %>%
            unite(Temp, Sce, Variable)%>% spread(Temp, Summary) 
      
}

#economic2
ad_erbind2<- erbind2 %>% split(erbind2$Sce)
efiles2<- list()

for(i in 1:length(ad_erbind2)){ 
      efiles2[[i]] <- ad_erbind2[[i]] %>% spread(parameter, change) %>% 
            gather(Variable, Summary, -(Regions:Sce)) %>%
            unite(Temp, Sce, Variable)%>% spread(Temp, Summary) 
      
}

#economic3
ad_erbind3<- erbind3 %>% split(erbind3$Sce)
efiles3<- list()

for(i in 1:length(ad_erbind3)){ 
      efiles3[[i]] <- ad_erbind3[[i]] %>% spread(parameter, change) %>% 
            gather(Variable, Summary, -(Regions:Sce)) %>%
            unite(Temp, Sce, Variable)%>% spread(Temp, Summary) 
      
}

######################################## Irrigacion  ###################################################

ad_jrbind<- jrbind %>% split(jrbind$Sce)
jfiles<- list()
#i=1
for(i in 1:length(ad_jrbind)){ 
      jfiles[[i]] <- ad_jrbind[[i]] %>% spread(parameter, change) %>% 
            gather(Variable, Summary, -(Commodity:Sce)) %>%
            unite(Temp, Sys, Variable)%>% spread(Temp, Summary) %>%
            gather(Variable, Summary, -(Commodity:Sce))%>%
            unite(Temp, Sce, Variable)%>% spread(Temp, Summary)
      
}


######################################### Analisis PCA #################################################
???