#Programa para generar graficos proyecto BID-----
#cargar librerias----
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(broom)


# Definir directorio de trabajo-------------
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID")

# Direción graficos-----------------------
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/graphs/")
grp<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase2/Test/")

copyHot<- ("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Hotspots/IMPACT/")

#Cargar marco de datos principal
md<- read.csv("Phase2/TestAllRegionsBID.csv",header=T) #five crops
md2<- read.csv("Phase2/TestgraphBenBID.csv", header = T)
Parameters<- c("GDPXAgg -- Income",
               "pcGDPXAgg -- Per Capita Income",
               "PopulationAtRiskXagg - Pop at Risk of Hunger",
               "ShareAtRiskXagg -- Share at Risk of Hunger",
               "PopXAgg -- Population","TAreaXAgg -- Total Area",
               "FoodAvailXAgg",
               "QDXAgg -- Total Demand", 
               "QMSHXAgg -- Import Share of Demand ",
               "QSupXAgg -- Commodity Supply")



#Hacer un subconjunto que sólo contenga las variables de mi interés y los 5 contenga los cinco cultivos analizados
mdsub<- filter(md, impactparameter %in% Parameters ) 
mdsub$impactparameter<- as.character(mdsub$impactparameter)
mdsub$scenario<- as.character(mdsub$scenario)
mdsub$commodity<- as.character(mdsub$commodity)
mdsub$region<- as.character(mdsub$region)
mdsub$productiontype<- as.character(mdsub$productiontype)

mdsub<- mdsub[grep(pattern = "LAC",x = mdsub$region, ignore.case = T),]
write.csv(x = mdsub, file = paste(copyHot,"five_crops_datos.csv", sep = ""))



#Hacer un subconjunto que sólo contenga las variables de mi interés y los 5 contenga los cinco cultivos analizados
mdsub2<- filter(md2, impactparameter %in% Parameters ) 
mdsub2$impactparameter<- as.character(mdsub2$impactparameter)
mdsub2$scenario<- as.character(mdsub2$scenario)
mdsub2$commodity<- as.character(mdsub2$commodity)
mdsub2$region<- as.character(mdsub2$region)
mdsub2$productiontype<- as.character(mdsub2$productiontype)

mdsub2<- mdsub2[grep(pattern = "LAC",x = mdsub2$region, ignore.case = T),]

write.csv(x = mdsub2, file = paste(copyHot,"All_crops_datos.csv", sep = ""))
