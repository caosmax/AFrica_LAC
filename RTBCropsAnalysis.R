# codigo para cargar datos por grupos y dejarlos listos para el procesamiento.
# Autor Carlos Edo Gonzalez R. 
g=gc;rm(list = ls())
# directories-------
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/USAIDForGFSF")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/Graphs/")
grp<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/CSVfiles/")
rdsFiles<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")
      
# librerias------------
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

#################################################### load files ###########################################


# Datos por sistema de produccion  riego y secano---------------
datasys<- c("YldXAgg","AreaXAgg")
clist<-list()
for( i in 1:length(datasys)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",datasys[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- datasys[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Sys"    
      names(clist[[i]])[5]<- "Year"
      names(clist[[i]])[6]<- "Val"
      cat(paste("Running the impactparameter ",datasys[i],  " it's done\n", sep = "" ))
      
      }
datasys<- do.call(rbind, clist)
saveRDS(datasys, file = paste(rdsFiles, "datasys.rds",sep = ""))
rm(clist,datasys)

# Datos categorias totales-------------
datatotal<- c("TAreaXAgg", "TYldXAgg") # agregado TYldXAgg, "QSupXAgg"
clist<-list()
for( i in 1:length(datatotal)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",datatotal[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- datatotal[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Year"    
      names(clist[[i]])[5]<- "Val"
      cat(paste("Running the impactparameter ",datatotal[i],  " it's done\n", sep = "" ))
      
}
datatotal<- do.call(rbind, clist)
saveRDS(datatotal, file = paste(rdsFiles, "datatotal.rds",sep = ""))
rm(clist,datatotal)


# Datos categorias agregados---------- 
dataagg<- c("QINTXAgg","QBFXAgg","QFXAgg", "QLXAgg","QOTHRXAgg" )
clist<-list()
for( i in 1:length(dataagg)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",dataagg[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- dataagg[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Year"    
      names(clist[[i]])[5]<- "Val"
      cat(paste("Running the impactparameter ",dataagg[i],  " it's done\n", sep = "" ))
      
}
dataagg<- do.call(rbind, clist)
saveRDS(dataagg, file = paste(rdsFiles, "dataagg.rds",sep = ""))
rm(clist,dataagg)

# datos categorias animales--------
dataanimal<- c("AnmlNumXAgg" , "AnmlYldXAgg")
clist<-list()
for( i in 1:length(dataanimal)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",dataanimal[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- dataanimal[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Year"    
      names(clist[[i]])[5]<- "Val"
      cat(paste("Running the impactparameter ",dataanimal[i],  " it's done\n", sep = "" ))
      
}
dataanimal<- do.call(rbind, clist)
saveRDS(dataanimal, file = paste(rdsFiles, "dataanimal.rds",sep = ""))
rm(clist,dataanimal)

# data net trade & food -------
TradeFood<- c("QNXAgg", "FoodAvailXAgg","PerCapKCalCXAgg", "QDXAgg","QSupXAgg")#"TYldXAgg"
clist<-list()
#i=3
for( i in 1:length(TradeFood)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",TradeFood[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- TradeFood[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Year"    
      names(clist[[i]])[5]<- "Val"
      cat(paste("Running the impactparameter ",TradeFood[i],  " it's done\n", sep = "" ))
      
}
TradeFood<- do.call(rbind, clist)
saveRDS(TradeFood, file = paste(rdsFiles, "TradeFood.rds",sep = ""))
rm(clist,TradeFood)



# data socieconomic-------
EcoFood<- c("PopulationAtRiskXagg","PopXAgg","TotalMalnourishedXagg")
clist<-list()
for( i in 1:length(EcoFood)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",EcoFood[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- EcoFood[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Regions"
      names(clist[[i]])[3]<-"Year"    
      names(clist[[i]])[4]<-"Val"
      cat(paste("Running the impactparameter ",EcoFood[i],  " it's done\n", sep = "" ))
}
EcoFood<- do.call(rbind, clist)
saveRDS(EcoFood, file = paste(rdsFiles, "EcoFood.rds",sep = ""))
rm(clist,EcoFood)


# data socieconomic2-------
EcoFood2<- c("GDPXAgg","pcGDPXAgg")
clist<-list()
for( i in 1:length(EcoFood2)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",EcoFood2[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- EcoFood2[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Regions"
      names(clist[[i]])[3]<-"Year"    
      names(clist[[i]])[4]<-"Val"
      cat(paste("Running the impactparameter ",EcoFood2[i],  " it's done\n", sep = "" ))
}
EcoFood2<- do.call(rbind, clist)
saveRDS(EcoFood2, file = paste(rdsFiles, "EcoFood2.rds",sep = ""))
rm(clist,EcoFood2)


# data socieconomic3-------
EcoFood3<- c("ShareAtRiskXagg")
clist<-list()
for( i in 1:length(EcoFood3)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",EcoFood3[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- EcoFood3[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Regions"
      names(clist[[i]])[3]<-"Year"    
      names(clist[[i]])[4]<-"Val"
      cat(paste("Running the impactparameter ",EcoFood3[i],  " it's done\n", sep = "" ))
}
EcoFood3<- do.call(rbind, clist)
saveRDS(EcoFood3, file = paste(rdsFiles, "EcoFood3.rds",sep = ""))
rm(clist,EcoFood3)


#Data parameters Water-------
green<- c("GreenwatXAgg")
clist<-list()

for( i in 1:length(green)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",green[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- green[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<-"Commodity"
      names(clist[[i]])[3]<-"Regions"    
      names(clist[[i]])[4]<-"Sys"
      names(clist[[i]])[5]<-"Year"
      names(clist[[i]])[6]<-"Val"
      cat(paste("Running the impactparameter ",green[i],  " it's done\n", sep = "" ))
}
green<- do.call(rbind, clist)
saveRDS(green, file = paste(rdsFiles, "green.rds",sep = ""))
rm(clist,green)


#Data parameter shock climatico-----------
Blue<- c("BlueWatXAgg")
clist<-list()

for( i in 1:length(Blue)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",Blue[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- Blue[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<-"Commodity"
      names(clist[[i]])[3]<-"Regions"    
      names(clist[[i]])[4]<-"Year"
      names(clist[[i]])[5]<-"Val"
      cat(paste("Running the impactparameter ",Blue[i],  " it's done\n", sep = "" ))
}
Blue<- do.call(rbind, clist)
saveRDS(Blue, file = paste(rdsFiles, "Blue.rds",sep = ""))
rm(clist,Blue)

#Data parameter shock climatico--------------
shock<- c("YldCliShkXAgg")
clist<-list()
for( i in 1:length(shock)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",shock[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- shock[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<-"Regions" 
      names(clist[[i]])[4]<-"Sys"
      names(clist[[i]])[5]<-"Year"
      names(clist[[i]])[6]<-"Val"
      cat(paste("Running the impactparameter ",shock[i],  " it's done\n", sep = "" ))
}
shock<- do.call(rbind, clist)
saveRDS(shock, file = paste(rdsFiles, "shock.rds",sep = ""))
rm(clist,shock)

# Precios --------------
precios<- c("PPXAgg","PCXAgg")
clist<-list()

for( i in 1:length(precios)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",precios[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- precios[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<-"Regions" 
      names(clist[[i]])[4]<-"Year"
      names(clist[[i]])[5]<-"Val"
      cat(paste("Running the impactparameter ",precios[i],  " it's done\n", sep = "" ))
}
precios<- do.call(rbind, clist)
saveRDS(precios, file = paste(rdsFiles, "precios.rds",sep = ""))
rm(clist,precios)


# 


