# Yields RTB crops # Autor Carlos Eduardo Gonzalez CIAT. 
# 050/08/2017 Cali Colombia

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
library(png)

# manejo de digitos
options(digits=3) 
options(scipen = 999)

#################################################### Load files ###########################################
rdsFiles<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")
cfiles<- readRDS(file = paste(rdsFiles,"datatotal.RDS", sep = ""))

#  Cambiar la categoria de las variables  & corregir single quotes importadas de .csv
cfiles$Scenarios<- as.character(cfiles$Scenarios);cfiles$Commodity<- as.character(cfiles$Commodity)
cfiles$Regions<- as.character(cfiles$Regions) 

cfiles$Scenarios<-  gsub("'", '',cfiles$Scenarios)
cfiles$Commodity<-  gsub("'", '',cfiles$Commodity)
cfiles$Regions<-  gsub("'", '',cfiles$Regions)
cfiles$Year<-  gsub("'", '',cfiles$Year)


# List crops, creación de subgrupos por regiones usando patrones
zone<- unique(cfiles$Regions)
alc<- cfiles[grep(pattern = "LAC",x = cfiles$Regions, ignore.case = T),]
sas<- cfiles[grep(pattern = "SAS",x = cfiles$Regions, ignore.case = T),]
eap<- cfiles[grep(pattern = "EAP",x = cfiles$Regions, ignore.case = T),]
eur<- cfiles[grep(pattern = "EUR",x = cfiles$Regions, ignore.case = T),]
fsu<- cfiles[grep(pattern = "FSU",x = cfiles$Regions, ignore.case = T),]
men<- cfiles[grep(pattern = "MEN",x = cfiles$Regions, ignore.case = T),]
nam<- cfiles[grep(pattern = "NAM",x = cfiles$Regions, ignore.case = T),]
ssa<- cfiles[grep(pattern = "SSA",x = cfiles$Regions, ignore.case = T),]



#lista de base de datos como vectores para ser usandos en proceso mas grandes
pots<- list(alc,sas,eap,eur,fsu,men,nam,ssa)
#r<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA")


# Escenarios IMPACT model y grupo de tuberculos
sce<- unique(cfiles$Scenarios)
crops<- unique(cfiles$Commodity)

# Vector con los cultivos para RTB incluyendo Bananas
rtb<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Other Roots","R&T-Cassava","F&V-Banana") 

# Eliminacion de los escenarios de cambio climatico. NoCC
sceNoCC<-  c("SSP2-NoCC", "SSP2-NoCC-SWHC2", "SSP2-NoCC-IRREXP-WUE2", "SSP2-NoCC-Pangloss2", "SSP2-NoCC-IRREXP2")

# Data con solo roots and tubers
z<- list()
for(i in 1:length(rtb)){
      #filtros por crops and Scenario
      z[[i]]<- alc[which(alc$Commodity==rtb[i]),]
      z[[i]]<- subset(z[[i]],Scenarios!="SSP2-NoCC" & 
                            Scenarios!="SSP2-NoCC-SWHC2" & 
                            Scenarios!="SSP2-NoCC-IRREXP-WUE2" & 
                            Scenarios!="SSP2-NoCC-Pangloss2" &
                            Scenarios!="SSP2-NoCC-IRREXP2") 
      #reshape 
      require(plyr)
      require(tidyr)
      z[[i]]<- z[[i]] %>% spread(Year, Val)
      rownames(z[[i]])<- 1:nrow(z[[i]])
      cat(paste("running this is crop= ", rtb[i]," it's done\n", sep = ""  ))
      
}

# Apilando los datos de una lista, datos completos 
rtbCrops<- do.call(rbind,z)

# Quitar la palabra SSP2.
rtbCrops$Scenarios<-  gsub("SSP2-", "",rtbCrops$Scenarios)
# Quitar la palabar LAC.
rtbCrops$Regions<-  gsub("^LAC-", "",rtbCrops$Regions)


################################################### Percentage Change   ###########################################################
# Calculo de los cambios porcentuales entre 2050 y 2005
rtbCrops_change<- rtbCrops
rtbCrops_change$Change<- ((rtbCrops_change$`2050`- rtbCrops_change$`2010`)/rtbCrops_change$`2010`)*100
rtbCrops_change<- rtbCrops_change[,-c(5:14)]
g<- unique(rtbCrops$Scenarios)

# Creacion de los escenarios por GCM
hgem<- c("HGEM-HiNARS2", "HGEM-HiREFF2","HGEM-HiYld2", "HGEM-IRREXP-WUE2", "HGEM-IRREXP2", "HGEM-LoYld2", "HGEM-MMEFF2",
         "HGEM-Pangloss2","HGEM-RegYld2","HGEM-SWHC2", "HGEM2" )
ipsl<- c("IPSL-IRREXP-WUE2", "IPSL-IRREXP2", "IPSL-Pangloss2","IPSL-SWHC2", "IPSL2")

#graph1 all-------
parameter<- c(unique(rtbCrops$parameter))
for(p in 1:length(parameter)) {
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename=paste(rdsFiles,"RTB_crops_",parameter[p], "_","HeapMap_ChPer.png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      n<- ggplot(data = rtbCrops_change[which(rtbCrops_change$parameter==parameter[p]),], aes(Scenarios,Regions)) + 
            geom_tile(aes(fill = Change), colour = "white")+  facet_grid(.~Commodity)+
            labs(x=NULL, y=NULL, title=paste("R&T crops, parameter=", parameter[p], " by scenarios\n Percentage Change (%) 2050-2010",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      plot(n)
      dev.off()  
      cat(paste("Running the impactparameter ",parameter[p],  " it's done\n", sep = "" ))
      
}

#graph1 by crops-------
crops<- c(unique(rtbCrops$Commodity))
for(p in 1:length(crops)) {
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename=paste(rdsFiles,"RTB_crops_",crops[p], "_","HeapMap_ChPer.png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      n<- ggplot(data = rtbCrops_change[which(rtbCrops_change$Commodity==crops[p]),], aes(Scenarios,Regions)) + 
            geom_tile(aes(fill = Change), colour = "white")+  facet_grid(.~parameter)+
            labs(x=NULL, y=NULL, title=paste(crops[p],",IMPACT parameters= area and yield total\n Percentage Change (%)  2050-2010"))+
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
            # guides(fill=guide_legend(title="Change\n Percentage (%)"))+
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      plot(n)
      dev.off()  
      cat(paste("Running the impactparameter ",crops[p],  " it's done\n", sep = "" ))
      
}

############################################################## Other analysis GCM HGEM ###############################
# Production tablas resumen
rtb_changesWide<- rtbCrops_change  %>% spread("Scenarios","Change")
names(rtb_changesWide)

# Productivity enhancement
ProdEnha1<- rtb_changesWide[,c("Commodity","Regions","parameter","HGEM-HiNARS2","HGEM-HiREFF2", "HGEM-HiYld2",
                               "HGEM-LoYld2","HGEM-RegYld2","HGEM2")]

#cambiando nombres columnas HGEM
names(ProdEnha1)[4]<- "HIGH+NARS";names(ProdEnha1)[5]<- "HIGH+RE";names(ProdEnha1)[6]<- "HIGH";names(ProdEnha1)[7]<- "MED"
names(ProdEnha1)[8]<- "REG"

cfiles<- ProdEnha1

for(i in 4:ncol(cfiles)){
      cfiles[i]<- cfiles[i]- cfiles[,9]#[,9]
      print(i)
      
}

#exportar copia en excel
write.csv(cfiles,paste(rdsFiles, "Productivity Enhancement", ".csv", sep = ""))
cfiles$HGEM2<- NULL
cfiles<- cfiles %>% gather("DiffSce","PP", 4:ncol(cfiles))

#grafico
for(p in 1:length(parameter)) {
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename=paste(rdsFiles,"ProductivityEnhancement_",parameter[p], "_","HeapMapPP.png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      
      n<- ggplot(data = cfiles[which(cfiles$parameter==parameter[p]),], aes(DiffSce,Regions)) + 
            geom_tile(aes(fill = PP), colour = "white")+  facet_grid(.~Commodity)+
            labs(x=NULL, y=NULL, title=paste("HGEM, Parameter ", parameter[p], "\n Scenario Group: Productivity Enhancement\n Percentage points (pp) 2050-2010\n",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      plot(n)
      dev.off()  
      
}



# Improved infraestructure
Infra1<- rtb_changesWide[,c("Commodity","Regions","parameter","HGEM-MMEFF2","HGEM2")]
names(Infra1)[4]<- "RMM"

cfiles<- Infra1

for(i in 4:ncol(cfiles)){
      cfiles[i]<- cfiles[i]- cfiles[,5]
      print(i)
      
}
#exportar copia en excel

write.csv(cfiles,paste(rdsFiles, "Improved Infraestructure", ".csv", sep = ""))
cfiles$HGEM2<- NULL
cfiles<- cfiles %>% gather("DiffSce","PP", 4:ncol(cfiles))


for(p in 1:length(parameter)) {
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename=paste(rdsFiles,"ImprovedInfraestructure_",parameter[p], "_","HeapMapPP.png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      
      n<- ggplot(data = cfiles[which(cfiles$parameter==parameter[p]),], aes(DiffSce,Regions)) + 
            geom_tile(aes(fill = PP), colour = "white")+  facet_grid(.~Commodity)+
            labs(x=NULL, y=NULL, title=paste("HGEM, Parameter ", parameter[p], "\n Scenario Group: Improved Infraestructure\n Percentage points (pp) 2050-2010\n",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + #coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      plot(n)
      dev.off()  
      
}



# Improved Water Resource Management
Water1<- rtb_changesWide[,c("Commodity","Regions","parameter","HGEM-IRREXP-WUE2","HGEM-IRREXP2","HGEM-SWHC2","HGEM2")]

names(Water1)[4]<- "IX+WUE"
names(Water1)[5]<- "IX"
names(Water1)[6]<- "ISW"

cfiles<- Water1

for(i in 4:ncol(cfiles)){
      cfiles[i]<- cfiles[i]- cfiles[,7]
      print(i)
      
}
#exportar copia en excel

write.csv(cfiles,paste(rdsFiles, "Improved Water Resource Management HGME-pp", ".csv", sep = ""))
cfiles$HGEM2<- NULL
cfiles<- cfiles %>% gather("DiffSce","PP", 4:ncol(cfiles))


for(p in 1:length(parameter)) {
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename=paste(rdsFiles,"HGME_ImprovedWaterResourceManagement_",parameter[p], "_","HeapMapPP.png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      
      n<- ggplot(data = cfiles[which(cfiles$parameter==parameter[p]),], aes(DiffSce,Regions)) + 
            geom_tile(aes(fill = PP), colour = "white")+  facet_grid(.~Commodity)+
            labs(x=NULL, y=NULL, title=paste("HGEM, Parameter ", parameter[p], "\n Scenario Group: Improved Water Resource Management\n Percentage points (pp) 2050-2010\n",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      plot(n)
      dev.off()  
      
}




# Comprehensive Investment
comple1<- rtb_changesWide[,c("Commodity","Regions","parameter","HGEM-Pangloss2", "HGEM2")]
names(comple1)[4]<- "COMP"
cfiles<- comple1

for(i in 4:ncol(cfiles)){
      cfiles[i]<- cfiles[i]- cfiles[,5]
      print(i)
      
}
#exportar copia en excel

write.csv(cfiles,paste(rdsFiles, "Comprehensive Investment", ".csv", sep = ""))
cfiles$HGEM2<- NULL
cfiles<- cfiles %>% gather("DiffSce","PP", 4:ncol(cfiles))


for(p in 1:length(parameter)) {
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename=paste(rdsFiles,"ComprehensiveInvestment_",parameter[p], "_","HeapMapPP.png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      
      n<- ggplot(data = cfiles[which(cfiles$parameter==parameter[p]),], aes(DiffSce,Regions)) + 
            geom_tile(aes(fill = PP), colour = "white")+  facet_grid(.~Commodity)+
            labs(x=NULL, y=NULL, title=paste("HGEM, Parameter ", parameter[p], "\n Scenario Group: Comprehensive Investment\n Percentage points (pp) 2050-2010\n",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      plot(n)
      dev.off()  
      
}



############################################################## Other analysis GCM IPSL ###############################

# Improved Water Resource Management
Water2<- rtb_changesWide[,c(1:3,15:19)]

# Cambiando nombres columnas IPSL
names(Water2)[4]<- "IX+WUE"
names(Water2)[5]<- "IX"
names(Water2)[6]<- "COMP"
names(Water2)[7]<- "ISW"

cfiles<- Water2

for(i in 4:ncol(cfiles)){
      cfiles[i]<- cfiles[i]- cfiles[,8] #cfiles[,9]
      print(i)
      
}

#exportar copia en excel
write.csv(cfiles,paste(rdsFiles, "Improved Water Resource Management IPSL-pp", ".csv", sep = ""))

cfiles$IPSL2<- NULL
cfiles<- cfiles %>% gather("DiffSce","PP", 4:ncol(cfiles))


for(p in 1:length(parameter)) {
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename=paste(rdsFiles,"IPSL_ImprovedWaterResourceManagement_",parameter[p], "_","HeapMapPP.png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      
      n<- ggplot(data = cfiles[which(cfiles$parameter==parameter[p]),], aes(DiffSce,Regions)) + 
            geom_tile(aes(fill = PP), colour = "white")+  facet_grid(.~Commodity)+
            labs(x=NULL, y=NULL, title=paste("IPSL, Parameter ", parameter[p], "\n Scenario Group: Improved Water Resource Management\n Percentage points (pp) 2050-2010\n",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      plot(n)
      dev.off()  
      
}



#exportar copia en excel
write.csv(cfiles,paste(rdsFiles, "Improved Water Resource Management_IPSL-pp", ".csv", sep = ""))



######## Analisis de Improved water resource management
z<- c("HGME", "IPSL")
g<- c("IX+WUE","IX","COMP","ISW")

cfiles<- list.files(rdsFiles, pattern = "Improved Water Resource Management ", full.names = T)
cfiles<- lapply(cfiles, read.csv)
cfiles<- lapply(cfiles,function(x) { x["X"] <- NULL; x })
cfiles<- lapply(cfiles,function(x){ x["COMP"]<-NULL; x})

for(i in 1:length(cfiles)){
      cfiles[[i]]$GCM<- names(cfiles[[i]])[7]
      cfiles[[i]]<- cfiles[[i]][,c("Commodity", "Regions", "parameter", "GCM", "IX.WUE", "IX", "ISW" )]
      cfiles[[i]]<- cfiles[[i]] %>% 
            gather("Sce","Val", 5:7)
      
}


waterSce<- do.call(rbind,cfiles)
waterSceGather<- waterSce %>% spread("GCM", "Val")

for(p in 1:length(parameter)) {
      png(filename=paste(rdsFiles,"GCMs_ImprovedWaterResourceManagement_",parameter[p], "_","ScatterPlot.png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      
      n<- ggplot(data = waterSceGather[which(waterSceGather$parameter==parameter[p]),], aes(x=HGEM2,y=IPSL2)) +
            geom_point(aes(colour = Commodity),shape=15,size = 2) + facet_grid(Sce~.,labeller =label_parsed,scales = "free")+
            ylab("IPSL2, percentage points pp") +
            xlab("HGEM2, percentage points pp") +
            theme(axis.text.x= element_text(size=12))+
            theme(axis.text.y= element_text(size=12))+
            guides(color=guide_legend("RTB crops")) +
            theme(strip.text.y = element_text(angle = 0,size = 12))+
            geom_hline(aes(yintercept=0))+ 
            geom_vline(aes(xintercept=0))+ 
            labs(title=paste("Parameter ", parameter[p], "\n Scenario Group: Improved Water Resource Management\n Percentage point (pp) 2050-2010\n",sep = ""))
      
      
      plot(n)
      dev.off()  
      
}


#exportar copia en excel
write.csv(waterSceGather,paste(rdsFiles, "Comparison of GCMs, Improved Water Resource Management", ".csv", sep = ""))





