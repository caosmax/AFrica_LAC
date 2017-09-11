#Visualize DSSAT runs

#Load libraries
library(dplyr)
library(tidyr)
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(maptools)
library(gridExtra)
library(sp)
library(maptools)
library(maps)
library(raster)

# library(ncdf)

#Directorios
path.root<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
#cargamos shape ALC
map <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/"
alc <- shapefile(paste0(map,"Latino_America1.shp"))


grd1<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/Test")
#copyall<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/DataAll/Raster/")

copyall<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/HotSpots/")
shp<- c ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/")
treat<- c("Irrigated","Rain-fed")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m", "WFD")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 

# Raster de plantilla
tmpRaster <- raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Rendimientos Raster/Arroz/1971-1998/_WFD_Riego_1971.asc")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

# Raster con Rendimientos Maximos Historicos WFD--------
dataF<-list.files(path = grd1,pattern=paste("YieldMax_", sep = ""),full.names = T)

dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
dataF<- do.call(rbind,dataF); dataF$X<- NULL

dataF$sys<- plyr::revalue(dataF$sys, c("IRRI"="Irrigated","RA"="Rain-fed"))
dataF<- filter(dataF, sce=="WFD")

dataF<- dataF %>% spread("year", "ymax")
dataF$mean<- apply(dataF[,9:ncol(dataF)],1,function(x){mean(x,na.rm = T)})
dataF<-  dataF[,-c(9:36)]

dataF<- aggregate(dataF$mean, by=list( dataF$long, dataF$lat, dataF$FPU, dataF$sys, dataF$crop),FUN=mean)
colnames( dataF)<- c( "long","lat","FPU","sys","crop","mean" )

Map_LatinAmerica1<- fortify(alc)
color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
labs2 = 'Yield\n(Kg/ha)'

png(filename = paste(copyall,"Test_Figure_MaxYieldWFD.png", sep=""), 
    width = 20, height = 12, units = 'in', res = 100)
y<- ggplot() +
      geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
      geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
      geom_raster(data= dataF, aes(x=long, y=lat,fill=mean))+ facet_grid(sys~crop)+
      theme(strip.text.x = element_text(angle = 0,size = 18),strip.background = element_rect(colour="white", fill="white")) + 
      theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
      scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
      labs(fill=labs2)+ 
      theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.text = element_text(size=14),
            legend.title = element_text(face="bold",size=14),
            legend.background = element_blank(),
            legend.key = element_blank(),
            strip.text.y = element_text(size=16, face="bold"),
            plot.title = element_text(face="bold", size=22),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            strip.background = element_rect(colour="white", fill="white")) 


plot(y)
dev.off()


    




 

# Raster HotsPots--------------------------------------------------- 
##crops
Map_LatinAmerica1<- fortify(alc)

p<- list()
c=1
for (c in 1:length(crops)){
      dataF<- list.files(path = grd1,pattern= paste("YieldMax_",crops[c],sep = ""),full.names = T)
      dataF<- lapply(dataF,read.csv,stringsAsFactors = F)
      dataF<- do.call(rbind,dataF); dataF$X<- NULL
      # dataF<- filter(dataF, sce!="WFD")
      cfiles<- dataF 
      cfiles<- cfiles %>% spread("year", "ymax")
      
      cfiles[,"ymean"] <- apply(cfiles[, 9:ncol(cfiles)], 1, mean)
      # cfiles$crop<- crops[c]
      cfiles<- cfiles[,c("long","lat", "pix","Area","FPU","sys","sce","crop","ymean")]
      cfiles<- cfiles %>% spread(sce, ymean)
      cfiles[,"meanGCM"] <- apply(cfiles[, 8:16], 1, mean)
      cfiles<- cfiles[,c("long","lat", "pix","Area","FPU","sys","crop","WFD","meanGCM")]
      
      cfiles$change<- ((cfiles$meanGCM-cfiles$WFD)/cfiles$WFD)*100

      if(length(which(cfiles$change == Inf)) > 0){
            cfiles$change[which(cfiles$change == Inf)] <- 0}else{}
      cfiles$change[is.na(cfiles$change)==TRUE]<- 0
      ### Eliminar ceros
      cfiles<-cfiles[!(cfiles$change==0),]

      # define a function to remove outliers
      FindOutliers <- function(data) {
            lowerq = quantile(data)[2]
            upperq = quantile(data)[4]
            iqr = upperq - lowerq 
            #Or use IQR(data)
            # we identify extreme outliers
            extreme.threshold.upper = (iqr * 3) + upperq
            extreme.threshold.lower = lowerq - (iqr * 3)
            result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
      }
      
#           percentil 95%_
#             q95<- quantile(cfiles$change, 0.95)
#             cfiles95 <- cfiles[-which(cfiles$change > q95),]
#             rownames(cfiles95) <- 1:nrow(cfiles95)


      # use the function to identify outliers
      temp <- FindOutliers(cfiles$change)
      cfOut<- cfiles[temp,]
      cfilesNEt<- cfiles[-temp,]
      cfilesNEt$sys<- plyr::revalue(cfilesNEt$sys, c("IRRI"="Irrigated","RA"="Rain-fed"))
      
      saveRDS(object = cfilesNEt, file = paste(copyall,crops[c],"_Data.RDS", sep=""))
      color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
      labs2 = 'Percentage Change\n (%)'
      
      
      #Coropleta 
     png(filename = paste(copyall,"Test_",crops[c],"_MeanYield__meanGCM.png", sep=""), 
         width = 20, height = 12, units = 'in', res = 100)
     p[[c]]<- ggplot() +
           geom_polygon(data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
           geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
           geom_raster(data= cfilesNEt, aes(x=long, y=lat,fill=change))+ facet_wrap(~sys)+
           theme(strip.text.x = element_text(angle = 0,size = 16),strip.background = element_rect(colour="white", fill="white")) + 
           theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
           scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
           labs(fill=labs2)+ 
           labs(title=crops[c])+
           theme(strip.text.x = element_text(angle = 0,size = 14, face = "bold.italic"))+
           theme(strip.text.y = element_text(angle = 0,size = 14, face = "bold.italic"))+
           theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 legend.text = element_text(size=11),
                 legend.title = element_text(face="bold",size=12),
                 legend.background = element_blank(),
                 legend.key = element_blank(),
                 strip.text.y = element_text(size=12, face="bold"),
                 plot.title = element_text(face="bold", size=20),
                 panel.border = element_blank(),
                 axis.ticks = element_blank(),
                 strip.background = element_rect(colour="white", fill="white")) 
     
                 plot(p[[c]])
                 dev.off()
                 trata<- unique(cfilesNEt$sys)
                 
                 # Exportar como archivo tipo raster
                 for(t in 1:length(trata)){
                       rasterNEt<- filter(cfilesNEt, sys==trata[t])
                       rasterNEt<- rasterNEt[,c("long","lat","change")]
                       cellID <- cellFromXY(object = tmpRaster, xy = rasterNEt[,c("long", "lat")])
                       tmpRaster[] <- NA
                       tmpRaster[cellID] <- rasterNEt[,3]
                       plot(tmpRaster)
                       writeRaster(tmpRaster, filename=paste(copyall,"Test_",crops[c],"_",trata[t],"_percentage_changes.tif",sep=""), format="GTiff", overwrite=TRUE)
                       saveRDS(object = tmpRaster, file = paste(copyall,crops[c],trata[t],"_DataRaster.RDS", sep=""))
                       cat(paste(crops[c]," ", trata[t]," Terminado\n it is done!!", sep = ""))
                 }
                 
}

 

g=gc;rm(list = ls())




