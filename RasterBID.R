#Visualize DSSAT runs

#Load libraries
library(dplyr)
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(maptools)
library(sp)
library(maptools)
library(maps)
# library(ncdf)



#Directorios
path.root<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

#cargamos shape ALC
map <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/"
alc <- shapefile(paste0(map,"Latino_America1.shp"))
plot(alc)

##########################################################WHEAT
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Wheat/future/final/WHEAT_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Wheat/future/final/WHEAT_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

copyall<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/DataAll/Raster/")
shp<- c ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/")
sis<- c("riego", "secano")
sis.en<- c("irrigated", "rainfed")
sis.ceg<- c("IRR","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

# raste de plantilla
tmpRaster <- raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Rendimientos Raster/Arroz/1971-1998/_WFD_Riego_1971.asc")



##loop para cargar datos RIEGO-----------------
variedades<- c("BrigadierBA")

for(v in 1:length(variedades)){
    
     for(g in 1:length(gcm)){
        
            load(paste(grd3,"Wheat","_",sis[s],".RDAT", sep = ""))

            #Extraemos las coordenadas de las matrices de cultivo
             Riego <- crop_riego[,c("x","y")]
            # cargamos los rendimientos por variedades y GCMs
             load(paste(gdr1,variedades[v],"_",gcm[g],".RDat",sep = ""))  


            #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
            rend<-matrix(nrow=length(Run), ncol=28)
            
            #cargamos los rendimientos por una variedad, para el total de pixeles
            for (i in 1:length(Run))
            {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  
            
            #descartar pixeles con demasiadas fallas
            rend[rend==-99] = 0  #convertir -99 a 0
            
            #find areas where consistently failing and discard these from aggregation
            zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
            ind.falla = which(zeros.wfd.r>=14)
            
            ##Convertir rend en un data.frame
            rend<-data.frame(rend)
            
            #Asignar nombres a el data frame de rendimientos
            colnames(rend)<-paste0("20",22:49)
            
            #calcular promedio por pixel
            rend$mean<-apply(rend[,1:ncol(rend)],1,function(x){mean(x,na.rm = T)})
            rend<- as.data.frame(rend[,"mean"])
            names(rend)[1]<- "mean"
            
            #Agregar las coordenadas
            row.names(Riego)<- 1:nrow(Riego)
            rend <- cbind(Riego[,c("x", "y")], rend)
            names(rend)[1:2] <- c("Long", "Lat")
            
            # Analisis grafico
            Map_LatinAmerica1<- fortify(alc)
            
            color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
            labs2 = 'Yield (ave) \n(Kg/ha)'
            y=ggplot() +
                geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
                geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
                coord_equal() +
                geom_raster(data=rend, aes(x=Long, y=Lat,fill=mean))+
                ggtitle(gcm[g],'Wheat \n Irrigated')+
                            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
                            theme_bw()+
                            labs(fill=labs2)+
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
                                  plot.title = element_text(face="bold", size=18),
                                  panel.border = element_blank(),
                                  axis.ticks = element_blank())
                        
                        ggsave(filename=paste(copyall,"Wheat_Riego_",gcm[g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
                        
                        # exportar como archivo tipo raster
                        library(raster)
                        cellID <- cellFromXY(object = tmpRaster, xy = rend[,c("Long", "Lat")])
                        tmpRaster[] <- NA
                        tmpRaster[cellID] <- rend[,3]
                        length(na.omit(tmpRaster[]))
                        plot(tmpRaster)
                        plot(alc, add = T)
                        writeRaster(tmpRaster, filename=paste(copyall,"Wheat","_",gcm[g],"_Irrigated",".tif",sep=""), format="GTiff", overwrite=TRUE)
                        
                        
     }
    }



           
s=2

##loop para cargar datos SECANO-----------------
variedades<- c("BrigadierBA")
s=2;g=1;v=1
for(v in 1:length(variedades)){
    
    for(g in 1:length(gcm)){
        
        load(paste(grd3,"Wheat","_",sis[s],".RDAT", sep = ""))
        
        #Extraemos las coordenadas de las matrices de cultivo
        Secano <- crop_secano[,c("x","y")]
        # cargamos los rendimientos por variedades y GCMs
        load(paste(gdr2,variedades[v],"_",gcm[g],".RDat",sep = ""))  
        
        
        #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
        rend<-matrix(nrow=length(Run), ncol=28)
        
        #cargamos los rendimientos por una variedad, para el total de pixeles
        for (i in 1:length(Run))
        {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  
        
        #descartar pixeles con demasiadas fallas
        rend[rend==-99] = 0  #convertir -99 a 0
        
        #find areas where consistently failing and discard these from aggregation
        zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
        ind.falla = which(zeros.wfd.r>=14)
        
        ##Convertir rend en un data.frame
        rend<-data.frame(rend)
        
        #Asignar nombres a el data frame de rendimientos
        colnames(rend)<-paste0("20",22:49)
        
        #calcular promedio por pixel
        rend$mean<-apply(rend[,1:ncol(rend)],1,function(x){mean(x,na.rm = T)})
        rend<- as.data.frame(rend[,"mean"])
        names(rend)[1]<- "mean"
        
        #Agregar las coordenadas
        row.names(Secano)<- 1:nrow(Secano)
        rend <- cbind(Secano[,c("x", "y")], rend)
        names(rend)[1:2] <- c("Long", "Lat")
        
        # Analisis grafico
        Map_LatinAmerica1<- fortify(alc)
        
        color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
        labs2 = 'Yield (ave) \n(Kg/ha)'
        y=ggplot() +
            geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
            geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
            coord_equal() +
            geom_raster(data=rend, aes(x=Long, y=Lat,fill=mean))+
            ggtitle('Wheat \n Rainfed')+
            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
            theme_bw()+
            labs(fill=labs2)+
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
                  plot.title = element_text(face="bold", size=18),
                  panel.border = element_blank(),
                  axis.ticks = element_blank())
        
        ggsave(filename=paste(copyall,"Wheat_Secano_",gcm[g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
        
        # exportar como archivo tipo raster
        library(raster)
        cellID <- cellFromXY(object = tmpRaster, xy = rend[,c("Long", "Lat")])
        tmpRaster[] <- NA
        tmpRaster[cellID] <- rend[,3]
        length(na.omit(tmpRaster[]))
        plot(tmpRaster)
        plot(alc, add = T)
        writeRaster(tmpRaster, filename=paste(copyall,"Wheat","_",gcm[g],"_Rainfed",".tif",sep=""), format="GTiff", overwrite=TRUE)
        
        
    }
}


##########################################################BEAN-----------
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Bean/future/final/BEAN_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Bean/future/final/BEAN_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

copyall<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/DataAll/Raster/")
shp<- c ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/")
sis<- c("riego", "secano")
sis.en<- c("irrigated", "rainfed")
sis.ceg<- c("IRR","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

# raste de plantilla
tmpRaster <- raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Rendimientos Raster/Arroz/1971-1998/_WFD_Riego_1971.asc")


s=1

##loop para cargar datos BEAN-----------------
variedades<- c("Perola")

for(v in 1:length(variedades)){
    
    for(g in 1:length(gcm)){
        
        load(paste(grd3,"Bean","_",sis[s],".RDAT", sep = ""))
        
        #Extraemos las coordenadas de las matrices de cultivo
        Riego <- crop_riego[,c("x","y")]
        # cargamos los rendimientos por variedades y GCMs
        load(paste(gdr1,variedades[v],"_",gcm[g],".RDat",sep = ""))  
        
        
        #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
        rend<-matrix(nrow=length(Run), ncol=28)
        
        #cargamos los rendimientos por una variedad, para el total de pixeles
        for (i in 1:length(Run))
        {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  
        
        #descartar pixeles con demasiadas fallas
        rend[rend==-99] = 0  #convertir -99 a 0
        
        #find areas where consistently failing and discard these from aggregation
        zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
        ind.falla = which(zeros.wfd.r>=14)
        
        ##Convertir rend en un data.frame
        rend<-data.frame(rend)
        
        #Asignar nombres a el data frame de rendimientos
        colnames(rend)<-paste0("20",22:49)
        
        #calcular promedio por pixel
        rend$mean<-apply(rend[,1:ncol(rend)],1,function(x){mean(x,na.rm = T)})
        rend<- as.data.frame(rend[,"mean"])
        names(rend)[1]<- "mean"
        
        #Agregar las coordenadas
        row.names(Riego)<- 1:nrow(Riego)
        rend <- cbind(Riego[,c("x", "y")], rend)
        names(rend)[1:2] <- c("Long", "Lat")
        
        # Analisis grafico
        Map_LatinAmerica1<- fortify(alc)
        
        color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
        labs2 = 'Yield (ave) \n(Kg/ha)'
        y=ggplot() +
            geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
            geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
            coord_equal() +
            geom_raster(data=rend, aes(x=Long, y=Lat,fill=mean))+
            ggtitle(gcm[g],'Wheat \n Irrigated')+
            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
            theme_bw()+
            labs(fill=labs2)+
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
                  plot.title = element_text(face="bold", size=18),
                  panel.border = element_blank(),
                  axis.ticks = element_blank())
        
        ggsave(filename=paste(copyall,"Bean_Riego_",gcm[g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
        
        # exportar como archivo tipo raster
        library(raster)
        cellID <- cellFromXY(object = tmpRaster, xy = rend[,c("Long", "Lat")])
        tmpRaster[] <- NA
        tmpRaster[cellID] <- rend[,3]
        length(na.omit(tmpRaster[]))
        plot(tmpRaster)
        plot(alc, add = T)
        writeRaster(tmpRaster, filename=paste(copyall,"Bean","_",gcm[g],"_Irrigated",".tif",sep=""), format="GTiff", overwrite=TRUE)
        
        
    }
}




s=2

##loop para cargar datos SECANO-----------------
variedades<- c("Perola")
s=2;g=1;v=1
for(v in 1:length(variedades)){
    
    for(g in 1:length(gcm)){
        
        load(paste(grd3,"Bean","_",sis[s],".RDAT", sep = ""))
        
        #Extraemos las coordenadas de las matrices de cultivo
        Secano <- crop_secano[,c("x","y")]
        # cargamos los rendimientos por variedades y GCMs
        load(paste(gdr2,variedades[v],"_",gcm[g],".RDat",sep = ""))  
        
        
        #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
        rend<-matrix(nrow=length(Run), ncol=28)
        
        #cargamos los rendimientos por una variedad, para el total de pixeles
        for (i in 1:length(Run))
        {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  
        
        #descartar pixeles con demasiadas fallas
        rend[rend==-99] = 0  #convertir -99 a 0
        
        #find areas where consistently failing and discard these from aggregation
        zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
        ind.falla = which(zeros.wfd.r>=14)
        
        ##Convertir rend en un data.frame
        rend<-data.frame(rend)
        
        #Asignar nombres a el data frame de rendimientos
        colnames(rend)<-paste0("20",22:49)
        
        #calcular promedio por pixel
        rend$mean<-apply(rend[,1:ncol(rend)],1,function(x){mean(x,na.rm = T)})
        rend<- as.data.frame(rend[,"mean"])
        names(rend)[1]<- "mean"
        
        #Agregar las coordenadas
        row.names(Secano)<- 1:nrow(Secano)
        rend <- cbind(Secano[,c("x", "y")], rend)
        names(rend)[1:2] <- c("Long", "Lat")
        
        # Analisis grafico
        Map_LatinAmerica1<- fortify(alc)
        
        color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
        labs2 = 'Yield (ave) \n(Kg/ha)'
        y=ggplot() +
            geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
            geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
            coord_equal() +
            geom_raster(data=rend, aes(x=Long, y=Lat,fill=mean))+
            ggtitle('Wheat \n Rainfed')+
            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
            theme_bw()+
            labs(fill=labs2)+
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
                  plot.title = element_text(face="bold", size=18),
                  panel.border = element_blank(),
                  axis.ticks = element_blank())
        
        ggsave(filename=paste(copyall,"Bean_Secano_",gcm[g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
        
        # exportar como archivo tipo raster
        library(raster)
        cellID <- cellFromXY(object = tmpRaster, xy = rend[,c("Long", "Lat")])
        tmpRaster[] <- NA
        tmpRaster[cellID] <- rend[,3]
        length(na.omit(tmpRaster[]))
        plot(tmpRaster)
        plot(alc, add = T)
        writeRaster(tmpRaster, filename=paste(copyall,"Bean","_",gcm[g],"_Rainfed",".tif",sep=""), format="GTiff", overwrite=TRUE)
        
        
    }
}

##########################################################Rice-----------
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Rice/future/final/RICE_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Rice/future/final/RICE_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

copyall<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/DataAll/Raster/")
shp<- c ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/")
sis<- c("riego", "secano")
sis.en<- c("irrigated", "rainfed")
sis.ceg<- c("IRR","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

# raste de plantilla
tmpRaster <- raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Rendimientos Raster/Arroz/1971-1998/_WFD_Riego_1971.asc")


s=1

##loop para cargar datos  IRRIGADO-----------------
variedades<- c("IR8")

for(v in 1:length(variedades)){
    
    for(g in 1:length(gcm)){
        
        load(paste(grd3,"Rice","_",sis[s],".RDAT", sep = ""))
        
        #Extraemos las coordenadas de las matrices de cultivo
        Riego <- crop_riego[,c("x","y")]
        # cargamos los rendimientos por variedades y GCMs
        load(paste(gdr1,variedades[v],"_",gcm[g],".RDat",sep = ""))  
        
        
        #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
        rend<-matrix(nrow=length(Run), ncol=28)
        
        #cargamos los rendimientos por una variedad, para el total de pixeles
        for (i in 1:length(Run))
        {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  
        
        #descartar pixeles con demasiadas fallas
        rend[rend==-99] = 0  #convertir -99 a 0
        
        #find areas where consistently failing and discard these from aggregation
        zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
        ind.falla = which(zeros.wfd.r>=14)
        
        ##Convertir rend en un data.frame
        rend<-data.frame(rend)
        
        #Asignar nombres a el data frame de rendimientos
        colnames(rend)<-paste0("20",22:49)
        
        #calcular promedio por pixel
        rend$mean<-apply(rend[,1:ncol(rend)],1,function(x){mean(x,na.rm = T)})
        rend<- as.data.frame(rend[,"mean"])
        names(rend)[1]<- "mean"
        
        #Agregar las coordenadas
        row.names(Riego)<- 1:nrow(Riego)
        rend <- cbind(Riego[,c("x", "y")], rend)
        names(rend)[1:2] <- c("Long", "Lat")
        
        # Analisis grafico
        Map_LatinAmerica1<- fortify(alc)
        
        color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
        labs2 = 'Yield (ave) \n(Kg/ha)'
        y=ggplot() +
            geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
            geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
            coord_equal() +
            geom_raster(data=rend, aes(x=Long, y=Lat,fill=mean))+
            ggtitle(gcm[g],'Wheat \n Irrigated')+
            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
            theme_bw()+
            labs(fill=labs2)+
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
                  plot.title = element_text(face="bold", size=18),
                  panel.border = element_blank(),
                  axis.ticks = element_blank())
        
        ggsave(filename=paste(copyall,"Bean_Riego_",gcm[g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
        
        # exportar como archivo tipo raster
        library(raster)
        cellID <- cellFromXY(object = tmpRaster, xy = rend[,c("Long", "Lat")])
        tmpRaster[] <- NA
        tmpRaster[cellID] <- rend[,3]
        length(na.omit(tmpRaster[]))
        plot(tmpRaster)
        plot(alc, add = T)
        writeRaster(tmpRaster, filename=paste(copyall,"Rice","_",gcm[g],"_Irrigated",".tif",sep=""), format="GTiff", overwrite=TRUE)
        
        
    }
}




s=2

##loop para cargar datos SECANO-----------------
variedades<- c("IR8")
s=2;g=1;v=1
for(v in 1:length(variedades)){
    
    for(g in 1:length(gcm)){
        
        load(paste(grd3,"Rice","_",sis[s],".RDAT", sep = ""))
        
        #Extraemos las coordenadas de las matrices de cultivo
        Secano <- crop_secano[,c("x","y")]
        # cargamos los rendimientos por variedades y GCMs
        load(paste(gdr2,variedades[v],"_",gcm[g],".RDat",sep = ""))  
        
        
        #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
        rend<-matrix(nrow=length(Run), ncol=28)
        
        #cargamos los rendimientos por una variedad, para el total de pixeles
        for (i in 1:length(Run))
        {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  
        
        #descartar pixeles con demasiadas fallas
        rend[rend==-99] = 0  #convertir -99 a 0
        
        #find areas where consistently failing and discard these from aggregation
        zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
        ind.falla = which(zeros.wfd.r>=14)
        
        ##Convertir rend en un data.frame
        rend<-data.frame(rend)
        
        #Asignar nombres a el data frame de rendimientos
        colnames(rend)<-paste0("20",22:49)
        
        #calcular promedio por pixel
        rend$mean<-apply(rend[,1:ncol(rend)],1,function(x){mean(x,na.rm = T)})
        rend<- as.data.frame(rend[,"mean"])
        names(rend)[1]<- "mean"
        
        #Agregar las coordenadas
        row.names(Secano)<- 1:nrow(Secano)
        rend <- cbind(Secano[,c("x", "y")], rend)
        names(rend)[1:2] <- c("Long", "Lat")
        
        # Analisis grafico
        Map_LatinAmerica1<- fortify(alc)
        
        color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
        labs2 = 'Yield (ave) \n(Kg/ha)'
        y=ggplot() +
            geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
            geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
            coord_equal() +
            geom_raster(data=rend, aes(x=Long, y=Lat,fill=mean))+
            ggtitle('Rice \n Rainfed')+
            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
            theme_bw()+
            labs(fill=labs2)+
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
                  plot.title = element_text(face="bold", size=18),
                  panel.border = element_blank(),
                  axis.ticks = element_blank())
        
        ggsave(filename=paste(copyall,"Rice_Secano_",gcm[g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
        
        # exportar como archivo tipo raster
        library(raster)
        cellID <- cellFromXY(object = tmpRaster, xy = rend[,c("Long", "Lat")])
        tmpRaster[] <- NA
        tmpRaster[cellID] <- rend[,3]
        length(na.omit(tmpRaster[]))
        plot(tmpRaster)
        plot(alc, add = T)
        writeRaster(tmpRaster, filename=paste(copyall,"Rice","_",gcm[g],"_Rainfed",".tif",sep=""), format="GTiff", overwrite=TRUE)
        
        
    }
}

##########################################################MAIZE-----------
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Maize/future/final/MAIZE_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Maize/future/final/MAIZE_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

copyall<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/DataAll/Raster/")
shp<- c ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/")
sis<- c("riego", "secano")
sis.en<- c("irrigated", "rainfed")
sis.ceg<- c("IRR","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

# raste de plantilla
tmpRaster <- raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Rendimientos Raster/Arroz/1971-1998/_WFD_Riego_1971.asc")


s=1

##loop para cargar datos IRRIGADO-----------------
variedades<- c("H6")

for(v in 1:length(variedades)){
    
    for(g in 1:length(gcm)){
        
        load(paste(grd3,"Maize","_",sis[s],".RDAT", sep = ""))
        
        #Extraemos las coordenadas de las matrices de cultivo
        Riego <- crop_riego[,c("x","y")]
        # cargamos los rendimientos por variedades y GCMs
        load(paste(gdr1,variedades[v],"_",gcm[g],".RDat",sep = ""))  
        
        
        #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
        rend<-matrix(nrow=length(Run), ncol=28)
        
        #cargamos los rendimientos por una variedad, para el total de pixeles
        for (i in 1:length(Run))
        {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  
        
        #descartar pixeles con demasiadas fallas
        rend[rend==-99] = 0  #convertir -99 a 0
        
        #find areas where consistently failing and discard these from aggregation
        zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
        ind.falla = which(zeros.wfd.r>=14)
        
        ##Convertir rend en un data.frame
        rend<-data.frame(rend)
        
        #Asignar nombres a el data frame de rendimientos
        colnames(rend)<-paste0("20",22:49)
        
        #calcular promedio por pixel
        rend$mean<-apply(rend[,1:ncol(rend)],1,function(x){mean(x,na.rm = T)})
        rend<- as.data.frame(rend[,"mean"])
        names(rend)[1]<- "mean"
        
        #Agregar las coordenadas
        row.names(Riego)<- 1:nrow(Riego)
        rend <- cbind(Riego[,c("x", "y")], rend)
        names(rend)[1:2] <- c("Long", "Lat")
        
        # Analisis grafico
        Map_LatinAmerica1<- fortify(alc)
        
        color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
        labs2 = 'Yield (ave) \n(Kg/ha)'
        y=ggplot() +
            geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
            geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
            coord_equal() +
            geom_raster(data=rend, aes(x=Long, y=Lat,fill=mean))+
            ggtitle(gcm[g],'Maize \n Irrigated')+
            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
            theme_bw()+
            labs(fill=labs2)+
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
                  plot.title = element_text(face="bold", size=18),
                  panel.border = element_blank(),
                  axis.ticks = element_blank())
        
        ggsave(filename=paste(copyall,"Maize_Riego_",gcm[g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
        
        # exportar como archivo tipo raster
        library(raster)
        cellID <- cellFromXY(object = tmpRaster, xy = rend[,c("Long", "Lat")])
        tmpRaster[] <- NA
        tmpRaster[cellID] <- rend[,3]
        length(na.omit(tmpRaster[]))
        plot(tmpRaster)
        plot(alc, add = T)
        writeRaster(tmpRaster, filename=paste(copyall,"Maize","_",gcm[g],"_Irrigated",".tif",sep=""), format="GTiff", overwrite=TRUE)
        
        
    }
}




s=2

##loop para cargar datos SECANO-----------------
variedades<- c("H6")
s=2;g=1;v=1
for(v in 1:length(variedades)){
    
    for(g in 1:length(gcm)){
        
        load(paste(grd3,"Maize","_",sis[s],".RDAT", sep = ""))
        
        #Extraemos las coordenadas de las matrices de cultivo
        Secano <- crop_secano[,c("x","y")]
        # cargamos los rendimientos por variedades y GCMs
        load(paste(gdr2,variedades[v],"_",gcm[g],".RDat",sep = ""))  
        
        
        #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
        rend<-matrix(nrow=length(Run), ncol=28)
        
        #cargamos los rendimientos por una variedad, para el total de pixeles
        for (i in 1:length(Run))
        {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  
        
        #descartar pixeles con demasiadas fallas
        rend[rend==-99] = 0  #convertir -99 a 0
        
        #find areas where consistently failing and discard these from aggregation
        zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
        ind.falla = which(zeros.wfd.r>=14)
        
        ##Convertir rend en un data.frame
        rend<-data.frame(rend)
        
        #Asignar nombres a el data frame de rendimientos
        colnames(rend)<-paste0("20",22:49)
        
        #calcular promedio por pixel
        rend$mean<-apply(rend[,1:ncol(rend)],1,function(x){mean(x,na.rm = T)})
        rend<- as.data.frame(rend[,"mean"])
        names(rend)[1]<- "mean"
        
        #Agregar las coordenadas
        row.names(Secano)<- 1:nrow(Secano)
        rend <- cbind(Secano[,c("x", "y")], rend)
        names(rend)[1:2] <- c("Long", "Lat")
        
        # Analisis grafico
        Map_LatinAmerica1<- fortify(alc)
        
        color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
        labs2 = 'Yield (ave) \n(Kg/ha)'
        y=ggplot() +
            geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
            geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
            coord_equal() +
            geom_raster(data=rend, aes(x=Long, y=Lat,fill=mean))+
            ggtitle('Maize \n Rainfed')+
            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
            theme_bw()+
            labs(fill=labs2)+
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
                  plot.title = element_text(face="bold", size=18),
                  panel.border = element_blank(),
                  axis.ticks = element_blank())
        
        ggsave(filename=paste(copyall,"Maize_Secano_",gcm[g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
        
        # exportar como archivo tipo raster
        library(raster)
        cellID <- cellFromXY(object = tmpRaster, xy = rend[,c("Long", "Lat")])
        tmpRaster[] <- NA
        tmpRaster[cellID] <- rend[,3]
        length(na.omit(tmpRaster[]))
        plot(tmpRaster)
        plot(alc, add = T)
        writeRaster(tmpRaster, filename=paste(copyall,"Maize","_",gcm[g],"_Rainfed",".tif",sep=""), format="GTiff", overwrite=TRUE)
        
        
    }
}

