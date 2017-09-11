## Kenya raster
g=gc;rm(list = ls())

#Load libraries---
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
library(maps)
library(raster)
library(rgdal)


#directories---
dir<-"C:/Users/CEGONZALEZ/Documents/GFSF"
#cargamos shape ALC
# Kenya <- shapefile(paste0(dir,"/","Kenya.shp"))
fpu<-  shapefile("C:/Users/CEGONZALEZ/Desktop/BID/BID/newBID/ImpactResults/Shapefiles mundo/fpu_shp/fpu_shp/fpu.shp")
atest <- raster(paste0(dir,"/","spam2005v2r0_yield_bean_total.tiff"))

# filter FPU kenya
fpu <- fpu[fpu@data$Region_Nam=="Kenya",]


# crop using Extent
crs(atest)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(fpu)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
#Recorte primero por extent y luego mascara
cr <- crop(atest, extent(fpu))
rr <- raster::mask(cr,fpu)

# ploting easier
plot(rr)
plot(fpu,add=T)


# To convert your RasterLayer to a data.frame, you need to convert it to
# a SpatialPixelsDataFrame first
r.spdf <- as(rr, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
head(r.df)
colnames(r.df)[1]<- "Val"
r.df<- filter(r.df, Val!=0)
# g <- ggplot(r.df, aes(x=x, y=y)) + geom_tile(aes(fill = Val)) + coord_equal()

#fortify fpu
fpu<- fortify(fpu)

color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
# labs2 = 'Harvest Area\n(ha)'
png(filename = paste(dir,"KenyaArea.png", sep=""), 
    width = 20, height = 12, units = 'in', res = 100)

y <- ggplot() +
      geom_polygon(data=fpu, aes(x=long, y=lat, group = group),colour="white", fill="white")+
      geom_path(data=fpu, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
      geom_raster(data= r.df, aes(x=x, y=y, fill=Val))+
      theme(strip.text.x = element_text(angle = 0,size = 18),strip.background = element_rect(colour="white", fill="white")) + 
      theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
      scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
#       labs(fill=labs2)+ 
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


############### Data  for extract information for built allocation matrix cultivation 

g<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/CASE_Jamleck"
sp<- c("rainfed", "irrigated")

i=1
# for(i in 1:length(sp)){
    r <- raster(paste(g,"/","spam2005v2r0_harvested-area_bean_","rainfed.tiff",sep = "")) # load
    crs(r)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"            # reproyect
    fpu<-  shapefile("C:/Users/CEGONZALEZ/Desktop/BID/BID/newBID/ImpactResults/Shapefiles mundo/fpu_shp/fpu_shp/fpu.shp")
    fpu <- fpu[fpu@data$Region_Nam=="Kenya",]
    
      cr <- crop(r, extent(fpu))
      rr <- raster::mask(cr,fpu) 
      r.spdf <- as(rr, "SpatialPixelsDataFrame")
      r.df <- as.data.frame(r.spdf)
      head(r.df)
      colnames(r.df)[1]<- "Area"
      r.df<- filter(r.df, Area!=0)
      r.df$New_FPU<- "HOA_KEN"
      r.df$Coincidencias<- row.names(r.df)
      r.df<- r.df[c("x","y","Area","New_FPU","Coincidencias" )]

     # Distribution areas 
     hist(r.df$Area)
     summary(r.df$Area)

    #Fortify fpu
    fpu<- fortify(fpu)
    labs2 = 'Harvest Area\n(ha)'
    color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
    
    png(filename = paste(g,"/","KenyaArea_rainfed.png", sep=""), 
        width = 20, height = 12, units = 'in', res = 100)
    
    #Creating graph
    y<-ggplot() +
          geom_polygon(data=fpu, aes(x=long, y=lat, group = group),colour="white", fill="white")+
          geom_path(data=fpu, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
          geom_raster(data= r.df, aes(x=x, y=y, fill=Area))+
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
    # export to CSV
    write.csv(r.df,paste(g,"/","KenyaAreaCoordinates_rainfed.csv", sep=""))
    # export as raster
    r.df<- r.df[,c("x", "y", "Area")]
    rd<- rasterFromXYZ(r.df) 
    plot(rd)
    writeRaster(rd, filename= paste(g,"/","Raster_Beans_",sp[i],"_.tif",sep=""), format="GTiff", overwrite=TRUE)

#### processing to get the same resolution -----------
    
## Spatial management Data---------------
    
# mask by  kenya
fpu<-  shapefile("C:/Users/CEGONZALEZ/Desktop/BID/BID/newBID/ImpactResults/Shapefiles mundo/fpu_shp/fpu_shp/fpu.shp")
# filter FPU kenya
fpu <- fpu[fpu@data$Region_Nam=="Kenya",]

require(ncdf4)
# data of planting date
da <- stack(x =paste(g,"/","Pulses_rf_growing_season_dates_v1.25.nc4", sep = ""),varname= "planting day")
cr <- crop(da, extent(fpu))
dake <- raster::mask(cr,fpu) 
pdata <- as(dake, "SpatialPixelsDataFrame")
pdata <- as.data.frame(pdata)
head(pdata)
colnames(pdata)[1]<- "plant_day"
pdata<- pdata[,c("x","y","plant_day")]
pdata$coord <- paste(pdata$x,pdata$y)

# data climate
pre <- raster(paste(g,"/","prec_1971_2000_01_avg.nc", sep = ""))
crs(pre)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  # reproyect

# Resample raster a 50km * 50km
preR <- resample(pre, dake, method = 'ngb') # precipitation


# load raster location
# data localizacion cultivos
r <- raster(paste(g,"/","spam2005v2r0_harvested-area_bean_","rainfed.tiff",sep = "")) # load
crs(r)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"            # reproyect
cr <- crop(r, extent(fpu))
rr <- raster::mask(cr,fpu) 

# convierto en dataframe
r.spdf <- as(rr, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
head(r.df)
colnames(r.df)[1]<- "Area"
r.df<- filter(r.df, Area!=0)
# r.df$New_FPU<- "HOA_KEN"
# r.df$Coincidencias<- row.names(r.df)
# r.df<- r.df[c("x","y","Area","New_FPU","Coincidencias" )]
r.df<- r.df[,c("x", "y", "Area")]
q1<- quantile(r.df$Area,c(0.25))
r.df<- filter(r.df, Area>q1)
rd<- rasterFromXYZ(r.df) 
crs(rd)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"   # reproyect


loc <- resample(rd, dake, method = 'ngb')  # localization crops 
matrix_mag<- as(loc, "SpatialPixelsDataFrame")
matrix_mag <- as.data.frame(matrix_mag)
matrix_mag$New_FPU<- "HOA_KEN"
matrix_mag$Coincidencias<- row.names(matrix_mag)
matrix_mag<- matrix_mag[c("x","y","Area","New_FPU","Coincidencias" )]
matrix_mag<- pdata[,c("x","y","plant_day")]
matrix_mag$coord <- paste(matrix_mag$x,matrix_mag$y)

# join planting data and local

ma<-left_join(matrix_mag, pdata, by=c("coord","x","y"))
ma<- na.omit(ma)
rownames(ma)<- 1:nrow(ma)
write.csv(ma,paste(g,"/","KenyaMatrixCrop_rainfed.csv", sep=""))


# Extraction coordinates for runing
jj<-preR # precipitation 
jj[jj>0]<-1
sum(jj[jj])

bean<-  loc # Area
bean[bean>0]<-1
sum(bean[bean])

ff<- dake # planting date
ff[ff>0]<-1

test<- bean+jj+ff
test[test==3]<-1
sum(test[test])

writeRaster(test, filename= paste(g,"/","rasterSpots_Rainfed.tif",sep=""), format="GTiff", overwrite=TRUE)

plot(test)
plot(fpu,add=T)


# obtener coordenadas
ltest <- as(test, "SpatialPixelsDataFrame")
pix<- as.data.frame(ltest@coords)
write.csv(x = pix,paste(g,"/","coordenadas.csv",sep=""))


### tabla manejo agronomico
head(r.df)


fp <- as(ff, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
