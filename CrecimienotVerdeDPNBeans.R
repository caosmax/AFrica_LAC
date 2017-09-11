#Visualize DSSAT runs/ Beans 

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
library(rasterVis)

#cargamos shape ALC
map <- "C:/Users/CEGONZALEZ/Documents/gisdata/"
world <- shapefile(paste0(map,"G2014_2013_0.shp"))
world<- world[world@data$ADM0_NAME!="Antarctica",]
world<- world[world@data$ADM0_NAME!="Greenland",]
plot(world)

pic<- c("C:/Users/CEGONZALEZ/Documents/GFSF/")

#visualizacion de raster
spam_yield<-raster("C:/Users/CEGONZALEZ/Documents/GFSF/spam2005v2r0_yield_bean_total.tiff")
levelplot(spam_yield)

p <- levelplot(spam_yield, layers=1,par.settings = RdBuTheme, margin = FALSE, main = "Beans yield in tons/ha")
p<- p  + layer(sp.lines(world, lwd=0.7, col="darkgray")) 


tiff(filename=paste(pic,"spamyield.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p

dev.off()


spam_yield[spam_yield[]==0]<- NA # smap_yield[] responde a todos los valores de raster
p1 <- levelplot(spam_yield, layers=1,par.settings = RdBuTheme, margin = FALSE, main = "Rendimientos de Frijol  kg/ha\nSPAM( Spatial Allocation Model",
                colorkey=list(space='bottom'))
p1<- p1  + layer(sp.lines(world, lwd=0.7, col="darkgray")) 


tiff(filename=paste(pic,"spamyieldBEAN.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p1

dev.off()

spam_yield[spam_yield>0]<-1
spam_yield<- spam_yield[spam_yield==1]
sum(spam_yield)

cr[cr>0]<-1
cr<- cr[cr==1]
sum(cr)