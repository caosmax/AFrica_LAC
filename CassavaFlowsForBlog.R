# codigos para graficos de blog de yuca.
# autor: Harold and Carlos 

#librerias------
library(XML)
library(treemap)
library(migest)
library(circlize)
library(ggplot2) 
library(dplyr)  
library(plyr)
library(grid)
library(gridExtra)
library(xtable)
library(dplyr)
library(tidyr)
library(lattice)
library(latticeExtra)
library(rgdal)
library(sp)
library(maptools)
library(tiff)
library(rasterVis)
library(dismo)


#directorio
setwd("D:/ToBackup/Modelling/global-futures-and-strategic-foresight")
# directorio de archivos para graficar
pic <-("C:/Users/CEGONZALEZ/Documents/cassava/graph/")
# respaldos en Excel
copy<- "C:/Users/CEGONZALEZ/Documents/cassava/copyData/"

# Limitar numero de decimales y remove scientific notation-----
options(digits=3) 
options(scipen=999)

#matrices de relaciones comerciales 
fao_matrix_trade<- read.csv("_data/_cassava_data/FAOSTAT_data_11-22-2016.csv", header = TRUE)

#fao_matrix_trade  depuracion de la base de datos --------------
fao_matrix_trade$Domain.Code<- NULL
fao_matrix_trade$Flag<- NULL
fao_matrix_trade$FlagD<- NULL
fao_matrix_trade$ItemCode<- NULL
fao_matrix_trade$Element.Code<- NULL
fao_matrix_trade$Domain<-NULL
fao_matrix_trade$Item.Code<-NULL
fao_matrix_trade$Year.Code<-NULL
fao_matrix_trade$NoRecords<-NULL
fao_matrix_trade$Flag.Description<-NULL
# fao_matrix_trade<- fao_matrix_trade[-(28389),]
names(fao_matrix_trade)[names(fao_matrix_trade) == 'Element'] <- 'variable'
names(fao_matrix_trade)[names(fao_matrix_trade) == 'AreaName'] <- 'region'
# fao_matrix_trade<- fao_matrix_trade[which(fao_matrix_trade$Unit!="1000 US$"),]
# fao_matrix_trade<- fao_matrix_trade[which(fao_matrix_trade$Item!="Cassava" ),]
# fao_matrix_trade<- fao_matrix_trade[which(fao_matrix_trade$variable!="Export Value" | fao_matrix_trade$variable!="Import Value"),]
fao_matrix_trade$RecordOrder<- NULL
fao_matrix_trade$Reporter.Country.Code<- NULL
fao_matrix_trade$Partner.Country.Code<- NULL
rownames(fao_matrix_trade)<- 1:nrow(fao_matrix_trade)

# Exportaciones---------------------
fao_matrix_trade<- fao_matrix_trade[which(fao_matrix_trade$variable=="Export Quantity"),]

# copia de la base de datos en excel  
write.csv(fao_matrix_trade, paste(copy, "Xmatrixtradecassava.csv", sep = ""), row.names=T)
fao_matrix_trade<- read.csv(paste(copy, "Xmatrixtradecassava.csv", sep = ""))
fao_matrix_trade$X<- NULL

# Periodos-------------

y1 <- 1986:1992
y2 <- 1993:1999
y3 <- 2000:2006
y4 <- 2007:2013

matrix<- list()

zz<- list(y1, y2, y3, y4)

for(i in 1:length(zz)) {
  matrix[[i]]<- fao_matrix_trade[which(fao_matrix_trade$Year==zz[[i]]),]  
}

p1<- as.data.frame(matrix[[1]])
rownames(p1)<- 1:nrow(p1)

p2<- as.data.frame(matrix[[2]])
rownames(p2)<- 1:nrow(p2)

p3<- as.data.frame(matrix[[3]])
rownames(p2)<- 1:nrow(p2)

p4<- as.data.frame(matrix[[4]])
rownames(p4)<- 1:nrow(p4)

# Medias de los periodos--------------

p1wide<- p1  %>% 
  spread("Year", "Value")
p2wide<- p2  %>% 
  spread("Year", "Value")
p3wide<- p3  %>% 
  spread("Year", "Value")
p4wide<- p4  %>% 
  spread("Year", "Value")

# Calculate mean
p1wide$mean <- rowMeans(x=p1wide[,6:ncol(p1wide)], na.rm=TRUE)
p2wide$mean <- rowMeans(x=p2wide[,6:ncol(p2wide)], na.rm=TRUE)
p3wide$mean <- rowMeans(x=p3wide[,6:ncol(p3wide)], na.rm=TRUE)
p4wide$mean <- rowMeans(x=p4wide[,6:ncol(p4wide)], na.rm=TRUE)

p1wide <- p1wide[,c("Reporter.Countries", "Partner.Countries", "mean" )]
p2wide <- p2wide[,c("Reporter.Countries", "Partner.Countries", "mean" )]
p3wide <- p3wide[,c("Reporter.Countries", "Partner.Countries", "mean" )]
p4wide <- p4wide[,c("Reporter.Countries", "Partner.Countries", "mean" )]

# Calculate median
library(matrixStats)
p1wide$median <- rowMedians(x=as.matrix(p1wide[,6:ncol(p1wide)]), na.rm=TRUE)
p2wide$median <- rowMedians(x=as.matrix(p2wide[,6:ncol(p2wide)]), na.rm=TRUE)
p3wide$median <- rowMedians(x=as.matrix(p3wide[,6:ncol(p3wide)]), na.rm=TRUE)
p4wide$median <- rowMedians(x=as.matrix(p4wide[,6:ncol(p4wide)]), na.rm=TRUE)

p1wide <- p1wide[,c("Reporter.Countries", "Partner.Countries", "median" )]
p2wide <- p2wide[,c("Reporter.Countries", "Partner.Countries", "median" )]
p3wide <- p3wide[,c("Reporter.Countries", "Partner.Countries", "median" )]
p4wide <- p4wide[,c("Reporter.Countries", "Partner.Countries", "median" )]

# Calculate skewness
library(e1071)
p1wide$skewness <- as.numeric(apply(X = p1wide[,6:ncol(p1wide)], MARGIN = 1, FUN = skewness, na.rm = T))
p2wide$skewness <- as.numeric(apply(X = p2wide[,6:ncol(p2wide)], MARGIN = 1, FUN = skewness, na.rm = T))
p3wide$skewness <- as.numeric(apply(X = p3wide[,6:ncol(p3wide)], MARGIN = 1, FUN = skewness, na.rm = T))
p4wide$skewness <- as.numeric(apply(X = p4wide[,6:ncol(p4wide)], MARGIN = 1, FUN = skewness, na.rm = T))

p1wide <- p1wide[,c("Reporter.Countries", "Partner.Countries", "skewness" )]
p2wide <- p2wide[,c("Reporter.Countries", "Partner.Countries", "skewness" )]
p3wide <- p3wide[,c("Reporter.Countries", "Partner.Countries", "skewness" )]
p4wide <- p4wide[,c("Reporter.Countries", "Partner.Countries", "skewness" )]

# Calculate mode
library(modeest)
p1wide$mode <- as.numeric(apply(X = p1wide[,6:ncol(p1wide)], MARGIN = 1, FUN = asselin, na.rm = T))
p2wide$mode <- as.numeric(apply(X = p2wide[,6:ncol(p2wide)], MARGIN = 1, FUN = asselin, na.rm = T))
p3wide$mode <- as.numeric(apply(X = p3wide[,6:ncol(p3wide)], MARGIN = 1, FUN = asselin, na.rm = T))
p4wide$mode <- as.numeric(apply(X = p4wide[,6:ncol(p4wide)], MARGIN = 1, FUN = asselin, na.rm = T))

p1wide <- p1wide[,c("Reporter.Countries", "Partner.Countries", "mode" )]
p2wide <- p2wide[,c("Reporter.Countries", "Partner.Countries", "mode" )]
p3wide <- p3wide[,c("Reporter.Countries", "Partner.Countries", "mode" )]
p4wide <- p4wide[,c("Reporter.Countries", "Partner.Countries", "mode" )]

mat <- list(p1wide, p2wide, p3wide, p4wide)

# Mean calculated
lapply(1:length(mat), function(i) write.csv(mat[[i]], paste('./_data/_cassava_data/periods_mean/periodo', i, '.csv', sep = '')))
# Median calculated
lapply(1:length(mat), function(i) write.csv(mat[[i]], paste('./_data/_cassava_data/periods_median/periodo', i, '.csv', sep = '')))
# Skewness calculated
lapply(1:length(mat), function(i) write.csv(mat[[i]], paste('./_data/_cassava_data/periods_skewness/periodo', i, '.csv', sep = '')))
# Mode calculated
lapply(1:length(mat), function(i) write.csv(mat[[i]], paste('./_data/_cassava_data/periods_mode/periodo', i, '.csv', sep = '')))

# p1-----------------------------------------------------------------------------
auxCarlos<- expand.grid(unique(p1wide$Reporter.Countries), unique(p1wide$Partner.Countries))
colnames(auxCarlos) <- colnames(p1wide[,1:2])
auxCarlos2 <- p1wide[,1:2]

require(sqldf)
auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
dim(auxCarlos2)
auxCarlos2$mean <- 0
p1wide <- rbind(p1wide, auxCarlos2)


p1wide <- as.data.frame(p1wide)
p1wide$Reporter.Countries <- as.character(p1wide$Reporter.Countries)
p1wide$Partner.Countries <- as.character(p1wide$Partner.Countries)
p1wide$mean <- as.numeric(p1wide$mean)
p1wide$mean[which(p1wide$mean==0)] <- 1
p1wide$mean <- log(p1wide$mean, base=exp(1))
p1wide$mean[which(p1wide$mean==0)] <- 0.5

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


png('p1wide.png', width=8, height=8, units='in', res=300)
chordDiagram(x = p1wide[p1wide$mean>1.5,], transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)


dev.off()

# p2--------------------------------------------------
uxCarlos<- expand.grid(unique(p2wide$Reporter.Countries), unique(p2wide$Partner.Countries))
colnames(auxCarlos) <- colnames(p2wide[,1:2])
auxCarlos2 <- p2wide[,1:2]

require(sqldf)
auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
dim(auxCarlos2)
auxCarlos2$mean <- 0
p2wide <- rbind(p2wide, auxCarlos2)


p2wide <- as.data.frame(p2wide)
p2wide$Reporter.Countries <- as.character(p2wide$Reporter.Countries)
p2wide$Partner.Countries <- as.character(p2wide$Partner.Countries)
p2wide$mean <- as.numeric(p2wide$mean)
p2wide$mean[which(p2wide$mean==0)] <- 1
p2wide$mean <- log(p2wide$mean, base=exp(1))
p2wide$mean[which(p2wide$mean==0)] <- 0.5

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


png('p2wide.png', width=8, height=8, units='in', res=300)
chordDiagram(x = p2wide[p2wide$mean>1.5,], transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)


dev.off()

# p3--------------------------------------------------
auxCarlos<- expand.grid(unique(p3wide$Reporter.Countries), unique(p3wide$Partner.Countries))
colnames(auxCarlos) <- colnames(p3wide[,1:2])
auxCarlos2 <- p3wide[,1:2]

require(sqldf)
auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
dim(auxCarlos2)
auxCarlos2$mean <- 0
p3wide <- rbind(p3wide, auxCarlos2)


p3wide <- as.data.frame(p3wide)
p3wide$Reporter.Countries <- as.character(p3wide$Reporter.Countries)
p3wide$Partner.Countries <- as.character(p3wide$Partner.Countries)
p3wide$mean <- as.numeric(p3wide$mean)
p3wide$mean[which(p3wide$mean==0)] <- 1
p3wide$mean <- log(p3wide$mean, base=exp(1))
p3wide$mean[which(p3wide$mean==0)] <- 0.5

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


png('p3wide.png', width=8, height=8, units='in', res=300)
chordDiagram(x = p3wide[p3wide$mean>1.5,], transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)


dev.off()

# p4-------------------------------------------------
auxCarlos<- expand.grid(unique(p4wide$Reporter.Countries), unique(p4wide$Partner.Countries))
colnames(auxCarlos) <- colnames(p4wide[,1:2])
auxCarlos2 <- p4wide[,1:2]

require(sqldf)
auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
dim(auxCarlos2)
auxCarlos2$mean <- 0
p4wide <- rbind(p4wide, auxCarlos2)


p4wide <- as.data.frame(p4wide)
p4wide$Reporter.Countries <- as.character(p4wide$Reporter.Countries)
p4wide$Partner.Countries <- as.character(p4wide$Partner.Countries)
p4wide$mean <- as.numeric(p4wide$mean)
p4wide$mean[which(p4wide$mean==0)] <- 1
p4wide$mean <- log(p4wide$mean, base=exp(1))
p4wide$mean[which(p4wide$mean==0)] <- 0.5

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


png('p4wide.png', width=8, height=8, units='in', res=300)
chordDiagram(x = p4wide[p4wide$mean>1.5,], transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)


dev.off()

# copia de matrices en csv.-----------

write.csv(p1wide, paste(copy, "periodo1.csv", sep = ""), row.names=T)
write.csv(p2wide, paste(copy, "periodo2.csv", sep = ""), row.names=T)
write.csv(p3wide, paste(copy, "periodo3.csv", sep = ""), row.names=T)
write.csv(p4wide, paste(copy, "periodo4.csv", sep = ""), row.names=T)

