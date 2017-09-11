# Codigos para el procesamiento y organizacion de los graficos para el CSA. Climate Smart Acgriculture
# Harold and Carlos 

#librerias------------
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(tidyr)
library(lattice)
library(latticeExtra)
library(colorspace)
library(Rcpp)


#Definir directorio de trabajo-------------
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.2/OutputFiles/Aggregation/")
#Direción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/CSA/"
#Direción copias seguridad
copy<-"C:/Users/CEGONZALEZ/Documents/CSA/copy/"

# manejo de digitos
options(digits=3) 
options(scipen = 999)

#Cargar marco de datos principal-------------
md<-read.csv("CSA_Philippines.csv",header=T)
phi<- md

# eliminar calorias percapita agregadas
phi<- phi[which(phi$impactparameter!="PerCapKCalXAgg"),]


#listra de productos
cultivos<- (unique(phi$commodity))


#lista de parametros
parametros<- (unique(phi$impactparameter))

# #Hacer un subconjunto que sólo contenga las variables de mi interés----------------
# mdsub_phi<-subset(phi,phi$impactparameter=="TAreaXAgg -- Total Area" |
#                       phi$impactparameter== "QNXAgg -- Net Trade" | 
#                       phi$impactparameter== "TYldXAgg -- Total Yield"|
#                       phi$impactparameter=="AnmlNumXAgg -- Animal Numbers"|
#                       phi$impactparameter=="AnmlYldXAgg -- Animal Yield")
# 
# mdsub_phi$impactparameter<-revalue(mdsub_phi$impactparameter, c("QNXAgg -- Net Trade"="Net Trade",
#                                                                     "TAreaXAgg -- Total Area"="Total Area",
#                                                                     "TYldXAgg -- Total Yield"= "Yield",
#                                                                     "AnmlNumXAgg -- Animal Numbers"="Animal Numbers",
#                                                                     "AnmlYldXAgg -- Animal Yield"="Animal Yield"))
# 

mdsub_phi<- subset(phi, phi$impactparameter=="PerCapKCalCXAgg -- PcKcal by Commodity"|
                   phi$impactparameter=="QDXAgg -- Total Demand"|
                   phi$impactparameter=="QMXAgg -- Import")


# 
# 
# #Hacer un subconjunto que sólo contenga los cinco cultivos analizados
# mdsub_phi<-subset(mdsub_phi,mdsub_phi$commodity=="ccafe"| mdsub_phi$commodity=="jcafe" |
#                     mdsub_phi$commodity=="cfv" | mdsub_phi$commodity=="jfv" | 
#                     mdsub_phi$commodity=="cmaiz" | mdsub_phi$commodity=="jmaiz" | 
#                     mdsub_phi$commodity=="cmeat" | mdsub_phi$commodity=="jmeat" |
#                     mdsub_phi$commodity=="cpork" | mdsub_phi$commodity=="jpork" |
#                     mdsub_phi$commodity=="crice" | mdsub_phi$commodity=="jrice" |
#                     mdsub_phi$commodity=="ctemf" | mdsub_phi$commodity=="jtemf" |
#                     mdsub_phi$commodity=="jsubf" | mdsub_phi$commodity== "csubf"|
#                     mdsub_phi$commodity=="ccas" |  mdsub_phi$commodity=="jcas" |
#                     mdsub_phi$commodity=="cvege" | mdsub_phi$commodity=="jvege" |
#                     mdsub_phi$commodity=="ccoco" | mdsub_phi$commodity=="jcoco"|
#                     mdusb_phi$commodity=="jcoconut" | mdsub_phi$commodity=="ccoconut")


mdsub_phi$productiontype<- NULL
row.names(mdsub_phi)<- 1: nrow(mdsub_phi)

# cambio de nombre de los cultivos 
mdsub_phi$commodity<-revalue(mdsub_phi$commodity, c( "ccafe"="Coffe",
                                                     "jcafe"="Coffe",
                                                     "cfv"="Fruits and Vegetables",
                                                     "jfv"="Fruits and Vegetables",
                                                     "cmaiz"="Maize",
                                                     "jmaiz"="Maize",
                                                     "cmeat"="Meat",
                                                     "jmeat"="Meat",
                                                     "cpork"="Pork",
                                                     "jpork"="Pork",
                                                     "crice"="Rice",
                                                     "jrice"="Rice",
                                                     "csubf"="sub-Tropical fruit",
                                                     "jsubf"="sub-Tropical fruit",
                                                     "ccoco"="Cocoa",
                                                     "jcoco"="Cocoa",
                                                     "jtemf"="Temperate Fruit",
                                                     "ctemf"="Temperate Fruit",
                                                     "jcoconut"= "Coconut-oil",
                                                     "ccoconut"= "Coconut-oil"))

mdwide_phi<- mdsub_phi %>%
  spread ("year", "Val")


# categorias de climas
mdwide_phi<-data.frame(mdwide_phi,"Cat"=ifelse(mdwide_phi$scenario=="NoCC","NoCC","CC"))
mdwide_phi<-mdwide_phi[,-c(5:19)]
rownames(mdwide_phi)<- 1: nrow(mdwide_phi)


#Copia de seguridad pais filtrado------
write.csv(mdwide_phi,paste(copy,"calories_perca_.csv", sep = ""), row.names = FALSE)

cal_per<- mdwide_phi[which(mdwide_phi$impactparameter=="PerCapKCalCXAgg -- PcKcal by Commodity"),]
year<- c("X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050")
cal_per <-cal_per[, c("impactparameter","scenario", "commodity", "region", "Cat",year)]



cal_per$Percentage_Change<- ((cal_per$X2050-cal_per$X2020)/cal_per$X2020)*100
cal_per<- cal_per[, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
cal_per<- cal_per %>%
  spread(scenario, Percentage_Change)


##Calculo de la media
cal_per$mean <- rowMeans(x=cal_per[,5:ncol(cal_per)], na.rm=TRUE)
cal_per <- cal_per[,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
cal_per$NoCC<- NULL
#Reshape
cal_per <- cal_per %>% spread(Cat, mean)

# exportar datos a excel
require(xlsx)
write.xlsx( x = cal_per,file= paste(copy, "Calories_Philippines.xlsx", sep = ""),
            sheetName = "Calories%", col.names = TRUE, append = TRUE, showNA = FALSE)

#reshape
cal_per<- cal_per %>%
  gather("scenarios", "Val" , 4:ncol(cal_per))


cal_per$commodity<- as.character(cal_per$commodity)
cal_per$scenarios<- as.character(cal_per$scenarios)
cal_per$region<- as.character(cal_per$region)
cal_per$impactparameter<- as.character(cal_per$impactparameter)

n<- ggplot(cal_per, aes(x=commodity, y=Val, fill=scenarios)) + theme_bw() + geom_bar(stat = "identity", position=position_dodge()) + coord_flip()
n<- n + ggtitle("Per capita calories by commodity") 
n<- n + theme(axis.title.x=element_text(size=12, face='bold'))
n<- n + theme(plot.title=element_text(size=15, face = 'bold'))
n<- n + ylab('Percentage Change') +  xlab("Commodity")
n<- n + scale_y_continuous(limits = c(-20, 50))


tiff(filename=paste(grd,"CaloriesIMPACT.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 500)

n

dev.off()


#Copia de seguridad para demanda total-------------- 
dem<- mdwide_phi[which(mdwide_phi$impactparameter=="QDXAgg -- Total Demand"),]
dem$Percentage_Change<- ((dem$X2050-dem$X2020)/dem$X2020)*100
dem<- dem[, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
dem<- dem %>%
  spread(scenario, Percentage_Change)


##Calculo de la media
dem$mean <- rowMeans(x=dem[,5:ncol(dem)], na.rm=TRUE)
dem <- dem[,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
dem$NoCC<- NULL
#Reshape
dem <- dem %>% spread(Cat, mean)

# exportar datos a excel
require(xlsx)
write.xlsx( x = dem,file= paste(copy,"Calories_Philippines.xlsx", sep = ""),
            sheetName = "DemandaTotal", col.names = TRUE, append = TRUE, showNA = FALSE)

#reshape
dem<- dem %>%
  gather("scenarios", "Val" , 4:ncol(cal_per))


dem$commodity<- as.character(dem$commodity)
dem$scenarios<- as.character(dem$scenarios)
dem$region<- as.character(dem$region)
dem$impactparameter<- as.character(dem$impactparameter)

nn<- ggplot(dem, aes(x=commodity, y=Val, fill=scenarios)) + theme_bw() + geom_bar(stat = "identity", position=position_dodge()) + coord_flip()
nn<- nn + ggtitle("Total demand for commodity  ") 
nn<- nn + theme(axis.title.x=element_text(size=12, face='bold'))
nn<- nn + theme(plot.title=element_text(size=15, face = 'bold'))
nn<- nn + ylab('Percentage Change') +  xlab("Commodity")


tiff(filename=paste(grd,"DemandaIMPACT.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 500)

nn

dev.off()




# #Copia de seguridad pais filtrado------
# write.csv(mdwide_phi,paste(copy,"mdwidePhilippinesTotal.csv", sep = ""), row.names = FALSE)
# 
# #Net Trade y  filtros logicos-----------------
# Phinet<- mdwide_phi[which(mdwide_phi$impactparameter=="Net Trade"),]
# 
# 
# ## logica de los valores 
# nn<-  which(Phinet$X2050<0 & Phinet$X2020<0) # net trade negativo  importador neto
# pn<-  which(Phinet$X2050>0 & Phinet$X2020<0) # impacto positivo inicia como importador termina como exportador
# np<-  which(Phinet$X2050<0 & Phinet$X2020>0) # impacto negativo inicia como exportador termina como importador
# pp<-  which(Phinet$X2050>0 & Phinet$X2020>0) # net trade positivo  exportador neto
# 
# # No se tienen datos de tendencia en el que 
# 
# # desempeño 
# export<-   c(pp)
# import <-  c(nn)
# #tran_XtoM<- c(np) # inicia exportador termina importador
# #tran_MtoX<- c(pn) # inicia importador termina exportador
# 
# hard<- c(nn,pp)
# 
# Phinet$impacto<- NA # para poner el impacto  cambio relativo
# 
# #loops 
# for(j in 1:nrow(Phinet)){
#   
#   #if(j %in% hard){
#     Phinet$impacto[j] <- ((Phinet$X2050[j] - Phinet$X2020[j])/Phinet$X2020[j]) * 100
# #  } else {  }
#   
# }
# 
# ##Copia de seguridad cambios relativos y vectores de desempeño
# write.csv(Phinet,paste(copy,"CambiosRelativosPhilippinesTOTAL.csv", sep = ""), row.names = FALSE)
# 
# # # copia ojo con el orden
# phill<- Phinet
# phill<- as.data.frame(phill)
# # phill$trend[tran_MtoX]<- "Transition M to X"
# # phill$trend[tran_XtoM]<- "Transition X to M"
# phill$trend[import] <- "Negative"
# phill$trend[export] <- "Positive"
# 
# #list por crops
# crops<- list( "Coconut", "Cocoa", "Coffe",  "Fruits and Vegetables",
#               "Maize", "Meat", "Pork", "Rice", "sub-Tropical fruit", 
#               "Temperate fruit", "Vegetables" ) 
# 
# c <- list()
# 
# for (i in 1:length(crops)){
#   
#   if(length(which(phill$commodity==crops[[i]]))>0){
#     z <- phill[which(phill$commodity==crops[[i]]),]
#     x <- table(z$trend[z$Cat=='CC'])
#     z <- z[,c("impactparameter","scenario", "commodity", "region", "Cat", "impacto")] %>% spread("scenario","impacto")
#     if(x >= 3){
#       z$trend <- names(x)
#     }
#     c[[i]] <- z
#   } else{
#     cat(paste('Commodity:', crops[i], 'does not have data\n', sep=''))
#     
#   }
#   
# }
# 
# ##Unir los cultivos
# c <- do.call(rbind, c)  
# 
# ##Calcular la media de los impactos
# c$CC_mean <- rowMeans(x=c[,5:8], na.rm=TRUE)
# c$NoCC_mean <- c[,9]
# c<- c[,c("impactparameter","commodity","region","trend","CC_mean","NoCC_mean")]
# write.csv(c, paste(copy, "PhilippinesNettradeTOTAL.csv", sep = ""),row.names = FALSE)
# 
# 
# 
# 
# #Area  y rendimientos---------------------------------------
# Phill2<- mdwide_phi[which(mdwide_phi$impactparameter=="Total Area" | 
#                             mdwide_phi$impactparameter=="Yield" |
#                             mdwide_phi$impactparameter=="Animal Yield" | 
#                             mdwide_phi$impactparameter=="Total Production"|
#                             mdwide_phi$impactparameter=="Animal Numbers"),]
# 
# Phill2$Percentage_Change<- ((Phill2$X2050-Phill2$X2020)/Phill2$X2020)*100
# 
# ##Eliminamos las columnas que no necesitamos 
# Phill2<- data.frame(Phill2[1:4], Phill2[,36:37])
# 
# ##Reshape
# Phill2_t<-Phill2 %>%
#   spread("scenario","Percentage_Change")
# ##Calculo de la media
# Phill2_t$mean <- rowMeans(x=Phill2_t[,5:ncol(Phill2_t)], na.rm=TRUE)
# Phill2_t <- Phill2_t[,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
# Phill2_t$NoCC<- NULL
# #Reshape
# Phill2_t <- Phill2_t %>% spread(Cat, mean)
# write.csv(Phill2_t, paste(copy, "PhilippinesTableSumm.csv", sep=""), row.names = FALSE)
# 
# #Solicitud datos CSA-------------
# mdwide_PhilCSA<- mdwide_phi
# mdwide_PhilCSA<- mdwide_PhilCSA[,c("impactparameter", "scenario","commodity","region","X2020","X2025","X2030", "X2035","X2040","X2045", "X2050", "Cat" )]
# mdwide_PhilCSA<- mdwide_PhilCSA[-which(mdwide_PhilCSA=="Net Trade"),]
# rownames(mdwide_PhilCSA)<- 1:nrow(mdwide_PhilCSA)
# 
# 
# #Listas para desarrollar el proceso
# #cultivos
# #periodos
# k<- list()
# testyear<- list()
# y<- c("X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050")
# v<- c("Total Area", "Yield" )
# 
# library(dplyr)
# library(tidyr)
# #library(Rcpp)
# #library(Rcpp11)
# csa <- mdwide_PhilCSA[,c("impactparameter", "commodity", paste("X", seq(from=2020, to=2050, by=5), sep = ""), "Cat")] %>% group_by(impactparameter, commodity, Cat) %>% summarise_each(funs(mean))
# csa <- csa %>% gather(Year, Value, X2020:X2050)
# csa <- csa %>% spread(Cat, Value)
# csa$Year <- as.numeric(gsub(pattern = "X", replacement = "", x = csa$Year))
# csa$percentual_change <- (csa$CC-csa$NoCC)/csa$NoCC * 100
# 
# write.csv(csa, paste(copy, "PhilippinesCSAAreaYield.csv", sep = ""), row.names = FALSE)
# 
# 
# 
# library(ggplot2)
# 
# #grafico Area Cultivos
# tiff(filename=paste(grd,"AreaCommoditiesPhili.tiff", sep=""), width = 10, height = 10, units = 'in', res =800)
# 
# n<- ggplot(csa[which(csa$impactparameter=="Total Area"),], aes(x=Year, y=percentual_change ))
# n<- n + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
# n<- n + geom_area(position = "stack", alpha = 0.4)
# n<- n + ggtitle("Area ") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
# n<- n + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
# n<- n + coord_cartesian(ylim = c(-15, 2)) +  scale_y_continuous( breaks=seq(-15, 2,0.5 ))
# n<- n + coord_cartesian(xlim = c(2020, 2050)) +   scale_x_continuous( breaks=seq(2020,2050,5))
# 
# n
# dev.off()
# 
# 
# library(ggplot2)
# #grafico numero de Aminales 
# tiff(filename=paste(grd,"Numer Animales Phili.tiff", sep=""), width = 10, height = 10, units = 'in', res =800)
# 
# n1<- ggplot(csa[which(csa$impactparameter=="Animal Numbers"),], aes(x=Year, y=percentual_change ))
# n1<- n1 + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
# n1<- n1 + geom_area(position = "stack", alpha = 0.4)
# n1<- n1 + ggtitle("Animal Numbers") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
# n1<- n1 + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
# n1<- n1 + coord_cartesian(ylim = c(-15, 0)) + scale_y_continuous( breaks=seq(-15,2,0.1))
# n1<- n1 + coord_cartesian(xlim = c(2020, 2050)) +   scale_x_continuous( breaks=seq(2020,2050,5))
# 
# n1
# dev.off()
# 


############################################## Ensayis ##################################################

#parametros, md
data<- mdsub_phi
cultivos<- as.character(unique(data$commodity))
data$impactparameter<- as.character(data$impactparameter)
data$commodity<- as.character(data$commodity)
data$scenario<- as.character(data$scenario)
data$region<- as.character(data$region)

# loop para filtrar parametros por cultivo
z<- list()
for(i in seq_along(cultivos)){
    z[[i]]<- data[which(data$commodity==cultivos[i]&
                          data$impactparameter=="QMXAgg -- Import"),] 
}


z<- do.call(rbind, z)
z<-data.frame(z,"Cat"=ifelse(z$scenario=="NoCC","NoCC","CC"))
z<- z %>% 
  spread( year, Val)

z$Percentage_Change<- ((z$`2050`-z$`2020`)/z$`2020`)*100
z<- z[,c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
z<- z %>%
  spread(scenario, Percentage_Change)


##Calculo de la media
z$mean <- rowMeans(x=z[,5:ncol(z)], na.rm=TRUE)
z <- z[,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
z$NoCC<- NULL
#Reshape
z <- z %>% spread(Cat, mean)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy,"Calories_Philippines.xlsx", sep = ""),
            sheetName = "Imports", col.names = TRUE, append = TRUE, showNA = FALSE)

#reshape
z<- z %>%
  gather("scenarios", "Val" , 4:ncol(z))


dem$commodity<- as.character(dem$commodity)
dem$scenarios<- as.character(dem$scenarios)
dem$region<- as.character(dem$region)
dem$impactparameter<- as.character(dem$impactparameter)

nnn<- ggplot(z, aes(x=commodity, y=Val, fill=scenarios)) + theme_bw() + geom_bar(stat = "identity", position=position_dodge()) + coord_flip()
nnn<- nnn + ggtitle("Imports for each country and traded commodity ") 
nnn<- nnn + theme(axis.title.x=element_text(size=12, face='bold'))
nnn<- nnn + theme(plot.title=element_text(size=15, face = 'bold'))
nnn<- nnn + ylab('Percentage Change') +  xlab("Commodity")
nnn<- nnn + scale_y_continuous(limits = c(-80, 350))

tiff(filename=paste(grd,"ImportsIMPACT.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 500)

nnn

dev.off()



