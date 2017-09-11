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
md<-read.csv("CSA_NPL.csv",header=T)
nepal<- md


#Hacer un subconjunto que sólo contenga las variables de mi interés----------------
mdsub_nepal<-subset(nepal,nepal$impactparameter=="TAreaXAgg -- Total Area" |
                         nepal$impactparameter== "QNXAgg -- Net Trade" | 
                         nepal$impactparameter== "TYldXAgg -- Total Yield"|
                      nepal$impactparameter=="AnmlNumXAgg -- Animal Numbers"|
                      nepal$impactparameter=="AnmlYldXAgg -- Animal Yield" |
                      nepal$impactparameter=="QSXAgg -- Total Production")

mdsub_nepal$impactparameter<-revalue(mdsub_nepal$impactparameter, c("QNXAgg -- Net Trade"="Net Trade",
                                                                          "TAreaXAgg -- Total Area"="Total Area",
                                                                          "TYldXAgg -- Total Yield"= "Yield",
                                                                          "AnmlNumXAgg -- Animal Numbers"="Animal Numbers",
                                                                          "AnmlYldXAgg -- Animal Yield"="Animal Yield",
                                                                          "QSXAgg -- Total Production"= "Total Production"))

#Hacer un subconjunto que sólo contenga los cinco cultivos analizados
mdsub_nepal<-subset(mdsub_nepal,mdsub_nepal$commodity=="jbeef"| mdsub_nepal$commodity=="cbeef" |
                      mdsub_nepal$commodity=="jmaiz"| mdsub_nepal$commodity=="cmaiz" |
                      mdsub_nepal$commodity=="jrice" | mdsub_nepal$commodity=="crice" | 
                      mdsub_nepal$commodity=="jlamb" | mdsub_nepal$commodity=="clamb" | 
                      mdsub_nepal$commodity=="jlent" | mdsub_nepal$commodity=="clent" |
                      mdsub_nepal$commodity=="jpork" | mdsub_nepal$commodity=="cpork" |
                      mdsub_nepal$commodity=="jpota" | mdsub_nepal$commodity=="cpota" |
                      mdsub_nepal$commodity=="jmilk" | mdsub_nepal$commodity=="cmilk" |
                      mdsub_nepal$commodity=="jpork" | mdsub_nepal$commodity=="cpork" |
                      mdsub_nepal$commodity=="jpota" | mdsub_nepal$commodity=="cpota" |
                      mdsub_nepal$commodity=="jpoul"| mdsub_nepal$commodity=="cpoul"|
                      mdsub_nepal$commodity=="jmeat" | mdsub_nepal$commodity=="cmeat"|
                      mdsub_nepal$commodity=="jwhea" | mdsub_nepal$commodity=="cwhea"|
                      mdsub_nepal$commodity=="jmeatcsa" | mdsub_nepal$commodity=="cmeatcsa")


 
mdsub_nepal$productiontype<- NULL
row.names(mdsub_nepal)<- 1: nrow(mdsub_nepal)


mdwide_nepal<- mdsub_nepal %>%
  spread ("year", "Val")


# cambio de nombre de los cultivos 
mdwide_nepal$commodity<-revalue(mdwide_nepal$commodity, c( "crice"="Rice",
                                                           "jrice"="Rice",
                                                            "cmaiz"="Maize",
                                                            "jmaiz"="Maize",
                                                            "cmilk"="Dairy production",
                                                            "jmilk"="Dairy production",
                                                            "jbeef"="Cattle",
                                                            "cbeef"="Cattle",
                                                            "cmeat"="Meat",
                                                            "jmeat"="Meat",
                                                            "jlamb"="Lamb",
                                                            "clamb"="Lamb",
                                                            "jlent"="Lent",
                                                            "clent"="Lent",
                                                            "jpork"="Pork",
                                                            "cpork"="Pork",
                                                            "jpota"="Potato",
                                                            "cpota"="Potato",
                                                            "jpoul"="Poultry",
                                                            "cpoul"="Poultry",
                                                            "jwhea"="Wheat",
                                                            "cwhea"="Wheat",
                                                           "jmeatcsa"= "Goat and Sheep",
                                                           "cmeatcsa"= "Goat and Sheep" ))

# categorias de climas
mdwide_nepal<-data.frame(mdwide_nepal,"Cat"=ifelse(mdwide_nepal$scenario=="NoCC","NoCC","CC"))
mdwide_nepal<-mdwide_nepal[,-c(5:19)]
rownames(mdwide_nepal)<- 1: nrow(mdwide_nepal)

#Copia de seguridad pais filtrado------
write.csv(mdwide_nepal,paste(copy,"mdwideNepalTotal.csv", sep = ""), row.names = FALSE)

#Net Trade y  filtros logicos-----------------
Nepnet<- mdwide_nepal[which(mdwide_nepal$impactparameter=="Net Trade"),]


## logica de los valores 
nn<-  which(Nepnet$X2050<0 & Nepnet$X2020<0) # net trade negativo  importador neto
pn<-  which(Nepnet$X2050>0 & Nepnet$X2020<0) # impacto positivo inicia como importador termina como exportador
np<-  which(Nepnet$X2050<0 & Nepnet$X2020>0) # impacto negativo inicia como exportador termina como importador
pp<-  which(Nepnet$X2050>0 & Nepnet$X2020>0) # net trade positivo  exportador neto

# No se tienen datos de tendencia en el que 

# desempeño 
export<-   c(pp)
import <-  c(nn)
tran_XtoM<- c(np) # inicia exportador termina importador
tran_MtoX<- c(pn) # inicia importador termina exportador

hard<- c(nn, pn, np, pp)

Nepnet$impacto<- NA # para poner el impacto  cambio relativo

#loops 
for(j in 1:nrow(Nepnet)){
  
  if(j %in% hard){
    Nepnet$impacto[j] <- abs(Nepnet$X2050[j] - Nepnet$X2020[j])/max(abs(Nepnet$X2050[j]), abs(Nepnet$X2020[j]), na.rm=TRUE) * 100
  } else {  }
  
}

##Copia de seguridad cambios relativos y vectores de desempeño
write.csv(Nepnet,paste(copy,"CambiosRelativosNepalTOTAL.csv", sep = ""), row.names = FALSE)

# # copia ojo con el orden
nep<- Nepnet
nep<- as.data.frame(nep)
nep$trend[tran_MtoX]<- "Transition M to X"
nep$trend[tran_XtoM]<- "Transition X to M"
nep$trend[export] <- "Positive"
nep$trend[import] <- "Negative"

#list por crops
crops<- list( "Cattle", "Lamb","Lent" , "Maize" , "Meat","Dairy production",
               "Pork", "Potato" , "Poultry" ,"Rice" , "Wheat", "Goat and Sheep" ) 

c <- list()

for (i in 1:length(crops)){
  
  if(length(which(nep$commodity==crops[[i]]))>0){
    z <- nep[which(nep$commodity==crops[[i]]),]
    x <- table(z$trend[z$Cat=='CC'])
    z <- z[,c("impactparameter","scenario", "commodity", "region", "Cat", "impacto")] %>% spread("scenario","impacto")
    if(x >= 3){
      z$trend <- names(x)
    }
    c[[i]] <- z
  } else{
    cat(paste('Commodity:', crops[i], 'does not have data\n', sep=''))
    
  }
  
}

##Unir los cultivos
c <- do.call(rbind, c)  

##Calcular la media de los impactos
c$CC_mean <- rowMeans(x=c[,5:8], na.rm=TRUE)
c$NoCC_mean <- c[,9]
c<- c[,c("impactparameter","commodity","region","trend","CC_mean","NoCC_mean")]
write.csv(c, paste(copy, "NepalNettradeTOTAL.csv", sep = ""),row.names = FALSE)




#Area  y rendimientos---------------------------------------
Nepnet2<- mdwide_nepal[which(mdwide_nepal$impactparameter=="Total Area" | 
                               mdwide_nepal$impactparameter=="Yield" |
                               mdwide_nepal$impactparameter=="Animal Yield" | 
                               mdwide_nepal$impactparameter=="Total Production"|
                               mdwide_nepal$impactparameter=="Animal Numbers"),]

Nepnet2$Percentage_Change<- ((Nepnet2$X2050-Nepnet2$X2020)/Nepnet2$X2020)*100

##Eliminamos las columnas que no necesitamos 
Nepnet2<- data.frame(Nepnet2[1:4], Nepnet2[,36:37])

##Reshape
Nepnet2_t<-Nepnet2 %>%
  spread("scenario","Percentage_Change")
##Calculo de la media
Nepnet2_t$mean <- rowMeans(x=Nepnet2_t[,5:ncol(Nepnet2_t)], na.rm=TRUE)
Nepnet2_t <- Nepnet2_t[,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
Nepnet2_t$NoCC<- NULL
#Reshape
Nepnet2_t <- Nepnet2_t %>% spread(Cat, mean)
write.csv(Nepnet2_t, paste(copy, "NepalTableSumm.csv", sep=""), row.names = FALSE)

#Solicitud datos CSA-------------
mdwide_nepalCSA<- mdwide_nepal
mdwide_nepalCSA<- mdwide_nepalCSA[,c("impactparameter", "scenario","commodity","region","X2020","X2025","X2030", "X2035","X2040","X2045", "X2050", "Cat" )]
mdwide_nepalCSA<- mdwide_nepalCSA[-which(mdwide_nepalCSA=="Net Trade"),]
rownames(mdwide_nepalCSA)<- 1:nrow(mdwide_nepalCSA)


#Listas para desarrollar el proceso
#cultivos
#periodos
k<- list()
testyear<- list()
y<- c("X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050")
v<- c("Total Area", "Yield" )

library(dplyr)
library(tidyr)
#library(Rcpp)
#library(Rcpp11)
csa <- mdwide_nepalCSA[,c("impactparameter", "commodity", paste("X", seq(from=2020, to=2050, by=5), sep = ""), "Cat")] %>% group_by(impactparameter, commodity, Cat) %>% summarise_each(funs(mean))
csa <- csa %>% gather(Year, Value, X2020:X2050)
csa <- csa %>% spread(Cat, Value)
csa$Year <- as.numeric(gsub(pattern = "X", replacement = "", x = csa$Year))
csa$percentual_change <- (csa$CC-csa$NoCC)/csa$NoCC * 100

write.csv(csa, paste(copy, "NepalCSAAreaYield.csv", sep = ""), row.names = FALSE)



library(ggplot2)

#grafico Area Cultivos
tiff(filename=paste(grd,"AreaCommoditiesNepal.tiff", sep=""), width = 10, height = 10, units = 'in', res =800)

n<- ggplot(csa[which(csa$impactparameter=="Total Area"),], aes(x=Year, y=percentual_change ))
n<- n + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
n<- n + geom_area(position = "stack", alpha = 0.4)
n<- n + ggtitle("Area ") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
n<- n + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
n<- n + coord_cartesian(ylim = c(-10, 10)) +   scale_y_continuous( breaks=seq(-10, 10, 2))
n<- n + coord_cartesian(xlim = c(2020, 2050)) +   scale_x_continuous( breaks=seq(2020,2050,5))

n
dev.off()


library(ggplot2)
#grafico numero de Aminales 
tiff(filename=paste(grd,"Numer Animales Nepal.tiff", sep=""), width = 10, height = 10, units = 'in', res =800)

n1<- ggplot(csa[which(csa$impactparameter=="Animal Numbers"),], aes(x=Year, y=percentual_change ))
n1<- n1 + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
n1<- n1 + geom_area(position = "stack", alpha = 0.4)
n1<- n1 + ggtitle("Animal Numbers") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
n1<- n1 + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
n1<- n1 + coord_cartesian(ylim = c(-0.5, 0.5)) + scale_y_continuous( breaks=seq(-0.5,0.5,0.2))
n1<- n1 + coord_cartesian(xlim = c(2020, 2050)) +   scale_x_continuous( breaks=seq(2020,2050,5))

n1
dev.off()

