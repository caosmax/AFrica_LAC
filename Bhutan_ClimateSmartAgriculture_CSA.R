# Codigos para el procesamiento y organizacion de los graficos para el CSA. Climate Smart Acgriculture

#librerias------------
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(tidyr)
library(lattice)
library(latticeExtra)
library(dplyr)

#Definir directorio de trabajo-------------
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.2/OutputFiles/Aggregation/")
#Direción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/CSA/"
#Direción copias seguridad
copy<-"C:/Users/CEGONZALEZ/Documents/CSA/copy/"

# manejo de digitos
options(digits=3) 
options(scipen = 999)

################################################ PARTE A. ####################################################################


#Cargar marco de datos principal-------------
md<-read.csv("CSA_BHU2.csv",header=T)
phi<- md

#Conocer los cultivos y agregaciones 
cult<- unique(phi$commodity)

# cambiar los nombres de los cultivos
phi$commodity<-revalue(phi$commodity, c( "jpota"="Potato",
                                         "cpota"="Potato",
                                         "cmaiz"="Maize",
                                         "jmaiz"="Maize",
                                         "cmilk"="Milk",
                                         "jmilk"="Milk",
                                         "ceggs"="Eggs",
                                         "jeggs"="Eggs",
                                         "crice"="Rice",
                                         "jrice"="Rice",
                                         "jvege"="Vegetable as group",
                                         "cvege"="Vegetable as group",
                                         "jpoul"="Poultry",
                                         "cpoul"="Poultry ",
                                         "jsubf"="Tropical fruit",
                                         "csubf"="Tropical fruit",
                                         "ctemf"="Temperate Fruit",
                                         "jtemf"="Temperate Fruit"))


# Dataframe para hacer analisis por systmas de produccion
phi_sys<- phi 


#Filtros por tipos de impactparameters------------------
# datos por sistema de produccion  riego y secano
tab<- table(phi$impactparameter, phi$productiontype!="total" & phi$productiontype!="-") #filtro
datasys<- c("YldXAgg -- Yield", "AreaXAgg -- Area" )

# 1. datos categorias totales
tab<- table(phi$impactparameter, phi$productiontype=="total")
datatotal<- c("TYldXAgg -- Total Yield", "TAreaXAgg -- Total Area")

# 2. datos categorias agregados 
tab<- table(phi$impactparameter, phi$productiontype=="-")
dataagg<- c("FoodAvailXAgg", "PerCapKCalCXAgg -- PcKcal by Commodity","QDXAgg -- Total Demand",
            "QEXAgg -- Export","QINTXAgg -- Intermediate Demand","QMXAgg -- Import")

rm(tab)

# 3. datos categorias animales
dataanimal<- c("AnmlNumXAgg -- Animal Numbers" , "AnmlYldXAgg -- Animal Yield")

# 4. data tratamiento especial
dataespecial<- c(  "QMSHXAgg -- Import Share of Demand",  "QNXAgg -- Net Trade")

# transformar  factor to character
phi$impactparameter<- as.character(phi$impactparameter)
phi$scenario<- as.character(phi$scenario)
phi$commodity<- as.character(phi$commodity)
phi$region<- as.character(phi$region)
phi$productiontype<- as.character(phi$productiontype)

# grupos de cultivos. Este se debe ajustar por pais analizado
# Data.frame no tiene en cuenta los sistemas de producción
phi$productiontype<- NULL
crops<- unique(phi$commodity)
cultivations<-  c( "Maize","Rice","Potato","Tropical fruit","Tropical fruit","Vegetable as group","Temperate Fruit" )
cultivationsTrade<- c("Poultry ","Eggs","Milk","Maize","Rice","Potato","Tropical fruit","Vegetable as group","Poultry","Temperate Fruit")

animals<- c("Eggs" , "Poultry", "Milk" )
other<- c("-")
row.names(phi)<- 1: nrow(phi)

#1. Datos totales--------------------------------

k<- list()

for(i in seq_along(datatotal)){
    
    # selecciono el cultivo
    k[[i]]<- phi[which(phi$impactparameter==datatotal[i]),]
    #reordeno los datos
    rownames(k[[i]])<- 1: nrow(k[[i]])
    #reshape a lo ancho  
    k[[i]]<- k[[i]] %>%
        spread ("year","Val")
    #creo variable para  con CC o NoCC
    k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
    #elimino fila innecesarias
    k[[i]]<-k[[i]][,-c(5:19)]
    #creo una variable= cambio porcentual 2020-2050
    k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
    #Elimino columnas inncesarias y de paso los organizo  
    k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
    #Reshape para tener cambio porcentual por GCM
    k[[i]]<- k[[i]] %>%
        spread(scenario, Percentage_Change)
    #Calculo de la media por los gcms
    k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
    #Selecciono las variable necesarias 
    k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
    k[[i]]$NoCC<- NULL
    #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
    k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    return 
}

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "BHU.xlsx", sep = ""),
            sheetName = "DatosTotales", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
    gather("scenarios", "Val" , 4:ncol(z))


n<- list()
for (i in seq_along(datatotal)){
    
    png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",datatotal[i],"BHU.png",sep=""), 
         width = 10, height = 10, units = 'in', res = 300)
    
    n[[i]]<- print(ggplot(z[which(z$impactparameter==datatotal[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                       theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(datatotal[i])+
                       theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                       ylab('Percentage Change') +  xlab("Commodity")+ scale_y_continuous(limits = c(-20, 50))
    )
    dev.off()
    print(i)
}  

rm(z)

#2. Datos agregados--------------------------------

k<- list()

for(i in seq_along(dataagg)){
    
    if (i== 2){
        
        # selecciono el cultivo
        k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
        #reordeno los datos
        rownames(k[[i]])<- 1: nrow(k[[i]])
        #reshape a lo ancho  
        k[[i]]<- k[[i]] %>%
            spread ("year","Val")
        #creo variable para  con CC o NoCC
        k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
        #elimino fila innecesarias
        k[[i]]<-k[[i]][,-c(5:7)]
        #creo una variable= cambio porcentual 2020-2050
        k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
        #Elimino columnas inncesarias y de paso los organizo  
        k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
        #Reshape para tener cambio porcentual por GCM
        k[[i]]<- k[[i]] %>%
            spread(scenario, Percentage_Change)
        #Calculo de la media por los gcms
        k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
        #Selecciono las variable necesarias 
        k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
        k[[i]]$NoCC<- NULL
        #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
        k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    }  else {
        
        # selecciono el cultivo
        k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
        #reordeno los datos
        rownames(k[[i]])<- 1: nrow(k[[i]])
        #reshape a lo ancho  
        k[[i]]<- k[[i]] %>%
            spread ("year","Val")
        #creo variable para  con CC o NoCC
        k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
        #elimino fila innecesarias
        k[[i]]<-k[[i]][,-c(5:19)]
        #creo una variable= cambio porcentual 2020-2050
        k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
        #Elimino columnas inncesarias y de paso los organizo  
        k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
        #Reshape para tener cambio porcentual por GCM
        k[[i]]<- k[[i]] %>%
            spread(scenario, Percentage_Change)
        #Calculo de la media por los gcms
        k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
        #Selecciono las variable necesarias 
        k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
        k[[i]]$NoCC<- NULL
        #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
        k[[i]] <- k[[i]]%>% spread(Cat, mean) 
        
    }
    return 
}

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "BHU.xlsx", sep = ""),
            sheetName = "DataAgregados", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
    gather("scenarios", "Val" , 4:ncol(z))


n<- list()
for (i in seq_along(dataagg)){
    
    png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataagg[i],"BHU.png",sep=""), 
         width = 10, height = 10, units = 'in', res = 300)
    
    n[[i]]<- print(ggplot(z[which(z$impactparameter==dataagg[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                       theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataagg[i])+
                       theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                       ylab('Percentage Change') +  xlab("Commodity") #+ scale_y_continuous(limits = c(-20, 50))
    )
    dev.off()
    print(i)
}  

rm(z)

#3. Datos animales-----------------


k<- list()

for(i in seq_along(dataanimal)){
    
    # selecciono el cultivo
    k[[i]]<- phi[which(phi$impactparameter==dataanimal[i]),]
    #reordeno los datos
    rownames(k[[i]])<- 1: nrow(k[[i]])
    #reshape a lo ancho  
    k[[i]]<- k[[i]] %>%
        spread ("year","Val")
    #creo variable para  con CC o NoCC
    k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
    #elimino fila innecesarias
    k[[i]]<-k[[i]][,-c(5:19)]
    #creo una variable= cambio porcentual 2020-2050
    k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
    #Elimino columnas inncesarias y de paso los organizo  
    k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
    #Reshape para tener cambio porcentual por GCM
    k[[i]]<- k[[i]] %>%
        spread(scenario, Percentage_Change)
    #Calculo de la media por los gcms
    k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
    #Selecciono las variable necesarias 
    k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
    k[[i]]$NoCC<- NULL
    #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
    k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    return 
}

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "BHU.xlsx", sep = ""),
            sheetName = "DataAnimal", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
    gather("scenarios", "Val" , 4:ncol(z))


n<- list()
for (i in seq_along(dataanimal)){
    
    png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataanimal[i],"BHU.png",sep=""), 
         width = 10, height = 10, units = 'in', res = 300)
    
    n[[i]]<- print(ggplot(z[which(z$impactparameter==dataanimal[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                       theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataanimal[i])+
                       theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                       ylab('Percentage Change') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
    )
    dev.off()
    print(i)
}  

rm(z)

#4. Datos especiales---------------

k<- list()

for(i in seq_along(dataespecial)){
    
    if(i==1){
        # selecciono el cultivo
        k[[i]]<- phi[which(phi$impactparameter==dataespecial[i]),]
        #reordeno los datos
        rownames(k[[i]])<- 1: nrow(k[[i]])
        #reshape a lo ancho  
        k[[i]]<- k[[i]] %>%
            spread ("year","Val")
        #creo variable para  con CC o NoCC
        k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
        #elimino fila innecesarias
        k[[i]]<-k[[i]][,-c(5:19)]
        #creo una variable= cambio porcentual 2020-2050
        k[[i]]$p.p<- (k[[i]]$X2050-k[[i]]$X2020)
        #Elimino columnas inncesarias y de paso los organizo  
        k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","p.p")]
        #Reshape para tener cambio porcentual por GCM
        k[[i]]<- k[[i]] %>%
            spread(scenario, p.p)
        #Calculo de la media por los gcms
        k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
        #Selecciono las variable necesarias 
        k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
        k[[i]]$NoCC<- NULL
        #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
        k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    } else {
        # selecciono el cultivo
        k[[i]]<- phi[which(phi$impactparameter==dataespecial[i]),]
        #reordeno los datos
        rownames(k[[i]])<- 1: nrow(k[[i]])
        #reshape a lo ancho  
        k[[i]]<- k[[i]] %>%
            spread ("year","Val")
        #creo variable para  con CC o NoCC
        k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
        #elimino fila innecesarias
        k[[i]]<-k[[i]][,-c(5:19)]
        #creo una variable= cambio porcentual 2020-2050
        k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
        #Elimino columnas inncesarias y de paso los organizo  
        k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
        #Reshape para tener cambio porcentual por GCM
        k[[i]]<- k[[i]] %>%
            spread(scenario, Percentage_Change)
        #Calculo de la media por los gcms
        k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
        #Selecciono las variable necesarias 
        k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
        k[[i]]$NoCC<- NULL
        #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
        k[[i]] <- k[[i]]%>% spread(Cat, mean) 
        return 
        
    }
    
}
z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "BHU.xlsx", sep = ""),
            sheetName = "DatosEspeciales", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
    gather("scenarios", "Val" , 4:ncol(z))


n<- list()
for (i in seq_along(dataespecial)){
    
    png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataespecial[i],"BHU.png",sep=""), 
         width = 10, height = 10, units = 'in', res = 300)
    
    n[[i]]<- print(ggplot(z[which(z$impactparameter==dataespecial[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                       theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataespecial[i])+
                       theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                       ylab('Impacts') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
    )
    dev.off()
    print(i)
}  

rm(z)

#5. Datos por sistemas de produccion--------

#sys<- c("air","arf")

k<- list()

for(i in seq_along(datasys)){
    
    # selecciono el cultivo
    k[[i]]<- phi_sys[which(phi_sys$impactparameter==datasys[i]),]
    #reordeno los datos
    rownames(k[[i]])<- 1: nrow(k[[i]])
    #reshape a lo ancho  
    k[[i]]<- k[[i]] %>%
        spread ("year","Val")
    #creo variable para  con CC o NoCC
    k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
    k[[i]]<-k[[i]][,-c(6:19)]
    k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
    #Elimino columnas inncesarias y de paso los organizo  
    k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region","productiontype" ,"Cat","Percentage_Change")]
    #Reshape para tener cambio porcentual por GCM
    k[[i]]<- k[[i]] %>%
        spread(scenario, Percentage_Change)
    #Calculo de la media por los gcms
    k[[i]]$mean <- rowMeans(x=k[[i]][,6:ncol(k[[i]])], na.rm=TRUE)
    #Selecciono las variable necesarias 
    k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "productiontype", "Cat", "mean", "NoCC")]
    k[[i]]$NoCC<- NULL
    #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
    k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    return 
}


z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "BHU.xlsx", sep = ""),
            sheetName = "SystemsProduction", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
    gather("scenarios", "Val" , 5:ncol(z))

z$productiontype<- as.character(z$productiontype)
z$impactparameter<- as.character(z$impactparameter)
z$region<- as.character(z$region)
z$scenarios<- as.character(z$scenarios)
z$commodity<- as.character(z$commodity)

n<- list()
for (i in seq_along(datasys)){
    
    png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",datasys[i],"BHU.png",sep=""), 
         width = 10, height = 10, units = 'in', res = 300)
    
    n[[i]]<- print(ggplot(z[which(z$impactparameter==datasys[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                       theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(datasys[i])+
                       facet_grid(productiontype~.)+ theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                       ylab('Percentage Change') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
    )
    dev.off()
    print(i)
}  



# limpiar todo---------
#g=gc; rm(list = ls())



################################################ PARTE B Generador de tablas. #################################################

#Hacer un subconjunto que sólo contenga las variables de mi interés----------------
mdsub<-subset(phi,phi$impactparameter=="TAreaXAgg -- Total Area" |
                  phi$impactparameter== "QNXAgg -- Net Trade" | 
                  phi$impactparameter== "TYldXAgg -- Total Yield"|
                  phi$impactparameter=="AnmlNumXAgg -- Animal Numbers"|
                  phi$impactparameter=="AnmlYldXAgg -- Animal Yield")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QNXAgg -- Net Trade"="Net Trade",
                                                        "TAreaXAgg -- Total Area"="Total Area",
                                                        "TYldXAgg -- Total Yield"= "Yield",
                                                        "AnmlNumXAgg -- Animal Numbers"="Animal Numbers",
                                                        "AnmlYldXAgg -- Animal Yield"="Animal Yield"))


mdsub<-data.frame(mdsub,"Cat"=ifelse(mdsub$scenario=="NoCC","NoCC","CC"))

row.names(mdsub)<- 1: nrow(mdsub)

mdwide_tan<- mdsub %>%
    spread ("year", "Val")

# elimino los periodos antes del 2020
mdwide_tan<- mdwide_tan[,-c(6:20)]
row.names(mdwide_tan)<- 1: nrow(mdwide_tan)


#Copia de seguridad pais filtrado------
write.csv(mdwide_tan,paste(copy,"mdwideBHUTotal.csv", sep = ""), row.names = FALSE)

#Net Trade y  filtros logicos-----------------
tznet<- mdwide_tan[which(mdwide_tan$impactparameter=="Net Trade"),]
row.names(tznet)<- 1:nrow(tznet)

## logica de los valores 
nn<-  which(tznet$`2050`<0 & tznet$`2020`<0) # net trade negativo  importador neto
pn<-  which(tznet$`2050`>0 & tznet$`2020`<0) # impacto positivo inicia como importador termina como exportador
np<-  which(tznet$`2050`<0 & tznet$`2020`>0) # impacto negativo inicia como exportador termina como importador
pp<-  which(tznet$`2050`>0 & tznet$`2020`>0) # net trade positivo  exportador neto


# desempeño 
export<-   c(pp)
import <-  c(nn)
tran_XtoM<- c(np) # inicia exportador termina importador
tran_MtoX<- c(pn) # inicia importador termina exportador

hard<- c(nn,pp,pn,np)

tznet$impacto<- NA # para poner el impacto  cambio relativo

#loops 
for(j in 1:nrow(tznet)){
    
    if(j %in% hard){
        tznet$impacto[j] <- abs(tznet$`2050`[j] - tznet$`2020`[j])/max(abs(tznet$`2050`[j]), abs(tznet$`2020`[j]), na.rm=TRUE) * 100
    } else {  }
    
}

##Copia de seguridad cambios relativos y vectores de desempeño
write.csv(tznet,paste(copy,"CambiosRelativosBHUTOTAL.csv", sep = ""), row.names = FALSE)

# copia
tanz<- tznet
tanz$trend<- NA
tanz$trend[import] <- "Negative"
tanz$trend[export] <- "Positive"
tanz$trend[tran_MtoX]<- "Transition from M to X"
tanz$trend[tran_XtoM]<- "Transition from X to M"
                 
c <- list()
for (i in 1:length(cultivationsTrade)){
    
    if(length(which(tanz$commodity==cultivationsTrade[[i]]))>0){
        z <- tanz[which(tanz$commodity==cultivationsTrade[[i]]),]
        x <- sort(table(z$trend[z$Cat=='CC']),decreasing = T)
        z <- z[,c("impactparameter","scenario", "commodity", "region", "Cat", "impacto")] %>% spread("scenario","impacto")
        if(x>= 3){
            z$trend <- names(x)[1]
        }
        c[[i]] <- z
    } else{
        cat(paste('Commodity:', cultivationsTrade[i], 'does not have data\n', sep=''))
        
    }
    
}



##Unir los cultivos
c <- do.call(rbind, c)  

##Calcular la media de los impactos
c$CC_mean <- rowMeans(x=c[,5:8], na.rm=TRUE)
c$NoCC_mean <- c[,9]
c<- c[,c("impactparameter","commodity","region","trend","CC_mean","NoCC_mean")]
write.csv(c, paste(copy, "NettradeBHUTOTAL.csv", sep = ""),row.names = FALSE)

#### asunto maíz

maizedf<- tanz[which(tanz$commodity==maize),]
chicha<-maizedf[-1,]
chicha <- chicha[,c("impactparameter","scenario", "commodity", "region", "Cat", "impacto", "trend")] %>% spread("scenario","impacto")
chicha$CC_mean <- rowMeans(x=chicha[,6:8], na.rm=TRUE)
chicha$NoCC_mean <- chicha[,9]
chicha<- chicha[,c("impactparameter","commodity","region","trend","CC_mean","NoCC_mean")]
write.csv(chicha, paste(copy, "NettradeMaizBHUTOTAL.csv", sep = ""),row.names = FALSE)


#Area  y rendimientos---------------------------------------
tznet2<- mdwide_tan
tznet2<-  tznet2[which(tznet2$impactparameter!="Net Trade"),]
tznet2$Percentage_Change<- ((tznet2$`2050`-tznet2$`2020`)/tznet2$`2020`)*100
tznet2<- tznet2[,-c(6:36)]

##Reshape
tznet2_t<-tznet2 %>%
    spread("scenario","Percentage_Change")

##Calculo de la media
tznet2_t$mean <- rowMeans(x=tznet2_t[,5:ncol(tznet2_t)], na.rm=TRUE)
tznet2_t <- tznet2_t[,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
tznet2_t$NoCC<- NULL
#Reshape
tznet2_t <- tznet2_t %>% spread(Cat, mean)

write.csv(x =tznet2_t, file =paste(copy,"areayieldBHU.csv",sep = ""))


## Asunto maiz


#Solicitud datos CSA-------------
mdwide_tzaCSA<- mdwide_tan
mdwide_tzaCSA<- mdwide_tzaCSA[,c("impactparameter", "scenario","commodity","region",2020,2025,2030,2035,2040,2045,2050, "Cat" )]
mdwide_tzaCSA<- mdwide_tzaCSA[-which(mdwide_tzaCSA=="Net Trade"),]
rownames(mdwide_tzaCSA)<- 1:nrow(mdwide_tzaCSA)


#Listas para desarrollar el proceso
cultivations

#periodos
k<- list()
testyear<- list()
y<- c(2020,2025,2030,2035,2040,2045,2050)
v<- c("Total Area", "Yield" )

library(dplyr)
library(tidyr)

csa <- mdwide_tzaCSA[,c("impactparameter", "commodity", paste(seq(from=2020, to=2050, by=5)), "Cat")] %>% group_by(impactparameter, commodity, Cat) %>% summarise_each(funs(mean))
csa <- csa %>% 
    gather(Year, Value, 4:10)
csa <- csa %>% 
    spread(Cat, Value)
csa$Year <- as.numeric(x = csa$Year)
csa$percentual_change <- (csa$CC-csa$NoCC)/csa$NoCC * 100

write.csv(csa, paste(copy, "BHUCSAAreaYield.csv", sep = ""), row.names = FALSE)


#grafico Area de cultivo
png(filename=paste(grd,"AreaCommoditiesBHU.png", sep=""), width = 10, height = 10, units = 'in', res =800)

n<- ggplot(csa[which(csa$impactparameter=="Total Area"),], aes(x=Year, y=percentual_change ))
n<- n + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
n<- n + geom_area(position = "stack", alpha = 0.4)
n<- n + ggtitle("Area") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
n<- n + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
n<- n + coord_cartesian(ylim = c(-10, 10)) +   scale_y_continuous( breaks=seq(-10, 10, 2))
n<- n + coord_cartesian(xlim = c(2020, 2050)) +  scale_x_continuous( breaks=seq(2020,2050,5))

#ggsave(file=paste(grd,"AreaCommodities.png", sep=""), n, width=10, height=10.5, units='in') #saves g

n
dev.off()



#grafico numero de Aminales 
png(filename=paste(grd,"Numer Animales BHU.png", sep=""), width = 10, height = 10, units = 'in', res =800)

n1<- ggplot(csa[which(csa$impactparameter=="Animal Numbers"),], aes(x=Year, y=percentual_change ))
n1<- n1 + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
n1<- n1 + geom_area(position = "stack", alpha = 0.4)
n1<- n1 + ggtitle("Animal Numbers") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
n1<- n1 + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
n1<- n1 + coord_cartesian(ylim = c(-0.5, 0.5)) + scale_y_continuous( breaks=seq(-0.5,0.5,0.2))
n1<- n1 + coord_cartesian(xlim = c(2020, 2050)) +   scale_x_continuous( breaks=seq(2020,2050,5))

n1
dev.off()


