##########################################Comparison between previous results 
#Autor Carlos Edo

library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(broom)
library(plyr)
library(xlsx)
library(RColorBrewer)

initial<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase1/")
updated<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase2/")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/graphs/")

#load data----------
#P1 datos iniciales
p1<- read.csv(paste(initial,"Resultados_Ciat_StCty_31_08_16_new.csv", sep = ""))

#P2 datos actuales
p2<- read.csv(paste(updated,"BID_allRegions_05_18.csv",sep = ""))


# Manejo de digitos
options(digits=3) 
options(scipen = 999)

# eliminar factor  y pasarlos caracter
p1$impactparameter<- as.character(p1$impactparameter)
p1$scenario<- as.character(p1$scenario)
p1$commodity<- as.character(p1$commodity)
p1$region<- as.character(p1$region)
p1$productiontype<- as.character(p1$productiontype)

p2$impactparameter<- as.character(p2$impactparameter)
p2$scenario<- as.character(p2$scenario)
p2$commodity<- as.character(p2$commodity)
p2$region<- as.character(p2$region)
p2$productiontype<- as.character(p2$productiontype)

# cambiar los nombres de los cultivos

p1$commodity<-  revalue(p1$commodity, c( "cmaiz"="Maize",
                                         "jmaize"="Maize",
                                         "crice"="Rice",
                                         "jrice"="Rice",
                                         "cwhea"="Wheat",
                                         "jwhea"="Wheat",
                                         "jbean"="Bean",
                                         "cbean"="Bean",
                                         "jsoyb"="Soybean",
                                         "csoyb"="Soybean"))

p2$commodity<-  revalue(p2$commodity, c( "cmaiz"="Maize",
                                         "jmaiz"="Maize",
                                         "crice"="Rice",
                                         "jrice"="Rice",
                                         "cwhea"="Wheat",
                                         "jwhea"="Wheat",
                                         "jbean"="Bean",
                                         "cbean"="Bean",
                                         "jsoyb"="Soybean",
                                         "csoyb"="Soybean"))

### filtro por cultivo
cropsFilter<- c("-", "Maize","Rice","Wheat", "Bean", "Soybean")
p1<- filter(p1, commodity=="-" | commodity=="Maize" |commodity=="Rice" |commodity=="Wheat" | commodity=="Bean" | commodity=="Soybean")%>% mutate(phase="Initial")
p2<- filter(p2, commodity=="-" | commodity=="Maize" |commodity=="Rice" |commodity=="Wheat" | commodity=="Bean" | commodity=="Soybean")%>% mutate(phase="Updated")

cfiles<-rbind(p1,p2)



### filtro por regiones

cfiles<- cfiles[grep(pattern = "LAC",x = cfiles$region, ignore.case = T),]


######################################################### Procesamiento de datos ##########################################################

#Filtros por tipos de impactparameters------------------
# datos por sistema de produccion  riego y secano
datasys<- c("YldXAgg -- Yield", "AreaXAgg -- Area" )

# 1. datos categorias totales
datatotal<- c("TYldXAgg -- Total Yield", "TAreaXAgg -- Total Area")

# 2. datos categorias agregados 
dataagg<- c("FoodAvailXAgg", "PerCapKCalCXAgg -- PcKcal by Commodity","QDXAgg -- Total Demand",
            "QEXAgg -- Export","QMXAgg -- Import")

# 3. data tratamiento especial
dataespecial<- c("QNXAgg -- Net Trade")



########################################################### Apilar en una sola base de datos #################################################
# Data.frame no tiene en cuenta los sistemas de producción
cf<- cfiles
cf$productiontype<- NULL
crops<- unique(cf$commodity)
cultivations<-  c("Maize","Rice","Wheat", "Bean", "Soybean")
cultivationsTrade<- c("Maize","Rice","Wheat", "Bean", "Soybean")

other<- c("-")
row.names(cf)<- 1: nrow(cf)

#1. Datos totales--------------------------------

k<- list()
#i=1
for(i in seq_along(datatotal)){
      
      # selecciono el cultivo
      k[[i]]<-  filter(cf,impactparameter==datatotal[i])
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      k[[i]]<- k[[i]][,c("impactparameter", "scenario","commodity","region","phase", "year", "Val")]
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>%
            spread ("year","Val")
      #creo variable para  con CC o NoCC
      k[[i]]<- data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
      
      #elimino fila innecesarias
      k[[i]]<- k[[i]][,-c(6:20)]
      
      #creo una variable= cambio porcentual 2020-2050
      k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100

      #Elimino columnas inncesarias y de paso los organizo  
      k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region","phase", "Cat","Percentage_Change")]
      #Reshape para tener cambio porcentual por GCM
      k[[i]]<- k[[i]] %>%
            spread(scenario, Percentage_Change)
      #Calculo de la media por los gcms
      k[[i]]$mean <- rowMeans(x=k[[i]][,6:ncol(k[[i]])], na.rm=TRUE)
      #Selecciono las variable necesarias 
      k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region","phase" , "Cat", "mean", "NoCC")]
      k[[i]]$NoCC<- NULL
      #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
      k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    
}

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "BID1.xlsx", sep = ""),
            sheetName = "DatosTotales", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 5:ncol(z))

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  


n<- list()
for (i in seq_along(datatotal)){
      
      png(filename=paste(copy,datatotal[i],"BID1.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
       n<-ggplot(z[which(z$impactparameter==datatotal[i]),], aes(region,commodity)) + 
            geom_tile(aes(fill = Val), colour = "white")+  facet_grid(scenarios~phase)+
            labs(x=NULL, y=NULL, title=paste("Parametro ", datatotal[i], "LAC\n Percentage difference (%) 2050-2020",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
     
       
       plot(n)
       dev.off()
      
      
     
}  

rm(z)

#2. Datos agregados--------------------------------

k<- list()

for(i in seq_along(dataagg)){
      
      if (i== 2){
            
            # selecciono el cultivo
            k[[i]]<- cf[which(cf$impactparameter==dataagg[i]),]
            #reordeno los datos
            rownames(k[[i]])<- 1: nrow(k[[i]])
            #reshape a lo ancho  
            k[[i]]<- k[[i]] %>%
                  spread ("year","Val")
            #creo variable para  con CC o NoCC
            k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
            
            #elimino fila innecesarias
            k[[i]]<-k[[i]][,-c(6:8)]
            #creo una variable= cambio porcentual 2020-2050
            k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
            
            #Elimino columnas inncesarias y de paso los organizo  
            k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region","phase", "Cat","Percentage_Change")]
            #Reshape para tener cambio porcentual por GCM
            k[[i]]<- k[[i]] %>%
                  spread(scenario, Percentage_Change)
            #Calculo de la media por los gcms
            k[[i]]$mean <- rowMeans(x=k[[i]][,6:ncol(k[[i]])], na.rm=TRUE)
            #Selecciono las variable necesarias 
            k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region","phase" , "Cat", "mean", "NoCC")]
            k[[i]]$NoCC<- NULL
            #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
            k[[i]] <- k[[i]]%>% spread(Cat, mean) 
      }  else {
            
            # selecciono el cultivo
            k[[i]]<- cf[which(cf$impactparameter==dataagg[i]),]
            #reordeno los datos
            rownames(k[[i]])<- 1: nrow(k[[i]])
            #reshape a lo ancho  
            k[[i]]<- k[[i]] %>%
                  spread ("year","Val")
            
            #creo variable para  con CC o NoCC
            k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
            
            #elimino fila innecesarias
            k[[i]]<-k[[i]][,-c(6:20)]
            #creo una variable= cambio porcentual 2020-2050
            k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
            #Elimino columnas inncesarias y de paso los organizo  
            k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region","phase", "Cat","Percentage_Change")]
            #Reshape para tener cambio porcentual por GCM
            k[[i]]<- k[[i]] %>%
                  spread(scenario, Percentage_Change)
            #Calculo de la media por los gcms
            k[[i]]$mean <- rowMeans(x=k[[i]][,6:ncol(k[[i]])], na.rm=TRUE)
            #Selecciono las variable necesarias 
            k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region","phase" , "Cat", "mean", "NoCC")]
            k[[i]]$NoCC<- NULL
            #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
            k[[i]] <- k[[i]]%>% spread(Cat, mean) 
            
      }
      return 
}

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "BID1.xlsx", sep = ""),
            sheetName = "DataAgregados", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 5:ncol(z))


n<- list()
for (i in seq_along(dataagg)){
      
      png(filename=paste(copy,dataagg[i],"BID1.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
    n<-ggplot(z[which(z$impactparameter==dataagg[i]),], aes(region,commodity)) + 
         geom_tile(aes(fill = Val), colour = "white")+  facet_grid(scenarios~phase)+
         labs(x=NULL, y=NULL, title=paste("Parametro ", dataagg[i], "LAC\n Percentage difference (%) 2050-2020",sep = "")) +
         scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
         scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
   
                     
                     
    plot(n)
    dev.off()  
}  

rm(z)

#3. Datos especiales---------------


# selecciono el cultivo
k<- filter(cf, impactparameter=="QNXAgg -- Net Trade")
#reordeno los datos
rownames(k)<- 1: nrow(k)
#reshape a lo ancho  
k<- k %>%
      spread ("year","Val")
#creo variable para  con CC o NoCC
k<-data.frame(k,"Cat"=ifelse(k$scenario=="NoCC","NoCC","CC"))
#elimino fila innecesarias
k<-k[,-c(6:20)]
#creo una variable= cambio porcentual 2020-2050
k$Percentage_Change<- ((k$X2050-k$X2020)/k$X2020)*100
#Elimino columnas inncesarias y de paso los organizo  
k<- k[, c("impactparameter","scenario", "commodity", "region","phase", "Cat","Percentage_Change")]
#Reshape para tener cambio porcentual por GCM
k<- k%>%
      spread(scenario, Percentage_Change)
#Calculo de la media por los gcms
k$mean <- rowMeans(x=k[,6:ncol(k)], na.rm=TRUE)
#Selecciono las variable necesarias 
k<- k[,c("impactparameter", "commodity", "region","phase" , "Cat", "mean", "NoCC")]
k$NoCC<- NULL
#Reshape final para obtener el cambio porcentual promedio por CC o NoCC
k <- k%>% spread(Cat, mean) 

z<- k

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "BID1.xlsx", sep = ""),
            sheetName = "DatosEspeciales", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 5:ncol(z))


png(filename=paste(copy,"NetTradeBID1.png",sep=""), 
 width = 10, height = 10, units = 'in', res = 300)

n<-ggplot(z, aes(region,commodity)) + 
   geom_tile(aes(fill = Val), colour = "white")+  facet_grid(scenarios~phase)+
   labs(x=NULL, y=NULL, title=paste("Parametro ", "Net Trade ", "LAC\n Percentage difference (%) 2050-2020",sep = "")) +
   scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
   theme(axis.text.x = element_text(angle = 90, hjust = 1))


plot(n)
dev.off() 
            
rm(z)

#5. Datos por sistemas de produccion--------
cf1<-cfiles
#sys<- c("air","arf")

k<- list()
#i=1
for(i in seq_along(datasys)){
      
      # selecciono el cultivo
      k[[i]]<- cf1[which(cf$impactparameter==datasys[i]),]
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>%
            spread ("year","Val")
      #creo variable para  con CC o NoCC
      k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
      k[[i]]<-k[[i]][,-c(7:21)]
      k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
      #Elimino columnas inncesarias y de paso los organizo  
      k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region","productiontype","phase", "Cat","Percentage_Change")]
      #Reshape para tener cambio porcentual por GCM
      k[[i]]<- k[[i]] %>%
            spread(scenario, Percentage_Change)
      #Calculo de la media por los gcms
      k[[i]]$mean <- rowMeans(x=k[[i]][,7:ncol(k[[i]])], na.rm=TRUE)
      #Selecciono las variable necesarias 
      k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "productiontype","phase", "Cat", "mean", "NoCC")]
      k[[i]]$NoCC<- NULL
      #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
      k[[i]] <- k[[i]]%>% spread(Cat, mean) 
      return 
}


z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "BID1.xlsx", sep = ""),
            sheetName = "SystemsProduction", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 6:ncol(z))

z$productiontype<- as.character(z$productiontype)
z$impactparameter<- as.character(z$impactparameter)
z$region<- as.character(z$region)
z$scenarios<- as.character(z$scenarios)
z$commodity<- as.character(z$commodity)

n<- list()
for (i in seq_along(datasys)){
      
      png(filename=paste(copy,datasys[i],"BID1.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      n<-ggplot(z[which(z$impactparameter==datasys[i]),], aes(region,commodity)) + 
            geom_tile(aes(fill = Val), colour = "white")+  facet_grid(scenarios~phase)+
            labs(x=NULL, y=NULL, title=paste("Parametro ", datasys[i], "LAC\n Percentage difference (%) 2050-2020",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + labs(x = "",y = "") + 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      plot(n)
      dev.off() 
}  


rm(z)





################################################ PARTE B Generador de tablas. #################################################

#Hacer un subconjunto que sólo contenga las variables de mi interés----------------
mdsub<-subset(cf,cf$impactparameter=="TAreaXAgg -- Total Area" |
                    cf$impactparameter== "QNXAgg -- Net Trade" | 
                    cf$impactparameter== "TYldXAgg -- Total Yield")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QNXAgg -- Net Trade"="Net Trade",
                                                        "TAreaXAgg -- Total Area"="Total Area",
                                                        "TYldXAgg -- Total Yield"= "Yield"))


mdsub<-data.frame(mdsub,"Cat"=ifelse(mdsub$scenario=="NoCC","NoCC","CC"))

row.names(mdsub)<- 1: nrow(mdsub)

mdwide_tan<- mdsub %>%
      spread ("year", "Val")

# elimino los periodos antes del 2020
mdwide_tan<- mdwide_tan[,-c(7:21)]
row.names(mdwide_tan)<- 1: nrow(mdwide_tan)


#Copia de seguridad pais filtrado------
write.csv(mdwide_tan,paste(copy,"mdwideBIDTotal.csv", sep = ""), row.names = FALSE)

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
            tznet$impacto[j] <- ((tznet$`2050`[j] - tznet$`2020`[j])/tznet$`2020`[j]) * 100
      } else {  }
      
}

##Copia de seguridad cambios relativos y vectores de desempeño
write.csv(tznet,paste(copy,"CambiosRelativosBIDTOTAL.csv", sep = ""), row.names = FALSE)

# copia
tanz<- tznet
tanz$trend<- NA
tanz$trend[import] <- "Negative"
tanz$trend[export] <- "Positive"
tanz$trend[tran_MtoX]<- "Transition from M to X"
tanz$trend[tran_XtoM]<- "Transition from X to M"

z<- tanz %>% group_by(commodity,region, phase) %>% do(.,as.data.frame(table(.$trend[.$Cat=='CC'])))
colnames(z)[4]<- "Trend"

z<- z %>% spread(phase, Freq)
z<- as.data.frame(z)
# exportar datos a excel
require(xlsx)
write.xlsx(x = z,file= paste(copy, "BID1.xlsx", sep = ""),
            sheetName = "TrendsNetTrade", col.names = TRUE, append = TRUE, showNA = FALSE)


#Area  y rendimientos---------------------------------------
tznet2<- mdwide_tan
tznet2<-  tznet2[which(tznet2$impactparameter!="Net Trade"),]
tznet2$Percentage_Change<- ((tznet2$`2050`-tznet2$`2020`)/tznet2$`2020`)*100
tznet2<- tznet2[,-c(7:37)]

##Reshape
tznet2_t<-tznet2 %>%
      spread("scenario","Percentage_Change")

##Calculo de la media
tznet2_t$mean <- rowMeans(x=tznet2_t[,6:ncol(tznet2_t)], na.rm=TRUE)
tznet2_t <- tznet2_t[,c("impactparameter", "commodity", "region","phase" ,"Cat", "mean", "NoCC")]
tznet2_t$NoCC<- NULL
#Reshape
tznet2_t <- tznet2_t %>% spread(Cat, mean)

require(xlsx)
write.xlsx(x = tznet2_t,file= paste(copy, "BID1.xlsx", sep = ""),
           sheetName = "Area&YieldTotal", col.names = TRUE, append = TRUE, showNA = FALSE)



#Solicitud datos CSA-------------
mdwide_tzaCSA<- mdwide_tan
# mdwide_tzaCSA<- mdwide_tzaCSA[,c("impactparameter", "scenario","commodity","region",2020,2025,2030,2035,2040,2045,2050, "Cat" )]
mdwide_tzaCSA<- mdwide_tzaCSA[-which(mdwide_tzaCSA=="Net Trade"),]
rownames(mdwide_tzaCSA)<- 1:nrow(mdwide_tzaCSA)


#periodos
k<- list()
testyear<- list()
y<- c(2020:2050)
v<- c("Total Area", "Yield" )



csa <- mdwide_tzaCSA[,c("impactparameter", "commodity","region", paste(seq(from=2020, to=2050, by=1)), "phase", "Cat")] %>% group_by(impactparameter, region,commodity, phase,Cat) %>% summarise_each(funs(mean))

csa <- csa %>% 
      gather(Year, Value, 6:36)
csa <- csa %>% 
      spread(Cat, Value)
csa$Year <- as.numeric(x = csa$Year)
csa$percentual_change <- (csa$CC-csa$NoCC)/csa$NoCC * 100

csa<- as.data.frame(csa)
require(xlsx)
write.xlsx(x = csa,file= paste(copy, "BID1.xlsx", sep = ""),
           sheetName = "dippArea&Yield", col.names = TRUE, append = TRUE, showNA = FALSE)


# 
# #grafico Area de cultivo
# png(filename=paste(copy,"AreaCommoditiesBID1.png", sep=""), width = 10, height = 10, units = 'in', res =800)
# 
# n<- ggplot(csa[which(csa$impactparameter=="Total Area"),], aes(x=Year, y=percentual_change,group=region,color=region ))+
#       geom_line(size=0.7) + facet_grid(phase~commodity,scales = "free")+
#       xlab('Years') + ylab('Percentual change') + coord_equal() +
#       coord_cartesian(ylim = c(-10, 10)) +  
#       coord_cartesian(xlim = c(2020, 2050)) +  
#       scale_x_continuous( breaks=seq(2020,2050,1))+
#       labs(title=paste("AREA by crops and comparison Initiall and Updated results\n All scenarios",sep = ""))
#       
# 
# 
# plot(n)
# dev.off()
# 
# 
# 
# 
# 
# c<-ggplot(data=z, aes(Val,Year,group=Scenarios,color=Scenarios)) +
#       geom_line(linetype="dashed",size=1)+ facet_grid(Commodity~., scales = "free") +
#       labs(title=paste("Net Trade by FPU= ",fpu[f], " ,RTB Crops\n All scenarios",sep = ""),y="Net trade (000 mt)",x="Year")+
#       theme(strip.text.y = element_text(angle = 0,size = 12))+
#       geom_hline(aes(yintercept=0)) 
# 
# plot(c)
# dev.off()  
# 
# 
# 
# 
# 
# 
# plots <- csa %>% group_by(region, phase) %>% do (plot = ggplot(., aes(x=Year, y=percentual_change ))+ 
#                                                      geom_line(aes(colour = commodity),  size = 0.5) + 
#                                                      facet_wrap( ~ commodity,ncol=2)+
#                                                      geom_area(position = "stack", alpha = 0.4)+
#                                                      ggtitle("Area") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))+
#                                                      xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")+
#                                                      coord_cartesian(ylim = c(-10, 10)) +   scale_y_continuous( breaks=seq(-10, 10, 2))+
#                                                      coord_cartesian(xlim = c(2020, 2050)))
#                                                         
# 
# 
# # #grafico numero de Aminales 
# # png(filename=paste(grd,"Numer Animales PK.png", sep=""), width = 10, height = 10, units = 'in', res =800)
# # 
# # n1<- ggplot(csa[which(csa$impactparameter=="Animal Numbers"),], aes(x=Year, y=percentual_change ))
# # n1<- n1 + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
# # n1<- n1 + geom_area(position = "stack", alpha = 0.4)
# # n1<- n1 + ggtitle("Animal Numbers") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
# # n1<- n1 + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
# # n1<- n1 + coord_cartesian(ylim = c(-0.5, 0.5)) + scale_y_continuous( breaks=seq(-0.5,0.5,0.2))
# # n1<- n1 + coord_cartesian(xlim = c(2020, 2050)) +   scale_x_continuous( breaks=seq(2020,2050,5))
# # 
# # n1
# # dev.off()
# # 
