##########################################################################################################
#=============================================Datos y graficas para Reporte===============================#


#Cargar librerias-----
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(xtable)
library(dplyr)
library(tidyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
#Limitar numero de decimales-----
options(digits=3) 
options(scipen = 999)
#Definir directorios de trabajo y de graficos------
#Directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/")
#Directorio de graficos
grd<-"C:/Users/CEGONZALEZ/Documents/GFSF/BeanGFSF/graph/"
#Directorio para generar archivos en CSV
copy<- "C:/Users/CEGONZALEZ/Documents/GFSF/BeanGFSF/copyData/"
#Directorio dapadfs
dapadfs<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BeansData/Data&Graphs/"
#Cargar lista de nombres de los archivos----
nombres <- list.files(pattern ="*.csv" )
#Crear muna lista con los nombre de los marco de datos
nombres_md_list<-strsplit(nombres,".csv")
#nombres_md<-c(do.call("cbind",nombres_md_list))  
categoria<-gsub(pattern="Bean_",replacement="",x=nombres_md_list)

#Crear una lista vacia
pl<-NULL
#Cargar los datos dentro de la lista-------

for (i in 1: length(nombres_md_list)) {
  pl[[i]]<-eval(parse(text=paste(nombres_md_list[[i]],'<-read.csv(file=\'',nombres[i],'\',header=T)',sep='')))
  pl[[i]]<-eval(parse(text=paste(nombres_md_list[[i]],'<-data.frame(',nombres_md_list[i],',','"categoria"','=',
                                 "categoria[i]",')',sep='')))
  print(i)
}

#Juntar los elementos de la lista en un marco de datos
md<-do.call(rbind, pl)
#Hacer un subconjunto que sólo contenga las variables de mi interés-----
mdsub_ben<-subset(md,md$impactparameter== "PerCapKCalCXAgg -- PcKcal by Commodity"|
                    md$impactparameter== "FoodAvailXAgg"|
                    md$impactparameter== "ShareAtRiskXagg -- Share at Risk of Hunger" |
                    md$impactparameter== "QMXAgg -- Import"|
                    md$impactparameter== "QEXAgg -- Export"|
                    md$impactparameter== "TotalMalnourishedXagg -- Malnurished Children"|
                    md$impactparameter=="QSXAgg -- Total Production" | 
                    md$impactparameter== "TAreaXAgg -- Total Area" |
                    md$impactparameter== "TYldXAgg -- Total Yield" | 
                    md$impactparameter== "QDXAgg -- Total Demand")

mdsub_ben$impactparameter<-revalue(mdsub_ben$impactparameter, c("QEXAgg -- Export"="Export",
                                                                 "QMXAgg -- Import"="Import",
                                                                 "PerCapKCalCXAgg -- PcKcal by Commodity"= "PerCapKCalCXAgg",
                                                                 "FoodAvailXAgg"="FoodAva", 
                                                                 "ShareAtRiskXagg -- Share at Risk of Hunger"= "ShareAtRiskXagg",
                                                                 "TotalMalnourishedXagg -- Malnurished Children"="TotalMalnourishedXagg",
                                                                 "QSXAgg -- Total Production"="Production",  
                                                                 "TAreaXAgg -- Total Area"="Area",
                                                                 "TYldXAgg -- Total Yield"="Yield",
                                                                 "QDXAgg -- Total Demand"="Demand"))

rownames(mdsub_ben) <- 1:nrow(mdsub_ben)

#Save as R Field 
saveRDS(mdsub_ben, file = paste(dapadfs,"BeanDataBEN.rds", sep = ""))

#Cambiamos variables de factor a Caracter
mdsub_ben$impactparameter <- as.character(mdsub_ben$impactparameter)
mdsub_ben$scenario <- as.character(mdsub_ben$scenario)
mdsub_ben$commodity <- as.character(mdsub_ben$commodity)
mdsub_ben$region <- as.character(mdsub_ben$region)
mdsub_ben$productiontype <- as.character(mdsub_ben$productiontype)
mdsub_ben$categoria <- as.character(mdsub_ben$categoria)

#Filtramos solo para Frijol
mdsubcropben <- subset(mdsub_ben,mdsub_ben$commodity=="PUL-Beans" | mdsub_ben$commodity=="-")
mdsubcropben$commodity <- revalue(mdsubcropben$commodity,c("PUL-Beans"="Beans"))


#Filtros paises
L <- mdsubcropben[grep(pattern = '^LAC', x = mdsubcropben$region, value = F, fixed = FALSE), ]
L$zona<- 'LAC'
A <- mdsubcropben[grep(pattern = "^SSA", x = mdsubcropben$region, value = F, fixed = FALSE), ]
A$zona<- 'AFRICA'

#Paises que quedaron por fuera del analisis
M<- mdsubcropben[which(mdsubcropben$region=="MEN-Algeria"|mdsubcropben$region=="MEN-Egypt"| mdsubcropben$region=="MEN-Morocco"|
                         mdsubcropben$region=="MEN-Libya"|mdsubcropben$region=="MEN-Mauritania"|mdsubcropben$region=="MEN-Tunisia"  ),]
M$zona<- "AFRICA"

#Apilar
dataBen <- rbind(L,A,M) # todos los datos 
rownames(dataBen) <- 1:nrow(dataBen)

#Reshape a lo ancho
mdwideFood<-dataBen %>%
  spread("year", "Val")
#Eliminar casos con datos faltantes por variable para los periodos 2020 & 2050---------------------
constraintsF <- as.data.frame(table(mdwideFood$region[is.na(mdwideFood$`2050`)& 
                                                        is.na(mdwideFood$`2020`)& mdwideFood$categoria == 'CC_Tec'], mdwideFood$impactparameter[is.na(mdwideFood$`2050`)& is.na(mdwideFood$`2020`) & mdwideFood$categoria == 'CC_Tec']))
names(constraintsF) <- c('Region', 'impactparameter', 'Count')
constraintsF <- constraintsF[constraintsF$Count >= 3,]; rownames(constraintsF) <- 1:nrow(constraintsF)

for(i in 1:nrow(constraintsF)){
  mdwideFood <- mdwideFood[-which(mdwideFood$impactparameter == constraintsF$impactparameter[i] & mdwideFood$region == constraintsF$Region[i]),]
}; rm(i, constraintsF)
#Eliminar casos con datos faltantes por variable para el periodo 2050----------------
constraintsF <- as.data.frame(table(mdwideFood$region[is.na(mdwideFood$`2050`)& mdwideFood$categoria == 'CC_Tec'], mdwideFood$impactparameter[is.na(mdwideFood$`2050`) & mdwideFood$categoria == 'CC_Tec']))
names(constraintsF) <- c('Region', 'impactparameter', 'Count')
constraintsF <- constraintsF[constraintsF$Count >= 3,]; rownames(constraintsF) <- 1:nrow(constraintsF)

for(i in 1:nrow(constraintsF)){
  mdwideFood <- mdwideFood[-which(mdwideFood$impactparameter == constraintsF$impactparameter[i] & mdwideFood$region == constraintsF$Region[i]),]
}; rm(i, constraintsF)
#Eliminar casos con datos faltantes por variable para los periodos consecutivos desde 2020 hasta 2040--------------
constraintsF <- as.data.frame(table(mdwideFood$region[is.na(mdwideFood$`2020`)&
                                                        is.na(mdwideFood$`2030`) &
                                                        is.na(mdwideFood$`2040`) &
                                                        mdwideFood$categoria == 'CC_Tec'],
                                    mdwideFood$impactparameter[is.na(mdwideFood$`2020`)&
                                                                 is.na(mdwideFood$`2030`) &
                                                                 is.na(mdwideFood$`2040`) &
                                                                 mdwideFood$categoria == 'CC_Tec']))
names(constraintsF) <- c('Region', 'impactparameter', 'Count')
constraintsF <- constraintsF[constraintsF$Count > 0,]; rownames(constraintsF) <- 1:nrow(constraintsF)

# Combinar con el conteo de GCM's por combinación variable vs país
countCGM <- as.data.frame(table(mdwideFood$region[mdwideFood$categoria == 'CC_Tec'], mdwideFood$impactparameter[mdwideFood$categoria == 'CC_Tec']))
names(countCGM) <- c('Region', 'impactparameter', 'GCM_count')
countCGM <- countCGM[countCGM$GCM_count > 0,]; rownames(countCGM) <- 1:nrow(countCGM)

constraintsF <- left_join(x = constraintsF, y = countCGM, by=c('Region', 'impactparameter')); rm(countCGM)
constraintsF <- constraintsF[constraintsF$Count >= constraintsF$GCM_count/2,]; rownames(constraintsF) <- 1:nrow(constraintsF)

for(i in 1:nrow(constraintsF)){
  mdwideFood <- mdwideFood[-which(mdwideFood$impactparameter == constraintsF$impactparameter[i] & mdwideFood$region == constraintsF$Region[i]),]
}; rm(i, constraintsF)


combinations <- unique(mdwideFood[,c('impactparameter', 'region')])
data_list <- list()
for(i in 1:nrow(combinations)) {
  
  sub_data <- mdwideFood[mdwideFood$region == combinations$region[i] & mdwideFood$impactparameter == combinations$impactparameter[i],]
  condition <- sum(is.na(sub_data$`2020`))
  sub_data$Case <- NA
  
  if(condition >= nrow(sub_data)/2){
    sub_data$`Cambio Porcentual` <- ((sub_data$`2050`-sub_data$`2030`)/sub_data$`2030`) * 100
    sub_data$Case <- 1
  } else {
    sub_data$`Cambio Porcentual` <- ((sub_data$`2050`-sub_data$`2020`)/sub_data$`2020`) * 100
    sub_data$Case <- 0
  }
  
  data_list[[i]] <- sub_data
  
}; rm(i, sub_data)
data_list <- do.call(rbind, data_list)

mdwideFood <- data_list; rm(data_list)
#Copia de los datos para grafico de produccion
q<- mdwideFood
mdwideFood$Case <- NULL

#Copia de datos para ser usados en grafico de produccion
saveRDS(mdwideFood,paste(copy,"FoodBenCopyData.rds", sep = "")) 

# Eliminar datos agrupados innecesarios
mdwideFood<-mdwideFood[!mdwideFood$region=="LAC",]  
mdwideFood<-mdwideFood[!mdwideFood$region=="MEN",]
mdwideFood<-mdwideFood[!mdwideFood$region=="SSA",]

# Eliminar datos que no adoptaron tecnología 
mdwideFood<-mdwideFood[!mdwideFood$region=="LAC-Cuba",]
mdwideFood<-mdwideFood[!mdwideFood$region=="LAC-Jamaica",]
mdwideFood<-mdwideFood[!mdwideFood$region=="MEN-Egypt",]
mdwideFood<-mdwideFood[!mdwideFood$region=="LAC-Other Caribbean",]
mdwideFood<-mdwideFood[!mdwideFood$region=="LAC-Haiti",]
mdwideFood<-mdwideFood[!mdwideFood$region=="LAC-Dominican Republic",]

mdwideFood<-data.frame(mdwideFood[,1:7],"Cambio Porcentual"=mdwideFood[,14]) # eliminamos las columnas innecesarias 

#Importante para renombrar las columnas
rownames(mdwideFood) <- 1:nrow(mdwideFood)
#Eliminamos no Cambio Climatico-------
testHaroldf <- mdwideFood
testHaroldf <- testHaroldf[-which(testHaroldf$categoria=='NoCC_NoTec' | 
                                    testHaroldf$categoria=='NoCC_Tec'),]
#Reshape para obtener datos por GCMs y calculo de las medias por filas--------------
mdwide_f <- testHaroldf %>%
  spread("scenario","Cambio.Porcentual")

mdwide_f$productiontype <- NULL 
mdwide_f$CC_mean <- rowMeans(x=mdwide_f[,6:ncol(mdwide_f)], na.rm=TRUE)

#para mantener estas variables
mdwide_f <- mdwide_f[,c("impactparameter", "commodity", "region","zona", "categoria", "CC_mean")]

#reshape
mdwide_f <- mdwide_f %>% 
  spread(categoria, CC_mean)

#copia para hacer graficos de densidades de rendimientos 
sinFood<- mdwide_f
write.csv(sinFood, paste(copy, "SumFoodInformesfood.csv", sep = ""))
#Logica en las variaciones  cambios y  diferencias relativas-----
nn <- which(mdwide_f$CC_No<0 & mdwide_f$CC_Tec<0) 
pp <- which(mdwide_f$CC_No>0 & mdwide_f$CC_Tec>0)
np <- which(mdwide_f$CC_No>0 & mdwide_f$CC_Tec<0)

nn <- intersect(nn, which(mdwide_f$CC_No > mdwide_f$CC_Tec))
pp <- intersect(pp, which(mdwide_f$CC_No > mdwide_f$CC_Tec))
np <- intersect(np, which(mdwide_f$CC_No > mdwide_f$CC_Tec))

noimpact <- c(nn, pp, np)
mdwide_f$impacto<-NA

#loops ajuste inclusion de los paises sin impacto
for(j in 1:nrow(mdwide_f)){
  
  if(j %in% noimpact){
    mdwide_f$impacto[j] <- (mdwide_f$CC_Tec[j] - mdwide_f$CC_NoTec[j])
  } else {
    mdwide_f$impacto[j] <- abs(mdwide_f$CC_Tec[j] - mdwide_f$CC_NoTec[j])
    
  }
  
}

#### grafico teniendo en cuenta grupos
#a) recuperación,  b) recuperación más mejoramiento  y c) mejoramiento
pn <- which(mdwide_f$CC_No<0 & mdwide_f$CC_Tec>0)
np <- which(mdwide_f$CC_No>0 & mdwide_f$CC_Tec<0)

nn <- which(mdwide_f$CC_No<0 & mdwide_f$CC_Tec<0) 
pp <- which(mdwide_f$CC_No>0 & mdwide_f$CC_Tec>0)

alivio <-intersect(nn,which(mdwide_f$CC_No < mdwide_f$CC_Tec))
mejora <-intersect(pp, which(mdwide_f$CC_NoTec < mdwide_f$CC_Tec))
ali_mejora <- intersect(pn,which(mdwide_f$CC_No < mdwide_f$CC_Tec))

# effect <- c(alivio, mejora, ali_mejora); rm(alivio, mejora, ali_mejora)

mdwide_f$trend<-NA

# vectores de posicion 
mdwide_f$trend[alivio] <- 'Loss -> Alliviated loss'
mdwide_f$trend[mejora] <- 'Gain -> Enhanced gain'
mdwide_f$trend[ali_mejora] <- 'Loss -> Gain'

#loops ajuste inclusion de los paises Alivio
for(i in 1:nrow(mdwide_f)){
  
  if(i %in% alivio){
    mdwide_f$trend[i] <- "Loss -> Alliviated loss"
    
  }
}

#loops ajuste inclusion de los paises mejora
for(i in 1:nrow(mdwide_f)){
  
  if(i %in% mejora){
    mdwide_f$trend[i] <- "Gain -> Enhanced gain"
  }
  
}

#loops ajuste inclusion de los paises alivio + mejora
for(i in 1:nrow(mdwide_f)){
  
  if(i %in% ali_mejora){
    mdwide_f$trend[i] <- "Loss -> Gain"
  }
  
}

# guardar base de datos correcta
write.csv(mdwide_f,paste(copy,"PPdatamixFood.csv", sep = ""), row.names = FALSE)
#Base de datos para graficos ---------------
mdwide_f <- mdwide_f %>% gather( Intervencion, Value, CC_NoTec:impacto)# para hacer el cluster
mdwideBean_f<-mdwide_f
#Grafico de densidades Cambio climatico y  Tecnología -------------

rendicc<- sinFood
rendicc<- rendicc %>% gather (categoria, CC_mean, 5:6)
dh<- ggplot(rendicc[which(rendicc$impactparameter=="Yield"),], aes(x=CC_mean, colour=categoria)) + geom_density(alpha=.3)
dh<- dh + scale_colour_discrete(name="Scenarios")
dh<- dh + xlab("% Change") 
dh<- dh + ggtitle("Percentage change in yield under climate change (2020-2050)\nwith and without technology") 
dh<- dh + theme(plot.title = element_text(lineheight=.8, face="bold")) 
dh
ggsave(file=paste(dapadfs,"HistograTechnYieldCC.png",sep = ""), dh, width=10, height=10.5, units='in') 
#Graficos verdes------------------

# Yield plot
gg <- ggplot(data=mdwideBean_f[mdwideBean_f$Intervencion!='impacto' & 
                                 mdwideBean_f$impactparameter=='Yield',], aes(y=Intervencion, x=region, fill=Value)) 
gg <- gg + geom_tile(color="white", size=0.1) 
gg <- gg + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") 
gg <- gg + coord_equal()
gg <- gg + ggtitle('Yield') + ylab('') + xlab('Countries')
gg <- gg + theme_bw()
gg <- gg + theme(axis.text.x=element_text(size=10, angle=90))
gg <- gg + theme(axis.text.y=element_text(size=10))
gg <- gg + theme(axis.title.x=element_text(size=12, face='bold'))
gg <- gg + theme(plot.title=element_text(size=15, face = 'bold'))
gg <- gg + theme(legend.text = element_text(size=10))
gg <- gg + theme(legend.title = element_text(size=10, face = 'bold'))
gg

# Area plot
gg1 <- ggplot(data=mdwideBean_f[mdwideBean_f$Intervencion!='impacto' & 
                                  mdwideBean_f$impactparameter=='Area',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg1 <- gg1 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg1 <- gg1 + coord_equal()
gg1 <- gg1 + ggtitle('Area') + ylab('') + xlab('Countries')
gg1 <- gg1 + theme_bw()
gg1 <- gg1 + theme(axis.text.x=element_text(size=10, angle=90))
gg1 <- gg1 + theme(axis.text.y=element_text(size=10))
gg1 <- gg1 + theme(axis.title.x=element_text(size=12, face='bold'))
gg1 <- gg1 + theme(plot.title=element_text(size=15, face = 'bold'))
gg1 <- gg1 + theme(legend.text = element_text(size=10))
gg1 <- gg1 + theme(legend.title = element_text(size=10, face = 'bold'))
gg1

# Production plot
gg2 <- ggplot(data=mdwideBean_f[mdwideBean_f$Intervencion!='impacto' & 
                                  mdwideBean_f$impactparameter=='Production',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg2 <- gg2 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg2 <- gg2 + coord_equal()
gg2 <- gg2 + ggtitle('Production') + ylab('') + xlab('Countries')
gg2 <- gg2 + theme_bw()
gg2 <- gg2 + theme(axis.text.x=element_text(size=10, angle=90))
gg2 <- gg2 + theme(axis.text.y=element_text(size=10))
gg2 <- gg2 + theme(axis.title.x=element_text(size=12, face='bold'))
gg2 <- gg2 + theme(plot.title=element_text(size=15, face = 'bold'))
gg2 <- gg2 + theme(legend.text = element_text(size=10))
gg2 <- gg2 + theme(legend.title = element_text(size=10, face = 'bold'))
gg2

grid.arrange(gg, gg1, gg2)

g <- arrangeGrob(gg, gg1, gg2, nrow=3) #generates g
ggsave(file=paste(dapadfs,"graphAreaYieldProductionBen.png", sep=''),g, width=10, height=10.5, units='in') 
#Graficos generales impactos como puntos porcentuales----- 
impactoDF <- subset(mdwideBean_f, subset=mdwideBean_f$Intervencion=='impacto')

impactList <- as.character(unique(impactoDF$impactparameter))
ggList <- lapply(1:length(impactList), function(i){
  
  df <- subset(impactoDF, impactoDF$impactparameter==impactList[i])
  df$region <- as.character(df$region)
  df$trend <- as.character(df$trend)
  df <- df[complete.cases(df),]  
  df$region <- factor(df$region, levels=df$region[order(df$Value)])  
  
  gg <- ggplot(df, aes(x=region, y=Value, fill=zona)) + theme_bw() + geom_bar(stat="identity") + coord_flip()
  gg <- gg + ggtitle(impactList[i]) + xlab('Countries') + ylab('Percentage Points')
  gg <- gg + theme_bw()
  
  if(i==2){
    
    gg <- gg + theme(axis.text.y=element_text(size=12))
    
  } else {
    
    gg <- gg + theme(axis.text.y=element_text(size=1))
    
  }
  
  gg <- gg + theme(axis.text.y=element_text(size=12))
  gg <- gg + theme(axis.title.x=element_text(size=12, face='bold'))
  gg <- gg + theme(plot.title=element_text(size=15, face = 'bold'))
  # gg <- gg + scale_colour_gradient(name='Relative differences', low="cadetblue1", high="dodgerblue3")
  # gg <- gg + scale_fill_gradient(name='Relative differences', low="cadetblue1", high="dodgerblue3")
  
  return(gg)
  
})

grid.arrange(ggList[[1]], ggList[[2]], ggList[[3]], ggList[[4]], ggList[[5]], ggList[[6]])

g <- arrangeGrob(ggList[[6]], ggList[[4]], ggList[[5]], nrow=3) 
h <- arrangeGrob(ggList[[1]], ggList[[2]], ggList[[3]], nrow=3) 
# ggsave(file=paste(dapadfs,"impacto_oferta.png", sep=''), g, width=10, height=10.5, units='in') 
# ggsave(file=paste(dapadfs,"impacto_comercio_demanda.png", sep=''), h, width=10, height=10.5, units='in') 
# ggsave(file=paste(dapadfs,"impacto_comercio_demanda.png", sep=''), h, width=10, height=10.5, units='in') 
ggsave(file=paste(dapadfs,"AREA.png",sep=''), ggList[[1]], width=10, height=10.5, units='in') 
ggsave(file=paste(dapadfs,"DEMAND.png",sep=''), ggList[[2]], width=10, height=10.5, units='in') 
ggsave(file=paste(dapadfs,"EXPORT.png",sep=''), ggList[[3]], width=10, height=10.5, units='in') 
ggsave(file=paste(dapadfs,"FOODAVA.png",sep=''), ggList[[4]], width=10, height=10.5, units='in') 
ggsave(file=paste(dapadfs,"IMPORT.png",sep=''), ggList[[5]], width=10, height=10.5, units='in') 
ggsave(file=paste(dapadfs,"PERCAPK.png",sep=''), ggList[[6]], width=10, height=10.5, units='in') 
#Graficos generales impactos como puntos porcentuales marca tendencia ----- 
impactoDF <- subset(mdwideBean_f, subset=mdwideBean_f$Intervencion=='impacto')

impactList <- as.character(unique(impactoDF$impactparameter))
ggListYield <- lapply(1:length(impactList), function(i){
  
  df <- subset(impactoDF, impactoDF$impactparameter==impactList[i])
  df$region <- as.character(df$region)
  df$trend <- as.character(df$trend)
  df <- df[complete.cases(df),]  
  df$region <- factor(df$region, levels=df$region[order(df$Value)])  
  
  gg <- ggplot(df, aes(x=region, y=Value, fill=trend)) + theme_bw() + geom_bar(stat="identity") + coord_flip()
  gg <- gg + ggtitle(impactList[i]) + xlab('Countries') + ylab('Percentage Points')
  gg <- gg + theme_bw()
  
  if(i==2){
    
    gg <- gg + theme(axis.text.y=element_text(size=12))
    
  } else {
    
    gg <- gg + theme(axis.text.y=element_text(size=1))
    
  }
  
  gg <- gg + theme(axis.text.y=element_text(size=12))
  gg <- gg + theme(axis.title.x=element_text(size=12, face='bold'))
  gg <- gg + theme(plot.title=element_text(size=15, face = 'bold'))
  # gg <- gg + scale_colour_gradient(name='Relative differences', low="cadetblue1", high="dodgerblue3")
  # gg <- gg + scale_fill_gradient(name='Relative differences', low="cadetblue1", high="dodgerblue3")
  
  return(gg)
  
})

grid.arrange(ggListYield[[10]])
#Grafico de Net Trade de Ben----

nettrade<- read.csv(paste(dapadfs,"impacts_aggreg_bes.csv", sep = ""))
nettrade_filtro<- nettrade[,c("country","zona","impact_trade")]
nettrade_filtro$country<- as.character(nettrade_filtro$country)
nettrade_filtro<-  nettrade_filtro[!nettrade_filtro$country=="MEN-Iran",]
nettrade_filtro<-  nettrade_filtro[!nettrade_filtro$country=="MEN-Iraq",]
nettrade_filtro<-  nettrade_filtro[!nettrade_filtro$country=="MEN-Lebanon",]
nettrade_filtro<-  nettrade_filtro[!nettrade_filtro$country=="MEN-Libya",]
nettrade_filtro<-  nettrade_filtro[!nettrade_filtro$country=="MEN-Yemen",]
nettrade_filtro<-  nettrade_filtro[!nettrade_filtro$country=="MEN-Iraq",]
rownames(nettrade_filtro) <- 1:nrow(nettrade_filtro)

names(nettrade_filtro)[3]<-paste("Impact")
names(nettrade_filtro)[2]<-paste("Region")


# densidades de net trade
dhb<- ggplot(nettrade_filtro, aes(x=Impact, colour=Region)) + geom_density(alpha=.3)
dhb<- dhb + xlab("'000 tonnes") + theme_get()
dhb<- dhb + ggtitle("Impact on Net-trade (2020-2050)") 
dhb<- dhb + theme(axis.text.y=element_text(size=12))
dhb<- dhb + theme(axis.text.y=element_text(size=12))
dhb<- dhb + theme(axis.title.x=element_text(size=15, face='bold'))
dhb<- dhb + theme(plot.title = element_text(lineheight=0.8, face="bold")) 
dhb
ggsave(file=paste(dapadfs,"NettradeDensity.png",sep = ""), dhb, width=10, height=10.5, units='in')                                                                                       
#Grafico de impactos Yield & Production-------------
q # la base de datos base es 

q<- q[!q$region=="LAC",]
q<- q[!q$region=="SSA",]
q<- q[!q$region=="MEN",]
q<-q[!q$region=="LAC-Cuba",]
q<-q[!q$region=="LAC-Jamaica",]
q<-q[!q$region=="MEN-Egypt",]
q<-q[!q$region=="LAC-Other Caribbean",]
q<-q[!q$region=="LAC-Haiti",]
q<-q[!q$region=="LAC-Dominican Republic",]

q$q_mean <- rowMeans(x=q[,10:ncol(q)], na.rm=TRUE)
only_q<-q[which(q$impactparameter=="Production"),]
only_q <- only_q[only_q$categoria!='NoCC_NoTec',]
only_q <- only_q[only_q$categoria!='NoCC_Tec',]

only_q$commodity<-NULL
only_q$productiontype<-NULL
only_q$zona<- NULL
only_q <- only_q[,c("impactparameter",  "region", "categoria", "q_mean")]
row.names(only_q)<- 1:nrow(only_q)

# Yield subset
yieldDF <- impactoDF[impactoDF$impactparameter=='Yield',]

# produccion sub set
only_qTotal <- only_q %>% group_by(region,categoria) %>% summarise(promedio=mean(q_mean))
only_qTotal <- as.data.frame(only_qTotal)

# Graficos  de rendiento  y produccion ordenados por los impactos de rendimientos.
yieldDF$region <- factor(yieldDF$region, levels=yieldDF$region[order(yieldDF$Value)])
only_qTotal$region <- factor(x=only_qTotal$region, levels=levels(yieldDF$region))
gg_prod <- ggplot(only_qTotal, aes(x=region, y=promedio, fill=categoria)) + theme_bw() + geom_bar(stat="identity", position = "dodge") + coord_flip()
gg_prod <- gg_prod + ggtitle('') +  ylab('Production average (ton), 2020-2050') + xlab(' ') + scale_fill_discrete("Categories")
gg_prod <- gg_prod + theme_bw() + ggtitle('Production')
gg_prod <- gg_prod + theme(axis.text.y=element_text(size=1))
gg_prod <- gg_prod + theme(axis.text.y=element_text(size=12))
gg_prod <- gg_prod + theme(axis.title.x=element_text(size=12, face='bold'))
gg_prod <- gg_prod + theme(plot.title=element_text(size=15, face = 'bold'))
gg_prod
ggListYield[[10]]
h_prod <- arrangeGrob(ggListYield[[10]], gg_prod, nrow=1) #generates g
ggsave(file=paste(grd,"production_yield.png", sep = ""), h_prod, width=10, height=10.5, units='in') #saves g



# grafico solo produccion con tecnologia 
only_qTotalTech <- only_qTotal[only_qTotal$categoria!="CC_NoTec",]
only_qTotalTech$categoria <- as.character(only_qTotalTech$categoria)
yieldDF$region <- factor(yieldDF$region, levels=yieldDF$region[order(yieldDF$Value)])
only_qTotalTech$region <- factor(x=only_qTotalTech$region, levels=levels(yieldDF$region))
gg_prod2 <- ggplot(only_qTotalTech, aes(x=region, y=promedio, fill=categoria)) + theme_bw() + geom_bar(stat="identity", position = "dodge") + coord_flip()
gg_prod2 <- gg_prod2 + ggtitle('') +  ylab('Production average (ton), 2020-2050') + xlab(' ') + scale_fill_discrete("Categories")
gg_prod2 <- gg_prod2 + theme_bw() + ggtitle('Production')
gg_prod2 <- gg_prod2 + theme(axis.text.y=element_text(size=1))
gg_prod2 <- gg_prod2 + theme(axis.text.y=element_text(size=12))
gg_prod2 <- gg_prod2 + theme(axis.title.x=element_text(size=12, face='bold'))
gg_prod2 <- gg_prod2 + theme(plot.title=element_text(size=15, face = 'bold'))
gg_prod2

ggListYield[[10]]
h_prod2 <- arrangeGrob(ggListYield[[10]], gg_prod2, nrow=1) #generates g
#ggsave(file=paste(grd,"production_yield3.png", sep = ""), h_prod2, width=10, height=10.5, units='in') #saves g

tiff(filename=paste(dapadfs,"YieldPP.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 500)

plot(h_prod2)

dev.off()
#Datos para hacer cluster-------
mdwidet_meltf <- mdwideBean_f[mdwideBean_f$Intervencion=='impacto',] %>% spread(impactparameter, Value)
mdwidet_meltf$trend <- NULL
mdwidet_meltf$commodity<- NULL
mdwidet_meltf$zona<- NULL


mean2 <- function(x){mean(x, na.rm = T)}
mdwidet_meltf <- mdwidet_meltf %>% group_by( region, Intervencion) %>% summarise_each(funs(mean2))
# mdwidet_melt <- mdwidet_melt[complete.cases(mdwidet_melt),]

# PCA + clustering analysis
pca_data_food <- as.data.frame(mdwidet_meltf)
rownames(pca_data_food) <- pca_data_food$region
pca_data_food$commodity <- pca_data_food$region <- pca_data_food$Intervencion <- NULL

saveRDS(pca_data_food, '//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BeansData/Data&Graphs/bean_pca_data_food.rds')

##########################################################################################################
#=============================================PCA========================================================#

# Analisis de Cluster usando CPA
#librerias-----------------
options(warn=-1); options(scipen=999)

if(!require(corrplot)){install.packages('corrplot'); library(corrplot)} else {library(corrplot)}
if(!require(FactoMineR)){install.packages("FactorMiner"); library (FactoMineR)} else {FactoMineR}
if(!require(factoextra)){install.packages("factoextra"); library (factoextra)} else {factoextra}
if(!require(dplyr)){install.packages("dplyr"); library (dplyr)} else {dplyr}
if(!require(missMDA)){install.packages("missMDA"); library (missMDA)} else {missMDA}
if(!require(ggdendro)){install.packages("ggdendro"); library (ggdendro)} else {ggdendro}

#Directorios de graficos 
grp<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BeansData/Data&Graphs/"
#Cargar Datos-----
pca_Food<- readRDS( "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BeansData/Data&Graphs/bean_pca_data_food.rds")
#Datos Food Security----

##Calcular porcentaje de datos faltantes por variable
miss <- apply(X=pca_Food, MARGIN = 2, FUN = function(x){ y <- sum(is.na(x))/nrow(pca_Food); return(y*100) })
pca_Food <- pca_Food[,names(which(miss <= 30))]

##Desarrollo analisis CPA
res.comp <- imputePCA(pca_Food, ncp=)
PCA_resFood <- FactoMineR::PCA(X=res.comp$completeObs, scale.unit = T)
CLU_rest1 <- FactoMineR::HCPC(PCA_resFood, nb.clust = -1)

datosCLustFood<-as.data.frame(CLU_rest1$data.clust)
write.csv(datosCLustFood, paste(dapadfs, "datosClusterPCAFood.csv", sep = ""))




##Graficas Eigen values
g <- fviz_eig(PCA_resFood, addlabels=TRUE, hjust = -0.3 )  + theme_classic()
g <- g + ggtitle("PCA: Food Securiy and Yield, Production and Area")
g <- g + theme(plot.title=element_text(size=15, face = 'bold'))
tiff(filename=paste(grp,"eigenvalues_variancesFoop.tiff", sep = ""), 
     width = 10, height = 10, units = 'in', res = 100)
g

dev.off()



##Quality of representation of each variable
pdf('rQuality.pdf', height=7, width=8)
par(mfrow=c(1,3))
corrplot(PCA_resFood$var$cos2[,1:2], is.corr=FALSE) # Representation quality of each variable
corrplot(PCA_resFood$var$contrib[,1:2], is.corr=FALSE) # Contribution of each variable to dimension
corrplot(PCA_resFood$var$cor[,1:2], method="ellipse", is.corr=TRUE) # Correlation of each variable to dimension
dev.off()

##Quality of representation of each variable
pdf('rQuality_ind.pdf', height=7, width=8)
par(mfrow=c(1,3))
corrplot(PCA_resFood$ind$cos2[,1:2], is.corr=FALSE) # Representation quality of each variable
corrplot(PCA_resFood$ind$contrib[,1:2], is.corr=FALSE) # Contribution of each variable to dimension
dev.off()

##Variable factor map
h<- fviz_pca_var(PCA_resFood, col.var = "steelblue") + theme_bw()
h 

##Control variable colors using their contributions
fviz_pca_var(PCA_resFood, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint = 96) +
  theme_bw()


# Show points only: geom = "point"
p <- fviz_pca_ind(PCA_resFood, geom = "point",
                  habillage=CLU_rest1$data.clust$clust, addEllipses=TRUE,
                  ellipse.level= 0.95, invisible = 'none')+ theme_bw()
print(p)

# Only variables are labelled
fviz_pca_biplot(PCA_resFood,  label="var", habillage=CLU_rest1$data.clust$clust,
                addEllipses=TRUE, ellipse.level=0.95) +
  theme_bw()

b<- fviz_pca_biplot(PCA_resFood,  label="var", habillage=CLU_rest1$data.clust$clust,
                    addEllipses=TRUE, ellipse.level=0.95) 
b<- b +theme_bw() + ggtitle("PCA: Food Securiy and Yield, Production and Area" )
b<- b + theme(plot.title=element_text(size=15, face = 'bold'))
tiff(filename=paste(grp,"foodsecurityPCA.tiff", sep = ""), 
     width = 10, height = 10, units = 'in', res = 100)
b
dev.off()


datosCLustFood<-as.data.frame(PCA_resFood$var$cor)
write.csv(datosCLustFood, paste(dapadfs, "datosClusterPCAFood.csv", sep = ""))

# 
# # Clustering
# dhc <- as.dendrogram(CLU_rest1$call$t$tree)
# # Rectangular lines
# ddata <- dendro_data(dhc)
# 
# p <- ggdendrogram(ddata, rotate = TRUE,size=3, theme_dendro = FALSE, segments = F)+ xlab('Countries')+ ylab('Hierarchical Classification')
# p <- p +ggtitle("Hierarchical Clustering")+ theme(plot.title=element_text(size=15, face = 'bold'))
# p 
# 
# 


##########################################################################################################
#=============================================COPY FILES=================================================#
saveRDS(md, file= paste(dapadfs, "FilesBEAN.rds", sep = ""))


##########################################################################################################
#===========================================Graphs Food security=========================================#

