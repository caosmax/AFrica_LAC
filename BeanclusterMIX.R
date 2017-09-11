#Programa para analizar los datos de evaluación de frijol con y sin tecnologia-----
#bajo distintos escenarios climaticos
#Por:  Harold y Carlos Edo

#=============================================AREA-PRODUCTION-YIELD============================================#
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
dapadfs<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BeansData/"
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
mdsub<-subset(md,md$impactparameter=="QSXAgg -- Total Production" | 
                md$impactparameter== "TAreaXAgg -- Total Area" |
                md$impactparameter== "TYldXAgg -- Total Yield" | 
                md$impactparameter== "QDXAgg -- Total Demand" |
                md$impactparameter== "QEXAgg -- Export" | 
                md$impactparameter== "QMXAgg -- Import" |
                md$impactparameter== "PWXAgg -- World Prices")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QSXAgg -- Total Production"="Production",  
                                                        "TAreaXAgg -- Total Area"="Area",
                                                        "TYldXAgg -- Total Yield"="Yield",
                                                        "QDXAgg -- Total Demand"="Demand",
                                                        "QEXAgg -- Export"="Export",
                                                        "QMXAgg -- Import"="Import",
                                                        "PWXAgg -- World Prices"="W Prices"))
rownames(mdsub) <- 1:nrow(mdsub)

#Save as R Field 
save(md, file = paste(copy,"md.RData", sep = ""))
save(mdsub, file=paste(copy,"mdsub.RData", sep = ""))
saveRDS(mdsub, file = paste(dapadfs,"BeanData.rds", sep = ""))

mdsub$impactparameter <- as.character(mdsub$impactparameter)
mdsub$scenario <- as.character(mdsub$scenario)
mdsub$commodity <- as.character(mdsub$commodity)
mdsub$region <- as.character(mdsub$region)
mdsub$productiontype <- as.character(mdsub$productiontype)
mdsub$categoria <- as.character(mdsub$categoria)

mdsubcrop <- subset(mdsub,mdsub$commodity=="PUL-Beans" | mdsub$commodity=="-")
mdsubcrop$commodity <- revalue(mdsubcrop$commodity,c("PUL-Beans"="Beans"))

##########################################################################################################
# filtros por las regiones de ALC y Africa---------------
L <- mdsubcrop[grep(pattern = '^LAC', x = mdsubcrop$region, value = F, fixed = FALSE), ]
L$zona<- 'LAC'
A <- mdsubcrop[grep(pattern = "^SSA", x = mdsubcrop$region, value = F, fixed = FALSE), ]
A$zona<- 'AFRICA'

# Paises que quedaron por fuera del analisis
M<- mdsubcrop[which(mdsubcrop$region=="MEN-Algeria"|mdsubcrop$region=="MEN-Egypt"| mdsubcrop$region=="MEN-Morocco"|
               mdsubcrop$region=="MEN-Libya"|mdsubcrop$region=="MEN-Mauritania" ),]
M$zona<- "AFRICA"

#Apilar
data1 <- rbind(L,A,M) # todos los datos 
rownames(data1) <- 1:nrow(data1)


mdwide<-data1 %>%
  spread("year", "Val")

mdwide$impactparameter <- as.character(mdwide$impactparameter)
mdwide$scenario <- as.character(mdwide$scenario)
mdwide$commodity <- as.character(mdwide$commodity)
mdwide$region <- as.character(mdwide$region)
mdwide$productiontype <- as.character(mdwide$productiontype)
mdwide$categoria <- as.character(mdwide$categoria)
mdwide$zona <- as.character(mdwide$zona)
# Eliminar casos con datos faltantes por variable para los periodos 2020 & 2050---------------------
constraints <- as.data.frame(table(mdwide$region[is.na(mdwide$`2050`)& is.na(mdwide$`2020`)& mdwide$categoria == 'CC_Tec'], mdwide$impactparameter[is.na(mdwide$`2050`)& is.na(mdwide$`2020`) & mdwide$categoria == 'CC_Tec']))
names(constraints) <- c('Region', 'impactparameter', 'Count')
constraints <- constraints[constraints$Count >= 3,]; rownames(constraints) <- 1:nrow(constraints)

for(i in 1:nrow(constraints)){
  mdwide <- mdwide[-which(mdwide$impactparameter == constraints$impactparameter[i] & mdwide$region == constraints$Region[i]),]
}; rm(i, constraints)
# Eliminar casos con datos faltantes por variable para el periodo 2050----------------
constraints <- as.data.frame(table(mdwide$region[is.na(mdwide$`2050`)& mdwide$categoria == 'CC_Tec'], mdwide$impactparameter[is.na(mdwide$`2050`) & mdwide$categoria == 'CC_Tec']))
names(constraints) <- c('Region', 'impactparameter', 'Count')
constraints <- constraints[constraints$Count >= 3,]; rownames(constraints) <- 1:nrow(constraints)

for(i in 1:nrow(constraints)){
  mdwide <- mdwide[-which(mdwide$impactparameter == constraints$impactparameter[i] & mdwide$region == constraints$Region[i]),]
}; rm(i, constraints)
# Eliminar casos con datos faltantes por variable para los periodos consecutivos desde 2020 hasta 2040--------------
constraints <- as.data.frame(table(mdwide$region[is.na(mdwide$`2020`)&
                                                   is.na(mdwide$`2030`) &
                                                   is.na(mdwide$`2040`) &
                                                   mdwide$categoria == 'CC_Tec'],
                                   mdwide$impactparameter[is.na(mdwide$`2020`)&
                                                            is.na(mdwide$`2030`) &
                                                            is.na(mdwide$`2040`) &
                                                            mdwide$categoria == 'CC_Tec']))
names(constraints) <- c('Region', 'impactparameter', 'Count')
constraints <- constraints[constraints$Count > 0,]; rownames(constraints) <- 1:nrow(constraints)

# Combinar con el conteo de GCM's por combinación variable vs país
countCGM <- as.data.frame(table(mdwide$region[mdwide$categoria == 'CC_Tec'], mdwide$impactparameter[mdwide$categoria == 'CC_Tec']))
names(countCGM) <- c('Region', 'impactparameter', 'GCM_count')
countCGM <- countCGM[countCGM$GCM_count > 0,]; rownames(countCGM) <- 1:nrow(countCGM)

constraints <- left_join(x = constraints, y = countCGM, by=c('Region', 'impactparameter')); rm(countCGM)
constraints <- constraints[constraints$Count >= constraints$GCM_count/2,]; rownames(constraints) <- 1:nrow(constraints)

for(i in 1:nrow(constraints)){
  mdwide <- mdwide[-which(mdwide$impactparameter == constraints$impactparameter[i] & mdwide$region == constraints$Region[i]),]
}; rm(i, constraints)


combinations <- unique(mdwide[,c('impactparameter', 'region')])
data_list <- list()
for(i in 1:nrow(combinations)) {
  
  sub_data <- mdwide[mdwide$region == combinations$region[i] & mdwide$impactparameter == combinations$impactparameter[i],]
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

mdwide <- data_list; rm(data_list)
q<- mdwide
mdwide$Case <- NULL

# copia de datos para ser usados en grafico de produccion
saveRDS(q,paste(copy,"production.rds", sep = "")) 

##########################################################################################################
#Calcular cambio porcentual entre 2020 y 2050----
mdwide<-data.frame(mdwide[,1:7],"Cambio Porcentual"=mdwide[,14]) # eliminamos las columnas innecesarias 


# Eliminar datos agrupados innecesarios
mdwide<-mdwide[!mdwide$region=="LAC",]  
mdwide<-mdwide[!mdwide$region=="MEN",]
mdwide<-mdwide[!mdwide$region=="SSA",]


#liminar datos que no adoptaron tecnología 
mdwide<-mdwide[!mdwide$region=="LAC-Cuba",]
mdwide<-mdwide[!mdwide$region=="LAC-Jamaica",]
mdwide<-mdwide[!mdwide$region=="MEN-Egypt",]
mdwide<-mdwide[!mdwide$region=="LAC-Other Caribbean",]
mdwide<-mdwide[!mdwide$region=="LAC-Haiti",]
mdwide<-mdwide[!mdwide$region=="LAC-Dominican Republic",]

# importante para renombrar las columnas
rownames(mdwide) <- 1:nrow(mdwide)

testHarold <- mdwide
# testHarold <- subset(mdwide, subset=mdwide$impactparameter=='Yield')
hist(testHarold$Cambio.Porcentual[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec')])
boxplot(testHarold$Cambio.Porcentual[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec')]~testHarold$scenario[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec')])
#Solo con cambio Climatico
testHarold <- testHarold[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec'),]

#reshape
mdwide_t <- testHarold %>%
  spread("scenario","Cambio.Porcentual")

mdwide_t$productiontype <- NULL 
mdwide_t$CC_mean <- rowMeans(x=mdwide_t[,6:ncol(mdwide_t)], na.rm=TRUE)

#para mantener estas variables
mdwide_t <- mdwide_t[,c("impactparameter", "commodity", "region", "zona", "categoria", "CC_mean")]
#reshape
mdwide_t <- mdwide_t %>% spread(categoria, CC_mean)
#copia
sin<- mdwide_t

write.csv(sin, paste(copy, "SumInformes.csv", sep = ""))
#logica en las variaciones  cambios y  diferencias relativas-----
nn <- which(mdwide_t$CC_No<0 & mdwide_t$CC_Tec<0) 
pp <- which(mdwide_t$CC_No>0 & mdwide_t$CC_Tec>0)
np <- which(mdwide_t$CC_No>0 & mdwide_t$CC_Tec<0)

nn <- intersect(nn, which(mdwide_t$CC_No > mdwide_t$CC_Tec))
pp <- intersect(pp, which(mdwide_t$CC_No > mdwide_t$CC_Tec))
np <- intersect(np, which(mdwide_t$CC_No > mdwide_t$CC_Tec))

noimpact <- c(nn, pp, np)
mdwide_t$impacto<-NA

#loops ajuste inclusion de los paises sin impacto
for(j in 1:nrow(mdwide_t)){
  
  if(j %in% noimpact){
    mdwide_t$impacto[j] <- (mdwide_t$CC_Tec[j] - mdwide_t$CC_NoTec[j])/mdwide_t$CC_NoTec[j]*100
  } else {
    mdwide_t$impacto[j] <- abs(mdwide_t$CC_Tec[j] - mdwide_t$CC_NoTec[j])/max(abs(mdwide_t$CC_Tec[j]), abs(mdwide_t$CC_NoTec[j]), na.rm=TRUE) * 100
    
  }
  
}

#### grafico teniendo en cuenta grupos
#a) recuperación,  b) recuperación más mejoramiento  y c) mejoramiento
pn <- which(mdwide_t$CC_No<0 & mdwide_t$CC_Tec>0)
np <- which(mdwide_t$CC_No>0 & mdwide_t$CC_Tec<0)

nn <- which(mdwide_t$CC_No<0 & mdwide_t$CC_Tec<0) 
pp <- which(mdwide_t$CC_No>0 & mdwide_t$CC_Tec>0)

alivio <-intersect(nn,which(mdwide_t$CC_No < mdwide_t$CC_Tec))
mejora <-intersect(pp, which(mdwide_t$CC_NoTec < mdwide_t$CC_Tec))
ali_mejora <- intersect(pn,which(mdwide_t$CC_No < mdwide_t$CC_Tec))

# effect <- c(alivio, mejora, ali_mejora); rm(alivio, mejora, ali_mejora)

mdwide_t$trend<-NA

# vectores de posicion 
mdwide_t$trend[alivio] <- 'Alleviate'
mdwide_t$trend[mejora] <- 'Impulse'
mdwide_t$trend[ali_mejora] <- 'Alleviate+Impulse'

#loops ajuste inclusion de los paises Alivio
for(i in 1:nrow(mdwide_t)){
  
  if(i %in% alivio){
    mdwide_t$trend[i] <- "Alleviate"
    
  }
}

#loops ajuste inclusion de los paises mejora
for(i in 1:nrow(mdwide_t)){
  
  if(i %in% mejora){
    mdwide_t$trend[i] <- "Impulse"
  }
  
}

#loops ajuste inclusion de los paises alivio + mejora
for(i in 1:nrow(mdwide_t)){
  
  if(i %in% ali_mejora){
    mdwide_t$trend[i] <- "Alleviate+Impulse"
  }
  
}

# guardar base de datos correcta
write.csv(mdwide_t,paste(copy,"datamix.csv", sep = ""), row.names = FALSE)

# grafica de impactos de rendimientos 
mdwide_t <- mdwide_t %>% gather( Intervencion, Value, CC_NoTec:impacto)# para hacer el cluster

mdwideBean_t<-mdwide_t


# # subset Solo cambio climatico 
# rendicc<-subset(mdwide, categoria!="NoCC_NoTec" & categoria!="NoCC_Tec")
# 
# # Creando  las variables con tecnologia y sin tecnologia--------
# rendicc_tech$adoption<-as.character(rendicc_tech$variable)
# rendicc$productiontype <- NULL
# rendicc$commodity<- NULL

# dh<- ggplot(rendicc[which(rendicc$impactparameter=="Yield"),], aes(x=Cambio.Porcentual, colour=categoria)) + geom_density(alpha=.3)
# dh<- dh + scale_colour_discrete(name="Scenarios\nPercent Change")
# dh<- dh + xlab("% Change") 
# dh<- dh + ggtitle("Yield, with Technology\n& without Technology by GCMs") 
# dh<- dh + theme(plot.title = element_text(lineheight=.8, face="bold")) 
# dh
# ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/HistograTechnYieldCC.png", dh, width=10, height=10.5, units='in')                                                                                       



##########################################################################################################
#Graficos verdes------------------

# Yield plot
gg <- ggplot(data=mdwideBean_t[mdwideBean_t$Intervencion!='impacto' & 
                                 mdwideBean_t$impactparameter=='Yield',], aes(y=Intervencion, x=region, fill=Value)) 
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
gg1 <- ggplot(data=mdwideBean_t[mdwideBean_t$Intervencion!='impacto' & 
                                  mdwideBean_t$impactparameter=='Area',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
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
gg2 <- ggplot(data=mdwideBean_t[mdwideBean_t$Intervencion!='impacto' & 
                                  mdwideBean_t$impactparameter=='Production',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
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

# Import plot
gg3 <- ggplot(data=mdwideBean_t[mdwideBean_t$Intervencion!='impacto' & 
                                  mdwideBean_t$impactparameter=='Import',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg3 <- gg3 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg3 <- gg3 + coord_equal()
gg3 <- gg3 + ggtitle('Imports') + ylab('') + xlab('Countries')
gg3 <- gg3 + theme_bw()
gg3 <- gg3 + theme(axis.text.x=element_text(size=10, angle=90))
gg3 <- gg3 + theme(axis.text.y=element_text(size=10))
gg3 <- gg3 + theme(axis.title.x=element_text(size=12, face='bold'))
gg3 <- gg3 + theme(plot.title=element_text(size=15, face = 'bold'))
gg3 <- gg3 + theme(legend.text = element_text(size=10))
gg3 <- gg3 + theme(legend.title = element_text(size=10, face = 'bold'))
gg3

# Export plot
gg4 <- ggplot(data=mdwideBean_t[mdwideBean_t$Intervencion!='impacto' & 
                                  mdwideBean_t$impactparameter=='Export',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg4 <- gg4 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg4 <- gg4 + coord_equal()
gg4 <- gg4 + ggtitle('Exports') + ylab('') + xlab('Countries')
gg4 <- gg4 + theme_bw()
gg4 <- gg4 + theme(axis.text.x=element_text(size=10, angle=90))
gg4 <- gg4 + theme(axis.text.y=element_text(size=10))
gg4 <- gg4 + theme(axis.title.x=element_text(size=12, face='bold'))
gg4 <- gg4 + theme(plot.title=element_text(size=15, face = 'bold'))
gg4 <- gg4 + theme(legend.text = element_text(size=10))
gg4 <- gg4 + theme(legend.title = element_text(size=10, face = 'bold'))
gg4

# Demand
gg5 <- ggplot(data=mdwideBean_t[mdwideBean_t$Intervencion!='impacto' & 
                                  mdwideBean_t$impactparameter=='Demand',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg5 <- gg5 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg5 <- gg5 + coord_equal()
gg5 <- gg5 + ggtitle('Demand') + ylab('') + xlab('Countries')
gg5 <- gg5 + theme_bw()
gg5 <- gg5 + theme(axis.text.x=element_text(size=10, angle=90))
gg5 <- gg5 + theme(axis.text.y=element_text(size=10))
gg5 <- gg5 + theme(axis.title.x=element_text(size=12, face='bold'))
gg5 <- gg5 + theme(plot.title=element_text(size=15, face = 'bold'))
gg5 <- gg5 + theme(legend.text = element_text(size=10))
gg5 <- gg5 + theme(legend.title = element_text(size=10, face = 'bold'))
gg5

grid.arrange(gg, gg1, gg2, gg3, gg4, gg5)

g <- arrangeGrob(gg, gg1, gg2, nrow=3) #generates g
h <- arrangeGrob(gg3, gg4, gg5, nrow=3) #generates g
ggsave(file=paste(grd,"graph1.png", sep=''),g, width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"graph2.png", sep=''),h, width=10, height=10.5, units='in') 



# Graficos 
impactoDF <- subset(mdwide_t, subset=mdwide_t$Intervencion=='impacto')

impactList <- as.character(unique(impactoDF$impactparameter))
ggList <- lapply(1:length(impactList), function(i){
  
  df <- subset(impactoDF, impactoDF$impactparameter==impactList[i])
  df$region <- as.character(df$region)
  df$trend <- as.character(df$trend)
  df <- df[complete.cases(df),]  
  df$region <- factor(df$region, levels=df$region[order(df$Value)])  
  
  gg <- ggplot(df, aes(x=region, y=Value, fill=trend)) + theme_bw() + geom_bar(stat="identity") + coord_flip()
  gg <- gg + ggtitle(impactList[i]) + xlab('Countries') + ylab('Relative differences')
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
ggsave(file=paste(grd,"impacto_oferta.png", sep=''), g, width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"impacto_comercio_demanda.png", sep=''), h, width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"impacto_comercio_demanda.png", sep=''), h, width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"demandaBarplot.png",sep=''), ggList[[1]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"exportBarplot.png",sep=''), ggList[[2]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"importBarplot.png",sep=''), ggList[[3]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"productionBarplot.png",sep=''), ggList[[4]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"areaBarplot.png",sep=''), ggList[[5]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"yieldBarplot.png",sep=''), ggList[[6]], width=10, height=10.5, units='in') 
#Analisis de Cluster----------------------------
#Reformatear datos para realizar graficos
mdwidet_melt <- mdwide_t[mdwide_t$Intervencion=='impacto',] %>% spread(impactparameter, Value)
mdwidet_melt$trend <- NULL
mdwidet_melt$Demand <- NULL
mdwidet_melt$Export <- NULL
mdwidet_melt$Import <- NULL


mean2 <- function(x){mean(x, na.rm = T)}
mdwidet_melt <- mdwidet_melt %>% group_by(commodity, region, Intervencion) %>% summarise_each(funs(mean2))
mdwidet_melt$zona<- NULL
mdwidet_melt <- mdwidet_melt[complete.cases(mdwidet_melt),]

#Analisis de cluster

x <- as.matrix(scale(mdwidet_melt[,4:6]))
rownames(x) <- mdwidet_melt$region
hclustDF <- hclust(dist(x))
plot(hclustDF)
#Guardar el archivo
tiff(filename=paste(grd,"clusterW.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)

plot(hclustDF)

dev.off()

#obtener estadisticas y mejorar graficos 
ctree <- cutree(hclustDF, k=7)
rect.hclust(hclustDF, k=7, border="red")



tiff(filename=paste(grd,"clusterRred.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)

plot(hclustDF)
rect.hclust(hclustDF, k=7, border="red")

dev.off()

# PCA + clustering analysis
pca_data <- as.data.frame(mdwidet_melt)
rownames(pca_data) <- pca_data$region
pca_data$commodity <- pca_data$region <- pca_data$Intervencion <- NULL
pca_data$Cluster<- NULL
saveRDS(pca_data, '//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BeansData/Data&Graphs/bean_pca_data.rds')
#saveRDS(pca_data,paste(copy, "bean_pca_data.rds", sep = ""))


mdwidet_melt$Cluster <- NA
for(i in 1:nrow(mdwidet_melt))
{
  mdwidet_melt$Cluster[i] <- ctree[which(mdwidet_melt$region[i]==names(ctree))]
}

library(dplyr)

data<- as.data.frame(summarise(group_by(mdwidet_melt, Cluster), meanProd=mean(Production), meanArea=mean(Area), meanYi=mean(Yield)))
write.csv(data, paste(copy,"dataMix.csv", sep=), row.names = FALSE)
write.csv(mdwidet_melt, paste(copy,"tablesumMix.csv", sep=), row.names = FALSE)

##########################################################################################################
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
gg_prod <- gg_prod + ggtitle('') +  ylab('Production average (ton)') + xlab(' ') + scale_fill_discrete("Categories")
gg_prod <- gg_prod + theme_bw() + ggtitle('Production')
gg_prod <- gg_prod + theme(axis.text.y=element_text(size=1))
gg_prod <- gg_prod + theme(axis.text.y=element_text(size=12))
gg_prod <- gg_prod + theme(axis.title.x=element_text(size=12, face='bold'))
gg_prod <- gg_prod + theme(plot.title=element_text(size=15, face = 'bold'))
gg_prod
ggList[[6]]
h_prod <- arrangeGrob(ggList[[6]], gg_prod, nrow=1) #generates g
ggsave(file=paste(grd,"production_yield.png", sep = ""), h_prod, width=10, height=10.5, units='in') #saves g



# grafico solo produccion con tecnologia 
only_qTotalTech <- only_qTotal[only_qTotal$categoria!="CC_NoTec",]
only_qTotalTech$categoria <- as.character(only_qTotalTech$categoria)
yieldDF$region <- factor(yieldDF$region, levels=yieldDF$region[order(yieldDF$Value)])
only_qTotalTech$region <- factor(x=only_qTotalTech$region, levels=levels(yieldDF$region))
gg_prod2 <- ggplot(only_qTotalTech, aes(x=region, y=promedio, fill=categoria)) + theme_bw() + geom_bar(stat="identity", position = "dodge") + coord_flip()
gg_prod2 <- gg_prod2 + ggtitle('') +  ylab('Production average (ton)') + xlab(' ') + scale_fill_discrete("Categories")
gg_prod2 <- gg_prod2 + theme_bw() + ggtitle('Production')
gg_prod2 <- gg_prod2 + theme(axis.text.y=element_text(size=1))
gg_prod2 <- gg_prod2 + theme(axis.text.y=element_text(size=12))
gg_prod2 <- gg_prod2 + theme(axis.title.x=element_text(size=12, face='bold'))
gg_prod2 <- gg_prod2 + theme(plot.title=element_text(size=15, face = 'bold'))
gg_prod2

ggList[[6]]
h_prod2 <- arrangeGrob(ggList[[6]], gg_prod2, nrow=1) #generates g
ggsave(file=paste(grd,"production_yield2.png", sep = ""), h_prod2, width=10, height=10.5, units='in') #saves g

##########################################################################################################
#=============================================FOOD SECURITY============================================#
#Filter by new variables using md-------------
md # datos base
mdsub_food<-subset(md,md$impactparameter== "PerCapKCalCXAgg -- PcKcal by Commodity"|
                     md$impactparameter== "FoodAvailXAgg"|
                     md$impactparameter== "ShareAtRiskXagg -- Share at Risk of Hunger" |
                     md$impactparameter== "QMXAgg -- Import"|
                     md$impactparameter== "QEXAgg -- Export"|
                     md$impactparameter== "TotalMalnourishedXagg -- Malnurished Children"|
                     md$impactparameter=="QSXAgg -- Total Production" | 
                     md$impactparameter== "TAreaXAgg -- Total Area" |
                     md$impactparameter== "TYldXAgg -- Total Yield" | 
                     md$impactparameter== "QDXAgg -- Total Demand")

mdsub_food$impactparameter<-revalue(mdsub_food$impactparameter, c("QEXAgg -- Export"="Export",
                                                                  "QMXAgg -- Import"="Import",
                                                                  "PerCapKCalCXAgg -- PcKcal by Commodity"= "PerCapKCalCXAgg",
                                                                  "FoodAvailXAgg"="FoodAva", 
                                                                  "ShareAtRiskXagg -- Share at Risk of Hunger"= "ShareAtRiskXagg",
                                                                  "TotalMalnourishedXagg -- Malnurished Children"="TotalMalnourishedXagg",
                                                                  "QSXAgg -- Total Production"="Production",  
                                                                  "TAreaXAgg -- Total Area"="Area",
                                                                  "TYldXAgg -- Total Yield"="Yield",
                                                                  "QDXAgg -- Total Demand"="Demand"))

rownames(mdsub) <- 1:nrow(mdsub)
 

#Save as R Field 
saveRDS(mdsub_food, file=paste(copy,"mdsubFood.rds", sep = ""))

#factor to character
mdsub_food$impactparameter <- as.character(mdsub_food$impactparameter)
mdsub_food$scenario <- as.character(mdsub_food$scenario)
mdsub_food$commodity <- as.character(mdsub_food$commodity)
mdsub_food$region <- as.character(mdsub_food$region)
mdsub_food$productiontype <- as.character(mdsub_food$productiontype)
mdsub_food$categoria <- as.character(mdsub_food$categoria)

# copia
cropsFiltro<-mdsub_food

# filtros para beans
cropsFiltro <- subset(cropsFiltro,cropsFiltro$commodity=="PUL-Beans" | cropsFiltro$commodity=="-")
cropsFiltro$commodity <- revalue(cropsFiltro$commodity,c("PUL-Beans"="Beans"))

#filtros paises
L <- cropsFiltro[grep(pattern = '^LAC', x = cropsFiltro$region, value = F, fixed = FALSE), ]
L$zona<- 'LAC'
A <- cropsFiltro[grep(pattern = "^SSA", x = cropsFiltro$region, value = F, fixed = FALSE), ]
A$zona<- 'AFRICA'

# Paises que quedaron por fuera del analisis
M<- cropsFiltro[which(cropsFiltro$region=="MEN-Algeria"|cropsFiltro$region=="MEN-Egypt"| cropsFiltro$region=="MEN-Morocco"|
                        cropsFiltro$region=="MEN-Libya"|cropsFiltro$region=="MEN-Mauritania" ),]
M$zona<- "AFRICA"

#Apilar
dataFood <- rbind(L,A,M) # todos los datos 
rownames(dataFood) <- 1:nrow(dataFood)


mdwideFood<-dataFood %>%
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
mdwideFood$Case <- NULL

# copia de datos para ser usados en grafico de produccion
saveRDS(mdwideFood,paste(copy,"FoodCopyData.rds", sep = "")) 

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

# importante para renombrar las columnas
rownames(mdwideFood) <- 1:nrow(mdwideFood)

testHaroldf <- mdwideFood
testHaroldf <- testHaroldf[-which(testHaroldf$categoria=='NoCC_NoTec' | 
                                    testHaroldf$categoria=='NoCC_Tec'),]


#reshape
mdwide_f <- testHaroldf %>%
  spread("scenario","Cambio.Porcentual")

mdwide_f$productiontype <- NULL 
mdwide_f$CC_mean <- rowMeans(x=mdwide_f[,6:ncol(mdwide_f)], na.rm=TRUE)

#para mantener estas variables
mdwide_f <- mdwide_f[,c("impactparameter", "commodity", "region","zona", "categoria", "CC_mean")]

#reshape
mdwide_f <- mdwide_f %>% 
  spread(categoria, CC_mean)

#copia
sinFood<- mdwide_t

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
    mdwide_f$impacto[j] <- (mdwide_f$CC_Tec[j] - mdwide_f$CC_NoTec[j])/mdwide_f$CC_NoTec[j]*100
  } else {
    mdwide_f$impacto[j] <- abs(mdwide_f$CC_Tec[j] - mdwide_f$CC_NoTec[j])/max(abs(mdwide_f$CC_Tec[j]), abs(mdwide_f$CC_NoTec[j]), na.rm=TRUE) * 100
    
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
mdwide_f$trend[alivio] <- 'Alleviate'
mdwide_f$trend[mejora] <- 'Impulse'
mdwide_f$trend[ali_mejora] <- 'Alleviate+Impulse'

#loops ajuste inclusion de los paises Alivio
for(i in 1:nrow(mdwide_f)){
  
  if(i %in% alivio){
    mdwide_f$trend[i] <- "Alleviate"
    
  }
}

#loops ajuste inclusion de los paises mejora
for(i in 1:nrow(mdwide_f)){
  
  if(i %in% mejora){
    mdwide_f$trend[i] <- "Impulse"
  }
  
}

#loops ajuste inclusion de los paises alivio + mejora
for(i in 1:nrow(mdwide_f)){
  
  if(i %in% ali_mejora){
    mdwide_f$trend[i] <- "Alleviate+Impulse"
  }
  
}

# guardar base de datos correcta
write.csv(mdwide_f,paste(copy,"datamixFood.csv", sep = ""), row.names = FALSE)
#Base de datos para graficos ---------------
mdwide_f <- mdwide_f %>% gather( Intervencion, Value, CC_NoTec:impacto)# para hacer el cluster
mdwideBean_f<-mdwide_f


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
#saveRDS(pca_data_food,paste(copy, "bean_pca_data_food.rds", sep = ""))


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
pca_AYP<- readRDS ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BeansData/Data&Graphs/bean_pca_data.rds")

cor(pca_AYP, use = "complete.obs")
# pca_data_food[complete.cases(pca_data_food),]
#Datos Yield production Area----
res.pca <- PCA(pca_AYP, graph = T, scale.unit = T)
CLU.rest <- FactoMineR::HCPC(res.pca, nb.clust = -1)
datosCLustAreaYied<-as.data.frame(CLU.rest$data.clust)
write.csv(datosCLustAreaYied, paste(grp, "datosClusterPCAAreYield.csv", sep = ""))

#Eigen values
gr <- fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3 ) + theme_classic()
gr <- gr + ggtitle("PCA: Yield, Production and Area")
gr <- gr + theme(plot.title=element_text(size=15, face = 'bold'))
tiff(filename=paste(grp,"eigenvalues_variancesYieldProduction.tiff", sep = ""), 
     width = 10, height = 10, units = 'in', res = 100)
gr
dev.off()


#Only variables are labelled
br<- fviz_pca_biplot(res.pca, label="var", habillage=CLU.rest$data.clust$clust,
                    addEllipses=TRUE, ellipse.level=0.95) 
br<- br + theme_bw() + ggtitle("PCA: Yield, Production and Area" )
br<- br + theme(plot.title=element_text(size=15, face = 'bold'))
tiff(filename=paste(grp,"AreaYieldProductionPCA.tiff", sep = ""), 
     width = 10, height = 10, units = 'in', res = 100)
br
dev.off()

#Extraer Datos
datosCLustAYP<-as.data.frame(res.pca$var$cor)
write.csv(datosCLustAYP, paste(grp, "datosClusterPCAAYP.csv", sep = ""))
#Datos Food Security----

##Calcular porcentaje de datos faltantes por variable
miss <- apply(X=pca_Food, MARGIN = 2, FUN = function(x){ y <- sum(is.na(x))/nrow(pca_Food); return(y*100) })
pca_Food <- pca_Food[,names(which(miss <= 33))]

##Desarrollo analisis CPA
res.comp <- imputePCA(pca_Food, ncp=7)
PCA_resFood <- FactoMineR::PCA(X=res.comp$completeObs, scale.unit = T)
CLU_rest1 <- FactoMineR::HCPC(PCA_resFood, nb.clust = -1)

datosCLustFood<-as.data.frame(CLU_rest1$data.clust)
write.csv(datosCLustFood, paste(grp, "datosClusterPCAFood.csv", sep = ""))


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

# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# 3. Use gradient color
fviz_pca_ind(PCA_resFood, repel = TRUE, col.ind = "cos2")+
  scale_color_gradient2(low="blue", mid="white",
                        high="red", midpoint=0.6)+
  theme_bw()

# Color by groups: habillage=iris$Species
# Show points only: geom = "point"
p <- fviz_pca_ind(PCA_resFood, geom = "point",
                  habillage=CLU_rest1$data.clust$clust, addEllipses=TRUE,
                  ellipse.level= 0.95, invisible = 'none')+ theme_bw()
print(p)

# Biplot of individuals and variables
# ++++++++++++++++++++++++++
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
write.csv(datosCLustFood, paste(grp, "datosClusterPCAFood.csv", sep = ""))


# Clustering
dhc <- as.dendrogram(CLU_rest1$call$t$tree, k=3)
# Rectangular lines
ddata <- dendro_data(dhc, type="rectangle")

p <- ggdendrogram(ddata, rotate = TRUE,size=3, theme_dendro = FALSE, segments = F)+ xlab('Countries')+ ylab('Hierarchical Classification')
p <- p +ggtitle("Hierarchical Clustering")+ theme(plot.title=element_text(size=15, face = 'bold'))
p 


# p <- ggplot(segment(ddata)) + 
#   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
#   coord_flip() + xlab('Countries') + ylab('Distance')
#   scale_y_reverse(expand = c(0.2, 0))
# p
# 


###########################################################################################################
#=============================================GRAPHS FOOD SECURITY========================================#
# Procesamiento de datos --------------
#using data1
dataFS<- data1
# 
# mdwide<-dataFS %>%
#   spread("year", "Val")
# 
# # calculo tasas de crecimiento  y  eliminacion de columnas innecesarias 2030 y 2050--------------
# mdwide50<- data.frame(mdwide,"Cambio.Porcentual"=( (mdwide$`2050`-mdwide$`2030`)/mdwide$`2030`)*100)
# mdwide30<- data.frame(mdwide,"Cambio.Porcentual"=( (mdwide$`2030`-mdwide$`2020`)/mdwide$`2020`)*100)
# 
# mdwide50<- data.frame(mdwide50[,1:7],"Cambio Porcentual"=mdwide50[,14]) 
# mdwide30<- data.frame(mdwide30[,1:7],"Cambio Porcentual"=mdwide30[,14]) 
# 
# mdwide50<- mdwide50[,c("impactparameter", "scenario", "commodity", "region", "productiontype", "zona","categoria",              
#                        "Cambio.Porcentual")]
# mdwide30<- mdwide30[,c("impactparameter", "scenario", "commodity", "region", "productiontype", "zona","categoria",              
#                        "Cambio.Porcentual")]
# 
# 
# # reshape de las tasas de crecimiento 
# mdwide_t50<-mdwide50 %>%
#   spread("scenario","Cambio.Porcentual")
# 
# mdwide_t30<-mdwide30 %>%
#   spread("scenario","Cambio.Porcentual")
# 
# testfoodAva50<- mdwide_t50
# testfoodAva30<- mdwide_t30
# 
# 
# 
# # calculode la media 
# testfoodAva50$CC_mean <- rowMeans(x=testfoodAva50[,7:ncol(testfoodAva50)], na.rm=TRUE)
# testfoodAva30$CC_mean <- rowMeans(x=testfoodAva30[,7:ncol(testfoodAva30)], na.rm=TRUE)
# 
# # eliminamos no cambio climatico
# testfoodAva50 <- testfoodAva50[-which(testfoodAva50$categoria=='NoCC_NoTec' |
#                                         testfoodAva50$categoria=='NoCC_Tec'),] 
# testfoodAva30 <- testfoodAva30[-which(testfoodAva30$categoria=='NoCC_NoTec' |
#                                         testfoodAva30$categoria=='NoCC_Tec'),] 
# 
# # organizamos y eliminamos las columnas innncesarias
# testfoodAva50 <- testfoodAva50[,c("impactparameter", "commodity", "region", "zona",  "categoria", "CC_mean")]
# testfoodAva30 <- testfoodAva30[,c("impactparameter", "commodity", "region", "zona",  "categoria", "CC_mean")]
# 
# # reordenamos el orden de la numeracion
# rownames(testfoodAva50) <- 1:nrow(testfoodAva50)
# rownames(testfoodAva30) <- 1:nrow(testfoodAva30)
# 
# testfoodAva50$impactparameter<- as.character(testfoodAva50$impactparameter)
# testfoodAva30$impactparameter<- as.character(testfoodAva30$impactparameter)
# 
# 
# #reshape
# testfoodAva50 <- testfoodAva50 %>% 
#   spread(categoria, CC_mean)
# 
# testfoodAva30 <- testfoodAva30 %>% 
#   spread(categoria, CC_mean)
# 
# 
# # tipos de impactos
# 
# ## periodo 2050-------
# nn <- which(testfoodAva50$CC_NoTec<0 & testfoodAva50$CC_Tec<0) 
# pp <- which(testfoodAva50$CC_NoTec>0 & testfoodAva50$CC_Tec>0)
# pn <- which(testfoodAva50$CC_NoTec>0 & testfoodAva50$CC_Tec<0)
# np <- which(testfoodAva50$CC_NoTec<0 & testfoodAva50$CC_Tec>0)
# 
# #existencia de impactos
# nn <- intersect(nn, which(testfoodAva50$CC_NoTec > testfoodAva50$CC_Tec)) # ambos negativos pero con tecnologia es menor
# pp <- intersect(pp, which(testfoodAva50$CC_NoTec > testfoodAva50$CC_Tec))
# np <- intersect(np, which(testfoodAva50$CC_NoTec > testfoodAva50$CC_Tec))
# 
# noimpact50 <- c(nn, pp, np)
# 
# testfoodAva50$impacto<-NA
# 
# #loops 
# for(j in 1:nrow(testfoodAva50)){
#   
#   if(j %in% noimpact50){
#     testfoodAva50$impacto[j] <- (testfoodAva50$CC_Tec[j] - testfoodAva50$CC_NoTec[j])/testfoodAva50$CC_NoTec[j]*100
#   } else {
#     testfoodAva50$impacto[j] <- abs(testfoodAva50$CC_Tec[j] - testfoodAva50$CC_NoTec[j])/unlist(lapply(1:nrow(testfoodAva50),
#                                                                                                        function(i){z <- max(abs(testfoodAva50$CC_Tec[i]), abs(testfoodAva50$CC_NoTec[i]), na.rm=TRUE); return(z)})) * 100
#     
#   }
#   
# }
# 
# ## periodo 2030--------
# nn <- which(testfoodAva30$CC_NoTec<0 & testfoodAva30$CC_Tec<0) 
# pp <- which(testfoodAva30$CC_NoTec>0 & testfoodAva30$CC_Tec>0)
# pn <- which(testfoodAva30$CC_NoTec>0 & testfoodAva30$CC_Tec<0)
# np <- which(testfoodAva30$CC_NoTec<0 & testfoodAva30$CC_Tec>0)
# 
# 
# nn <- intersect(nn, which(testfoodAva30$CC_NoTec > testfoodAva30$CC_Tec))
# pp <- intersect(pp, which(testfoodAva30$CC_NoTec > testfoodAva30$CC_Tec))
# np <- intersect(np, which(testfoodAva30$CC_NoTec > testfoodAva30$CC_Tec))
# 
# noimpact30 <- c(nn, pp, np)
# 
# testfoodAva30$impacto<-NA
# 
# #loops 
# for(j in 1:nrow(testfoodAva30)){
#   
#   if(j %in% noimpact30){
#     testfoodAva30$impacto[j] <- (testfoodAva30$CC_Tec[j] - testfoodAva30$CC_NoTec[j])/testfoodAva30$CC_NoTec[j]*100
#   } else {
#     testfoodAva30$impacto[j] <- abs(testfoodAva30$CC_Tec[j] - testfoodAva30$CC_NoTec[j])/unlist(lapply(1:nrow(testfoodAva30),
#                                                                                                        function(i){z <- max(abs(testfoodAva30$CC_Tec[i]), abs(testfoodAva30$CC_NoTec[i]), na.rm=TRUE); return(z)})) * 100
#     
#   }
#   
# }
# 
# ##respaldo de los datos generados-----------
# write.csv(testfoodAva50, paste(copy, "dataFoodAV50.csv", sep = ""), row.names=T)
# write.csv(testfoodAva30, paste(copy, "dataFoodAV30.csv", sep = ""), row.names=T)
# 
# testfoodAva50<- read.csv(paste(copy, "dataFoodAV50.csv", sep = ""))
# testfoodAva30<- read.csv(paste(copy, "dataFoodAV30.csv", sep = ""))
# 
# # Para futuros casos iniciar desde aquí
# testfoodAva50$sce<-"2030-2050"
# testfoodAva30$sce<-"2020-2030"
# 
# beansZonas <- rbind(testfoodAva50,testfoodAva30)
# beansZonas <- beansZonas %>% gather( Intervencion, Value, CC_NoTec:impacto)
# 
# beansZonas$X<- NULL
# rownames(beansZonas) <- 1:nrow(beansZonas)
# 
# # loops graficos--------------
# ## Impacto total
# py <- list()
# 
# for (i in 1:length(variables)) {
#   
#   #   tiff(filename=paste(grd,variables[i],"_analisys.tiff",sep=""), 
#   #        width = 10, height = 7, units = 'in', res = 100)
#   #   
#   py[[i]]<- (ggplot(beansZonas[which(beansZonas$Intervencion=="impacto" 
#                                      & beansZonas$impactparameter==variables[i]),], 
#                     aes(x=Value, colour=zona))
#              + geom_density(size = 0.3) + facet_grid(~sce) 
#              + scale_colour_discrete(name="Regions\nPercent Change") 
#              + theme(strip.text.x = element_text(size = 20, colour = "blue"))
#              + xlab("% Change") + ggtitle(variables[i])
#              + theme(plot.title=element_text(size=18, face = 'bold'))
#              + theme(plot.title = element_text(lineheight=.8, face="bold", size=15)) 
#              + theme(legend.title = element_text(size=14, face = 'bold'))
#              + theme(axis.text.y=element_text(size=15))
#              + theme(axis.text.x=element_text(size=15)) 
#              + geom_vline(xintercept = 0, colour="darkblue", linetype = "longdash"))
#   
#   #   dev.off()
#   #   print(i)
# }
# 
# h<- arrangeGrob(py[[1]], py[[2]], py[[3]], nrow=4,top = textGrob("Impacts of technology\non food security",gp=gpar(fontsize=30,font=3)),
#                 bottom = textGrob(" Bioeconomic modeling team",gp=gpar(fontsize=20,font=3)))
# ggsave(file=paste(grd, "total.png", sep = ""), h,  width=12, height=15, units='in') 
# 
# #calculo del area bajo del curva.---------------------------------------
# ##creando un subgrupo
# xx<- beansZonas[which(beansZonas$Intervencion=="impacto" 
#                       & beansZonas$impactparameter=="FoodAva"),]
# yy<- beansZonas[which(beansZonas$Intervencion=="impacto" 
#                       & beansZonas$impactparameter=="PerCapKCalCXAgg"),]
# ##utilizando un paquete
# library(sm)
# attach(xy)
# 
# #adjust configura el smooth= suavizado  adjust=0.1 - 10
# xx_foodava<- density(xx$Value, bw = "nrd", na.rm = TRUE)
# plot(xx_foodava)
# yy_perca<- density(yy$Value, bw = "nrd", na.rm = TRUE)
# plot(yy_perca)
# 
# 
# #integrate.xy(xy_foodava2$, xy_foodava2$y)
# 
# xy_foodavaAA<- sm.density.compare(xy$Value, zona, xlab="Impacts")
# 
# ##calcular las areas debajo de curvas
# library(sfsmisc)
# integrate.xy()
# 
# 
# 


####################################################################################################################
