#Programa para analizar los datos de evaluación de frijol con y sin tecnologia-----
#bajo distintos escenarios climaticos
#Por:  por Harold y Carlos Edo

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

#Limitar numero de decimales-----
options(digits=3) 

#Definir directorios de trabajo y de graficos------
#Directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/")
#Dirrectorio de graficos
grd<-"C:/Users/CEGONZALEZ/Documents/GFSF/BeanGFSF/graph/"
# Directorio de copias
copy<- "C:/Users/CEGONZALEZ/Documents/GFSF/BeanGFSF/copyData/"

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
                md$impactparameter=="TAreaXAgg -- Total Area" |
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


#rownames(mdsub) <- 1:nrow(mdsub)
mdsubcrop<-droplevels(subset(mdsub,mdsub$commodity=="PUL-Beans" | mdsub$commodity=="-"))
mdsubcrop$commodity<-revalue(mdsubcrop$commodity,c("PUL-Beans"="Beans"))

lac_africa_reg<-levels(mdsubcrop$region)[c(68:95,101:103,121,127:161)]
mdsubcrop_lac_afri<-mdsubcrop[which(mdsubcrop$region %in% lac_africa_reg),]
mdsubcrop_lac_afri<-droplevels(mdsubcrop_lac_afri)
# d1<- d1[which(d1$commodity=="PUL-Beans" | d1$commodity=="-"),]
# d1<- droplevels(d1)
# d1$commodity<-revalue(d1$commodity,c("PUL-Beans"="Beans"))
# 
# 
# ## filtros por las regiones de ALC y Africa
# L <- d1[grep(pattern = '^LAC', x = d1$region, value = F, fixed = FALSE), ]
# #L$zona<- 'LAC'
# A <- d1[grep(pattern = "^SSA", x = d1$region, value = F, fixed = FALSE), ]
# #A$zona<- 'AFRICA'
# 
# ## paises que quedaron por fuera del analisis
# M<- d1[which(dbase1$region=="MEN-Algeria"|d1$region=="MEN-Egypt"| d1$region=="MEN-Morocco"|
#                d1$region=="MEN-Libya"|d1$region=="MEN-Mauritania" ),]
# #M$zona<- "AFRICA"
# 
# #Apliar
# data1 <- rbind(L,A,M) # todos los datos 
# data1<- droplevels(data1)
# rownames(data1) <- 1:nrow(data1)
# 
# # Eliminamos regiones grandes, solo para trabajar con paises, se podria mas adelate analizar estos.-----
# data1<-data1[-which(data1$region=="DVG"| data1$region=="LAC"| data1$region=="WLD" |
#                        data1$region=="MEN"| data1$region=="SAS"| data1$region=="FSU" | data1$region=="EAS" |
#                        data1$region=="EUR"| data1$region=="DVD" | data1$region=="EAP" | data1$region=="SSA"|
#                        data1$region=="NAM"), ]
# 
# 


mdwideBean<-mdsubcrop_lac_afri %>%
  spread("year", "Val")


#Calcular cambio porcentual entre 2020 y 2050----------------
mdwideBean<- data.frame(mdwideBean,"Cambio Porcentual"=( (mdwideBean$`2050`-mdwideBean$`2020`)/mdwideBean$`2020`)*100)
mdwideBean<-data.frame(mdwideBean[,1:6],"Cambio Porcentual"=mdwideBean[,13]) # eliminamos las columnas innecesarias 

#Eliminar datos agrupados innecesarios
mdwideBean<-mdwideBean[!mdwideBean$region=="LAC",]  
mdwideBean<-mdwideBean[!mdwideBean$region=="MEN",]
mdwideBean<-mdwideBean[!mdwideBean$region=="SSA",]

# Eliminar datos que no adoptaron tecnología -----------------
mdwideBean<-mdwideBean[!mdwideBean$region=="LAC-Cuba",]
mdwideBean<-mdwideBean[!mdwideBean$region=="LAC-Jamaica",]
mdwideBean<-mdwideBean[!mdwideBean$region=="MEN-Egypt",]
mdwideBean<-mdwideBean[!mdwideBean$region=="LAC-Other Caribbean",]
mdwideBean<-mdwideBean[!mdwideBean$region=="LAC-Haiti",]
mdwideBean<-mdwideBean[!mdwideBean$region=="LAC-Dominican Republic",]

# importante para renombrar las columnas
rownames(mdwideBean) <- 1:nrow(mdwideBean)

#mdwide$categoria <- NULL
mdwideBean<- droplevels(mdwideBean)
mdwideBean$categoria <- as.character(mdwideBean$categoria)
testBean <- mdwideBean
testBean <- testBean[-which(testBean$categoria=='NoCC_NoTec' | testBean$categoria=='NoCC_Tec'),]

#reshape
mdwideBean_t<-testBean %>%
  spread("scenario","Cambio.Porcentual")

mdwideBean_t$productiontype <- NULL # activado por primera vez
mdwideBean_t$CC_mean <- rowMeans(x=mdwideBean_t[,6:ncol(mdwideBean_t)], na.rm=TRUE)

# para mantener estas variables
mdwideBean_t <- mdwideBean_t[,c("impactparameter","commodity", "region", "categoria", "CC_mean")]

# reshape
mdwideBean_t <- mdwideBean_t %>% spread(categoria, CC_mean)

# logica en las variaciones  cambios y  diferencias relativas-----
nn <- which(mdwideBean_t$CC_No<0 & mdwideBean_t$CC_Tec<0) 
pp <- which(mdwideBean_t$CC_No>0 & mdwideBean_t$CC_Tec>0)
np <- which(mdwideBean_t$CC_No>0 & mdwideBean_t$CC_Tec<0)

nn <- intersect(nn, which(mdwideBean_t$CC_No > mdwideBean_t$CC_Tec))
pp <- intersect(pp, which(mdwideBean_t$CC_No > mdwideBean_t$CC_Tec))
np <- intersect(np, which(mdwideBean_t$CC_No > mdwideBean_t$CC_Tec))

noimpact <- c(nn, pp, np)
mdwideBean_t$impacto<-NA


#loops ajuste inclusion de los paises sin impacto
for(j in 1:nrow(mdwideBean_t)){
  
  if(j %in% noimpact){
    mdwideBean_t$impacto[j] <- (mdwideBean_t$CC_Tec[j] - mdwideBean_t$CC_NoTec[j])/mdwideBean_t$CC_NoTec[j]*100
  } else {
    mdwideBean_t$impacto[j] <- abs(mdwideBean_t$CC_Tec[j] - mdwideBean_t$CC_NoTec[j])/unlist(lapply(1:nrow(mdwideBean_t), function(i){z <- max(abs(mdwideBean_t$CC_Tec[i]), abs(mdwideBean_t$CC_NoTec[i]), na.rm=TRUE); return(z)})) * 100
  }
  
}

#max(abs(tanznet$X2050[j]), abs(tanznet$X2020[j]), na.rm=TRUE) * 100
#unlist(lapply(1:nrow(mdwideBean_t), function(i){z <- max(abs(mdwideBean_t$CC_Tec[i]), abs(mdwideBean_t$CC_NoTec[i]), na.rm=TRUE); return(z)})) * 100
mdwideBean_t <- mdwideBean_t %>% gather( Intervencion, Value, CC_NoTec:impacto)
Beancluster<- mdwideBean_t #para hacer el cluster
Beancluster <- Beancluster[Beancluster$Intervencion=='impacto',] %>% spread(impactparameter, Value)

Beancluster$Export <- NULL
Beancluster$Import <- NULL
Beancluster$Demand <- NULL

#Beancluster$FoodAva<- NULL
#Beancluster$Production<- NULL
#Beancluster$PerCapKCalCXAgg<- NULL
#Beancluster$productiontype<-NULL

mean2 <- function(x){mean(x, na.rm = T)}
Beancluster <- Beancluster %>% group_by(commodity, region, Intervencion) %>% summarise_each(funs(mean2))
Beancluster <- Beancluster[complete.cases(Beancluster),]


#Analisis de cluster-----

x <- as.matrix(scale(Beancluster[,4:6]))
rownames(x) <- Beancluster$region
hclustDF <- hclust(dist(x))
plot(hclustDF)
#Guardar el archivo
tiff(filename=paste(grd,"clusterBean.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)

plot(hclustDF)

dev.off()

#obtener estadisticas y mejorar graficos 
ctree <- cutree(hclustDF, k=7)
rect.hclust(hclustDF, k=7, border="red")



tiff(filename=paste(grd,"clusterRredFoodSecurity.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)

plot(hclustDF)
rect.hclust(hclustDF, k=7, border="red")

dev.off()

# 

Beancluster$Cluster <- NA
for(i in 1:nrow(Beancluster))
{
  Beancluster$Cluster[i] <- ctree[which(Beancluster$region[i]==names(ctree))]
}

library(dplyr)
data<- as.data.frame(summarise(group_by(Beancluster, Cluster), meanArea=mean(Area), meanYi=mean(Yield)))
write.table(data, paste(copy,"dataVersionCasa.txt", sep = ""), sep="\t")
write.table(Beancluster, paste(copy,"datacompleteCasa.txt", sep=""), sep="\t")




