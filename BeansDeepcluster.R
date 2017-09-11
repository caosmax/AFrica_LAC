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
                md$impactparameter== "QEXAgg -- Export" | 
                md$impactparameter== "QMXAgg -- Import" |
                md$impactparameter== "PerCapKCalCXAgg -- PcKcal by Commodity"|
                md$impactparameter== "FoodAvailXAgg")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QSXAgg -- Total Production"="Production",  
                                                        "TAreaXAgg -- Total Area"="Area",
                                                        "TYldXAgg -- Total Yield"="Yield",
                                                        "QEXAgg -- Export"="Export",
                                                        "QMXAgg -- Import"="Import",
                                                        "PerCapKCalCXAgg -- PcKcal by Commodity"= "PerCapKCalCXAgg",
                                                        "FoodAvailXAgg"="FoodAva"))

rownames(mdsub) <- 1:nrow(mdsub)

# Hacer un subgrupo sin nombre de subregiones, eliminando regiones grandes
d1<- mdsub  

# Eliminamos regiones grandes, solo para trabajar con paises, se podria mas adelate analizar estos.-----
dbase1<-d1[-which(d1$region=="DVG"| d1$region=="LAC"| d1$region=="WLD" |
                    d1$region=="MEN"| d1$region=="SAS"| d1$region=="FSU" | d1$region=="EAS" |
                    d1$region=="EUR"| d1$region=="DVD" | d1$region=="EAP" | d1$region=="SSA"|
                    d1$region=="NAM"), ]

## filtros por las regiones de ALC y Africa
L <- dbase1[grep(pattern = '^LAC', x = dbase1$region, value = F, fixed = FALSE), ]
L$zona<- 'LAC'
A <- dbase1[grep(pattern = "^SSA", x = dbase1$region, value = F, fixed = FALSE), ]
A$zona<- 'AFRICA'

## paises que quedaron por fuera del analisis
M<- dbase1[which(dbase1$region=="MEN-Algeria"|dbase1$region=="MEN-Egypt"| dbase1$region=="MEN-Morocco"|
                   dbase1$region=="MEN-Libya"|dbase1$region=="MEN-Mauritania" ),]
M$zona<- "AFRICA"


###Colapsar datos 
data1 <- rbind(L,A, M) # todos los datos 
rownames(data1) <- 1:nrow(data1)
data1$impactparameter<- as.character(data1$impactparameter)
data1$impactparameter<- as.factor(data1$impactparameter)
Bean<- data1
Bean<- Bean[which(Bean$commodity=="PUL-Beans" | Bean$commodity=="-"),]
Bean$commodity<-revalue(Bean$commodity,c("PUL-Beans"="Beans"))
Bean$commodity<- as.factor(Bean$commodity)
rownames(Bean) <- 1:nrow(Bean)


variables<- c("Area","Export","Import","PerCapKCalCXAgg","Production","Yield","FoodAva" )

mdwideBean<-Bean %>%
  spread("year", "Val")

#Calcular cambio porcentual entre 2020 y 2050----------------
mdwideBean<- data.frame(mdwideBean,"Cambio Porcentual"=( (mdwideBean$`2050`-mdwideBean$`2020`)/mdwideBean$`2020`)*100)
mdwideBean<-data.frame(mdwideBean[,1:7],"Cambio Porcentual"=mdwideBean[,14]) # eliminamos las columnas innecesarias 

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
mdwideBean$categoria <- as.character(mdwideBean$categoria)
testBean <- mdwideBean
testBean <- testBean[-which(testBean$categoria=='NoCC_NoTec' | testBean$categoria=='NoCC_Tec'),]
#reshape
mdwideBean_t<-testBean %>%
  spread("scenario","Cambio.Porcentual")

#mdwide_t$productiontype <- NULL
mdwideBean_t$CC_mean <- rowMeans(x=mdwideBean_t[,7:ncol(mdwideBean_t)], na.rm=TRUE)

# para mantener estas variables
mdwideBean_t <- mdwideBean_t[,c("impactparameter", "productiontype", "commodity", "region", "categoria", "CC_mean")]

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

mdwideBean_t <- mdwideBean_t %>% gather( Intervencion, Value, CC_NoTec:impacto)
Beancluster<- mdwideBean_t #para hacer el cluster
Beancluster <- Beancluster[Beancluster$Intervencion=='impacto',] %>% spread(impactparameter, Value)

#just area & Yield
Beancluster$productiontype<-NULL

# podria generar subsets por cada columna y luego hacer el merge  y luego 
f <- c( "commodity", "region", "Intervencion" , "FoodAva" )
food <- Beancluster[f] 
a<- c( "commodity", "region", "Intervencion" , "Area" )
area<- Beancluster[a]
y<- c( "commodity", "region", "Intervencion" , "Yield"  )
yield<- Beancluster[y]

Beancluster2 <- merge(food, area)#, by=c("commodity","region")
Beancluster3 <- merge (Beancluster2, yield)
# Beancluster$Export <- NULL
# Beancluster$Import <- NULL
# #Beancluster$FoodAva<- NULL
# Beancluster$Production<- NULL
# Beancluster$PerCapKCalCXAgg<- NULL
# Beancluster$productiontype<-NULL

#Beancluster<- Beancluster[which(Beancluster$productiontype!="-"),]
Beancluster3 <- Beancluster3[complete.cases(Beancluster3),]
Beancluster<- Beancluster3
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
write.table(data, paste(copy,"dataVersionfood.txt", sep = ""), sep="\t")
write.table(Beancluster, paste(copy,"datacompleteFood.txt", sep=""), sep="\t")



####### others graphs
w<- ggplot(Beancluster, aes(x=Beancluster$Area, 
                            y=Beancluster$Yield, 
                            color=Beancluster$Cluster)) +
  geom_boxplot(shape=20)
w<- w + geom_smooth(method='loess',se=TRUE)  
w<- w + theme_classic() + scale_x_continuous("Area")
w<- w + scale_y_continuous("Yield")
#w<- w + facet_wrap(~Cluster)
w


