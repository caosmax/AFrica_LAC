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

###########################################################################################################
# Just Bean
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

#grafico teniendo en cuenta grupos
#a) recuperación,  b) recuperación más mejoramiento  y c) mejoramiento
pn <- which(mdwideBean_t$CC_No<0 & mdwideBean_t$CC_Tec>0)

nn <- which(mdwideBean_t$CC_No<0 & mdwideBean_t$CC_Tec<0) 
pp <- which(mdwideBean_t$CC_No>0 & mdwideBean_t$CC_Tec>0)
np <- which(mdwideBean_t$CC_No>0 & mdwideBean_t$CC_Tec<0)

alivio <-intersect(nn,which(mdwideBean_t$CC_No < mdwideBean_t$CC_Tec))
mejora <-intersect(pp, which(mdwideBean_t$CC_NoTec < mdwideBean_t$CC_Tec))
ali_mejora <- intersect(pn,which(mdwideBean_t$CC_No < mdwideBean_t$CC_Tec))


mdwideBean_t$trend<-NA

# vectores de posicion 
mdwideBean_t$trend[alivio] <- 'Alleviate'
mdwideBean_t$trend[mejora] <- 'Impulse'
mdwideBean_t$trend[ali_mejora] <- 'Alleviate+Impulse'

#loops ajuste inclusion de los paises Alivio-------------------
for(i in 1:nrow(mdwideBean_t)){
  
  if(i %in% alivio){
    mdwideBean_t$trend[i] <- "Alleviate"
    
  }
}


#loops ajuste inclusion de los paises mejora
for(i in 1:nrow(mdwideBean_t)){
  
  if(i %in% mejora){
    mdwideBean_t$trend[i] <- "Impulse"
  }
  
}

#loops ajuste inclusion de los paises alivio + mejora
for(i in 1:nrow(mdwideBean_t)){
  
  if(i %in% ali_mejora){
    mdwideBean_t$trend[i] <- "Alleviate+Impulse"
  }
  
}

# guardar base de datos correcta
write.csv(mdwideBean_t, paste(copy,"dataAllBean.csv", sep = ""), row.names = FALSE)

# Para futuros casos iniciar desde aquí
mdwideBean_t<- read.csv("dataAllBean.csv")

# grafica de impactos de rendimientos------------------

mdwideBean_t <- mdwideBean_t %>% gather( Intervencion, Value, CC_NoTec:impacto)
Beancluster<- mdwideBean_t #para hacer el cluster

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

# PerCapKCalCXAgg plot
gg5 <- ggplot(data=mdwideBean_t[mdwideBean_t$Intervencion!='impacto' & 
                                  mdwideBean_t$impactparameter=='PerCapKCalCXAgg',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg5 <- gg5 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg5 <- gg5 + coord_equal()
gg5 <- gg5 + ggtitle('PerCapKCalCXAgg') + ylab('') + xlab('Countries')
gg5 <- gg5 + theme_bw()
gg5 <- gg5 + theme(axis.text.x=element_text(size=10, angle=90))
gg5 <- gg5 + theme(axis.text.y=element_text(size=10))
gg5 <- gg5 + theme(axis.title.x=element_text(size=12, face='bold'))
gg5 <- gg5 + theme(plot.title=element_text(size=15, face = 'bold'))
gg5 <- gg5 + theme(legend.text = element_text(size=10))
gg5 <- gg5 + theme(legend.title = element_text(size=10, face = 'bold'))
gg5


# FoodAva
gg6 <- ggplot(data=mdwideBean_t[mdwideBean_t$Intervencion!='impacto' & 
                                  mdwideBean_t$impactparameter=='FoodAva',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg6 <- gg6 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg6 <- gg6 + coord_equal()
gg6 <- gg6 + ggtitle('FoodAva') + ylab('') + xlab('Countries')
gg6 <- gg6 + theme_bw()
gg6 <- gg6 + theme(axis.text.x=element_text(size=10, angle=90))
gg6 <- gg6 + theme(axis.text.y=element_text(size=10))
gg6 <- gg6 + theme(axis.title.x=element_text(size=12, face='bold'))
gg6 <- gg6 + theme(plot.title=element_text(size=15, face = 'bold'))
gg6 <- gg6 + theme(legend.text = element_text(size=10))
gg6 <- gg6 + theme(legend.title = element_text(size=10, face = 'bold'))
gg6

grid.arrange(gg, gg1, gg2, gg3, gg4, gg5, gg6)

g <- arrangeGrob(gg, gg2, gg5, nrow=3) #generates g
h <- arrangeGrob(gg1, gg4, gg3, gg6, nrow=4) #generates g
ggsave(file=paste(grd,"graph1.png", sep=''),g, width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"graph2.png", sep=''),h, width=10, height=10.5, units='in') 


#impactoDF <- subset(mdwideBean_t, subset=mdwideBean_t$Intervencion=='impacto')
impactoDF<- mdwideBean_t
impactoDF<- impactoDF[which(impactoDF$Intervencion=="impacto"),]
impactList <- as.character(unique(impactoDF$impactparameter))
ggList <- lapply(1:length(impactList), function(i){
  
  df <- subset(impactoDF, impactoDF$impactparameter==impactList[i])
  df$region <- as.character(df$region)
  df$trend <- as.character(df$trend)
  df <- df[complete.cases(df),]  # borrar filas con NA
  df$region <- factor(df$region, levels=df$region[order(df$Value)])  # ordenar las regiones por valor de los impactos Value
  
  gg <- ggplot(df, aes(x=region, y=Value, fill=trend)) + theme_bw() + geom_bar(stat="identity") + coord_flip()
  gg <- gg + ggtitle(impactList[i]) + xlab('Countries') + ylab('Relative differences')
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.text.y=element_text(size=12))
  #gg <- gg + theme(axis.text.y=element_text(size=12))
  gg <- gg + theme(axis.title.x=element_text(size=12, face='bold'))
  gg <- gg + theme(plot.title=element_text(size=15, face = 'bold'))
  # gg <- gg + scale_colour_gradient(name='Relative differences', low="cadetblue1", high="dodgerblue3")
  # gg <- gg + scale_fill_gradient(name='Relative differences', low="cadetblue1", high="dodgerblue3")
  
  return(gg)
  
})

grid.arrange(ggList[[1]], ggList[[2]], ggList[[3]], ggList[[4]], ggList[[5]], ggList[[6]], ggList[[7]])

gx <- arrangeGrob(ggList[[6]], ggList[[4]], ggList[[5]],  nrow=3) #generates g
hx <- arrangeGrob(ggList[[1]], ggList[[2]], ggList[[3]], nrow=3) #generates g

# graficos
ggsave(file=paste(grd,"graph3.png", sep=''),gx, width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"graph4.png", sep=''),hx, width=10, height=10.5, units='in') 
# graficos por variables 
ggsave(file=paste(grd,"production.png", sep=''),ggList[[6]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"imports.png", sep=''),ggList[[4]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"percapcal.png", sep=''),ggList[[5]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"yield.png", sep=''),ggList[[7]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"area.png", sep=''),ggList[[1]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"exports.png", sep=''),ggList[[2]], width=10, height=10.5, units='in') 
ggsave(file=paste(grd,"foodava.png", sep=''),ggList[[3]], width=10, height=10.5, units='in') 


## Cluster-----------------------

#Beancluster <- na.omit(mdwideBean_t) 

Beancluster <- Beancluster[Beancluster$Intervencion=='impacto',] %>% spread(impactparameter, Value)

#just area & Yield

Beancluster$Export <- NULL
Beancluster$Import <- NULL
#Beancluster$FoodAva<- NULL
Beancluster$Production<- NULL
Beancluster$PerCapKCalCXAgg<- NULL

#Beancluster<- Beancluster[which(Beancluster$productiontype!="-"),]
Beancluster <- Beancluster[complete.cases(Beancluster),]

#Analisis de cluster-----

x <- as.matrix(scale(Beancluster[,5:6]))
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



tiff(filename=paste(grd,"clusterRred.tiff",sep=""), 
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
write.table(data, paste(copy,"dataVersion2.txt", sep = ""), sep="\t")
write.table(Beancluster, paste(copy,"datacompleteALC.txt", sep=""), sep="\t")






