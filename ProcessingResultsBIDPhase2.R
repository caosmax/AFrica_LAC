#Reanalisis datos de BID, Phase 2 and comparision with phase1 results


#cargar librerias----
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(broom)


#Definir directorio de trabajo-------------
setwd("\\\\dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID")

#Dirección de graficos y tablas
initial<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase1/")
updated<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase2/")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/graphs/")


#Cargar marco de datos principal---------------
# md<-read.csv("Resultados_StCty_31_08_16_new.csv",header=T) # datos por regiones 
# s<- read.csv("Resultados_Ciat_StCty_31_08_16_new.csv", header=T) # datos por paises


#By countries
p1regions<- read.csv("Resultados_Ciat_StCty_31_08_16_new.csv")
p2regions<- read.csv("V1_allRegions.csv")

#By subRegions
p1BidRegions<- read.csv("Phase1/")
p2BidRegions<- read.csv("Phase2/V1_BIDRegions.csv")

#Hacer un subconjunto que sólo contenga las variables de mi interés--------------------
mdsub<-subset(md,md$impactparameter=="QSXAgg -- Total Production" | 
                    md$impactparameter=="TAreaXAgg -- Total Area" |
                    md$impactparameter== "QNXAgg -- Net Trade" | 
                    md$impactparameter== "QDXAgg -- Total Demand"|
                    md$impactparameter== "TYldXAgg -- Total Yield" |
                    md$impactparameter== "YldCliShkXAgg -- Climate Shock")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QDXAgg -- Total Demand"="Total Demand",
                                                        "QNXAgg -- Net Trade"="Net Trade",
                                                        "QSXAgg -- Total Production"="Total Production",
                                                        "TAreaXAgg -- Total Area"="Total Area",
                                                        "TYldXAgg -- Total Yield"= "Yield",
                                                        "YldCliShkXAgg -- Climate Shock"= "YClima"))


# reshape de la base  de datos.-------------------------
mdwide <- reshape(mdsub, v.names = "Val", idvar = 
                        c("scenario","commodity","region","productiontype",
                          "impactparameter"),timevar = "year", direction = "wide")


# calcular de nuevo cambio porcentual.---------------------
mdwide<-data.frame(mdwide,'Percentage_Change'=((mdwide$Val.2050-mdwide$Val.2020)/mdwide$Val.2020)*100)


mdwide$commodity<-revalue(mdwide$commodity, c("cbean"= "Bean",
                                              "cmaiz"="Maize",
                                              "crice"="Rice",
                                              "csoyb2"="Soybean",
                                              "cwhea"="Wheat",
                                              "jbean"="Bean",
                                              "jmaize"="Maize",
                                              "jrice"="Rice",
                                              "jsoyb2"="Soybean",
                                              "jwhea"="Wheat"))

# eliminar commodities 
mdwide1<-mdwide

#levels(mdwide1$commodity)
mdwide1<-subset(mdwide1,mdwide1$commodity!="csoyb")
mdwide1<-subset(mdwide1,mdwide1$commodity!="jsbnt")
mdwide1<-subset(mdwide1,mdwide1$commodity!="jsoyb")
mdwide1<-subset(mdwide1,mdwide1$commodity!="csbnt")
rownames(mdwide1) <- 1:nrow(mdwide1)

mdwide1<- as.data.frame(mdwide1)
mdwide1$commodity <- as.character(mdwide1$commodity)
mdwide1$impactparameter <- as.character(mdwide1$impactparameter)
mdwide1$region <- as.character(mdwide1$region)
mdwide1_change <- mdwide1[,-(5:51)] 

# variable <- factor(x = variable, levels = c('LAC', 'CEN', 'BRA'), ordered = TRUE)
# DF <- DF[DF$variable!='BRA',]
# levels(DF$variable) <- c('LAC', 'CEN')
# DF$variable <- as.character(DF$variable)
# DF$variable <- as.factor(DF$variable)

#Mediana de los cambios porcentuales por categorias. -------
gcmList<- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2",
            "gfld_esm2g","inm_cm4","ipsl_cm5a_lr",
            "miroc_miroc5","mpi_esm_mr","ncc_noresm1_m")
noccList<- c("NoCC")

mdwide1_change$GCM<- NA
mdwide1_change$GCM[mdwide1_change$scenario %in% gcmList] <- 'CC'
mdwide1_change$GCM[mdwide1_change$scenario %in% noccList] <- 'NoCC'


# calcular la media usando aggregate
dataagg<-aggregate(mdwide1_change[,"Percentage_Change"],
                   by=list(mdwide1_change$region,mdwide1_change$impactparameter,
                           mdwide1_change$commodity,mdwide1_change$GCM),
                   FUN=median)
write.csv(dataagg,paste(grd,"dataagg.csv",sep=""))
View(dataagg)


dataagg_diff<-ddply(dataagg,.(Group.1,Group.2,Group.3),summarize,d1=-diff(x,1))
write.csv(dataagg_diff,paste(grd,"dataagg_diff.csv",sep=""))
View(dataagg_diff)

dataagg2<-aggregate(mdwide1_change[,"Percentage_Change"],by=list(mdwide1_change$impactparameter,
                                                                 mdwide1_change$GCM),FUN=median)
View(ddply(dataagg2,.(Group.1),summarize,d1=-diff(x,1)))
View(dataagg2)
write.csv(dataagg2,paste(grd,"dataagg2.csv",sep=""))


#Tabla 7. documento-----
sub_rend_all<-droplevels(subset(mdwide1,mdwide1$scenario=="cccma_canesm2" |
                                      mdwide1$scenario=="ipsl_cm5a_lr" |
                                      mdwide1$scenario=="NoCC"))  
t5<-sub_rend_all[,-c(5:20)]
t5or<-cast(t5,region+impactparameter~commodity+scenario)

t5or<-droplevels(subset(t5or,t5or$Var!="Total Demand"  &
                              t5or$Var!="Net Trade"))
write.csv(t5or,paste(grd,"t5or.csv",sep=""))


#Sub conjunto de cultivos------

#cropsj<-c("jbean","jmaiz","jrice","jsoyb","jwhea")

#t_nt<-2020:2050

sub_rend_all2<-subset(mdwide1_change,mdwide1_change$Percentage_Change >=quantile(mdwide1_change$Percentage_Change, c(0.01)) &
                            mdwide1_change$Percentage_Change <=quantile(mdwide1_change$Percentage_Change, c(0.94)))

sub_rend_all2<-subset(mdwide1_change,mdwide1_change$Percentage_Change >quantile(mdwide1_change$Percentage_Change, c(0.01)) &
                            mdwide1_change$Percentage_Change <quantile(mdwide1_change$Percentage_Change, c(0.94)))

sub_rend_all2_datag<-aggregate(sub_rend_all2[,"Percentage_Change"],
                               by=list(sub_rend_all2$impactparameter,sub_rend_all2$GCM),
                               FUN=median)
View(sub_rend_all2_datag)
write.csv(sub_rend_all2_datag,paste(grd,"sub_rend_all2.csv",sep=""))


#Grafico 6. todas las variables------


tiff(filename=paste(grd,"all_crops_all_variables_all_GCMs.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)
ggplot(mdwide1_change,aes(x=GCM,y=Percentage_Change,color=impactparameter))  +
      facet_wrap( ~ impactparameter,ncol=5,shrink=T) +
      geom_boxplot(alpha=0.4,stat="boxplot") +
      labs(y="Percentage change",x="") +
      coord_cartesian(ylim = c(-100, 200)) + 
      scale_y_continuous( breaks=seq(-100, 200, 20)) + theme(legend.position="none")

dev.off()


pl<-NULL

# for (i in 1:length(unique(mdwide1_change$commodity))) {
#   
#   tiff(filename=paste(grd,levels(mdwide1_change$commodity)[i],"_all_variables.tiff",sep=""), 
#        width = 10, height = 7, units = 'in', res = 300)
#   
#   pl[[i]]<-print(ggplot(data=droplevels(subset(mdwide1_change,mdwide1_change$commodity ==levels(mdwide1_change$commodity)[i]
#                                                & mdwide1_change$impactparameter!="Net Trade" 
#                                                & mdwide1_change$GCM!="NoCC")),
#                         aes(x=region,y=Percentage_Change,color=impactparameter))  +
#                    facet_wrap( ~ impactparameter,ncol=4,shrink=T) +
#                    geom_point(aes(shape=GCM),data=droplevels(subset(mdwide1_change,mdwide1_change$commodity==levels(mdwide1_change$commodity)[i]
#                                                                     & mdwide1_change$impactparameter!="Net Trade" 
#                                                                     & mdwide1_change$GCM=="NoCC")),
#                               alpha = 0.4,size=3) +
#                    geom_boxplot(alpha=0.4,stat="boxplot",outlier.shape = NA) +
#                    guides(color=FALSE) + theme(legend.position="bottom",legend.title=element_blank())+
#                    labs(y="Percentage change",x="")
#   )
#   
#   
#   dev.off()
#   print(i)
# }              

for(i in 1:length(unique(mdwide1_change$commodity))){
      
      tiff(filename=paste(grd,unique(mdwide1_change$commodity)[i],"_all_variables2.tiff",sep=""), 
           width = 10, height = 7, units = 'in', res = 300)
      
      mdwide1_change_commodity <- subset(mdwide1_change, mdwide1_change$commodity==unique(mdwide1_change$commodity)[i]
                                         & mdwide1_change$impactparameter!="Net Trade" 
                                         & mdwide1_change$GCM!="NoCC")
      rownames(mdwide1_change_commodity) <- 1:nrow(mdwide1_change_commodity)
      mdwide1_change_commodity$impactparameter <- factor(x=mdwide1_change_commodity$impactparameter, levels=unique(as.character(mdwide1_change_commodity$impactparameter)), ordered=F)
      
      data_point <- droplevels(subset(mdwide1_change, mdwide1_change$commodity==unique(mdwide1_change$commodity)[i]
                                      & mdwide1_change$impactparameter!="Net Trade" 
                                      & mdwide1_change$GCM=="NoCC"))
      data_point$impactparameter <-factor(x=data_point$impactparameter, levels = unique(data_point$impactparameter))
      
      pl[[i]] <- print( ggplot(data=mdwide1_change_commodity, aes(x=as.factor(region), y=Percentage_Change, color=impactparameter)) +
                              facet_wrap( ~ impactparameter, ncol=4, shrink=T) +
                              geom_point(aes(shape=GCM),data=data_point, alpha = 0.4,size=3) +
                              geom_boxplot(alpha=0.4,stat="boxplot",outlier.shape = NA) +
                              guides(color=FALSE) + theme(legend.position="bottom",legend.title=element_blank()) +
                              labs(y="Percentage change",x="") )
      
      dev.off()
      print(i)
      
}




#Evolution of production by crop and by region in the NOcc------
datos<- mdwide1
# reemplazar los val.year por valor
datos<-droplevels(subset(mdwide1,mdwide1$impactparameter==unique(mdwide1$impactparameter)[2] 
                         & mdwide1$scenario=="NoCC"))
datos<-datos[,-which(names(datos)=="Percentage_Change")]
colnames(datos)[6:51] <- paste(2005:2050, sep='')
datos <- datos[,-c(6:20)]

base2020 <- datos[,"2020"]
for(i in 1:length(paste(2020:2050))) {
      datos[,paste(2020:2050)[i]] <- datos[,paste(2020:2050)[i]]/base2020
}
datos <- datos[,-6]

datos_long<-reshape(datos,varying=paste(2021:2050,sep=""),idvar=c("region","impactparameter","scenario","commodity")
                    ,direction="long",v.names="Index_of_Production",times=2021:2050)
rownames(datos_long) <- 1:nrow(datos_long)


tiff(filename=paste(grd,"Evolution of production for all crops and all regions.tiff"),width = 10,
     height = 12, units = 'in', res = 300)
ggplot(datos_long, aes(time, Index_of_Production, colour=commodity)) + 
      geom_line(size=1,linetype=6) + facet_wrap(~region,nrow=5) +
      theme(legend.position="bottom",legend.text=element_text(size=12),
            legend.key.size = unit(1, "cm"),text = element_text(size=12))+
      labs(y="Index of production", x="Year") +
      scale_colour_brewer(palette="Set1",name = "")
dev.off()



#Evolution of Net Trade by crop and by region--------
datosnocc<-droplevels(subset(mdwide1,mdwide1$impactparameter==unique(mdwide1$impactparameter)[3] 
                             & mdwide1$scenario=="NoCC"))
colnames(datosnocc)[6:51] <- paste(2005:2050, sep='')
datosnocc<-datosnocc[,-which(names(datosnocc)=="Percentage_Change")]
datosnocc <- datosnocc[,-c(6:20)]


datosnocc<-reshape(datosnocc,varying=paste(2020:2050,sep=""),idvar=c("region","impactparameter","scenario","commodity")
                   ,direction="long",v.names="Net_trade",times=2020:2050)
rownames(datosnocc) <- 1:nrow(datosnocc)

datosnocc<-datosnocc[,-c(which(names(datosnocc)=="impactparameter"),which(names(datosnocc)=="scenario"),
                         which(names(datosnocc)=="productiontype"))]

### grafico de Arroz Mexico

tiff(filename=paste(grd,"Net-Trade MEX.tiff"),width = 10,
     height = 12, units = 'in', res = 300)
mex<- ggplot(datosnocc[which(datosnocc$commodity=="Rice" & datosnocc$region=="MEX"),], aes(time,Net_trade,group=region,color=region)) 
mex <- mex + geom_line(linetype="dashed",size=1)
mex
dev.off()



### otro proceso
datos<-droplevels(subset(mdwide1,mdwide1$impactparameter==unique(mdwide1$impactparameter)[3]))
colnames(datos)[6:51] <- paste(2005:2050, sep='')
datos<-datos[,-which(names(datos)=="Percentage_Change")]
datos <- datos[,-c(6:20)]


datmin<-aggregate(datos[,paste(2020:2050,sep="")],by=list(datos$region,datos$commodity),FUN=min)

datmin<-reshape(datmin,varying = paste(2020:2050,sep=""),idvar = c("Group.1","Group.2"),
                direction="long",v.names = "datmin",times=2020:2050)
names(datmin)<-c("region","commodity","time","datmin")
rownames(datmin) <- 1:nrow(datmin)



datmax<-aggregate(datos[,paste(2020:2050,sep="")],by=list(datos$region,datos$commodity),FUN=max)
datmax<-reshape(datmax,varying = paste(2020:2050,sep=""),idvar = c("Group.1","Group.2"),
                direction="long",v.names = "datmax",times=2020:2050)
names(datmax)<-c("region","commodity","time","datmax")
rownames(datmax) <- 1:nrow(datmax)


extremos<-merge(datmin,datmax)

datost<-merge(datosnocc,extremos)

py<-NULL
for (i in 1:length(unique(datost$commodity))) {
      
      tiff(filename=paste(grd,unique(datost$commodity)[i],"_net_trade.tiff",sep=""), 
           width = 10, height = 7, units = 'in', res = 300)
      
      
      py[[i]]<-print(ggplot(data=droplevels(subset(datost,datost$commodity==unique(datost$commodity)[i])),
                            aes(time,Net_trade,group=region,color=region)) + 
                           geom_line(linetype="dashed",size=1)+
                           geom_ribbon(aes(ymin=datmin,ymax=datmax,fill=region,colour=region,linetype=NA),alpha=0.1)+
                           labs(y="Net trade (000 mt)",x="Year")+
                           theme(legend.position="bottom")
                     
      )
      
      
      dev.off()
      print(i)
}  


#Analisis para variables relacionadas con la seguridad alimentaria a nivel de país------

#Población en riesgo de padecer hambre
mdsubfs_pop<-subset(md,md$impactparameter=="PopulationAtRiskXagg - Pop at Risk of Hunger")

#Hacer un subconjunto de md que sólo contenga los paises de LAC
# lac_reg_fs<-levels(mdsubfs_pop$region)[68:92]
# mdsubfs_pop_lac<-mdsubfs_pop[which(mdsubfs_pop$region %in% lac_reg_fs),]
# mdsubfs_pop_lac<-droplevels(mdsubfs_pop_lac)
mdwidefs_pop <- reshape(mdsubfs_pop, v.names = "Val", idvar = 
                              c("scenario","commodity","region","productiontype","impactparameter"),
                        timevar = "year", direction = "wide")


#Regiones a analizar
# pop_and<-subset(mdwidefs_pop,mdwidefs_pop$region %in% and_p )
# pop_and<-droplevels(pop_and)
# 
# pop_mex<-subset(mdwidefs_pop,mdwidefs_pop$region=="LAC-Mexico")
# pop_mex<-droplevels(pop_mex)
# 
# pop_bra<-subset(mdwidefs_pop,mdwidefs_pop$region=="LAC-Brazil" | mdwidefs_pop$region=="LAC-Guyanas")
# pop_bra<-droplevels(pop_bra)
# 
# pop_csur<-subset(mdwidefs_pop,mdwidefs_pop$region %in% csur_p)
# pop_csur<-droplevels(pop_csur)
# 
# pop_cen<-subset(mdwidefs_pop,mdwidefs_pop$region %in% cen_p)
# pop_cen<-droplevels(pop_cen)
# 
# #Agregar por variables para las regiones a analizar
# pop_and<-aggregate(pop_and[,paste("Val.",seq(from=2020,to=2050,by=5),sep='')],
#                    by=list(pop_and$impactparameter,pop_and$scenario),FUN=sum)
# 
# pop_and<-data.frame("Region"=rep("AND",dim(pop_and)[1]),pop_and,
#                     'Change'=(pop_and$Val.2050-pop_and$Val.2020))
# 
# names(pop_and)<-c("Region","Pop","GCM",seq(from=2020,to=2050,by=5),"Change")
# 
# pop_mex<-aggregate(pop_mex[,paste("Val.",seq(from=2020,to=2050,by=5),sep='')],
#                    by=list(pop_mex$impactparameter,pop_mex$scenario),FUN=sum)
# 
# pop_mex<-data.frame("Region"=rep("MEX",dim(pop_mex)[1]),pop_mex,
#                     'Change'=(pop_mex$Val.2050-pop_mex$Val.2020))
# 
# names(pop_mex)<-c("Region","Pop","GCM",seq(from=2020,to=2050,by=5),"Change")
# 
# 
# pop_csur<-aggregate(pop_csur[,paste("Val.",seq(from=2020,to=2050,by=5),sep='')],
#                     by=list(pop_csur$impactparameter,pop_csur$scenario),FUN=sum)
# 
# pop_csur<-data.frame("Region"=rep("SUR",dim(pop_csur)[1]),pop_csur,
#                      'Change'=(pop_csur$Val.2050-pop_csur$Val.2020))
# 
# names(pop_csur)<-c("Region","Pop","GCM",seq(from=2020,to=2050,by=5),"Change")
# 
# 
# 
# pop_bra<-aggregate(pop_bra[,paste("Val.",seq(from=2020,to=2050,by=5),sep='')],
#                    by=list(pop_bra$impactparameter,pop_bra$scenario),FUN=sum)
# 
# pop_bra<-data.frame("Region"=rep("BRA",dim(pop_bra)[1]),pop_bra,
#                     'Change'=(pop_bra$Val.2050-pop_bra$Val.2020))
# 
# names(pop_bra)<-c("Region","Pop","GCM",seq(from=2020,to=2050,by=5),"Change")
# 
# 
# 
# pop_cen<-aggregate(pop_cen[,paste("Val.",seq(from=2020,to=2050,by=5),sep='')],
#                    by=list(pop_cen$impactparameter,pop_cen$scenario),FUN=sum)
# 
# pop_cen<-data.frame("Region"=rep("CEN",dim(pop_cen)[1]),pop_cen,
#                     'Change'=(pop_cen$Val.2050-pop_cen$Val.2020))
# 
# names(pop_cen)<-c("Region","Pop","GCM",seq(from=2020,to=2050,by=5),"Change")
# 
# 

#Juntar todos los marcos de datos
#pop_all<-rbind(pop_and,pop_bra,pop_cen,pop_mex,pop_csur)

#pop_all<-data.frame(pop_all,"Cat"=ifelse(pop_all$GCM=="NoCC","NoCC","CC"))

gcmList<- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2",
            "gfld_esm2g","inm_cm4","ipsl_cm5a_lr",
            "miroc_miroc5","mpi_esm_mr","ncc_noresm1_m")
noccList<- c("NoCC")

mdwidefs_pop$GCM<- NA
mdwidefs_pop$GCM[mdwidefs_pop$scenario %in% gcmList] <- 'CC'
mdwidefs_pop$GCM[mdwidefs_pop$scenario %in% noccList] <- 'NoCC'

mdwidefs_pop$Change <- (mdwidefs_pop$Val.2050-mdwidefs_pop$Val.2020)

pop_all2<-data.frame(aggregate(mdwidefs_pop[,c("Change")],
                               by=list(mdwidefs_pop$region,mdwidefs_pop$GCM),FUN=median))
pop_all2$x<-pop_all2$x*1000000
pop_all2 <- transform(pop_all2,Group.1 = reorder(Group.1, x))

tiff(filename=paste(grd,"Impact of CC (in the median GCM) in FS.tiff"), 
     width = 9, height = 9, units = 'in', res = 300)
ggplot(pop_all2, aes(Group.1, x, fill = Group.2)) + 
      geom_bar(stat="identity", position = "dodge") +
      scale_fill_brewer(palette = "Set1") + 
      coord_flip()+
      labs(x="Region",y="Change in the number of people at risk of hunger between 2020 and 2050")
dev.off()

# write.csv(pop_all,paste(grd,"datos_pop.csv",sep=""))



#Difference between the reduction of the people under risk of hunger in the median 
# GCM and the NoCC scenario
nocc<-subset(pop_all2,pop_all2$Group.2=="NoCC")
cc<-subset(pop_all2,pop_all2$Group.2=="CC")
f1<-function(nocc,cc) {data.frame("Region"=nocc[,1],"Diff_nocc_cc"=(nocc[,3]-cc[,3]))}
gr<-f1(nocc,cc)
gr <- transform(gr,Region = reorder(Region, Diff_nocc_cc))


dif<-ddply(pop_all2,.(Group.1),summarize,d1=-diff(x,1))
options(scipen=999)
tiff(filename=paste(grd,"Difference between the reduction of people under risk of hunger NoCC y CC.tiff"), 
     width = 9, height = 6, units = 'in', res = 300)
ggplot(gr, aes(Region, Diff_nocc_cc)) + geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + coord_flip() + 
      labs(x="Region",y="Difference between the change of people under risk of hunger in NoCC and the median GCM")
dev.off()

#Analisis de precios
md_prices<-read.csv(file="Resultados_Prices_12_02_15.csv",header=T)
md_prices_w<-subset(md_prices,md_prices$impactparameter=="PWXAgg -- World Prices")
cropsc<-c("cbean","cmaiz","crice","csoyb","cwhea")
md_prices<-md_prices_w[which(md_prices_w$commodity %in% cropsc),]
md_prices<-droplevels(md_prices)
md_prices <- reshape(md_prices, v.names = "Val", 
                     idvar = c("scenario","commodity","region","productiontype","impactparameter"),
                     timevar = "year", direction = "wide")

md_prices<-data.frame(md_prices,'Percentage_change'=((md_prices$Val.2050-md_prices$Val.2020)/
                                                           md_prices$Val.2020)*100)

md_prices<-md_prices[,c(1:3,52)]
md_prices$commodity<-revalue(md_prices$commodity, c("cbean"="Dry bean",
                                                    "cmaiz"="Maize",
                                                    "crice"="Rice",
                                                    "csoyb"="Soybean",
                                                    "cwhea"="Wheat"))
md_prices$impactparameter<-revalue(md_prices$impactparameter,
                                   c("PWXAgg -- World Prices"="World Prices"))

#write.csv(md_prices,paste(grd,"world_prices.csv",sep=""))

#Tabla documento
tpor<-cast(md_prices,impactparameter+commodity~scenario)
write.csv(tpor,paste(grd,"tpor.csv",sep=""))


for (c in 1:length(levels(rend_all$Crop))) {
      
      tiff(filename=paste(grd,"Evolution of ",levels(rend_all$Var)[2]," of ",levels(rend_all$Crop)[c],".tiff",sep=""),
           width = 9,height = 7, units = 'in', res = 300)
      
      datos<-droplevels(subset(rend_all,rend_all$Var==levels(rend_all$Var)[2] 
                               & rend_all$Crop==levels(rend_all$Crop)[c]))
      
      par(mfrow=c(2,3))
      for (r in 1:length(levels(datos$Region))) {
            plot(t_nt,as.matrix(subset(datos,datos$Region==levels(datos$Region)[r])[10,5:35]),xlab=levels(rend_all$Region)[r],ylab="000 mt",
                 ylim=c(min(subset(datos,datos$Region==levels(datos$Region)[r])[,5:35]),
                        max(subset(datos,datos$Region==levels(datos$Region)[r])[,5:35])),type="l",col=1,lwd=2)
            
            for (h in 1:(dim(subset(datos,datos$Region==levels(datos$Region)[r]))[1]-1)) {
                  lines(t_nt,subset(datos,datos$Region==levels(datos$Region)[r])[h,5:35],col=h+1,lwd=1.5,lty=(h+1))
            }
      }
      par(mfrow=c(1,1))
      
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      legend(x=0.5,y=-0.2,legend=levels(rend_all$GCM),lty=c(2:10,1),col=c(2:10,1),lwd=c(rep(1,9),2))
      mtext(paste("Evolution of",levels(rend_all$Var)[2]," of ",levels(rend_all$Crop)[c]),
            side = 3, line = -2, outer = TRUE,cex=0.9) 
      print(c)
      dev.off()
}

# Share at Risk of Hunger
mdsubfs_risk<-subset(md,md$impactparameter=="ShareAtRiskXagg -- Share at Risk of Hunger")
mdsubfs_risk<-droplevels(mdsubfs_risk)

#Hacer un subconjunto de md que sólo contenga los paises de LAC
lac_reg_fs_risk<-levels(mdsubfs_risk$region)[63:85]

mdsubfs_risk_lac<-mdsubfs_risk[which(mdsubfs_risk$region %in% lac_reg_fs_risk),]

mdsubfs_risk_lac<-droplevels(mdsubfs_risk_lac)

mdwidefs_risk <- reshape(mdsubfs_risk_lac, v.names = "Val", idvar = 
                               c("scenario","commodity","region","productiontype","impactparameter"),
                         timevar = "year", direction = "wide")

mdwidefs_risk<-subset(mdwidefs_risk,mdwidefs_risk$region !="LAC")

mdwidefs_risk$region<-revalue(mdwidefs_risk$region, c("LAC-Argentina"="Argentina",
                                                      "LAC-Belize"="Belize","LAC-Bolivia"="Bolivia",
                                                      "LAC-Brazil"="Brazil","LAC-Chile"="Chile",
                                                      "LAC-Colombia"="Colombia",
                                                      "LAC-Costa Rica"="Costa Rica",
                                                      "LAC-Cuba"="Cuba",
                                                      "LAC-Dominican Republic"="Dominican Republic",
                                                      "LAC-Ecuador"="Ecuador",
                                                      "LAC-El Salvador"="El Salvador",
                                                      "LAC-Guatemala"="Guatemala",
                                                      "LAC-Guyanas"="Guyanas",
                                                      "LAC-Haiti"="Haiti",
                                                      "LAC-Honduras"="Honduras",
                                                      "LAC-Mexico"="Mexico",
                                                      "LAC-Nicaragua"="Nicaragua",
                                                      "LAC-Panama"="Panama",
                                                      "LAC-Paraguay"="Paraguay",
                                                      "LAC-Peru"="Peru",
                                                      "LAC-Uruguay"="Uruguay",
                                                      "LAC-Venezuela"="Venezuela"))

summary(mdwidefs_risk)

mdwidefs2_risk<-data.frame(mdwidefs_risk,"Cat"=ifelse(mdwidefs_risk$scenario=="NoCC","NoCC","CC"))

datos_risk<-data.frame(aggregate(mdwidefs2_risk[,c("Val.2020","Val.2050")],
                                 by=list(mdwidefs2_risk$region,mdwidefs2_risk$Cat),FUN=median))
datos_risk<-data.frame(datos_risk,"Change"=datos_risk$Val.2050-datos_risk$Val.2020)
datos_risk <- transform(datos_risk,Group.1 = reorder(Group.1, Val.2050 ))


tiff(filename=paste(grd,"Impact of CC (in the median GCM) in percent people at risk of hunger.tiff"), 
     width = 9, height = 6, units = 'in', res = 300)
ggplot(datos_risk, aes(Group.1,Val.2050, fill = Group.2)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + coord_flip() + 
      ylab("Share of population at risk of hunger in 2050 (%)") +
      xlab("Country") + theme(legend.title = element_blank())
dev.off()



