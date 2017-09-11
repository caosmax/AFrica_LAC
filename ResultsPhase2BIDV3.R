#Programa para generar graficos proyecto BID-----
#cargar librerias----
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(broom)


#Definir directorio de trabajo
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID")

#Dirreción graficos
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/graphs/")
grp<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase2/Test/")

#Cargar marco de datos principal
md<-read.csv("Phase2/V3_allRegions.csv",header=T)

Parameters<- c("QSXAgg -- Total Production","TAreaXAgg -- Total Area", "QNXAgg -- Net Trade","QDXAgg -- Total Demand")
crops<- c("jbean", "jrice", "jwhea" , "cmaiz", "cs", "jmaiz", "js", "cbean", "crice", "cwhea")
#Hacer un subconjunto que sólo contenga las variables de mi interés y los 5 contenga los cinco cultivos analizados

mdsub<- filter(md, impactparameter %in% Parameters ) %>% filter(., commodity %in% crops)
mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QDXAgg -- Total Demand"="Total Demand",
                                                        "QNXAgg -- Net Trade"="Net Trade",
                                                        "QSXAgg -- Total Production"="Total Production",
                                                        "TAreaXAgg -- Total Area"="Total Area"))

mdsub$impactparameter<- as.character(mdsub$impactparameter)
mdsub$scenario<- as.character(mdsub$scenario)
mdsub$commodity<- as.character(mdsub$commodity)
mdsub$region<- as.character(mdsub$region)
mdsub$productiontype<- as.character(mdsub$productiontype)

#Hacer un subconjunto de md que sólo contenga los paises de LAC
alc<- mdsub[grep(pattern = "LAC",x = mdsub$region, ignore.case = T),]
alc<- alc %>% spread(year, Val)
alc<- alc[,-c(6:20)]

#lista de paises por regiones
and_p<-c("LAC-Bolivia","LAC-Colombia","LAC-Ecuador","LAC-Peru","LAC-Venezuela")
csur_p<-c("LAC-Argentina","LAC-Chile","LAC-Uruguay","LAC-Paraguay")
cen_p<-c("LAC-Belize","LAC-Other Caribbean","LAC-Costa Rica","LAC-Cuba",
         "LAC-Dominican Republic","LAC-Guatemala","LAC-Honduras","LAC-Haiti",
         "LAC-Jamaica","LAC-Nicaragua","LAC-Panama","LAC-El Salvador")

#Regiones a analizar
and<- filter(alc,region %in% and_p ) %>% mutate(., region="AND")
mex<- filter(alc,region=="LAC-Mexico")%>% mutate(., region="MEX")
bra<- filter(alc,region=="LAC-Brazil" | region=="LAC-Guyanas")%>% mutate(., region="BRA")
csur<-filter(alc,region %in% csur_p)%>% mutate(., region="SUR")
cen<- filter(alc,region %in% cen_p)%>% mutate(., region="CEN")
cfiles<- list(and,mex,bra,csur,cen)


for(i in 1:length(cfiles)){
      cfiles[[i]]<- aggregate( cfiles[[i]][,6:ncol(cfiles[[i]])],
                               by=list( cfiles[[i]]$impactparameter, cfiles[[i]]$scenario, cfiles[[i]]$commodity, cfiles[[i]]$region),FUN=sum)
      cfiles[[i]]$Percentage_Change<-((cfiles[[i]]$`2050`-cfiles[[i]]$`2020`)/cfiles[[i]]$`2020`)*100
      colnames(cfiles[[i]])[1]<-"Var"
      colnames(cfiles[[i]])[2]<-"GCM"
      colnames(cfiles[[i]])[3]<-"Crop"
      colnames(cfiles[[i]])[4]<-"Region"
      cfiles[[i]]<- cfiles[[i]][,c("Region","Var","GCM","Crop","Percentage_Change",paste(2020:2050,sep='')) ]
      print(i)
}


#Juntar todos los marcos de datos
var_all<- do.call(rbind,cfiles)

# facilmente los Rendimientos ponderados por regiones (Andes, CSUR, CEN, BRA, MEX)
alc<- alc %>% gather(year, Val, 6:ncol(alc))
mdsubcast<- cast(alc,scenario+commodity+region+year~impactparameter)       

#Borrar la variable QNXAgg -- Net Trade
mdsubcast<- mdsubcast[,-c(5,7)]

#Borrar los cultivos que inicien con c elo estan asociados con la variable que acabo de borrar
jcrops<-c ("jbean","jmaiz","jwhea","js","jrice")
mdsubcast<- mdsubcast[which(mdsubcast$commodity %in% jcrops),]


#Calcular rendimientos de las regiones a analizar
ren_and<- filter(mdsubcast,region %in% and_p )%>%  mutate(., region="AND")
ren_mex<- filter(mdsubcast,region=="LAC-Mexico")%>% mutate(., region="MEX")
ren_bra<- filter(mdsubcast,region=="LAC-Brazil" | region=="LAC-Guyanas")%>% mutate(., region="BRA")
ren_csur<- filter(mdsubcast,region %in% csur_p)%>% mutate(., region="SUR")
ren_cen<- filter(mdsubcast,region %in% cen_p)%>% mutate(., region="CEN")

ctest<- list(ren_and,ren_mex,ren_bra,ren_csur,ren_cen)

for( i in 1:length(ctest)){
      ctest[[i]]<-aggregate(ctest[[i]][,c(5,6)],
                            by=list(ctest[[i]]$year,ctest[[i]]$scenario,ctest[[i]]$commodity, ctest[[i]]$region),FUN=sum)
      names(ctest[[i]])<-c("year","GCM","Crop","Region","Total Production","Total Area")
}

ctest<- do.call(rbind,ctest)

#Calcular los rendimientos ponderados
ctest<- mutate(ctest, yield= ctest[,5]/ctest[,6]) 
mdsubcastrend<-cast(ctest,GCM+Crop+Region~year,value=c("yield"))
rend_all<- mutate(mdsubcastrend, Percentage_Change= (((mdsubcastrend[,34]-mdsubcastrend[,4])/mdsubcastrend[,4])*100))
rend_all$Var<- "Yield"
names(var_all)
names(rend_all)
var_all<- var_all[,c("Region", "Var", "GCM","Crop","Percentage_Change", 2020:2050)]
rend_all<- rend_all[,c("Region", "Var", "GCM","Crop","Percentage_Change", 2020:2050)]

#Realizar proceso para graficar rendimientos ponderados por regiones.
tfiles<-rbind(var_all,rend_all)


tfiles$Crop<-revalue(tfiles$Crop, c("cbean"= "Bean",
                                    "cmaiz"="Maize",
                                    "crice"="Rice",
                                    "cs"="Soybean",
                                    "cwhea"="Wheat",
                                    "jbean"="Bean",
                                    "jmaiz"="Maize",
                                    "jrice"="Rice",
                                    "js"="Soybean",
                                    "jwhea"="Wheat"))

filesCC<-data.frame(tfiles,"Cat"=ifelse(tfiles$GCM=="NoCC","NoCC","CC"))

#Mediana de los cambios porcentuales por categorias.
anal_data<-data.frame(tfiles[,-c(6:36)],"Cat"=ifelse(tfiles$GCM=="NoCC","NoCC","CC"))
anal_datag<-aggregate(anal_data[,"Percentage_Change"],
                      by=list(anal_data$Region,anal_data$Var,anal_data$Crop,anal_data$Cat),
                      FUN=median)
write.csv(anal_datag,paste(grp,"anal_datagV3.csv",sep=""))
anal_datag_diff<- ddply(anal_datag,.(Group.1,Group.2,Group.3),summarize,d1=-diff(x,1))
write.csv(anal_datag_diff,paste(grp,"anal_datag_diffV3.csv",sep=""))

anal_datag2<-aggregate(anal_data[,"Percentage_Change"],by=list(anal_data$Var,anal_data$Cat),FUN=median)
View(ddply(anal_datag2,.(Group.1),summarize,d1=-diff(x,1)))
View(anal_datag2)
write.csv(anal_datag2,paste(grp,"anal_datag2V3.csv",sep=""))

#Tabla documento
gcmInf<- c("cccma_canesm2","ipsl_cm5a_lr", "NoCC")
sub_rend_all<- filter(tfiles, GCM %in% gcmInf)

t5<-sub_rend_all[,-c(6:ncol(sub_rend_all))]
t5or<-cast(t5,Region+Var~Crop+GCM)

t5or<- subset(t5or,t5or$Var!="Total Demand" & t5or$Var!="Net Trade")
write.csv(t5or,paste(grp,"t5orV3.csv",sep=""))


#Sub conjunto de cultivos--------------
cropsj<-c("jbean","jmaiz","jrice","js","jwhea")

t_nt<-2020:2050

sub_rend_all2<-subset(filesCC,filesCC$Percentage_Change >=quantile(filesCC$Percentage_Change, c(0.01)) &
                            filesCC$Percentage_Change <=quantile(filesCC$Percentage_Change, c(0.94)))

sub_rend_all2<-subset(filesCC,filesCC$Percentage_Change >quantile(filesCC$Percentage_Change, c(0.01)) &
                            filesCC$Percentage_Change <quantile(filesCC$Percentage_Change, c(0.94)))

sub_rend_all2_datag<-aggregate(sub_rend_all2[,"Percentage_Change"],
                               by=list(sub_rend_all2$Var,sub_rend_all2$Cat),
                               FUN=median)

write.csv(sub_rend_all2_datag,paste(grp,"sub_rend_all2V3.csv",sep=""))


#Graficos todos juntos juntar var_all y rend_all (net trade area production y rend)


png(filename=paste(grp,"all_crops_all_variables_all_GCMsV3.png",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)
ggplot(filesCC,aes(x=Cat,y=Percentage_Change,color=Var))  +
      facet_wrap( ~ Var,ncol=5,shrink=T) +
      geom_boxplot(alpha=0.4,stat="boxplot") +
      labs(y="Percentage change",x="") +
      coord_cartesian(ylim = c(-100, 200)) + 
      scale_y_continuous( breaks=seq(-100, 200, 20))
dev.off()



#Realizar proceso para graficar todas las variables por regiones.
cultivations<- unique(filesCC$Crop)
filesCC$Cat<- as.character(filesCC$Cat)
pl<-NULL

for(c in 1:length(cultivations)){
      
      png(filename=paste(grp,cultivations[c],"_all_variablesV3.png",sep=""), 
           width = 10, height = 7, units = 'in', res = 300)
      
      g1<- filesCC %>% filter(.,Crop==cultivations[c]& Var !="Net Trade" & GCM!= "NoCC")
      
      pl[[c]]<-print(ggplot(data=g1,aes(x=Region,y=Percentage_Change,color = Var))  +
                           facet_wrap( ~ Var,ncol=4,shrink=T) +
                           geom_point(aes(shape=Cat),data=subset(filesCC,filesCC$Crop ==cultivations[c]
                                                                 & filesCC$Var !="Net Trade" 
                                                                 & filesCC$GCM =="NoCC"),alpha = 0.4,size=3) +
                           geom_boxplot(alpha=0.4,stat="boxplot",outlier.shape = NA) +
                           guides(color=FALSE) + theme(legend.position="bottom",legend.title=element_blank())+
                           labs(y="Percentage change",x="")
                     
      )
      
      dev.off()
      print(c)
}              


#Evolution of production by crop and by region in the NOcc
datos<- subset(tfiles,tfiles$Var=="Total Production" & tfiles$GCM=="NoCC")

datos[,paste(2020:2050,sep="")]<-datos[,paste(2020:2050,sep="")]/datos[,"2020"]

datos<- datos[,-which(names(datos)=="Percentage_Change")]
datos_long<- datos %>% gather("Year","Index_of_Production", 5:35)
row.names(datos_long)<- 1:nrow(datos_long)
datos_long$Year<- as.numeric(datos_long$Year)

png(filename=paste(grp,"Evolution of production for all crops and all regionsV3.png"),width = 10,
     height = 12, units = 'in', res = 300)

ggplot(datos_long,aes(Year,Index_of_Production,color=Crop)) + 
      geom_line(size=1,linetype=6) + facet_wrap(~Region,nrow=5) +
      theme(legend.position="bottom",legend.text=element_text(size=12),
            legend.key.size = unit(1, "cm"),text = element_text(size=12))+
      labs(y="Index of production", x="Year") +
      scale_colour_brewer(palette="Set1",name = "")
dev.off()



#Evolution of Net Trade by crop and by region----------
datosnocc<- subset(tfiles,tfiles$Var=="Net Trade" & tfiles$GCM=="NoCC")
datosnocc$Percentage_Change<- NULL
datosnocc<- datosnocc %>% gather("Year","Net_trade", 5:35)

datosnocc<-datosnocc[,-c(which(names(datosnocc)=="Var"),which(names(datosnocc)=="GCM"),
                         which(names(datosnocc)=="Percentage_Change"))]


datos<- subset(tfiles,tfiles$Var=="Net Trade")
datmin<-aggregate(datos[,paste(2020:2050,sep="")],by=list(datos$Region,datos$Crop),FUN=min)
datmin<- datmin %>% gather(Year,datmin, 3:33 )

names(datmin)<-c("Region","Crop","Year","datmin")

datmax<-aggregate(datos[,paste(2020:2050,sep="")],by=list(datos$Region,datos$Crop),FUN=max)
datmax<- datmax %>% gather(Year,datmax, 3:33)
names(datmax)<-c("Region","Crop","Year","datmax")

extremos<-merge(datmin,datmax)

datost<-merge(datosnocc,extremos)

py<-NULL


for(c in 1:length(cultivations)) {
      
      png(filename=paste(grp,cultivations[c],"_net_tradeV3.png",sep=""), 
           width = 10, height = 7, units = 'in', res = 300)
      
      g1<- datost %>% filter(.,Crop==cultivations[c])
      
      py[[c]]<-print(ggplot(data=g1, aes(Year,Net_trade,group=Region,color=Region)) + 
                           geom_line(linetype="dashed",size=1)+
                           geom_ribbon(aes(ymin=datmin,ymax=datmax,fill=Region,colour=Region,linetype=NA),alpha=0.1)+
                           labs(y="Net trade (000 mt)",x="Year")+
                           theme(legend.position="bottom")
                     
      )
      
      
      dev.off()
      print(c)
}  


#Analisis para variables relacionadas con la seguridad alimentaria a nivel de país--------------

#Población en riesgo de padecer hambre
mdsubfs_pop<-subset(md,md$impactparameter=="PopulationAtRiskXagg - Pop at Risk of Hunger")

#Hacer un subconjunto de md que sólo contenga los paises de LAC
# lac_reg_fs<- levels(mdsubfs_pop$region)[68:92]
zona<- unique(alc$region)
mdsubfs_pop_lac<- filter(mdsubfs_pop, region %in% zona)
mdsubfs_pop_lac$impactparameter<- as.character(mdsubfs_pop_lac$impactparameter)
mdsubfs_pop_lac$scenario<- as.character(mdsubfs_pop_lac$scenario)
mdsubfs_pop_lac$commodity<- as.character(mdsubfs_pop_lac$commodity)
mdsubfs_pop_lac$region<-  as.character(mdsubfs_pop_lac$region)
mdsubfs_pop_lac$productiontype<- as.character(mdsubfs_pop_lac$productiontype)

mdwidefs_pop<- mdsubfs_pop_lac %>% spread(year, Val)

#Regiones a analizar
pop_and<- filter(mdwidefs_pop,region %in% and_p )%>%  mutate(., region="AND")
pop_mex<- filter(mdwidefs_pop,region=="LAC-Mexico")%>%  mutate(., region="MEX")
pop_bra<- filter(mdwidefs_pop,region=="LAC-Brazil" | mdwidefs_pop$region=="LAC-Guyanas")%>%  mutate(., region="BRA")
pop_csur<- filter(mdwidefs_pop,region %in% csur_p)%>%  mutate(., region="SUR")
pop_cen<- filter(mdwidefs_pop,region %in% cen_p)%>%  mutate(., region="CEN")

zpob<- list(pop_and,pop_mex,pop_bra, pop_bra, pop_csur, pop_cen)
#Agregar por variables para las regiones a analizar

for( i in 1:length(zpob)){
      zpob[[i]]<-aggregate(zpob[[i]][,c(9:15)],
                           by=list(zpob[[i]]$impactparameter,zpob[[i]]$scenario,zpob[[i]]$region),FUN=sum)
      zpob[[i]]$Change<-(zpob[[i]]$`2050`- zpob[[i]]$`2020`)
      names(zpob[[i]])<-c("Pob","GCM","Region",seq(from=2020,to=2050,by=5),"Change")
}

#Juntar todos los marcos de datos
zpob<- do.call(rbind,zpob)


pop_all<-data.frame(zpob,"Cat"=ifelse(zpob$GCM=="NoCC","NoCC","CC"))

pop_all2<-data.frame(aggregate(pop_all[,c("Change")],
                               by=list(pop_all$Region,pop_all$Cat),FUN=median))
pop_all2$x<-pop_all2$x*1000000
pop_all2 <- transform(pop_all2,Group.1 = reorder(Group.1, x))

png(filename=paste(grp,"Impact of CC (in the median GCM) in FSV3.png"), 
     width = 9, height = 9, units = 'in', res = 300)
ggplot(pop_all2, aes(Group.1, x, fill = Group.2)) + 
      geom_bar(stat="identity", position = "dodge") +
      scale_fill_brewer(palette = "Set1") + 
      coord_flip()+
      labs(x="Region",y="Change in the number of people at risk of hunger between 2020 and 2050")
dev.off()

write.csv(pop_all,paste(grp,"datos_popV3.csv",sep=""))



#Difference between the reduction of the people under risk of hunger in the median 
# GCM and the NoCC scenario
nocc<- subset(pop_all2,pop_all2$Group.2=="NoCC")
cc<- subset(pop_all2,pop_all2$Group.2=="CC")
f1<- function(nocc,cc) {data.frame("Region"=nocc[,1],"Diff_nocc_cc"=(nocc[,3]-cc[,3]))}
gr<- f1(nocc,cc)
gr <- transform(gr,Region = reorder(Region, Diff_nocc_cc))


dif<-ddply(pop_all2,.(Group.1),summarize,d1=-diff(x,1))
options(scipen=999)
png(filename=paste(grp,"Difference between the reduction of people under risk of hunger NoCC y CCV3.png"), 
     width = 9, height = 6, units = 'in', res = 300)
ggplot(gr, aes(Region, Diff_nocc_cc)) + geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + coord_flip() + 
      labs(x="Region",y="Difference between the change of people under risk of hunger in NoCC and the median GCM")
dev.off()


#rm(md,mdsub)

# #Analisis de precios-----------------
# md_prices<-read.csv(file="Phase1/Resultados_Prices_12_02_15.csv",header=T)
# md_prices_w<-subset(md_prices,md_prices$impactparameter=="PWXAgg -- World Prices")
# cropsc<-c("cbean","cmaiz","crice","csoyb","cwhea")
# md_prices<-md_prices_w[which(md_prices_w$commodity %in% cropsc),]
# md_prices<-droplevels(md_prices)
# md_prices<- md_prices %>% spread(year, Val)
# 
# md_prices<-data.frame(md_prices,'Percentage_change'=((md_prices$`2050`-md_prices$`2020`)/
#                                                            md_prices$`2020`)*100)
# 
# md_prices<- md_prices[,c(1:3,52)]
# md_prices$commodity<-revalue(md_prices$commodity, c("cbean"="Dry bean",
#                                                     "cmaiz"="Maize",
#                                                     "crice"="Rice",
#                                                     "csoyb"="Soybean",
#                                                     "cwhea"="Wheat"))
# md_prices$impactparameter<-revalue(md_prices$impactparameter,
#                                    c("PWXAgg -- World Prices"="World Prices"))
# 
# #Tabla documento
# tpor<- cast(md_prices,impactparameter+commodity~scenario)
# write.csv(tpor,paste(grp,"tporV3.csv",sep=""))




# Share at Risk of Hunger----------------
mdsubfs_risk<- subset(md,md$impactparameter=="ShareAtRiskXagg -- Share at Risk of Hunger")
mdsubfs_risk$impactparameter<- as.character(mdsubfs_risk$impactparameter)
mdsubfs_risk$scenario<- as.character(mdsubfs_risk$scenario)
mdsubfs_risk$commodity<- as.character(mdsubfs_risk$commodity)
mdsubfs_risk$region<- as.character(mdsubfs_risk$region)
mdsubfs_risk$productiontype<- as.character(mdsubfs_risk$productiontype)

#Hacer un subconjunto de md que sólo contenga los paises de LAC
mdsubfs_risk_lac<- mdsubfs_risk[grep(pattern = "LAC",x = mdsubfs_risk$region, ignore.case = T),]
mdsubfs_risk_lac<- mdsubfs_risk_lac %>% spread(year, Val)
mdsubfs_risk_lac<- filter(mdsubfs_risk_lac, region!="LAC")

mdsubfs_risk_lac$region<-  gsub("^LAC-", "",mdsubfs_risk_lac$region)


mdwidefs2_risk<- data.frame(mdsubfs_risk_lac,"Cat"=ifelse(mdsubfs_risk_lac$scenario=="NoCC","NoCC","CC"))

datos_risk<- data.frame(aggregate(mdwidefs2_risk[,c(9:15)],
                                  by=list(mdwidefs2_risk$region,mdwidefs2_risk$Cat),FUN=median))

datos_risk<- data.frame(datos_risk,"Change"=datos_risk$X2050-datos_risk$X2020)
datos_risk <- transform(datos_risk,Group.1 = reorder(Group.1, X2050 ))


png(filename=paste(grp,"Impact of CC (in the median GCM) in percent people at risk of hungerV3.png"), 
     width = 9, height = 6, units = 'in', res = 300)
ggplot(datos_risk, aes(Group.1,X2050, fill = Group.2)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + coord_flip() + 
      ylab("Share of population at risk of hunger in 2050 (%)") +
      xlab("Country") + theme(legend.title = element_blank())
dev.off()
