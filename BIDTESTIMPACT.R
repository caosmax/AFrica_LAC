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


# Definir directorio de trabajo-------------
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID")

# Direci?n graficos-----------------------
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/graphs/")
grp<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase2/Test/")

#Cargar marco de datos principal
md<-read.csv("Phase2/TestRegionsBID.csv",header=T)

Parameters<- c("TYldXAgg -- Total Yield", "QSXAgg -- Total Production","TAreaXAgg -- Total Area", "QNXAgg -- Net Trade","QDXAgg -- Total Demand")
crops<- c("jbean", "jrice", "jwhea" , "cmaiz", "cs", "jmaiz", "js", "cbean", "crice", "cwhea")

#Hacer un subconjunto que s?lo contenga las variables de mi inter?s y los 5 contenga los cinco cultivos analizados
mdsub<- filter(md, impactparameter %in% Parameters ) %>% filter(., commodity %in% crops)
mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QDXAgg -- Total Demand"="Total Demand",
                                                        "QNXAgg -- Net Trade"="Net Trade",
                                                        "TYldXAgg -- Total Yield"= "Total Yield",
                                                        "QSXAgg -- Total Production"="Total Production",
                                                        "TAreaXAgg -- Total Area"="Total Area"))

mdsub$impactparameter<- as.character(mdsub$impactparameter)
mdsub$scenario<- as.character(mdsub$scenario)
mdsub$commodity<- as.character(mdsub$commodity)
mdsub$region<- as.character(mdsub$region)
mdsub$productiontype<- as.character(mdsub$productiontype)

#Hacer un subconjunto de md que s?lo contenga los paises de LAC
alc<- mdsub
alc<- alc %>% spread(year, Val)
alc<- alc[,-c(6:20)]
alc$Percentage_Change<-((alc$`2050`- alc$`2020`)/alc$`2020`)*100


alc$commodity<-revalue(alc$commodity, c("cbean"= "Bean",
                                        "cmaiz"="Maize",
                                        "crice"="Rice",
                                        "cs"="Soybean",
                                        "cwhea"="Wheat",
                                        "jbean"="Bean",
                                        "jmaiz"="Maize",
                                        "jrice"="Rice",
                                        "js"="Soybean",
                                        "jwhea"="Wheat"))

alc<- data.frame(alc,"Cat"=ifelse(alc$scenario=="NoCC","NoCC","CC"))


#Mediana de los cambios porcentuales por categorias.
anal_data<- alc[,-c(6:36)]

anal_datag<- aggregate(anal_data[,"Percentage_Change"],
                       by=list(anal_data$region,anal_data$impactparameter,anal_data$commodity,anal_data$Cat),
                       FUN=median)

write.csv(anal_datag,paste(grp,"Test_anal_datag_BIDRegionsV2.csv",sep=""))
anal_datag_diff<- ddply(anal_datag,.(Group.1,Group.2,Group.3),summarize,d1=-diff(x,1))
write.csv(anal_datag_diff,paste(grp,"Test_anal_datag_BIDRegionsV2.csv",sep=""))

anal_datag2<- aggregate(anal_data[,"Percentage_Change"],by=list(anal_data$impactparameter,anal_data$Cat),FUN=median)
anal_dataoverall<-(ddply(anal_datag2,.(Group.1),summarize,d1=-diff(x,1)))

write.csv(anal_dataoverall,paste(grp,"Test_anal_datag_Overall_BIDRegionsV2.csv",sep=""))

#Tabla documento
gcmInf<- c("bcc_csm1_1","ipsl_cm5a_lr", "NoCC")
sub_rend_all<- filter(alc, scenario %in% gcmInf)
sub_rend_all$productiontype<-NULL
sub_rend_all<- sub_rend_all[,c("impactparameter", "scenario", "commodity", "region","Percentage_Change",paste0("X20",20:50))]

t5<-sub_rend_all[,-c(6:ncol(sub_rend_all))]
t5or<- cast(t5,region+impactparameter~commodity+scenario)

t5or<- subset(t5or,t5or$impactparameter!="Total Demand" & t5or$impactparameter!="Net Trade")
write.csv(t5or,paste(grp,"Test_t5or_BIDRegionsV2.csv",sep=""))

# Sub conjunto de cultivos--------------
cropsj<-c("jbean","jmaiz","jrice","js","jwhea")

t_nt<-2020:2050

sub_rend_all2<-subset(alc,alc$Percentage_Change >=quantile(alc$Percentage_Change, c(0.01)) &
                            alc$Percentage_Change <=quantile(alc$Percentage_Change, c(0.94)))

sub_rend_all2<-subset(alc,alc$Percentage_Change >quantile(alc$Percentage_Change, c(0.01)) &
                            alc$Percentage_Change <quantile(alc$Percentage_Change, c(0.94)))

sub_rend_all2<- data.frame(sub_rend_all2,"Cat"=ifelse(sub_rend_all2$scenario=="NoCC","NoCC","CC"))

sub_rend_all2_datag<- aggregate(sub_rend_all2[,"Percentage_Change"],
                                by=list(sub_rend_all2$impactparameter,sub_rend_all2$Cat),
                                FUN=median)

write.csv(sub_rend_all2_datag,paste(grp,"Test_sub_rend_BIDRegionsV2.csv",sep=""))


#Graficos todos juntos juntar var_all y rend_all (net trade area production y rend)
# alc<- data.frame(alc,"Cat"=ifelse(alc$scenario=="NoCC","NoCC","CC"))

png(filename=paste(grp,"Test_all_crops_all_variables_all_GCMs_BIDRegionsV2.png",sep=""), 
    width = 10, height = 10, units = 'in', res = 100)
ggplot(alc,aes(x=Cat,y=Percentage_Change,color=impactparameter))  +
      facet_wrap( ~ impactparameter,ncol=5,shrink=T) +
      geom_boxplot(alpha=0.4,stat="boxplot") +
      labs(y="Percentage change",x="") +
      coord_cartesian(ylim = c(-100, 200)) + 
      scale_y_continuous( breaks=seq(-100, 200, 20))
dev.off()



#Realizar proceso para graficar todas las variables por regiones.
cultivations<- unique(alc$commodity)
alc$Cat<- as.character(alc$Cat)
pl<-NULL

for(c in 1:length(cultivations)){
      
      png(filename=paste(grp,"Test_", cultivations[c],"Test_all_variables_BIDRegionsV2.png",sep=""), 
          width = 10, height = 7, units = 'in', res = 300)
      
      g1<- alc %>% filter(.,commodity==cultivations[c]& impactparameter!="Net Trade" & scenario!= "NoCC")
      
      pl[[c]]<-print(ggplot(data=g1,aes(x=region,y=Percentage_Change,color = impactparameter))  +
                           facet_wrap( ~ impactparameter,ncol=4,shrink=T) +
                           geom_point(aes(shape=Cat),data=subset(alc,alc$commodity==cultivations[c]
                                                                 & alc$impactparameter!="Net Trade" 
                                                                 & alc$scenario=="NoCC"),alpha = 0.4,size=3) +
                           geom_boxplot(alpha=0.4,stat="boxplot",outlier.shape = NA) +
                           guides(color=FALSE) + theme(legend.position="bottom",legend.title=element_blank())+
                           labs(y="Percentage change",x="")
                     
      )
      
      dev.off()
      print(c)
}              


#Evolution of production by crop and by region in the NoCC
datos<- subset(alc,alc$impactparameter=="Total Production" & alc$scenario=="NoCC")

datos[,paste("X20",20:50,sep="")]<-datos[,paste("X20",20:50,sep="")]/datos[,"X2020"]
datos<- datos[,-which(names(datos)=="Percentage_Change")]
datos$productiontype<- NULL
datos_long<- datos %>% gather("Year","Index_of_Production", 5:35)
row.names(datos_long)<- 1:nrow(datos_long)
datos_long$Year<-  gsub("X", "",datos_long$Year)
datos_long$Year<- as.numeric(datos_long$Year)

png(filename=paste(grp,"TestEvolution of production for all crops and all regions_BIDRegionsV2.png"),width = 10,
    height = 12, units = 'in', res = 300)

ggplot(datos_long,aes(Year,Index_of_Production,color=commodity)) + 
      geom_line(size=1,linetype=6) + facet_wrap(~region,nrow=5) +
      theme(legend.position="bottom",legend.text=element_text(size=12),
            legend.key.size = unit(1, "cm"),text = element_text(size=12))+
      labs(y="Index of production", x="Year") +
      scale_colour_brewer(palette="Set1",name = "")
dev.off()

# Evolution of Net Trade by crop and by region
datosnocc<- subset(alc,alc$impactparameter=="Net Trade" & alc$scenario=="NoCC")
datosnocc$Percentage_Change<- NULL
datosnocc$productiontype<- NULL
datosnocc<- datosnocc %>% gather("Year","Net_trade", 5:35)
datosnocc$Year<-  gsub("X", "",datosnocc$Year)
datosnocc$Year<- as.numeric(datosnocc$Year)

datosnocc<-datosnocc[,-c(which(names(datosnocc)=="impactparameter"),which(names(datosnocc)=="scenario"),
                         which(names(datosnocc)=="Percentage_Change"))] # ya se habia eliminado pc


datos<- subset(alc,alc$impactparameter=="Net Trade")
datos$productiontype<- NULL
datmin<- aggregate(datos[,paste("X20",20:50,sep="")],by=list(datos$region,datos$commodity),FUN=min)
datmin<- datmin %>% gather(Year,datmin, 3:33 )
datmin$Year<-  gsub("X", "",datmin$Year)

names(datmin)<- c("region","commodity","Year","datmin")

datmax<-aggregate(datos[,paste("X20",20:50,sep="")],by=list(datos$region,datos$commodity),FUN=max)

datmax<- datmax %>% gather(Year,datmax, 3:33)
names(datmax)<- c("region","commodity","Year","datmax")
datmax$Year<-  gsub("X", "",datmax$Year)

extremos<- merge(datmin,datmax)
extremos$Year<- as.numeric(extremos$Year)

datost<- merge(datosnocc,extremos)

py<-NULL

for(c in 1:length(cultivations)) {
      
      png(filename=paste(grp,"Test", cultivations[c],"_net_trade_BIDRegionsV2.png",sep=""), 
          width = 10, height = 7, units = 'in', res = 300)
      
      g1<- datost %>% filter(.,commodity==cultivations[c])
      
      py[[c]]<-print(ggplot(data=g1, aes(Year,Net_trade,group=region,color=region)) + 
                           geom_line(linetype="dashed",size=1)+
                           geom_ribbon(aes(ymin=datmin,ymax=datmax,fill=region,colour=region,linetype=NA),alpha=0.1)+
                           labs(y="Net trade (000 mt)",x="Year")+
                           theme(legend.position="bottom")
                     
      )
      
      
      dev.off()
      print(c)
}  


# Analisis para variables relacionadas con la seguridad alimentaria a nivel de pa?s
# Poblaci?n en riesgo de padecer hambre
mdsubfs_pop<-subset(md,md$impactparameter=="PopulationAtRiskXagg - Pop at Risk of Hunger")

#Hacer un subconjunto de md que s?lo contenga los paises de LAC
# lac_reg_fs<- levels(mdsubfs_pop$region)[68:92]
zona<- unique(alc$region)
mdsubfs_pop_lac<- filter(mdsubfs_pop, region %in% zona)
mdsubfs_pop_lac$impactparameter<- as.character(mdsubfs_pop_lac$impactparameter)
mdsubfs_pop_lac$scenario<- as.character(mdsubfs_pop_lac$scenario)
mdsubfs_pop_lac$commodity<- as.character(mdsubfs_pop_lac$commodity)
mdsubfs_pop_lac$region<-  as.character(mdsubfs_pop_lac$region)
mdsubfs_pop_lac$productiontype<- as.character(mdsubfs_pop_lac$productiontype)

mdwidefs_pop<- mdsubfs_pop_lac %>% spread(year, Val)
mdwidefs_pop$productiontype<- NULL

mdwidefs_pop$Change<-(mdwidefs_pop$`2050`- mdwidefs_pop$`2020`)


pop_all<-data.frame(mdwidefs_pop,"Cat"=ifelse(mdwidefs_pop$scenario=="NoCC","NoCC","CC"))

pop_all2<-data.frame(aggregate(pop_all[,c("Change")],
                               by=list(pop_all$region,pop_all$Cat),FUN=median))

pop_all2$x<-pop_all2$x*1000000
pop_all2 <- transform(pop_all2,Group.1 = reorder(Group.1, x))

png(filename=paste(grp,"TestImpact of CC (in the median GCM) in FS_BIDRegionsV2.png.png"), 
    width = 9, height = 9, units = 'in', res = 300)
ggplot(pop_all2, aes(Group.1, x, fill = Group.2)) + 
      geom_bar(stat="identity", position = "dodge") +
      scale_fill_brewer(palette = "Set1") + 
      coord_flip()+
      labs(x="Region",y="Change in the number of people at risk of hunger between 2020 and 2050")
dev.off()

write.csv(pop_all,paste(grp,"Test_datos_pop_BIDRegionsV2.csv",sep=""))


#Difference between the reduction of the people under risk of hunger in the median 
# GCM and the NoCC scenario
nocc<- subset(pop_all2,pop_all2$Group.2=="NoCC")
cc<- subset(pop_all2,pop_all2$Group.2=="CC")
f1<- function(nocc,cc) {data.frame("region"=nocc[,1],"Diff_nocc_cc"=(nocc[,3]-cc[,3]))}
gr<- f1(nocc,cc)
gr <- transform(gr,region = reorder(region, Diff_nocc_cc))


dif<-ddply(pop_all2,.(Group.1),summarize,d1=-diff(x,1))
options(scipen=999)
png(filename=paste(grp,"Test_Difference between the reduction of people under risk of hunger NoCC y CC_BIDRegionsV2.png"), 
    width = 9, height = 6, units = 'in', res = 300)
ggplot(gr, aes(region, Diff_nocc_cc)) + geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + coord_flip() + 
      labs(x="Region",y="Difference between the change of people under risk of hunger in NoCC and the median GCM")
dev.off()



# Analisis de precios
md_prices<-read.csv(file="Phase2/Vtest_prices.csv",header=T)

prices2<- read.csv(file="Phase2/PricesV2.csv",header=T)
prices1<- read.csv(file="Phase1/Resultados_Prices_12_02_15.csv",header=T)

c<- unique(md_prices$commodity)
yr<- 2020:2050
cropsc<-c("cbean","cmaiz","crice","csoyb","cwhea")
md_prices<- filter(md_prices, commodity %in% cropsc)
md_prices<- filter(md_prices, year %in% yr)
md_prices_w<- filter(md_prices, impactparameter=="PWXAgg -- World Prices")

md_prices_w$commodity<- as.character(md_prices_w$commodity)
md_prices_w$scenario<- as.character(md_prices_w$scenario)
md_prices_w$productiontype<- NULL
md_prices_w$region<- NULL
md_prices_w<- md_prices_w %>% spread(year, Val)

md_prices_w<-data.frame(md_prices_w,'Percentage_change'=((md_prices_w$`2050`-md_prices_w$`2020`)/md_prices_w$`2020`)*100)

md_prices_w<- md_prices_w[,c(1:3,50)]
md_prices_w$commodity<-revalue(md_prices_w$commodity, c("cbean"="Dry bean",
                                                    "cmaiz"="Maize",
                                                    "crice"="Rice",
                                                    "csoyb"="Soybean",
                                                    "cwhea"="Wheat"))
# md_prices$impactparameter<- "World Prices"
md_prices_w<- md_prices_w[,c("impactparameter","commodity", "scenario","Percentage_change"  )]


#Tabla documento
tpor<- cast(md_prices_w,impactparameter+commodity~scenario)
write.csv(tpor,paste(grp,"Vtest_tpor_BIDRegionsV2.csv",sep=""))

# Share at Risk of Hunger----------------
mdsubfs_risk<- subset(md,md$impactparameter=="ShareAtRiskXagg -- Share at Risk of Hunger")
mdsubfs_risk$impactparameter<- as.character(mdsubfs_risk$impactparameter)
mdsubfs_risk$scenario<- as.character(mdsubfs_risk$scenario)
mdsubfs_risk$commodity<- as.character(mdsubfs_risk$commodity)
mdsubfs_risk$region<- as.character(mdsubfs_risk$region)
mdsubfs_risk$productiontype<- NULL
mdsubfs_risk$commodity<- NULL

#Hacer un subconjunto de md que s?lo contenga los paises de LAC
mdsubfs_risk_lac<- mdsubfs_risk %>% spread(year, Val)
mdsubfs_risk_lac<- filter(mdsubfs_risk_lac, region!="LAC")
mdsubfs_risk_lac$region<-  gsub("^LAC-", "",mdsubfs_risk_lac$region)
mdwidefs2_risk<- data.frame(mdsubfs_risk_lac,"Cat"=ifelse(mdsubfs_risk_lac$scenario=="NoCC","NoCC","CC"))

datos_risk<- data.frame(aggregate(mdwidefs2_risk[,c(7:13)],
                                  by=list(mdwidefs2_risk$region,mdwidefs2_risk$Cat),FUN=median))

datos_risk<- data.frame(datos_risk,"Change"=datos_risk$X2050-datos_risk$X2020)
datos_risk <- transform(datos_risk,Group.1 = reorder(Group.1, X2050 ))


png(filename=paste(grp,"Test_Impact of CC (in the median GCM) in percent people at risk of hunger_BIDRegionsV2.png.png"), 
    width = 9, height = 6, units = 'in', res = 300)
ggplot(datos_risk, aes(Group.1,X2050, fill = Group.2)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + coord_flip() + 
      ylab("Share of population at risk of hunger in 2050 (%)") +
      xlab("Country") + theme(legend.title = element_blank())
dev.off()


# Ratio supply/demand five and all  crops
VarRatio<- c("QDXAgg -- Total Demand", "QSupXAgg -- Commodity Supply")
dataRatio<- filter(md, impactparameter %in% VarRatio )

dataRatio$impactparameter<- as.character(dataRatio$impactparameter)
dataRatio$scenario<- as.character(dataRatio$scenario)
dataRatio$commodity<- as.character(dataRatio$commodity)
dataRatio$region<- as.character(dataRatio$region)
dataRatio$productiontype<- as.character(dataRatio$productiontype)


dataRatio$commodity<-revalue(dataRatio$commodity, c("cbean"= "Bean",
                                                    "cmaiz"="Maize",
                                                    "crice"="Rice",
                                                    "cs"="Soybean",
                                                    "cwhea"="Wheat",
                                                    "jbean"="Bean",
                                                    "jmaiz"="Maize",
                                                    "jrice"="Rice",
                                                    "js"="Soybean",
                                                    "jwhea"="Wheat"))
cratio<- c("Bean", "Maize", "Soybean" , "Wheat", "Rice")
dataRatio<- filter(dataRatio, commodity %in% cratio )

dataRatio<- data.frame(dataRatio,"Cat"=ifelse(dataRatio$scenario=="NoCC","NoCC","CC"))
dataRatio$productiontype<- NULL

Ratio<- dataRatio %>% spread(impactparameter, Val)
Ratio<- mutate(Ratio, R= Ratio[,7]/Ratio[,6])

library(RColorBrewer)

png(filename=paste(grp,"Test_No2RatioFood_BIDRegionsV2.png"), 
    width = 9, height = 6, units = 'in', res = 300)

ggplot(Ratio,aes(reorder(region,R, FUN = median), R, fill=region))+
      geom_boxplot(aes(colour = region),outlier.colour = "red", outlier.size=NA ,alpha=0.4,stat="boxplot")+
      scale_x_discrete(name = "Regions")+
      scale_y_continuous(name = "Ratio Domestic Supply/\n Food Demand")+
      theme_bw()+
      theme(text = element_text(size = 13, family = "Tahoma"),
            axis.title = element_text(face="bold"),
            axis.text.x=element_text(size = 13),
            axis.text.y=element_text(size = 13),
            legend.title = element_blank()) +
      scale_fill_brewer(palette = "Set1") +
      geom_abline(intercept=1, slope = 0)



dev.off()



#Comparison Share at risk of hunger and food Availability percapita all time-------------
comp<- read.csv("Phase2/TestAllRegionsBID.csv", header = T)
varCompa<- c("ShareAtRiskXagg -- Share at Risk of Hunger", "FoodAvailXAgg")
dataCompa<- filter(comp, impactparameter %in% varCompa )
dataCompa$impactparameter<- as.character(dataCompa$impactparameter)
dataCompa$scenario<- as.character(dataCompa$scenario)
dataCompa$region<- as.character(dataCompa$region)
dataCompa$commodity<- as.character(dataCompa$commodity)

##Share
share<-  filter(dataCompa, impactparameter=="ShareAtRiskXagg -- Share at Risk of Hunger")
share$commodity<- NULL; share$productiontype<- NULL
ontime<- 2020:2050
share<- filter(share, year %in% ontime )
share<- share %>% spread(scenario,Val)
share$NoCC<- NULL

# share$year<- NULL
share$MeanShare<- apply(share[,4:ncol(share)],1,function(x){mean(x,na.rm = T)})
write.csv(share, paste(grp,"Test_sharealltime.csv", sep=""))
Bigzones<- c("NAM","SSA", "LAC", "EAP","WLD", "SAS", "EUR","FSU", "MEN" )
share<- filter(share, !region %in% Bigzones)
share<- share[,-c(1,4:12)]

##food
time<- seq(from = 2020, to = 2050, by= 5)
food<- filter(dataCompa, impactparameter=="FoodAvailXAgg")
food<- filter(food, year %in% time )
food<- filter(food, !region %in% Bigzones)
food$productiontype<- NULL
food<- food %>% spread(scenario,Val)
food$NoCC<- NULL
food$Meanfood<- apply(food[,5:ncol(food)],1,function(x){mean(x,na.rm = T)})
food<- food[,-c(1,5:13)]
food<- aggregate(food$Meanfood,by= list(food$region, food$year),FUN=sum)

names(food)<-c("region","year","Meanfood")
write.csv(share, paste(grp,"Test_foodalltime.csv", sep=""))

cfiles<- left_join(food,share, by=c("region", "year"))
getout<- cfiles[!complete.cases(cfiles),]
cfiles<- na.omit(cfiles)
getout2<-cfiles[!complete.cases(cfiles),]

cfiles$zone <- NA
cfiles$zone[grep("LAC-",cfiles$region)] <- "LAC"
cfiles$zone <- replace(cfiles$zone,is.na(cfiles$zone),"WORLD")




png(filename=paste(grp,"Test_Trends_Population_hunger_food_ava_ALLTimesBIDRegionsV2.png"), 
    width = 9, height = 6, units = 'in', res = 300)

ggplot(cfiles,aes(y=MeanShare,x=Meanfood)) +
      geom_point(aes(fill=zone),shape=22, size = 5, position = "jitter") +
      # geom_smooth(method ='loess',se = F, fullrange=T,size=1,color="green")+ 
      scale_x_continuous(name="Food availability per capita\n (kg per person)")+
      scale_y_continuous(name="Share at risk of huger\n (%)", limits=c(0.5,50))  +
      theme() + 
      scale_colour_manual(values=c("LAC"="red","WORLD"="blue")) +
      scale_fill_manual(values=c("LAC"="red", "WORLD"="blue"))+
      theme(text = element_text(size = 13, family = "Tahoma"),
            axis.title = element_text(face="bold"),
            axis.text.x=element_text(size = 13),
            axis.text.y=element_text(size = 13),
            legend.title = element_blank())
dev.off()



# grafica de media de disponibilidad de alimetnos in kg/person/yaer and percent change in the same period
perK<- filter(comp, impactparameter=="PerCapKCalXAgg")
perK<- filter(perK, !region %in% Bigzones)
perK$impactparameter<- as.character(perK$impactparameter);perK$scenario<- as.character(perK$scenario)
perK$commodity<- NULL; perK$productiontype<- NULL
perK<- perK[grep(pattern = "LAC",x = perK$region, ignore.case = T),]
perK$region<-  gsub("^LAC-", "",perK$region)
time<- c("2020", "2025", "2030", "2035", "2040", "2045", "2050")
perK<- filter(perK, year %in% time) %>% filter(., scenario!="NoCC")
rownames(perK)<- 1:nrow(perK)
perK<- aggregate(perK$Val, by=list(perK$region,perK$scenario, perK$year),FUN=mean)
names(perK)<- c("region","scenario","year", "mean")
meanperK<-  perK %>% spread(scenario,mean)
meanperK$mean<- apply(meanperK[,3:ncol(meanperK)],1,function(x){mean(x,na.rm = T)})
meanperK<- meanperK[,-c(3:11)]
meanperK<-  meanperK %>% spread(year,mean)
meanperK$mean<- apply(meanperK[,2:ncol(meanperK)],1,function(x){mean(x,na.rm = T)})
meanperK<- meanperK[,-c(2:8)]



# grafico de calorias percapita 
meanperK$region <- factor(meanperK$region, levels=meanperK$region[order(meanperK$mean)])
gg <- ggplot(meanperK, aes(x=region, y=mean)) + geom_bar(stat="identity") +
      xlab('Countries') + ylab('Mean Per capita calories\n available 2020-2050')+
      theme(axis.text.y=element_text(size=14,face='bold'))+
      theme(axis.title.x=element_text(size=14, face='bold'))+
      theme(plot.title=element_text(size=15, face = 'bold')) + coord_flip()+
      geom_abline(intercept=2100, slope = 0,color="red")+ theme(legend.position="none")

gg



chanceperK<-  perK %>% spread(year,mean)
chanceperK$change<- ((chanceperK$`2050`-chanceperK$`2020`)/chanceperK$`2020`)*100
chanceperK<- chanceperK[,-c(3:9)]
chanceperK<-  chanceperK %>% spread(scenario,change)
chanceperK$mean<- apply(chanceperK[,2:ncol(chanceperK)],1,function(x){mean(x,na.rm = T)})
chanceperK<- chanceperK[,-c(2:10)]

chanceperK$region <- factor(x=chanceperK$region, levels=levels(meanperK$region))
cc<-ggplot(chanceperK, aes(x=region, y=mean)) + geom_bar(stat="identity") +
      xlab('Countries') + ylab('% Change per capita calories\n available 2020-2050')+
      theme(axis.text.y=element_text(size=14,face='bold'))+
      theme(axis.title.x=element_text(size=14, face='bold'))+
      theme(plot.title=element_text(size=15, face = 'bold')) + coord_flip()+
      theme(legend.position="none")

cc
library(gridExtra)
graph <- arrangeGrob(gg, cc, nrow=1) #generates g
ggsave(file=paste(grp,"Test_Foov_Calories.png", sep = ""), graph, width=10, height=10.5, units='in') #saves g


#Ajuste grafico 17
#Analisis para variables relacionadas con la seguridad alimentaria a nivel de pa?s

md2<-read.csv("Phase2/TestAllRegionsBID.csv",header=T)

alc2<- md2[grep(pattern = "LAC",x = md2$region, ignore.case = T),]
alc2<-subset(alc2,alc2$impactparameter=="ShareAtRiskXagg -- Share at Risk of Hunger")

alc2$scenario<- as.character(alc2$scenario)
alc2$commodity<- as.character(alc2$commodity)
alc2$region<-  as.character(alc2$region)
t2<- c("2050","2020")
alc2<- filter(alc2, year %in% t2)
alc2$commodity<- NULL; alc2$productiontype<- NULL;alc2$impactparameter<- NULL


alc2<- filter(alc2, region!="LAC")
alc2$region<-  gsub("^LAC-", "",alc2$region)
alc2<- data.frame(alc2,"Cat"=ifelse(alc2$scenario=="NoCC","NoCC","CC"))
datos_risk2<- data.frame(aggregate(alc2$Val, by=list(alc2$region,alc2$Cat, alc2$year),FUN=median))
# datos_risk2 <- transform(datos_risk,Group.1 = reorder(Group.1, x))
colnames(datos_risk2)<- c("region","Cat", "year", "Val")

#graph2    
library(RColorBrewer)
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  

legend_title <- "Percentage\n (%)"  
png(filename=paste(grp,"Test_grafico17_BIDRegionsV2.png"), 
    width = 9, height = 6, units = 'in', res = 300)
aa<-ggplot(datos_risk2, aes(region,Cat)) + 
      geom_tile(aes(fill = Val), colour = "white")+  facet_grid(year~.,scales = "fixed", space = "fixed")+
      scale_fill_gradientn(legend_title,colours = hm.palette(100))+ 
      theme_grey() + labs(x = "",y = "") +   coord_equal()+ 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
      theme(axis.text.y = element_text(hjust = 1, size = 12))+
      theme(strip.text.x = element_text(size = 12, face = "bold.italic"))+
      theme(strip.text=element_text(size=12))+ ggtitle('Share of population at risk of hunger')+
      theme(strip.text.y = element_text(angle = 0,size = 12))
plot(aa)
dev.off()


#graph2
hdata<- datos_risk2
hdata<- hdata %>% spread (Cat, Val)
hdata$difpp<- hdata$CC - hdata$NoCC

library(RColorBrewer)
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  

legend_title <- "Difference\n in percentage points\n (pp)"  

png(filename=paste(grp,"Test_grafico17V2_BIDRegionsV2.png"), 
    width = 9, height = 6, units = 'in', res = 300)
a<-ggplot(hdata, aes(region, factor(year))) + 
      geom_tile(aes(fill = difpp), colour = "white")+ 
      scale_fill_gradient2(name=legend_title,low="darkblue", high="darkgreen", guide="colorbar") +
      theme_grey() + labs(x = "",y = "") +  coord_equal()+ 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
      theme(axis.text.y = element_text(hjust = 1, size = 12))+
      theme(strip.text.x = element_text(size = 12, face = "bold.italic"))+
      theme(strip.text=element_text(size=12))+ ggtitle('Differences between CC and NoCC')+
      theme(strip.text.y = element_text(angle = 0,size = 12))
plot(a)
dev.off()
graphShare <- arrangeGrob(aa, a,nrow=2) #generates g
ggsave(file=paste(grp,"Test_ShareHungerNew.png", sep = ""), graphShare, width=10, height=8, units='in') #saves g



##### grafico de STATA
#install.packages("readstata13")
library(readstata13)
gpvCrops <- read.dta13(file = paste(grp,"rate.dta", sep = ""),convert.factors = F)
gpvCrops<- as.data.frame(gpvCrops)
gpvCrops$indicator<- NULL

gpvCrops$zonas<- factor(gpvCrops$zonas)

gpvCrops$zonas<-revalue(gpvCrops$zonas, c("1"="Caribbean",
                                          "2"="Central America",
                                          "3"="South America"))

png(filename=paste(grp,"grafico1_BIDRegionsV2.png"), 
    width = 9, height = 6, units = 'in', res = 300)
ggplot(gpvCrops,aes(year,rate,color=zonas, group=zonas)) + 
      geom_line(size=1) +
      theme(legend.position="bottom",legend.text=element_text(size=12),
            legend.key.size = unit(1, "cm"),text = element_text(size=12))+
      labs(y="Share of gross production value (%),\n constant 2004-2006 million US$", x="Years") +
      scale_colour_brewer(palette="Set1",name = "")+ scale_x_continuous(breaks= c(1960:2013,4))+
      scale_y_continuous(breaks= c(1:25,1))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))+
      theme(axis.text.y = element_text(hjust = 1, size = 12))+
      theme(strip.text.x = element_text(size = 10, face = "bold.italic"))+
      theme(strip.text=element_text(size=11))+
      theme(strip.text.y = element_text(angle = 90,size = 11))
dev.off()   




######################################################################### FINAL #############################################################     