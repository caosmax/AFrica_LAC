# import and proccesing data
g=gc;rm(list = ls())

#library---------------
library(plyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(dplyr)
library(gdxrrw)
ggir<- "C:/Users/CEGONZALEZ/Desktop/IMPACT3-Model-ver3.3/OutputFiles/Scenarios/"
gres<- c("C:/Users/CEGONZALEZ/Desktop/exp/")

#to read GDX files we need the gdxrrw package
igdx(gamsSysDir ="C:/GAMS/win64/24.4")
#read the data created in GAMS
# colnames<-c("QSX0","ANMLNUMCTYX0","AREACTYX0","FoodAvailability","GDPHX0", "GDPX0","pcGDPX0","PCX0",
#             "PerCapKCal","PerCapKCal_Com","PEX0","PMX0","PNETX0","POPHX0","PopulationAtRisk","POPX0","PPX0","PWX0","QBFX0",
#             "QDX0","QEX0","QFX0","QHDX0","QINTX0","QLX0","QMSH","QMX0","QNSH1","QNSH2","QNX0","QOTHRX0","QSUPX0","QSX0",
#             "ShareAtRisk","TotalMalnourished","YldCliShkCtyX0","YLDCTYX0","YldInt2CtyX0","YldShkCtyX0"); 

colnames<- list("QSX0","ANMLNUMCTYX0","AREACTYX0","FoodAvailability","GDPHX0", "GDPX0","pcGDPX0","PCX0",
                "PerCapKCal","PerCapKCal_Com","PEX0","PMX0","PNETX0","POPHX0","PopulationAtRisk","POPX0","PPX0","PWX0","QBFX0",
                "QDX0","QEX0","QFX0","QHDX0","QINTX0","QLX0","QMX0","QNX0","QOTHRX0","QSUPX0","QSX0",
                "ShareAtRisk","TotalMalnourished","YldCliShkCtyX0","YLDCTYX0","YldInt2CtyX0")


# load data from GDX
Test <- lapply(1:length(colnames), function(x){
     rgdx.param(gdxName =paste(ggir,"TeCNoCC.gdx", sep = ""),symName = colnames[[x]],ts = T,compress = F,
                            names = c("crop", "Regions","YRS",colnames[[x]]),useDomInfo = 3) 
   
})


# creating groups using attr

yy<- list()
xx<- list()

x=1
lapply(1:length(Test),function(x){
      
      data<- Test[[x]]
#       data$parameter<- colnames(data)[ncol(data)]
#       colnames(data)[(ncol(data))-1]<-"Val"
#       
            if(length(attr(data,which = "domains" ))>=4){
                  data$parameter<- colnames(data)[ncol(data)]
                  colnames(data)[(ncol(data))-1]<-"Val"
                  
                  yy[i]<- list(data)
            }else{
                  data$parameter<- colnames(data)[ncol(data)]
                  colnames(data)[(ncol(data))-1]<-"Val"
                  xx[i]<- list(data)  
            }
      
})


   