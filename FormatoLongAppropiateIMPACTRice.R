#Codigo para presentar el docunmento en un formato apropiado ajuste arroz

#directorio datos
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/TasasCrecimiento/")
pic<-("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/GraphGrownRate/")
      
# Limitar numero de decimales
options(digits=3) 
options(scipen=999)

#Librerias
library(reshape)
library(ggplot2)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)

#Lista de tipos de sistemas
sys<- c( "IRRI", "RA")
crops<- c("Rice","Bean","Wheat","Maize", "Soybean")

#Lista de cultivos
crops.en<-c("rice","maiz","soyb","bean","whea")
crops.enj<-c("jrice","jbean","jwhea","jmaiz", "jsoyb") 

# GCMs
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

#treat
treat<- c("air", "arf")

# load files 
c_files<- list.files()
c_files<- lapply(c_files, read.csv)
c_files<- do.call(rbind, c_files)

colnames(c_files)[1]<- "fpu"
colnames(c_files)[2]<- "j"
colnames(c_files)[3]<- "lnd"


# ajustando nombres
c_files$j<-revalue(c_files$j, c("Bean"="jbean",
                                "Rice"="jrice",
                                "Maize"="jmaiz",
                                "Wheat"="jwhea",
                                "Soybean"="jsoyb"))
c_files$lnd<- revalue(c_files$lnd, c("IRRI"="air",
                                "RA"="arf"))
# formato adecuado para IMPACT                                                   
require(plyr)
require(tidyr)
c_files<- c_files %>% 
      gather(gcm, val, 4:ncol(c_files))

#organizar
rownames(c_files)<-1:nrow(c_files)


#Los NAs los reemplazo por ceros
c_files[is.na(c_files)==TRUE]=0

write.csv(c_files,file='//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/LongFormat.csv',row.names=F)

### poner las versiones que se estan trabajando
write.csv(c_files,file='//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/tc_copy/LongFormat_v2.csv',row.names=F)

#comparaciones con el archivo original
data1<- read.csv("C:/Users/CEGONZALEZ/Documents/Scripts/BID/CCPROCESSING/InputFiles/LongFormat.csv")
data2<- c_files


row.names(data1)<- 1:nrow(data1)

data1$fpu<- as.character(data1$fpu)
data2$fpu<- as.character(data2$fpu)

data1$j<- as.character(data1$j)
data2$j<- as.character(data2$j)

data1$lnd<- as.character(data1$lnd)
data2$lnd<- as.character(data2$lnd)


data1$gcm<- as.character(data1$gcm)
data2$gcm<- as.character(data2$gcm)


total <- merge(data1, data2,by=c("fpu","j","lnd","gcm"))


colnames(total)[5]<-"initial"
colnames(total)[6]<-"updated"


#Regression lineal
reg<- function(x){ 
      lm(data=x, updated~ initial)}

# Filter using treat rainfed
Function_rainfed<- function(x)
{subset(x=x, grepl("arf",lnd))}

# Filter using treat irrigated
Function_irrigated<- function(x)
{subset(x=x, grepl("air",lnd))}

# Filter using crops
rice<- function(x)
      {subset(x=x, grepl("jrice",j))}

bean<- function(x)
      {subset(x=x, grepl("jbean",j))}

wheat<- function(x)
      {subset(x=x, grepl("jwhea",j))}


maize<- function(x)
      {subset(x=x, grepl("jmaiz",j))}

soybean<- function(x)
      {subset(x=x, grepl("jsoyb",j))}


   


proof<- total
#listas vacias
z<- list()
r2<- list()

# una lista con 9 gcms
for(g in 1:length(gcm)){
      z[[g]]<- proof[which(proof$gcm==gcm[g]),] # una lista con 9 gcms
}

# lista con solo rainfed
secano<-  lapply(z,Function_rainfed) 
irrigado<- lapply(z,Function_irrigated)      

#Rice-----------
a1<- lapply(irrigado,rice)
a2<- lapply(secano,rice)
a<- list(a1,a2)
##reg
ar1<- lapply(a1, reg)
ar2<- lapply(a2, reg)


#Maize----------
m1<- lapply(irrigado,maize)
m2<- lapply(secano,maize)
m<- list(m1,m2)
##reg
mr1<- lapply(m1, reg)
mr2<- lapply(m2, reg)


#frijol-------------
f1<- lapply(irrigado,bean)
f2<- lapply(secano,bean)
f<- list(f1,f2)
##reg
fr1<- lapply(f1, reg)
fr2<- lapply(f2, reg)


#trigo---------
t1<- lapply(irrigado,wheat)
t2<- lapply(secano,wheat)
t<- list(t1,t2)
##reg
tr1<- lapply(t1, reg)
tr2<- lapply(t2, reg)



#Soybean------

s1<- lapply(irrigado,soybean)
s2<- lapply(secano,soybean)
s<- list(s1,s2)
##reg
sr1<- lapply(s1, reg)
sr2<- lapply(s2, reg)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Analysis total %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#--------------------------
cfiles<- c(a1,a2,f1,f2,m1,m2,t1,t2,s1,s2)

for(i in 1:length(cfiles)){
      row.names(cfiles[[i]])<- 1: nrow(cfiles[[i]])
      ## logica de los valores 
      nn<-  which(cfiles[[i]]$initial<0 & cfiles[[i]]$updated<0) 
      pn<-  which(cfiles[[i]]$initial>0 & cfiles[[i]]$updated<0) 
      np<-  which(cfiles[[i]]$initial<0 & cfiles[[i]]$updated>0) 
      pp<-  which(cfiles[[i]]$initial>0 & cfiles[[i]]$updated>0)
      weird<-  which(cfiles[[i]]$initial==0 & cfiles[[i]]$updated!=0) 
      
      positive <- c(pp)
      negative <-  c(nn)
      tran_NegatoPosi<- c(np) 
      tran_PositoNega<- c(pn)
      weird <- c(weird)
      
      cfiles[[i]]$comparison<- NA 
      cfiles[[i]]$comparison[positive] <- "Positive"
      cfiles[[i]]$comparison[negative] <- "Negative"
      cfiles[[i]]$comparison[tran_NegatoPosi]<- "Before negative- now positive"
      cfiles[[i]]$comparison[tran_PositoNega]<- "Before positive- now negative"
      cfiles[[i]]$comparison[weird]<- "Special cases"
      
      
}

datacomplete<- do.call(rbind, cfiles)
write.csv(datacomplete,paste(pic,"tc_comparison_V2.csv", sep = ""))



g=gc;rm(list=ls())



