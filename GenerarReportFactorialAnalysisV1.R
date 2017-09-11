### indicador FPU

# librerias------------
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(tidyr)
library(lattice)
library(latticeExtra)
library(dplyr)
library(RColorBrewer)
library(png)
library(readxl)
library(broom)


# Limitar numero de decimales
options(digits=3) 
options(scipen=999)


################################################# Cambios rendimientos IMPACT por FPU ##########################################################
grd<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase2/")

cfiles<- list.files(grd,pattern = "X01",full.names = T)
cfiles<- lapply(cfiles, read_excel)

for(i in 1:length(cfiles)){
      r1 <- data.frame(t(colnames(cfiles[[i]]))); names(r1) <- c("crops", "FPU", "System", "Year", "Val")
      r1$Year <- as.numeric(as.character(r1$Year))
      r1$Val <- as.numeric(as.character(r1$Val))
      names(cfiles[[i]]) <- c("crops", "FPU", "System", "Year", "Val")
      cfiles[[i]] <- rbind(r1, cfiles[[i]])   

}

area<- as.data.frame(cfiles[[1]])
area$parameter<-"area"
yield<- as.data.frame(cfiles[[2]])
yield$parameter<-"yield"
yshock<- as.data.frame(cfiles[[3]])
yshock$parameter<-"yshock"

fputotal<- rbind(area,yield, yshock)

fputotal$crops<-  revalue(fputotal$crops, c("jmaiz"="Maize",
                                         "jrice"="Rice",
                                         "jwhea"="Wheat",
                                         "jbean"="Bean",
                                         "jsoyb"="Soybean"))
fputotal<- filter(fputotal, crops %in% c("Maize","Rice","Wheat","Bean","Soybean"))
ffiles<- fputotal
ffiles<- ffiles %>% spread(Year, Val)
ffiles<- ffiles[,-c(5:19)]
ffiles$mean<- rowMeans(ffiles[,5:ncol(ffiles)]) 
ffiles$Percentage_Change<- ((ffiles$`2050`-ffiles$`2020`)/ffiles$`2020`)*100
ffiles<- ffiles[,-c(5:35)]
names(ffiles)

# filtro por FPUs
pots<- read.csv("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ListFPUs.csv")
listFpu<-as.character(unique(pots$FPUs))

ffiles<- filter(ffiles, FPU %in% listFpu)
ffiles$FPU<- as.character(ffiles$FPU)

################################################### Tasas de Crecimiento Crop modelling  #######################################
tc_all<- read.csv("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/GraphGrownRate/tc_comparison_V1.csv")
tc_all$X<- NULL
tc_all$j<-  revalue(tc_all$j, c("jmaiz"="Maize",
                                "jrice"="Rice",
                                "jwhea"="Wheat",
                                "jbean"="Bean",
                                "jsoyb"="Soybean"))
colnames(tc_all)[1]<- "FPU"
colnames(tc_all)[2]<- "crops"
colnames(tc_all)[3]<- "System"

tc_all<- tc_all[,c("crops","FPU","System","gcm","initial","updated","comparison")]


############################################### Status #######################################################################      
      
grd1<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/outliers/")
Ymean<- read.csv(paste(grd1, "PromediosresumenPCA.csv", sep = ""))
Ymean$X<- NULL
colnames(Ymean)[1]<-"FPU"
colnames(Ymean)[2]<-"System"
colnames(Ymean)[4]<-"crops"
Ymean<- Ymean[,c("crops","System","FPU","num_pixels","bcc_csm1_1","bnu_esm","cccma_canesm2","gfld_esm2g", "inm_cm4",      
                 "ipsl_cm5a_lr","miroc_miroc5","mpi_esm_mr","ncc_noresm1_m", "WFD")]

status<- read.csv(paste(grd1, "resumenPCA.csv", sep = ""))
status$X<- NULL
colnames(status)[2]<- "System"


grd2<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/outliers/")
out<- read.csv(paste(grd2, "SummaryOutliers.csv", sep = ""))
out$X<- NULL
colnames(out)[2]<-"crops"


################################################ Exportar a RDS ###############################################################
grd3<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")
cfiles<- list(ffiles, tc_all,Ymean,status,out)
saveRDS(object = cfiles,file =paste(grd3,"AnalysisBID_V1.RDS", sep = ""))
