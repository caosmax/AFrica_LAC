#Codigo para presentar el docunmento en un formato apropiado

#directorio datos
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/TasasCrecimiento/Test/")
pic<-("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/TasasCrecimiento/Test/ResultsTest/")


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
c_files<- list.files(pattern = ".csv")
c_files<- lapply(c_files, read.csv)
c_files<- do.call(rbind, c_files)
c_files<- c_files[,c("FPU","j", "sys","bcc_csm1_1","bnu_esm", "cccma_canesm2", "gfld_esm2g","inm_cm4","ipsl_cm5a_lr",
                     "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m",  "WFD")]
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

write.csv(c_files,file= paste(pic, "LongFormat.csv", sep="") ,row.names=F)
