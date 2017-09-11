
### review primer resultado de IMPACT

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

data_out<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/outliers/")
wfd<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/AnalysisDataHistorical/")
yw<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/FPUWeight/")

gcm<-readRDS(paste(data_out, "SummaryLowest&HightestWFD.RDS", sep = ""))
wfdData<- read.csv(paste(wfd,"SummaryLowest&HightestWFD.csv", sep = ""))
wfdData$X<-NULL

names(gcm)
names(wfdData)

cfiles<- rbind(gcm, wfdData)
names(cfiles)

z<- cfiles %>% group_by(FPU, sys, sce, crop) %>%  do(.,as.data.frame(table(.$status)))
colnames(z)[5]<- "Staus"
z<- z %>% spread(crop, Freq)
write.csv(z, paste(data_out,"resumenPCA.csv", sep = ""))

## cargando rendimientos ponderados para comparar wfd con gcms promedios


wfiles<- list.files(path =yw, pattern = "FPU", full.names = T)
wfiles<-lapply(wfiles,read.csv,stringsAsFactors = F)
wfiles<- do.call(rbind,wfiles)
wfiles$X<- NULL
names(wfiles)
wfiles<- wfiles[, c("pot","sys","Sce", "num_pixels","crop", paste0("Rend_fpu_20",22:49))]


# wfiles<- wfiles %>% 
#       gather(year,val, 5:32)

# wfiles$year<- sub(pattern = "Rend_fpu_", replacement = "", x = wfiles$year, ignore.case = T)
# wfiles$year<- as.numeric(wfiles$year)
# wfiles<- wfiles %>% 
#       spread(year,val)




wfiles$mean<- rowMeans(wfiles[,6:ncol(wfiles)]) 
wfiles<- wfiles[,-c(6:33)]
wfiles<- wfiles %>% spread(Sce,mean)
write.csv(wfiles, paste(data_out,"PromediosresumenPCA.csv", sep = ""))


w <- wfiles %>% group_by(pot,sys,Sce,crop) %>% do(.%>% mutate(mean=rowMeans(.,6:33)))

