#Codigo para presentar el docunmento en un formato apropiado

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
c_files<- list.files(pattern = ".csv")
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
write.csv(c_files,file='//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/tc_copy/LongFormat_Final.csv',row.names=F)

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
# 
# #Rice-----------
a1<- lapply(irrigado,rice)
a2<- lapply(secano,rice)
a<- list(a1,a2)
##reg
ar1<- lapply(a1, reg)
ar2<- lapply(a2, reg)

# 
# ## graficos de arroz
# #irrigado
# #summary(ar1[[1]])
# 
# 
# w<- list()
# #i=1
# for(i in 1:length(a1)){
#       
#       png(filename = paste(pic,unique(a1[[i]]$j),"_",
#                            unique(a1[[i]]$lnd),"_", unique(a1[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = a1[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(ar1[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(ar1[[i]])$coef[[2]], 5),
#                                " P =",signif(summary(ar1[[i]])$coef[2,4], 5),"\nGCM = ",unique(a1[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             theme(legend.position="none")+
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous')
# 
# 
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(a1[[i]]$j),
#                 "_", unique(a1[[i]]$gcm),
#                 "_", unique(a1[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# library(gtable)
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# 
# 
# png(filename = paste(pic,"RiceIrrigatedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# rice_irrigated<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                              top= textGrob("Rice Irrigated by GCMs\n growth rate Initial Vs updated", 
#                                       gp=gpar(fontsize=20,font=8)), legend)
# plot(rice_irrigated)
# dev.off()
# 
# #secano
# 
# w<- list()
# 
# for(i in 1:length(a2)){
#       
#       png(filename = paste(pic,unique(a2[[i]]$j),"_",
#                            unique(a2[[i]]$lnd),"_", unique(a2[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = a2[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(ar2[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(ar2[[i]])$coef[[2]], 5),
#                                " P =",signif(summary(ar2[[i]])$coef[2,4], 5),"\nGCM = ",unique(a2[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous') 
# 
#       
#       
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(a2[[i]]$j),
#                 "_", unique(a2[[i]]$gcm),
#                 "_", unique(a2[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# png(filename = paste(pic,"RiceRainfedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# rice_rainfed<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                              top= textGrob("Rice Rainfed by GCMs\n growth rate Initial Vs updated", 
#                                            gp=gpar(fontsize=20,font=8)), legend)
# plot(rice_rainfed)
# dev.off()
# 

#Maize----------
m1<- lapply(irrigado,maize)
m2<- lapply(secano,maize)
m<- list(m1,m2)
##reg
mr1<- lapply(m1, reg)
mr2<- lapply(m2, reg)
# 
# 
# #irrigado
# 
# w<- list()
# 
# for(i in 1:length(m1)){
#       
#       png(filename = paste(pic,unique(m1[[i]]$j),"_",
#                            unique(m1[[i]]$lnd),"_", unique(m1[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = m1[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(mr1[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(mr1[[i]])$coef[[2]], 5),
#                                " P =",signif(summary(mr1[[i]])$coef[2,4], 5),"\nGCM = ",unique(m1[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous') 
# 
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(m1[[i]]$j),
#                 "_", unique(m1[[i]]$gcm),
#                 "_", unique(m1[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# 
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# png(filename = paste(pic,"MaizeIrrigatedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# maize_irrigated<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                              top= textGrob("Maize Irrigated by GCMs\n growth rate Initial Vs updated", 
#                                            gp=gpar(fontsize=20,font=8)), legend)
# plot(maize_irrigated)
# dev.off()
# 
# #secano
# 
# w<- list()
# 
# for(i in 1:length(m2)){
#       
#       png(filename = paste(pic,unique(m2[[i]]$j),"_",
#                            unique(m2[[i]]$lnd),"_", unique(m2[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = m2[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(mr2[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(mr2[[i]]$coef[[2]], 5)),
#                                " P =",signif(summary(mr2[[i]])$coef[2,4], 5),"\nGCM = ",unique(m2[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous')
# 
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(m2[[i]]$j),
#                 "_", unique(m2[[i]]$gcm),
#                 "_", unique(m2[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# 
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# png(filename = paste(pic,"MaizeRainfedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# maize_rainfed<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                               top= textGrob("Maize Rainfed by GCMs\n growth rate Initial Vs updated", 
#                                             gp=gpar(fontsize=20,font=8)), legend)
# plot(maize_rainfed)
# dev.off()
# 
# 
# 
#frijol-------------
f1<- lapply(irrigado,bean)
f2<- lapply(secano,bean)
f<- list(f1,f2)
##reg
fr1<- lapply(f1, reg)
fr2<- lapply(f2, reg)
# 
# ## graficos de arroz
# #irrigado
# 
# w<- list()
# 
# for(i in 1:length(f1)){
#       
#       png(filename = paste(pic,unique(f1[[i]]$j),"_",
#                            unique(f1[[i]]$lnd),"_", unique(f1[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = f1[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(fr1[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(fr1[[i]])$coef[[2]], 5),
#                                " P =",signif(summary(fr1[[i]])$coef[2,4], 5), "\nGCM = ",unique(f1[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous') 
# 
#       
#       
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(f1[[i]]$j),
#                 "_", unique(f1[[i]]$gcm),
#                 "_", unique(f1[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# png(filename = paste(pic,"BeansIrrigatedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# bean_irrigated<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                             top= textGrob("Beans Irrigated by GCMs\n growth rate Initial Vs updated", 
#                                           gp=gpar(fontsize=20,font=8)), legend)
# plot(bean_irrigated)
# dev.off()
# 
# 
# #secano
# 
# w<- list()
# 
# for(i in 1:length(f2)){
#       
#       png(filename = paste(pic,unique(f2[[i]]$j),"_",
#                            unique(f2[[i]]$lnd),"_", unique(f2[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = f2[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(fr2[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(fr2[[i]])$coef[[2]], 5),
#                                " P =",signif(summary(fr2[[i]])$coef[2,4], 5),"\nGCM = ",unique(f2[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous')
# 
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(f2[[i]]$j),
#                 "_", unique(f2[[i]]$gcm),
#                 "_", unique(f2[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# png(filename = paste(pic,"BeanRainfedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# bean_rainfed<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                              top= textGrob("Beans Rainfed by GCMs\n growth rate Initial Vs updated", 
#                                            gp=gpar(fontsize=20,font=8)), legend)
# plot(bean_rainfed)
# dev.off()
# 
# 
#trigo---------
t1<- lapply(irrigado,wheat)
t2<- lapply(secano,wheat)
t<- list(t1,t2)
##reg
tr1<- lapply(t1, reg)
tr2<- lapply(t2, reg)
# 
# #irrigado
# 
# w<- list()
# 
# for(i in 1:length(t1)){
#       
#       png(filename = paste(pic,unique(t1[[i]]$j),"_",
#                            unique(t1[[i]]$lnd),"_", unique(t1[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = t1[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(tr1[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(tr1[[i]])$coef[[2]], 5),
#                                " P =",signif(summary(tr1[[i]])$coef[2,4], 5),"\nGCM = ",unique(t1[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous') 
# 
#       
#       
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(t1[[i]]$j),
#                 "_", unique(t1[[i]]$gcm),
#                 "_", unique(t1[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# png(filename = paste(pic,"WheatIrrigatedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# Wheat_irrigated<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                            top= textGrob("Wheat Irrigated by GCMs\n growth rate Initial Vs updated", 
#                                          gp=gpar(fontsize=20,font=8)), legend)
# plot(Wheat_irrigated)
# dev.off()
# 
# 
# 
# #secano
# 
# w<- list()
# 
# for(i in 1:length(t2)){
#       
#       png(filename = paste(pic,unique(t2[[i]]$j),"_",
#                            unique(t2[[i]]$lnd),"_", unique(t2[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = t2[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(tr2[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(tr2[[i]])$coef[[2]], 5),
#                                " P =",signif(summary(tr2[[i]])$coef[2,4], 5),"\nGCM = ",unique(t2[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous')
# 
#       
#       
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(t2[[i]]$j),
#                 "_", unique(t2[[i]]$gcm),
#                 "_", unique(t2[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# 
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# png(filename = paste(pic,"WheatRainfedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# wheat_rainfed<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                               top= textGrob("Wheat Rainfed by GCMs\n growth rate Initial Vs updated", 
#                                             gp=gpar(fontsize=20,font=8)), legend)
# plot(wheat_rainfed)
# dev.off()
# 
# 
#Soybean------

s1<- lapply(irrigado,soybean)
s2<- lapply(secano,soybean)
s<- list(s1,s2)
##reg
sr1<- lapply(s1, reg)
sr2<- lapply(s2, reg)
# 
# #irrigado
# 
# w<- list()
# 
# for(i in 1:length(s1)){
#       
#       png(filename = paste(pic,unique(s1[[i]]$j),"_",
#                            unique(s1[[i]]$lnd),"_", unique(s1[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = s1[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(sr1[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(sr1[[i]])$coef[[2]], 5),
#                                " P =",signif(summary(sr1[[i]])$coef[2,4], 5),"\nGCM = ",unique(s1[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous') 
# 
#       
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(s1[[i]]$j),
#                 "_", unique(s1[[i]]$gcm),
#                 "_", unique(s1[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# png(filename = paste(pic,"SoybeanIrrigatedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# Soybean_irrigated<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                               top= textGrob("Soybean Irrigated by GCMs\n growth rate Initial Vs updated", 
#                                             gp=gpar(fontsize=20,font=8)), legend)
# plot(Soybean_irrigated)
# dev.off()
# 
# 
# 
# #secano
# 
# w<- list()
# 
# for(i in 1:length(s2)){
#       
#       png(filename = paste(pic,unique(s2[[i]]$j),"_",
#                            unique(s2[[i]]$lnd),"_", unique(s2[[i]]$gcm),
#                            "_TC",".png",sep=""), 
#           width = 15, height = 15, units = 'in', res = 150)
#       
#       w[[i]]<- ggplot(data = s2[[i]],aes(x=initial,y=updated))+ 
#             geom_point(aes(color=fpu),size=3) +
#             stat_smooth(method = "lm", col= "red")+
#             labs(title = paste("R2 = ",signif(summary(sr2[[i]])$r.squared, 5),
#                                " Slope =",signif(summary(sr2[[i]])$coef[[2]], 5),
#                                " P =",signif(summary(sr2[[i]])$coef[2,4], 5),"\nGCM = ",unique(s2[[i]]$gcm)))+
#             theme(axis.text.x=element_text(size=16))+
#             theme(axis.text.y=element_text(size=16))+
#             theme(legend.position="none")+
#             guides(color=guide_legend("FPUs"))+ 
#             geom_hline(aes(yintercept=0))+ 
#             geom_vline(aes(xintercept=0))+ 
#             ylab('Growth rate annual updated') +  
#             xlab('Growth rate annual Previous')
# 
#       
#       
#       plot(w[[i]])
#       dev.off()
#       
#       cat(paste("Running comparasion ", unique(s2[[i]]$j),
#                 "_", unique(s2[[i]]$gcm),
#                 "_", unique(s2[[i]]$lnd),
#                 " it's done!!!\n", sep = ""))
#       
#       
# }
# 
# 
# legend <- gtable_filter(ggplot_gtable(ggplot_build(w[[1]] + theme(legend.position="bottom"))), "guide-box")
# png(filename = paste(pic,"SoybeanRainfedComparasions&R2.png"), 
#     width = 20, height = 20, units = 'in', res = 150)
# wheat_rainfed<- arrangeGrob(w[[1]],w[[2]],w[[3]],w[[4]],w[[5]],w[[6]],w[[7]],w[[8]], w[[9]],ncol=4, nrow =3,
#                             top= textGrob("Soybean Rainfed by GCMs\n growth rate Initial Vs updated", 
#                                           gp=gpar(fontsize=20,font=8)), legend)
# plot(wheat_rainfed)
# dev.off()
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Analysis total %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#--------------------------
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
write.csv(datacomplete,paste(pic,"tc_comparison_V1.csv", sep = ""))



g=gc;rm(list=ls())



