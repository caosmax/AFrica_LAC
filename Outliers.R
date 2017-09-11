## Deteccion de outliers 
## Autor Carlos Eduardo Gonzalez R. 


########################################### Detection outliers ####################################################
gdr1<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/")
sys<- c( "IRRI", "RA")
crops<- c("Rice","Bean","Wheat","Maize", "Soybean") 
data_out<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/outliers/")

########################################### Proceso para detectar los outliers generacion de tablas--------

#c=1
for(c in 1:length(crops)){ 
      #cargar files
      c_files <- list.files(path= paste(gdr1,crops[c],"/",sep=""), pattern =".csv",full.names = T)
      c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
      c_files <- do.call(rbind,c_files)
      c_files$X<- NULL

      require(plyr)
      require(tidyr)
      c_files<- c_files %>% 
            gather(year,val, 9:36) 
      c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
      
      c_files<- c_files[which(c_files$val!=0),]
      c_files$crops<-crops[c]
      rownames(c_files)<- 1:nrow(c_files)
      
      # define a function to remove outliers
      FindOutliers <- function(data) {
            lowerq = quantile(data)[2]
            upperq = quantile(data)[4]
            iqr = upperq - lowerq 
            #Or use IQR(data)
            # we identify extreme outliers
            extreme.threshold.upper = (iqr * 3) + upperq
            extreme.threshold.lower = lowerq - (iqr * 3)
            result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
      }
      
      # use the function to identify outliers
      temp <- FindOutliers(c_files$val)
      cfOut<- c_files[temp,]
      write.csv(cfOut,paste(data_out, crops[c],"OutliersGCMs.csv", sep = ""))
      cat(paste("Detection outliers in ", crops[c], " processing results\n complete!!!", sep = ""))
      
      
      
      }

########################################### Report Cases ########################
# Deteccion y generacion de la tablas para informes de status de pixeles a nivel FPU
gdr1<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/")
sys<- c( "IRRI", "RA")
crops<- c("Rice","Bean","Wheat","Maize", "Soybean") 
data_out<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/outliers/")


library(dplyr)
library(tidyr)


by_pots<-list()
for(c in 1:length(crops)){ 
      #cargar files
      c_files <- list.files(path= paste(gdr1,crops[c],"/",sep=""), pattern =".csv",full.names = T)
      c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
      c_files <- do.call(rbind,c_files)
      c_files$X<- NULL
      pots<- unique(c_files$FPU)
      
      for(p in 1:length(pots)){ 
            by_pots[[p]]<- filter(c_files, FPU==pots[p])

            if(nrow(by_pots[[p]]) >= 1){
                  by_pots[[p]]<- by_pots[[p]] %>% 
                        gather(year,val, 9:36) 
                  by_pots[[p]]$year<- sub(pattern = "X", replacement = "", x = by_pots[[p]]$year, ignore.case = T)
                  by_pots[[p]]$val[which(is.na(by_pots[[p]]$val))] <- 0

                  
                  
                  #segunda parte
                  test <- sum(by_pots[[p]]$val)
                  
                  if(test>0){  
                        SinCeros<- filter(by_pots[[p]], val!=0)
                        
                        sumstat<- SinCeros %>%
                              #select and rename variables
                              select(
                                    'Yield (kg/ha)'= val,
                                    'Area (ha)'= Area)%>%
                              summarise_each(funs( mean, sd, min, max))%>%
                              gather(key, value, everything()) %>% 
                              separate(key, into = c("variable", "stat"), sep = "_") %>%
                              spread(stat, value) %>%
                              select(variable, mean, sd, min, max) %>%
                              mutate_each(funs(round(., 1)), -variable)
                              sumstat$crop<- crops[c]
                              sumstat$pots<- pots[p]
                        
                        write.csv(sumstat,paste(data_out,crops[c],"_",pots[p],"_TableSummary.csv",sep = "")) 
                        
                        q1<- quantile(SinCeros$val,probs = 0.05,na.rm = T)
                        q2<- quantile(SinCeros$val,probs = 0.99,na.rm = T)
                        
                        
                        inf<- by_pots[[p]] 
                        
                        for(i in 1:nrow(inf)){
                              if(((inf$val[i]<=q1)+(inf$val[i]!=0))>=2) {inf$status[i]<- "lowest Yield (lower than 5% probs)"}else{} 
                              if(inf$val[i]==0){inf$status[i]<- "Fallo total (yield==0)"}else{}
                              if(((inf$val[i]!=0)+(inf$val[i]>q1))>=2){inf$status[i]<- "Normal"}else{} 
                              if(((inf$val[i]!=0)+(inf$val[i]>q2))>=2){inf$status[i]<- "Highest (Higher than 99% probs)"}else{} 
                        }

                        infRepor<-  filter(inf,status!="Normal") %>% mutate(crop=crops[c])
                        write.csv(infRepor,paste(data_out,"DataExtremes_",pots[p],"_",crops[c],".csv", sep = ""))
                        
                        
                        #tabla resumen datos
                        tab<-as.data.frame(table(inf$sce, inf$status, inf$sys, inf$v))
                        colnames(tab)[1]<- 'Scenario'; colnames(tab)[2]<- 'Status'; colnames(tab)[3]<- 'System'
                        colnames(tab)[4]<- 'Variety';  colnames(tab)[5]<- 'Freq'
                        
                        tab<- tab %>% spread(Status,Freq)
                        tab$System<- as.character(tab$System)
                        tab$Variety<- as.character(tab$Variety)
                        tab$N<- rowSums(tab[,4:ncol(tab)],na.rm = T)
                        
                        if(ncol(tab)==7){
                              tab$`Fallo total (yield==0)`<-0
                              tab<- tab[,c("System","Variety","Scenario","N","Fallo total (yield==0)","Highest (Higher than 99% probs)","lowest Yield (lower than 5% probs)", "Normal")]
                              tab<- tab %>% gather("Status", "Freq", 5:ncol(tab))%>% mutate(ratio=(Freq/N)*100,crop=crops[c],fpu= pots[p])                      
                              #Reporte esta de cada FPU
                              write.csv(tab,paste(data_out,"Report_Cases_Yields_",crops[c],"_",pots[p],"_FPU.csv", sep = ""))
                              
                        }else{cat(paste(" Cultivo ", crops[c]," para el FPU= " , pots[p], " tiene todas las columnas\n", sep = ""))
                              tab<- tab[,c("System","Variety","Scenario","N","Fallo total (yield==0)","Highest (Higher than 99% probs)","lowest Yield (lower than 5% probs)", "Normal")]
                        
                              tab<- tab %>% gather("Status", "Freq", 5:ncol(tab))%>% mutate(ratio=(Freq/N)*100,crop=crops[c],fpu= pots[p])                      
                              #Reporte esta de cada FPU
                              write.csv(tab,paste(data_out,"Report_Cases_Yields_",crops[c],"_",pots[p],"_FPU.csv", sep = ""))}
                              
                  }else{cat(paste(pots[p], " tiene ceros en los datos\n descarta", sep = ""))}
                  
            }else{cat(paste(" Cultivo ", crops[c]," para el FPU= " , pots[p], " does not have varieties\n", sep = ""))}
            cat(paste(crops[c], " ", pots[p], " It has been complete\n", sep = ""))
            print(p)
            }                   

}


##################### Apilar los datos finales para el reporte
###  compilacion reportes
c_files <- list.files(path=data_out, pattern =c("DataExtremes_"),full.names = T)
c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
c_files <- do.call(rbind,c_files)
c_files$X<- NULL
write.csv(c_files,paste(data_out, "SummaryLowest&HightestWFD.csv", sep = ""))           
saveRDS(c_files,paste(data_out, "SummaryLowest&HightestWFD.RDS", sep = "")) 


##################### Reporte casos panorama inicial 
for(c in 1:length(crops)){
      c_files <- list.files(path=data_out, pattern = paste("Report_Cases_Yields_",crops[c],"_", sep = ""),full.names = T)
      c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
      
   for(i in 1:length(c_files)){
         
      if(ncol(c_files[[i]])<10){
      cat(paste("Este archivo le faltan de datos ", print(i), " va ha ser eliminado\n", sep = ""))
      c_files[[i]]<-NULL
      }else{}
            
} 
      c_files <- do.call(rbind,c_files)
      c_files$X<- NULL
      write.csv(c_files,paste(data_out,crops[c],"SummaryCases.csv", sep = ""))     
      cat(paste(crops[c], " It has been completed\n", sep="")) 
}
     

########################################### IRRIGADO ----   

library(dplyr)
library(RColorBrewer)


 for(c in 1:length(crops)){
            c_files <- list.files(path=data_out, pattern="SummaryCases.csv", full.names = T)
            c_files<- lapply(c_files,read.csv,stringsAsFactors = F)
            c_files <- do.call(rbind,c_files)
            c_files$X<- NULL
            c_files<- filter(c_files, System=="IRRI")
            
            c_files<- filter(c_files, crop==crops[c])
            hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
            
            png(filename = paste(data_out,crops[c],"_IRRI_behaviour_Data",".png",sep=""), 
                width = 20, height = 12, units = 'in', res = 100)
            
            require(ggplot2)
            n<- ggplot(data =c_files, aes(Scenario,fpu)) + 
                  geom_tile(aes(fill = ratio), colour = "white")+ 
                  labs(x=NULL, y=NULL, title="") +
                  scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + 
                  labs(x = "",y = "") + labs(title = paste( "Rendimientos muy altos, muy bajos, fallidos, y normales a nivel de pixel. Estos, se expresan como tasa usando la distribución de datos a nivel de FPU \nCultivo = ",crops[c], "", ", sistema= Irrigado", sep = ""))+
                  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
                  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
                  theme(axis.text.y = element_text(hjust = 1, size = 12))+
                  theme(strip.text.x = element_text(size = 12, face = "bold.italic"))+
                  facet_grid(.~Status, scales = "fixed", space = "fixed") +theme(strip.text=element_text(size=8))+
                  theme(strip.text.y = element_text(angle = 0,size = 12))
            
            plot(n)
            dev.off() 
            
            cat(paste(crops[c], " It has been completed\n", sep="")) 
            
      } 
      
      

      

########################################### SECANO ----   

library(dplyr)
library(RColorBrewer)

for(c in 1:length(crops)){
      c_files <- list.files(path=data_out, pattern="SummaryCases.csv", full.names = T)
      c_files<- lapply(c_files,read.csv,stringsAsFactors = F)
      c_files <- do.call(rbind,c_files)
      c_files$X<- NULL
      c_files<- filter(c_files, System=="RA")
      
      c_files<- filter(c_files, crop==crops[c])
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      
      png(filename = paste(data_out,crops[c],"_RA_behaviour_Data",".png",sep=""), 
          width = 20, height = 12, units = 'in', res = 100)
      
      require(ggplot2)
      n<- ggplot(data =c_files, aes(Scenario,fpu)) + 
            geom_tile(aes(fill = ratio), colour = "white")+ 
            labs(x=NULL, y=NULL, title="") +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + 
            labs(x = "",y = "") + labs(title = paste( "Rendimientos muy altos, muy bajos, fallidos, y normales a nivel de pixel. Estos, se expresan como tasa usando la distribución de datos a nivel de FPU \nCultivo = ",unique(c_files$crop), "", ", sistema= Secano", sep = ""))+
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
            theme(axis.text.y = element_text(hjust = 1, size = 12))+
            theme(strip.text.x = element_text(size = 12, face = "bold.italic"))+
            facet_grid(.~Status, scales = "fixed", space = "fixed") +theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 12))
      
      plot(n)
      dev.off() 
      
      cat(paste(crops[c], " It has been completed\n", sep="")) 
      
} 

########################################## outliers detection #################################################

for(c in 1:length(crops)){ 
      #cargar files
      c_files <- list.files(path= paste(data_out,crops[c],"/",sep=""), pattern ="WFD",full.names = T)
      c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
      c_files <- do.call(rbind,c_files)
      c_files$X<- NULL
      
      
      require(plyr)
      require(tidyr)
      c_files<- c_files %>% 
            gather(year,val, 9:36) 
      c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
      
      c_files<- c_files[which(c_files$val!=0),]
      rownames(c_files)<- 1:nrow(c_files)
      
      # define a function to remove outliers
      FindOutliers <- function(data) {
            lowerq = quantile(data)[2]
            upperq = quantile(data)[4]
            iqr = upperq - lowerq 
            #Or use IQR(data)
            # we identify extreme outliers
            extreme.threshold.upper = (iqr * 3) + upperq
            extreme.threshold.lower = lowerq - (iqr * 3)
            result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
      }
      
      # use the function to identify outliers
      temp <- FindOutliers(c_files$val)
      cfOut<- c_files[temp,]
      write.csv(cfOut,paste(resum, crops[c],"Outliers.csv", sep = ""))
}


g=gc;rm(list = ls())

########################################## Generacion de informes outliers-----------

c_files <- list.files(path= data_out, pattern ="OutliersGCMs.csv",full.names = T)
c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
c_files <- do.call(rbind,c_files)
c_files$X<- NULL

write.csv(c_files,paste(data_out,"TotalOutliers.csv", sep = ""))     


tab<-as.data.frame(table(c_files$FPU, c_files$crops, c_files$sys, c_files$v, c_files$sce))
colnames(tab)[1]<- 'FPU'; colnames(tab)[2]<- 'Crop'; colnames(tab)[3]<- 'System'
colnames(tab)[4]<- 'Variety';  colnames(tab)[5]<- 'Sce'

tab<- tab %>% spread(Sce,Freq)

write.csv(tab,paste(data_out,"SummaryOutliers.csv", sep = ""))     

########################################## Tabla resumen medias rangos,medias, desviacion----------
# # produccion de datos descriptivos
c_files <- list.files(path=resum,pattern ="Table",full.names = T)
c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
c_files <- do.call(rbind,c_files)
c_files$X<- NULL

c_files<- c_files[,c("crop","sys","variable", "mean","sd", "min", "max")]
write.csv(c_files,file = paste(resum,"SummaryALLcrops.csv", sep = ""))


####################### 

library(mice)
tempnomiss<- mice(replacecolumn)
