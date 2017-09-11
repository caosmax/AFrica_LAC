

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#directorios------
gdr1<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/")
grdw<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/")
resum<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/")
hist<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/hist/")
crops<- c("Rice","Bean","Wheat","Maize", "Soybean") 
sys<- c("IRRI","RA")


# previsualizacion y detección de datos no deseables-------------
for(c in 1:length(crops)){
       for(s in 1:length(sys)){
            cat(paste("Running review yield value: ",crops[c], " ", sys[s], " done!!!\n", sep = ""))
            
            c_files <- list.files(path=paste(gdr1,"/", crops[c],sep = ""),pattern =sys[s],full.names = T)
            c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
            c_files <- do.call(rbind,c_files)
            #reshape corregir año
            require(tidyr)
            require(dplyr)
            c_files<- c_files %>% gather(year, val, 10:37)
            c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
            c_files$X<- NULL
            c_filesMinusZero<- c_files[which(c_files$val!=0),]
            c_filesall<- c_files
            
            #grafico para inspeccionar datos extremos
            jpeg(paste(hist,crops[c],"_",sys[s],'_Hist&dens.jpg',sep = ""))
          
            hist(c_filesall$val, breaks=length(c_filesall),xlab = "Yield", ylab = "Frecuency",ylim = c(0,0.00050),
                 main= "Histograma de rendimientos", font.main=2, font.lab=2, cex.main=1.25,freq =F,border="gray22")
            par(new=T)
            hist(c_filesMinusZero$val, breaks=length(c_filesMinusZero),xlab = "Yield", ylab = "Frecuency",ylim = c(0,0.00050),
                 main= "Histograma de rendimientos", font.main=2, font.lab=2, cex.main=1.25,freq =F,border="gray55")
            lines(density(c_filesall$val,na.rm=TRUE),lwd=2, col="red")
            lines(density(c_filesMinusZero$val,na.rm=TRUE),lwd=2, col="blue")
            legend(5000,0.00015,c("All Yields", "Yields>0"), lty = c(1,1), lwd = c(2,2),col = c("red", "blue"))
            dev.off()
            
            
            #Generación de tablas 
            sumstat<- c_filesMinusZero %>%
                  #select and rename variables
                  select(
                        'Yield (kg/ha)'= val,
                        'Area (ha)'= Area)%>%
                  
                  #find the mean, st.dev, min and max each variable
                  summarise_each(funs( mean, sd, min, max))%>%
                  gather(key, value, everything()) %>% 
                  separate(key, into = c("variable", "stat"), sep = "_") %>%
                  spread(stat, value) %>%
                  # Set order of summary statistics 
                  select(variable, mean, sd, min, max) %>%
                  
                  # Round all numeric variables to one decimal point
                  mutate_each(funs(round(., 1)), -variable)
            sumstat$crop<- crops[c]
            sumstat$sys<- sys[s]
            write.csv(sumstat,paste(resum,"TableSummary",crops[c],"_",sys[s],".csv",sep = "")) 
       }
}

rm(c_files, c_filesMinusZero, c_filesall)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% reporte   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# produccion de datos descriptivos-------------
c_files <- list.files(path=resum,pattern ="Table",full.names = T)
c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
c_files <- do.call(rbind,c_files)
c_files$X<- NULL
          
c_files<- c_files[,c("crop","sys","variable", "mean","sd", "min", "max")]

write.csv(c_files,file = paste(resum,"SummaryALLcrops.csv", sep = ""))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Nopixeles %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Resumenes de datos con max y min, descriptivos-------------
for(c in 1:length(crops)){
      for(s in 1:length(sys)){
            cat(paste("Running review yield value: ",crops[c], " ", sys[s], " done!!!\n", sep = ""))
            
            c_files <- list.files(path=paste(gdr1,"/", crops[c],sep = ""),pattern =sys[s],full.names = T)
            c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
            c_files <- do.call(rbind,c_files)
            #reshape corregir año
            require(tidyr)
            require(dplyr)
            c_files<- c_files %>% gather(year, val, 10:37)
            c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
            c_files$X<- NULL
            data<- c_files
            data<- data[which(data$val>0),]
            data<- data %>% summarise_all(c("min", "max"))
            data$crop<- crops[c]
            write.csv(data,paste(resum,"MinMax_",crops[c],"_",sys[s],".csv", sep = ""))
           
      }
}


c_files <- list.files(path=resum,pattern ="MinMax",full.names = T)
c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
c_files <- do.call(rbind,c_files)
c_files$X<- NULL
write.csv(c_files,file = paste(resum,"SummaryALLcropsMinMax.csv", sep = ""))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#Lista de pixeles por fuera del intervalo de confianza--------------



for(c in 1:length(crops)){
      for(s in 1:length(sys)){
            cat(paste("Running review yield value: ",crops[c], " ", sys[s], " done!!!\n", sep = ""))
            
            c_files <- list.files(path=paste(gdr1,"/", crops[c],sep = ""),pattern =sys[s],full.names = T)
            c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
            c_files <- do.call(rbind,c_files)
            #reshape corregir año
            require(tidyr)
            require(dplyr)
            c_files<- c_files %>% gather(year, val, 10:37)
            c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
            c_files$X<- NULL
            c_files<- c_files[which(c_files$val>0),]
            rownames(c_files)<- 1:nrow(c_files)
            summary(c_files$val)
            
            #lowest
            qWierd1<- quantile(c_files$val, probs=0.001,na.rm=TRUE)
            c_filesqWierd1<- c_files[which(c_files$val<=qWierd1),] 
            c_filesqWierd1$description<- "Lowest"
            c_filesqWierd1$crop<-crops[c]
            
            #hightes
            qWeird2<- quantile(c_files$val, prob=0.9999,na.rm=TRUE)
            c_filesqWierd2<- c_files[which(c_files$val>qWeird2),] 
            c_filesqWierd2$description<- "Hightest"
            c_filesqWierd2$crop<-crops[c]
      
            weirdest<- rbind(c_filesqWierd2, c_filesqWierd1)
            rownames(weirdest)<- 1:nrow(weirdest)
            
            
           write.csv(weirdest,paste(resum,"ExtremeData",crops[c],"_",sys[s] ,".csv",sep = "")) 
      
            
      }
}
rm(c_files)

c_files <- list.files(path=resum,pattern ="^ExtremeData",full.names = T)
c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
c_files <- do.call(rbind,c_files)
c_files$X<- NULL


write.csv(c_files,file = paste(resum,"SummaryExtremeDataCrops.csv", sep = ""))
