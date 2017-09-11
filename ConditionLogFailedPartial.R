# Seleccion de mayores rendimientos por prixel por Año

########################################################### GCMs -------------------------------

#directorios
gdr1<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/")
grdw<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/")
resum<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/")
hist<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/hist/")

########### PARTE B
#cargar files
crops<- c("Rice","Bean","Wheat","Maize", "Soybean") 
sys<- c("IRRI","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")
variedades<- c("A193", "BAT881","BRSRadiante","Carioca", "ICTAOstua", "Manitou", "Perola")

c=2
s=2
for(c in 1:length(crops)){
      for(s in 1:length(sys)){
            cat(paste("Running select to max value for yield: ",crops[c], " ", sys[s], " done!!!\n", sep = ""))
            
            c_files <- list.files(path=paste(gdr1,"/", crops[c],sep = ""),pattern = sys[s],full.names = T)
            c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
            c_files <- do.call(rbind,c_files)
            c_files$X<- NULL
            #reshape corregir año
            require(plyr)
            require(tidyr)
            c_files<- c_files %>% 
                  gather(year,val, 9:36) 
            c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
            
            # rendimeintos por pixel/variedad/año
            c_files<- c_files %>% 
                  spread(v,val) 
            c_files[,"ymax"] <- apply(c_files[, 9:ncol(c_files)], 1, max)
      
            ##Analisis de rendimientos seleccionados
            c_filesMinusZero<- c_files[which(c_files$ymax!=0),]
            c_filesall<- c_files
            
       
            #######################################Generación de tablas summary
            sumstat<- c_filesMinusZero %>%
                  #select and rename variables
                  select(
                        'Yield (kg/ha)'= ymax,
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
            write.csv(sumstat,paste(resum,"YieldSelectTableSummary_",crops[c],"_",sys[s],".csv",sep = "")) 
            
            #######################################Extreme yields
            
            #lowest
            qWierd1<- quantile(c_filesMinusZero$ymax, probs=0.01,na.rm=TRUE)
            c_filesqWierd1<- c_filesMinusZero[which(c_filesMinusZero$ymax<=qWierd1),] 
            c_filesqWierd1$description<- "Lowest"
            c_filesqWierd1$crop<-crops[c]
            
            #hightes
            qWeird2<- quantile(c_filesMinusZero$ymax, prob=0.99,na.rm=TRUE)
            c_filesqWierd2<- c_filesMinusZero[which(c_filesMinusZero$ymax>=qWeird2),] 
            c_filesqWierd2$description<- "Hightest"
            c_filesqWierd2$crop<-crops[c]
            
            weirdest<- rbind(c_filesqWierd2, c_filesqWierd1)
            rownames(weirdest)<- 1:nrow(weirdest)
            
            write.csv(weirdest,paste(resum,"SelectVar_ExtremeData",crops[c],"_",sys[s] ,".csv",sep = "")) 
            
            c_filesqWierd1$review<- NA
            
            # logica patron de variedades con fallos y variedades con rendimientos muy bajos.
            rownames(c_filesqWierd1)<- 1:nrow(c_filesqWierd1)
            for(i in 1:nrow(c_filesqWierd1)){
                  if (((c_filesqWierd1$Perola[i]==0) + (c_filesqWierd1$ICTAOstua[i]==0) + (c_filesqWierd1$BRSRadiante[i]==0) + (c_filesqWierd1$A193[i]==0) +(c_filesqWierd1$Manitou[i]==0) + (c_filesqWierd1$Carioca[i] == 0) + (c_filesqWierd1$BAT881[i]== 0)) >= 4){c_filesqWierd1$review[i]<- 1} else{ c_filesqWierd1$review[i]<- 0}
                  }
                 
            write.csv(c_filesqWierd1,paste(resum,"SelectVar_Lowest_ExtremeData",crops[c],"_",sys[s] ,".csv",sep = "")) 
      
      }
}



g=gc;rm(list = ls())
