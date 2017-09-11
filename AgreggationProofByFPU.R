# codigo para hacer agregacion pof fpu para hacer evaluaciones.
# Carlos Eduardo Gonzalez R. 


library(RColorBrewer)
#GCMs----------------------
#directorios
grd1<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/")
copy<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/FPUWeight/")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m", "WFD")
resum<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/hist/")

#objetos
sys<- c( "IRRI", "RA")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 

############################################### Agreggation by FPU and evaluation    ##########################################

for(c in 1:length(crops)){
      
      for(s in 1:length(sys)){
            
                for(p in 1:length(pots)){

                  dataF<-list.files(path = grd1,pattern =paste(crops[c],"_", sys[s], sep =""),full.names = T)
                  dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
                  DataFiles<- do.call(rbind,dataF); DataFiles$X<- NULL
                  gcm<- unique(DataFiles$sce) #; pots<- unique(DataFiles$FPU)
                  

                  #por FPU
                        
                  DataFiles<- DataFiles[which(DataFiles$FPU==pots[p]),]
                  
                  if(nrow(DataFiles) >= 1){
                        #Agregar columnas de producción de 2022 a 2049
                        DataFiles[,paste0("Prod_20",22:49)]<-DataFiles[,"Area"]*DataFiles[,paste0("X20",22:49)]
                        DataFiles[,'ones'] = 1
                        
                        #Eliminar las columnas de los rendimientos
                        DataFiles<-DataFiles[,!names(DataFiles) %in% (paste0("X20",22:49))]
                        
                        #Agregar producción y area a nivel de fpu
                        DataFiles_fpu<-aggregate(DataFiles[,c("ones","Area",paste0("Prod_20",22:49))], by=list(DataFiles[,"sce"]),FUN= function(x) {sum(x, na.rm=TRUE)} )
                        
                        #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
                        DataFiles_fpu[,paste0("Rend_fpu_20",22:49)]<-DataFiles_fpu[,paste0("Prod_20",22:49)]/DataFiles_fpu[,"Area"]
                        
                        #Crear un data frame con sólo FPU y rend a nivel de fpu 
                        DataFiles_fpu<- DataFiles_fpu[,c("Group.1","ones",paste0("Rend_fpu_20",22:49))]
                  
                        #Asignar nombres apropiados a las columnas
                        colnames(DataFiles_fpu)<-c("Sce","num_pixels",paste0("Rend_fpu_20",22:49))
                        DataFiles_fpu$sys<- sys[s]
                        DataFiles_fpu$crop<- crops[c]
                        DataFiles_fpu$pot<- pots[p]

                        #Exportar resultados
                        write.csv(DataFiles_fpu,paste(copy,crops[c],"_",sys[s],"_", pots[p],"_FPU.csv", sep = ""))
                        
                        cat(paste("Running Yields weight and aggregate for ", crops[c]," ", sys[s], " it's done\n", sep = "" ))
                  }else{cat(paste(" Cultivo ", crops[c]," para el FPU= " , pots[p], " does not have varieties\n", sep = ""))}
                  
            }      
      }      
}

###################### apliar 

by_pots<- list()
by_graphs<- list()

for(c in 1:length(crops)){
      c_files<-list.files(path = copy,pattern =crops[c],full.names = T)
      c_files<-lapply(c_files,read.csv,stringsAsFactors = F)
      c_files<- do.call(rbind,c_files); c_files$X<- NULL
      c_files<- c_files[,c("Sce", "num_pixels","sys","crop", "pot",paste0("Rend_fpu_20", 22:49))]
      pots<- unique(c_files$pot)
      
      for(p in 1:length(pots)){ 
            by_pots[[p]] <- c_files[which(c_files$pot==pots[[p]]),]
            require(plyr)
            require(tidyr)
            by_pots[[p]]<- by_pots[[p]] %>% 
                  gather(year,val, 6:33) 
            by_pots[[p]]$year<- sub(pattern = "Rend_fpu_", replacement = "", x = by_pots[[p]]$year, ignore.case = T)
                  
#             ### Trend
#             png(filename = paste(resum,crops[c],"_",pots[p],"AggWeight_trend",".png",sep=""), 
#                 width = 20, height = 12, units = 'in', res = 100)
            
            require(ggplot2)
            by_graphs[[p]]<-ggplot(data=by_pots[[p]], aes(x=year,y=val, colour=Sce))+ 
                        geom_line(aes(group=Sce))+ geom_point()+ 
                        facet_grid(~factor(sys))+ ylab("Yield") +  xlab("Years") +
                        theme(axis.text.x=element_text(size=7, angle=90))+
                        labs(title=paste(crops[c], ":"," Yields weighted by  FPU= ",pots[p], " ,Irrigated and Rainfed\n Trend 2020-2049", sep = ""))+
                        theme(strip.text.y = element_text(angle = 0,size = 12)) 
#             plot(n1)
#             dev.off()
#             
            
          
           
            ###### Ribbon
            by_potsR<-  by_pots[[p]]
            by_potsR<- by_potsR %>% spread("Sce", "val")
            names(by_potsR)

            by_potsR[,"Ymax"] <- apply(by_potsR[, 6:14], 1, max)
            by_potsR[,"Ymin"] <- apply(by_potsR[, 6:14], 1, min)
            by_potsR<-by_potsR[,-c(6:14)]
            wfd<- by_potsR[,c(1:6)]
            dmin<- by_potsR[,c(1:5,8)]
            dmax<- by_potsR[,-c(1:5,7)]
            #by_potsR<- by_potsR %>% gather("Sce", "val", 6:ncol(by_potsR))
            
            png(filename = paste(resum,crops[c],"_",pots[p]," AggWeight_Ribbon",".png",sep=""), 
                width = 15, height = 15, units = 'in', res = 100)
            
            require(ggplot2)
            n2<- ggplot(data= by_potsR, aes(year,group=sys)) + 
                  geom_ribbon(aes(ymin=Ymin,ymax=Ymax,linetype=NA),fill="red", alpha=0.3)+ 
                  geom_line(data=by_potsR,aes(year, WFD, group = sys),size=0.75, alpha=0.7)+ 
#                   theme(axis.text.x=element_text(size=12, angle=90))+
#                   theme(axis.text.y =element_text(size=12, angle=90))+
                  facet_grid(.~sys,labeller =label_parsed,scales = "free")+ ylab("Yield Weighted (kg/ha)") +  xlab("Years") +
                  labs(title=paste(crops[c], ":"," Yields weighted by  FPU= ",pots[p], " ,Irrigated and Rainfed\n Trend 2020-2049", sep = ""))+
                  theme(strip.text.y = element_text(angle = 0,size = 12)) + theme_grey()+ coord_equal() 
                  
            
            plot(n2)
            dev.off()
            
            
            
            cat(paste(crops[c], " ", pots[p], " it's done\n", sep = ""))   
      }
      
      
      }



g=gc;rm(list = ls())




