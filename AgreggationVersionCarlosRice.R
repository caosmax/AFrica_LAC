# codigo de agregacion version carlos 


#GCMs----------------------
#directorios
grd1<-("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/")
copy<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/YieldsWeight/")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")


#objetos
sys<- c( "IRRI", "RA")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 

# c=1
# s=1
# g=1
for(c in 1:length(crops)){
      
      for(g in 1:length(gcm)){
            
            try(for(s in 1:length(sys)){
                  dataF<-list.files(path =grd1,pattern = paste(crops[c],"_",sys[s],"_.csv",sep=""),full.names = T)
                  dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
                  DataFiles<- do.call(rbind,dataF)
                  DataFiles$X<- NULL
                  gcm<- unique(DataFiles$sce)
                  
                  if((crops[c]=="Rice")){
                        
                        require(dplyr)
                        require(tidyr)
                        test<- filter(DataFiles,FPU %in% c("CHC_CHL","NWS_ECU"))
                        DataFiles<- DataFiles[which(DataFiles$FPU!="CHC_CHL"& DataFiles$FPU!="NWS_ECU"),]
                        zeros.wfd.r = apply(test[,8:ncol(test)],1,function(x) sum(x==0,na.rm=T))
                        ind.falla = which(zeros.wfd.r>=14)
                        
                        if(sum(ind.falla)!=0){ 
                               test<-test[-ind.falla,]
                        }else{}
                        
                  DataFiles<- rbind(DataFiles,test)
                  }else{}
                  
                  DataFiles<- DataFiles[which(DataFiles$sce==gcm[g]),]
                  #Agregar columnas de producción de 2022 a 2049
                  DataFiles[,paste0("Prod_20",22:49)]<-DataFiles[,"Area"]*DataFiles[,paste0("X20",22:49)]
                  DataFiles[,'ones'] = 1
                  
                 
                  #Eliminar las columnas de los rendimientos
                  DataFiles<-DataFiles[,!names(DataFiles) %in% (paste0("X20",22:49))]
                  
                  
                  #Agregar producción y area a nivel de fpu
                  DataFiles_fpu<- aggregate(DataFiles[,c("ones","Area",paste0("Prod_20",22:49))],by=list(DataFiles[,"FPU"]),FUN= function(x) {sum(x, na.rm=TRUE)} )
                  
                  #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
                  DataFiles_fpu[,paste0("Rend_fpu_20",22:49)]<-DataFiles_fpu[,paste0("Prod_20",22:49)]/DataFiles_fpu[,"Area"]
                  
                  #Crear un data frame con sólo FPU y rend a nivel de fpu 
                  DataFiles_fpu<- DataFiles_fpu[,c("Group.1","ones",paste0("Rend_fpu_20",22:49))]
                  
                  #Asignar nombres apropiados a las columnas
                  colnames(DataFiles_fpu)<-c("FPU","num_pixels",paste0("Rend_fpu_20",22:49))
                  # DataFiles_fpu$mean<- rowMeans(DataFiles_fpu[,3:30]) 
                  
                  DataFiles_fpu$sce<- gcm[g]
                  DataFiles_fpu$sys<- sys[s]
                  
                  #Ordenar datos
                  DataFiles_fpu<- DataFiles_fpu[,c("FPU","num_pixels", "sce", "sys", paste0("Rend_fpu_20",22:49))]
                  
                  #Exportar resultados
                  write.csv(DataFiles_fpu,paste(copy,crops[c],"_",sys[s],"_",gcm[g], "_FPU.csv", sep = ""))
                  
                  cat(paste("Running Yields weight and aggregate for ", crops[c]," ", sys[s], " ",  gcm[g], " it's done\n", sep = "" ))
                  
            } )     
      }      
}





#Datos historicos---------------
#directorios


#objetos
w<- "WFD"


for(c in 1:length(crops)){
             for(s in 1:length(sys)){

                  dataF<-list.files(path =grd1,pattern =paste(crops[c],"_", sys[s],"_WFD",sep = ""), full.names = T)
                  dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
                  DataFiles<- do.call(rbind,dataF)
                  DataFiles$X<- NULL
                  rownames(DataFiles)<- 1:nrow(DataFiles)
                  
                  if((crops[c]=="Rice")){
                        
                        require(dplyr)
                        require(tidyr)
                        test<- filter(DataFiles,FPU %in% c("CHC_CHL","NWS_ECU"))
                        DataFiles<- DataFiles[which(DataFiles$FPU!="CHC_CHL"& DataFiles$FPU!="NWS_ECU"),]
                        zeros.wfd.r = apply(test[,8:ncol(test)],1,function(x) sum(x==0,na.rm=T))
                        ind.falla = which(zeros.wfd.r>=14)
                        
                        if(sum(ind.falla)!=0){ 
                              test<-test[-ind.falla,]
                        }else{}
                        
                        DataFiles<- rbind(DataFiles,test)
                  }else{} 

                  #Agregar columnas de producción de 2022 a 2049
                  DataFiles[,paste0("Prod_20",22:49)]<-DataFiles[,"Area"]*DataFiles[,paste0("X20",22:49)]
                  DataFiles[,'ones'] = 1
                  
                  #Eliminar las columnas de los rendimientos
                  DataFiles<-DataFiles[,!names(DataFiles) %in% (paste0("X20",22:49))]
                  
                  
                  #Agregar producción y area a nivel de fpu
                  DataFiles_fpu<-aggregate(DataFiles[,c("ones","Area",paste0("Prod_20",22:49))],by=list(DataFiles[,"FPU"]),FUN= function(x) {sum(x, na.rm=TRUE)} )
                  
                  #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
                  DataFiles_fpu[,paste0("Rend_fpu_20",22:49)]<-DataFiles_fpu[,paste0("Prod_20",22:49)]/DataFiles_fpu[,"Area"]
                  
                  #Crear un data frame con sólo FPU y rend a nivel de fpu 
                  DataFiles_fpu<- DataFiles_fpu[,c("Group.1","ones",paste0("Rend_fpu_20",22:49))]
                  
                  #Asignar nombres apropiados a las columnas
                  colnames(DataFiles_fpu)<-c("FPU","num_pixels",paste0("Rend_fpu_20",22:49))
                  DataFiles_fpu$sce<- "WFD"
                  DataFiles_fpu$sys<- sys[s]
                  #DataFiles_fpu$mean<- rowMeans(DataFiles_fpu[,3:30]) 
                  
                  #Ordenar datos
                  DataFiles_fpu<- DataFiles_fpu[,c("FPU","num_pixels", "sce", "sys", paste0("Rend_fpu_20",22:49))]
                  
                  
                  #Exportar resultados
                  write.csv(DataFiles_fpu,paste(copy,crops[c],"_",sys[s],"_","WFD", "_FPU.csv", sep = ""))
                  
                  cat(paste("Running Yields weight and aggregate for ", crops[c], " " , sys[s] , " WFD", " it's done\n", sep = "" ))
                  
            }      
      }      


g=gc;rm(list = ls())
