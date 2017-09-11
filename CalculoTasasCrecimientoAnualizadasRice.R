### calculo de tasas de crecimiento anualizada, ajuste y correcion Rice

grd<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/YieldsWeight/")
copy<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/TasasCrecimiento/")
export<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/YieldMeanByGCM/")
      
# Limitar numero de decimales
options(digits=3) 
options(scipen=999)

#Lista de tipos de sistemas
sys<- c( "IRRI", "RA")
crops<- c("Rice","Bean","Wheat","Maize", "Soybean") 



models<- c("bcc_csm1_1","bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr", "miroc_miroc5" ,"mpi_esm_mr","ncc_noresm1_m","WFD") 

# c=1
# s=1
# m=1
for (c in 1:length(crops)){
      for (s in 1:length(sys)) {
            for(m in 1:length(models)){
                  dataF<-list.files(path =grd,pattern = paste(crops[c],"_",sys[s],sep = ""),full.names = T)
                  dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
                  dataF<- do.call(rbind,dataF)
                  dataF$X<- NULL
                  
                  row.names(dataF)<- 1:nrow(dataF)
                 
                  #promedio de los rendimientos para todo los tiempos
                  dataF$mean<- rowMeans(dataF[,5:ncol(dataF)])          #  na.rm=TRUE        
                  
                  #Eliminar columnas innecesarias
                  dataF<- dataF[, c("FPU", "sce", "sys","mean")] # "num_pixels"
             
                  #para poner  los gcms como columnas      
                  require(plyr)
                  require(tidyr)
                  y<- dataF %>% spread(sce,mean) 
                  
                  #creando matrix para vaciar los datos
                  tc_an<- matrix(nrow =nrow(y),ncol = ncol(y))
                  t<-1986:2036 # est o ha sido ajustado

                  ###calculo de la tasa de crecimiento
                  for (i in 3:ncol(y)){
                        tc_an[,i]<-  (log(y[,i]/y[,ncol(y)])/(length(t)-1))
                        }                  
                  
                  
                  #correcion y ajuste nombres para la matriz
                  #Los NAs los reemplazo por ceros
                  tc_an[,3:11][is.infinite(tc_an[,3:11])] = NA                  
                  tc_an[is.na(tc_an)==TRUE]=0
                  tc_an<- tc_an[,-c(1:2)]
                  colnames(tc_an)<- models
                  rownames(tc_an)<- y$FPU
            
                  tc_an<- as.data.frame(tc_an)
                  tc_an$crops<- crops[c]
                  tc_an$treat<- sys[s]
                  tc_an<- tc_an[,c( "crops", "treat", models)]
                  
                  write.csv(x = tc_an,file = paste(copy,"tc_an_",crops[c],"_",sys[s],"_FPU.csv", sep = ""))
                  cat(paste("Running rates", crops[c], " ", sys[s], " it's done\n", sep = "" ))
                  }
            }
      }
   

      


g=gc;rm(list=ls())


