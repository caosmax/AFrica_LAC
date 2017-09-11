###########################################ProofByPixel####################################################
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/future/final/SOY_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/soybean/future/final/SOY_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyfuture<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Soybean_IRR/Soybean_Future/")
copyhistorical<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Soybean_IRR/Soybean_Historical/")
copyPix<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/Soybean/pix/")
grdpix<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/Soybean/")


##Cargar información de latitud, longitud, area de spam, fpu, etc.-------------------
load(paste(grd3,"Soybeans_riego",'.RDat',sep=''))


########### PARTE A

##----------------------------------------------------------------IRRIGATED 
###Future-------------

###GCMs y variedades de trigo
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

variedades<-  c("DONMARIO", "Hutcheson")
reg<- read.csv("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ListFPUs.csv",header = T,stringsAsFactors = F )
pots<- reg[,1]



##loop para cargar datos 
for (v in 1:length(variedades)){
      for (g in 1:length(gcm)){
            load(paste(gdr1,variedades[v],"_",gcm[g],".RDat",sep = "")) 
            
            #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
            rend<-matrix(nrow=length(Run), ncol=28)
            for (i in 1:length(Run))
            {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
            
            #descartar pixeles con demasiadas fallas
            rend[rend==-99] = 0  #convertir -99 a 0
            
            #             #find areas where consistently failing and discard these from aggregation
            #             zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
            #             ind.falla = which(zeros.wfd.r>=14)
            #             
            #Convertir rend en un data.frame
            rend<-data.frame(rend)
            
            #Asignar nombres a el data frame de rendimientos
            colnames(rend)<-paste0("20",22:49)
            
            
            #Creo un dataframe
            eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                                  Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                                  v=variedades[v],sce=gcm[g],sys="IRRI",rend)',sep='')))
            
            #             # Descartar pixeles con más de 13 años con fallas en la linea base
            #             if(sum(ind.falla) == 0)
            #             {
            #                   md<-md
            #             } else {
            #                   md<-md[-ind.falla,]
            #             }
            
            md$pix<- NA
            for (row in 1:nrow(md)){
                  md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
            }
            
            md<- md[,c("long","lat","pix","Area","FPU","v","sys", "sce", paste0("X20",22:49))]
            
            #Computador personal
            write.csv(md,paste(grdpix,"IRRI",'_',variedades[v],'_',gcm[g],".csv",sep=''),row.names=T)
            cat(paste("Running SOY IRRI ", variedades[v], " of gcm ", gcm[g], " Done!!\n", sep = "" ))
            
            
      } 
}




##----------------------------------------------------------------RAINFED 

###GCMs y variedades de soy
##Cargar información de latitud, longitud, area de spam, fpu, etc.-------------------
load(paste(grd3,"Soybeans_secano",'.RDat',sep=''))


##loop para cargar datos 
for (v in 1:length(variedades)){
      for (g in 1:length(gcm)){
            
            load(paste(gdr2,variedades[v],"_",gcm[g],".RDat",sep = ""))
            
            #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
            rend<-matrix(nrow=length(Run), ncol=28)
            for (i in 1:length(Run))
            {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
            
            #descartar pixeles con demasiadas fallas
            rend[rend==-99] = 0  #convertir -99 a 0
            
#             #find areas where consistently failing and discard these from aggregation
#             zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
#             ind.falla = which(zeros.wfd.r>=14)
            
            #Convertir rend en un data.frame
            rend<-data.frame(rend)
            
            #Asignar nombres a el data frame de rendimientos
            colnames(rend)<-paste0("20",22:49)
            
            
            #Creo un dataframe
            eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                                  Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                                  v=variedades[v],sce=gcm[g],sys="RA",rend)',sep='')))
            
#             # Descartar pixeles con más de 13 años con fallas en la linea base
#             if(sum(ind.falla) == 0)
#             {
#                   md<-md
#             } else {
#                   md<-md[-ind.falla,]
#             }
            
            md$pix<- NA
            for (row in 1:nrow(md)){
                  md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
            }
            
            md<- md[,c("long","lat","pix","Area","FPU","v","sys", "sce", paste0("X20",22:49))]
            
            #Computador personal
            write.csv(md,paste(grdpix,"RA",'_',variedades[v],'_',gcm[g],".csv",sep=''),row.names=T)
            cat(paste("Running SOY RA ", variedades[v], " of gcm ", gcm[g], " Done!!\n", sep = "" ))
            
            
      } 
}



g=gc;rm(list = ls())


