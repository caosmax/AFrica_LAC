#### Escenarios historicos WFD

copyRice<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Rice/")
copyWheat<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Wheat/")
copyBean<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Bean/")
copyMaize<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Maize/")
copySoybean<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Soybean/")

###########################################ProofByPixel####################################################

#RICE --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Rice/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Rice/historical/final/RICE_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Rice/historical/final/RICE_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyRice<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Rice/")

###GCMs y variedades de trigo
variedades<- c("IR8", "IR64","IR72")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Rice_riego",'.RDat',sep=''))



#loop para cargar datos 
for (v in 1:length(variedades)){
      
      load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = ""))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
    
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                              Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                              v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys", "sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyRice,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running RICE IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  

#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Rice_secano",'.RDat',sep=''))


###GCMs y variedades de trigo
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

variedades<- c("IR8", "IR64","IR72")

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD", sys="RA", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys", "sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyRice,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running RICE RA ", variedades[v], " of gcm WFD", " Done!!\n", sep = "" ))
      
}  


g=gc;rm(list = ls())

#BEAN --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Bean/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Bean/historical/final/BEAN_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Bean/historical/final/BEAN_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyBean<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Bean/")

###GCMs y variedades de FRIJOL
variedades<- c("A193", "BAT881","BRSRadiante","Carioca", "ICTAOstua", "Manitou", "Perola")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Bean_riego",'.RDat',sep=''))


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                            Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyBean,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running BEAN IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  



#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Bean_secano",'.RDat',sep=''))

### variedades de trigo

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="RA", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyBean,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running BEAN RA ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  


g=gc;rm(list = ls())

#MAIZE --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Maize/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Maize/historical/final/MAIZE_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Maize/historical/final/MAIZE_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyMaize<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Maize/")

###GCMs y variedades de FRIJOL
variedades<-c("H6","FM6","MCCURDY6714")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Maize_riego",'.RDat',sep=''))


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                            Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyMaize,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running MAIZE IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  

#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Maize_secano",'.RDat',sep=''))

### variedades de trigo

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="RA", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyMaize,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running MAIZE RA ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  


g=gc;rm(list = ls())

#WHEAT --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Wheat/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Wheat/historical/final/WHEAT_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Wheat/historical/final/WHEAT_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyWheat<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Wheat/")

###GCMs y variedades de FRIJOL
variedades<-c("BrigadierBA","DonErnestoBA","Gerek79BA","HalconsnaBA" ,"Seri82BA","TajanBA")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Wheat_riego",'.RDat',sep=''))


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                            Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyWheat,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running WHEAT IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  

#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Wheat_secano",'.RDat',sep=''))

### variedades de trigo

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="RA", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyWheat,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running WHEAT RA ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  


g=gc;rm(list = ls())

#SOYBEAN --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/historical/final/SOY_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/historical/final/SOY_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copySoybean<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Soybean/")

###GCMs y variedades de FRIJOL
variedades<-c("DONMARIO","Hutcheson")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Soybeans_riego",'.RDat',sep=''))


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                            Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copySoybean,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running SOy IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  

#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Soybeans_secano",'.RDat',sep=''))

### variedades de trigo

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="RA", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copySoybean,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running SOY RA ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  

######################################################### Informes Outliers and Reports ####################
# sys<- c( "IRRI", "RA")
# crops<- c("Rice","Bean","Wheat","Maize", "Soybean") 
# reg<- read.csv("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ListFPUs.csv",header = T,stringsAsFactors = F )
# pots<- reg[,1]
# copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/")
# resum<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/AnalysisDataHistorical/")
# 
# library(dplyr)
# library(tidyr)
# 
# by_pots<-list()
# #c=p=1
# for(c in 1:length(crops)){ 
#       #cargar files
#       c_files <- list.files(path= paste(copy,crops[c],"/",sep=""), pattern ="WFD",full.names = T)
#       c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
#       c_files <- do.call(rbind,c_files)
#       c_files$X<- NULL
#       
#             for(p in 1:length(pots)){ 
#             by_pots[[p]] <- c_files[which(c_files$FPU==pots[p]),]
#             
#             if(nrow(by_pots[[p]]) >= 1){
#                   by_pots[[p]]<- by_pots[[p]] %>% 
#                         gather(year,val, 9:36) 
#                   by_pots[[p]]$year<- sub(pattern = "X", replacement = "", x = by_pots[[p]]$year, ignore.case = T)
#                   
#                   by_pots[[p]]$val[which(is.na(by_pots[[p]]$val))] <- 0
#                  
#                   #segunda parte
#                   test <- sum(by_pots[[p]]$val)
#                    
#                    if(test>0){  
#                         SinCeros<-  by_pots[[p]][which( by_pots[[p]]$val!=0),]
#                         q1<- quantile(SinCeros$val,probs = 0.05,na.rm = T)
#                         q2<- quantile(SinCeros$val,probs = 0.99,na.rm = T)
#                         
#                         
#                         inf<- by_pots[[p]] 
#                         
#                         for(i in 1:nrow(inf)){
#                               if(((inf$val[i]<=q1)+(inf$val[i]!=0))>=2) {inf$status[i]<- "lowest Yield (lower than 5% probs)"}else{} 
#                               if(inf$val[i]==0){inf$status[i]<- "Fallo total (yield==0)"}else{}
#                               if(((inf$val[i]!=0)+(inf$val[i]>q1))>=2){inf$status[i]<- "Normal"}else{} 
#                               if(((inf$val[i]!=0)+(inf$val[i]>q2))>=2){inf$status[i]<- "Highest (Higher than 99% probs)"}else{} 
#                               
#                          }
#                         
#                         infRepor<- inf[which(inf$status!="Normal"),]
#                         infRepor$crop<- crops[c]
#                         write.csv(infRepor,paste(resum,"DataExtremes_",pots[p],"_",crops[c],".csv", sep = ""))
#                         
#                         
#                         #tabla resumen datos
#                         tab<-as.data.frame(table(inf$sce, inf$status, inf$sys, inf$v))
#                         colnames(tab)[1]<- 'Scenario'; colnames(tab)[2]<- 'Status'; colnames(tab)[3]<- 'System'
#                         colnames(tab)[4]<- 'Variety';  colnames(tab)[5]<- 'Freq'
#                         
#                         tab<- tab %>% spread(Scenario,Freq)
#                         tab$System<- as.character(tab$System)
#                         tab$Status<- as.character(tab$Status)
#                         tab$Variety<- as.character(tab$Variety)
#                         
# #                         tab<- tab %>% group_by(System,Variety) %>%
# #                               summarise(Total= sum(WFD))
# #                         
# #                         tab<- as.data.frame(dplyr::summarise(group_by(tab, System,Variety), sum(WFD)))
#                       
#                         tab<- tab %>% spread(Status,WFD)
#                         tab$N<- rowSums(tab[,3:ncol(tab)],na.rm = T)
#                         
#                         if(ncol(tab)==6){
#                              tab$`Fallo total (yield==0)`<-0
#                              tab<- tab[,c("System","Variety","N","Fallo total (yield==0)","Highest (Higher than 99% probs)","lowest Yield (lower than 5% probs)", "Normal")]
#                              }else{}
#                    
#                           
#                         tab<- tab[,c("System","Variety","N","Fallo total (yield==0)","Highest (Higher than 99% probs)","lowest Yield (lower than 5% probs)", "Normal")]
#                         tab<- tab %>% gather("Status", "Freq", 4:ncol(tab))                        
#                         tab$ratio<- (tab$Freq/tab$N)*100
#                         tab$crop<-crops[c]
#                         tab$fpu<- pots[p]
#                         
#                         
#                         #Reporte esta de cada FPU
#                         write.csv(tab,paste(resum,"Report_Cases_Yields_",crops[c],"_",pots[p],"_WFD_","_FPU.csv", sep = ""))
#                         
#                         
#                    }else{cat(paste(pots[p], " tiene ceros en los datos\n descarta", sep = ""))}
#                   
#             }else{cat(paste(" Cultivo ", crops[c]," para el FPU= " , pots[p], " does not have varieties\n", sep = ""))}
#             print(p)
#             }
#       cat(paste(crops[c], " ", pots[p], " It has been complete\n", sep = ""))
# }
# 
# ##reporte final
# c_files <- list.files(path=resum, pattern ="DataExtremes_",full.names = T)
# c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
# c_files <- do.call(rbind,c_files)
# c_files$X<- NULL
# write.csv(c_files,paste(resum, "SummaryLowest&HightestWFD.csv", sep = ""))           
# 
# c_files <- list.files(path=resum, pattern ="Report_Cases_Yields_",full.names = T)
# c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
# c_files <- do.call(rbind,c_files)
# c_files$X<- NULL
# write.csv(c_files,paste(resum, "SummaryCasesWFD.csv", sep = ""))          
# 
# 
# 
# ########################################## outliers #################################################
# 
# library(extremevalues)
# library(outliers)
# 
# 
# 
# for(c in 1:length(crops)){ 
#       #cargar files
#       c_files <- list.files(path= paste(copy,crops[c],"/",sep=""), pattern ="WFD",full.names = T)
#       c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
#       c_files <- do.call(rbind,c_files)
#       c_files$X<- NULL
#       
#       
#       require(plyr)
#       require(tidyr)
#       c_files<- c_files %>% 
#             gather(year,val, 9:36) 
#       c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
#       
#       c_files<- c_files[which(c_files$val!=0),]
#       rownames(c_files)<- 1:nrow(c_files)
# 
#       # define a function to remove outliers
#       FindOutliers <- function(data) {
#             lowerq = quantile(data)[2]
#             upperq = quantile(data)[4]
#             iqr = upperq - lowerq 
#             #Or use IQR(data)
#             # we identify extreme outliers
#             extreme.threshold.upper = (iqr * 3) + upperq
#             extreme.threshold.lower = lowerq - (iqr * 3)
#             result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
#       }
#       
#       # use the function to identify outliers
#       temp <- FindOutliers(c_files$val)
#       cfOut<- c_files[temp,]
#       write.csv(cfOut,paste(resum, crops[c],"Outliers.csv", sep = ""))
#       }
#      
# 
# g=gc;rm(list = ls())
# 
#                   

 