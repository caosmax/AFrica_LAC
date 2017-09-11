# Soybean irrigated  and rainfed
# Carlos Eduardo Gonzalez

##-----------------------------------------------------------------IRRIGATED
##----------------------------------------------------------Directorios generales irrigated------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/future/final/SOY_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/future/final/SOY_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyfuture<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/SoybeanIRRI/Soybean_Future/")
copyhistorical<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/SoybeanIRRI/Soybean_Historical/")



##Cargar información de latitud, longitud, area de spam, fpu, etc.-------------------
load(paste(grd3,"Soybeans_riego",'.RDat',sep=''))



##----------------------------------------------------------------IRRIGATED 

###Future-------------

###GCMs y variedades de trigo
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

variedades<- c("DONMARIO", "Hutcheson")


##loop para cargar datos 
for (v in 1:length(variedades)){
      for (g in 1:length(gcm)){
            
            try(load(paste(gdr1,variedades[v],"_",gcm[g],".RDat",sep = ""))) # tomanda de la version anterior
            
            #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
            rend<-matrix(nrow=length(Run), ncol=28)
            for (i in 1:length(Run))
            {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
            
            #descartar pixeles con demasiadas fallas
            rend[rend==-99] = 0  #convertir -99 a 0
            
            #         #find areas where consistently failing and discard these from aggregation
            #         zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
            #         ind.falla = which(zeros.wfd.r>=14)
            
            #Convertir rend en un data.frame
            rend<-data.frame(rend)
            
            #Asignar nombres a el data frame de rendimientos
            colnames(rend)<-paste0("Rend_20",22:49)
            
            #Creo un dataframe
            eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                                  Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"], rend)',sep='')))
            
            
            #Agregar columnas de producción de 2022 a 2046
            md[,paste0("Prod_20",22:49)]<-md[,"Area"]*md[,paste0("Rend_20",22:49)]
            md[,'ones'] = 1
            #Eliminar las columnas de los rendimientos
            md<-md[,!names(md) %in% (paste0("Rend_20",22:49))]
            
            #         # Descartar pixeles con más de 13 años con fallas en la linea base
            #         if(sum(ind.falla) == 0)
            #         {
            #             md<-md
            #         } else {
            #             md<-md[-ind.falla,]
            #         }
                     
            #Agregar producción y area a nivel de fpu
            md_fpu<-aggregate(md[,c("ones","Area", paste0("Prod_20",22:49))],by=list(md[,"FPU"]),
                              FUN= function(x) {sum(x, na.rm=TRUE)} )
            
            #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
            md_fpu[,paste0("Rend_fpu_20",22:49)]<-md_fpu[,paste0("Prod_20",22:49)]/md_fpu[,"Area"]
            
            #Crear un data frame con sólo FPU y rend a nivel de fpu 
            rend_fpu<- md_fpu[,c("Group.1","ones", paste0("Rend_fpu_20",22:49))]
            
            #Asignar nombres apropiados a las columnas
            colnames(rend_fpu)<-c("FPU","num_pixels",paste0("Rend_fpu_20",22:49))
            
            #Count pixels per FPU originalmente
            pixel.FPU = eval(parse(text=paste('table(crop_riego$New_FPU)',sep='')))
            pixel.FPU = pixel.FPU[pixel.FPU>0]
            
            #Create big matrix with rows for original FPU's
            rend_fpu2 = array(NA,dim=c(length(pixel.FPU),29))  #24 años + numero de pixeles en la agregación
            ind.fpu = match(rend_fpu$FPU,names(pixel.FPU))
            rend_fpu2[ind.fpu,] = as.matrix(rend_fpu[,2:30])
            colnames(rend_fpu2) = colnames(rend_fpu)[2:30]
            
            #create data frame
            rend_fpu3 = data.frame(pixels.original = pixel.FPU,rend_fpu2)
            
            # crear  columnas por variaedades y gcms para identificar mas adelante.
            rend_fpu3$sce<- gcm[g]
            rend_fpu3$var<- variedades[v]
            
            # organizar las columnas
            rend_fpu3<- rend_fpu3[,c("pixels.original.Var1", "pixels.original.Freq",
                                     "sce","var", "num_pixels",paste0("Rend_fpu_20",22:49))]
            
            #Computador personal
            write.csv(rend_fpu3,paste(copyfuture,"IRRI",'_',variedades[v],'_',gcm[g],'_FPU.csv',sep=''),row.names=T)
            cat(paste("Running SOY ", variedades[v], " of gcm ", gcm[g], " Done!!\n", sep = "" ))
            
      } 
}  

###Historical----------------------

##Directorios
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/historical/final/SOY_irrigation_")


##WFD y variedades 
variedades<- c("DONMARIO", "Hutcheson")


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #     #find areas where consistently failing and discard these from aggregation
      #     zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
      #     ind.falla = which(zeros.wfd.r>=14)
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("Rend_20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                            Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"], rend)',sep='')))
      
      #Agregar columnas de producción de 2022 a 2046
      md[,paste0("Prod_20",22:49)]<-md[,"Area"]*md[,paste0("Rend_20",22:49)]
      md[,'ones'] = 1
      #Eliminar las columnas de los rendimientos
      md<-md[,!names(md) %in% (paste0("Rend_20",22:49))]
      
      #     # Descartar pixeles con más de 13 años con fallas en la linea base
      #     if(sum(ind.falla) == 0)
      #     {
      #         md<-md
      #     } else {
      #         md<-md[-ind.falla,]
      #     }
      
      #Agregar producción y area a nivel de fpu
      md_fpu<-aggregate(md[,c("ones","Area",paste0("Prod_20",22:49))],by=list(md[,"FPU"]),
                        FUN= function(x) {sum(x, na.rm=TRUE)} )
      
      #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
      md_fpu[,paste0("Rend_fpu_20",22:49)]<-md_fpu[,paste0("Prod_20",22:49)]/md_fpu[,"Area"]
      
      #Crear un data frame con sólo FPU y rend a nivel de fpu 
      rend_fpu<- md_fpu[,c("Group.1","ones",paste0("Rend_fpu_20",22:49))]
      
      #Asignar nombres apropiados a las columnas
      colnames(rend_fpu)<-c("FPU","num_pixels",paste0("Rend_fpu_20",22:49))
      
      #Count pixels per FPU originalmente
      pixel.FPU = eval(parse(text=paste('table(crop_riego$New_FPU)',sep='')))
      pixel.FPU = pixel.FPU[pixel.FPU>0]
      
      #Create big matrix with rows for original FPU's
      rend_fpu2 = array(NA,dim=c(length(pixel.FPU),29))  #24 años + numero de pixeles en la agregación
      ind.fpu = match(rend_fpu$FPU,names(pixel.FPU))
      rend_fpu2[ind.fpu,] = as.matrix(rend_fpu[,2:30])
      colnames(rend_fpu2) = colnames(rend_fpu)[2:30]
      
      #create data frame
      rend_fpu3 = data.frame(pixels.original = pixel.FPU,rend_fpu2)
      
      #crear  columnas por variaedades y gcms para identificar mas adelante.
      rend_fpu3$sce<- "WFD"
      rend_fpu3$var<- variedades[v]
      
      # Organizar las columnas
      rend_fpu3<- rend_fpu3[,c("pixels.original.Var1", "pixels.original.Freq",
                               "sce","var", "num_pixels",paste0("Rend_fpu_20",22:49))]
      
      #Computador personal
      write.csv(rend_fpu3,paste(copyhistorical,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running SOY ", variedades[v], " of gcm WFD", " Done!!\n", sep = "" ))
      
}  


g=gc;rm(list = ls())



##-----------------------------------------------------------------RAINFED

##----------------------------------------------------------Directorios generales rainfed------------------

setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/future/final/SOY_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyfuture<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/SoybeanRA/Soybean_Future/")
copyhistorical<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/SoybeanRA/Soybean_Historical/")


##Future--------------------------------------------

###GCMs y variedades de trigo
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

variedades<- c("DONMARIO", "Hutcheson")

##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Soybeans_secano",'.RDAT',sep=''))

##loop para cargar datos 
for (v in 1:length(variedades)){
      for (g in 1:length(gcm)){
            
            
            try(load(paste(gdr1,variedades[v],"_",gcm[g],".RDat",sep = "")))
            
            #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
            rend<-matrix(nrow=length(Run), ncol=28)
            for (i in 1:length(Run))
            {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
            
            #descartar pixeles con demasiadas fallas
            rend[rend==-99] = 0  #convertir -99 a 0
            
            #         #find areas where consistently failing and discard these from aggregation
            #         zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
            #         ind.falla = which(zeros.wfd.r>=14)
            #         
            #Convertir rend en un data.frame
            rend<-data.frame(rend)
            
            #Asignar nombres a el data frame de rendimientos
            colnames(rend)<-paste0("Rend_20",22:49)
            
            #Creo un dataframe
            eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                                  Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"], rend)',sep='')))
            
            
            #Agregar columnas de producción de 2022 a 2046
            md[,paste0("Prod_20",22:49)]<-md[,"Area"]*md[,paste0("Rend_20",22:49)]
            md[,'ones'] = 1
            #Eliminar las columnas de los rendimientos
            md<-md[,!names(md) %in% (paste0("Rend_20",22:49))]
            
            #         # Descartar pixeles con más de 13 años con fallas en la linea base
            #         if(sum(ind.falla) == 0)
            #         {
            #             md<-md
            #         } else {
            #             md<-md[-ind.falla,]
            #         }
            #         
            #Agregar producción y area a nivel de fpu
            md_fpu<-aggregate(md[,c("ones","Area", paste0("Prod_20",22:49))],by=list(md[,"FPU"]),
                              FUN= function(x) {sum(x, na.rm=TRUE)} )
            
            #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
            md_fpu[,paste0("Rend_fpu_20",22:49)]<-md_fpu[,paste0("Prod_20",22:49)]/md_fpu[,"Area"]
            
            #Crear un data frame con sólo FPU y rend a nivel de fpu 
            rend_fpu<- md_fpu[,c("Group.1","ones", paste0("Rend_fpu_20",22:49))]
            
            #Asignar nombres apropiados a las columnas
            colnames(rend_fpu)<-c("FPU","num_pixels",paste0("Rend_fpu_20",22:49))
            
            #Count pixels per FPU originalmente
            pixel.FPU = eval(parse(text=paste('table(crop_secano$New_FPU)',sep='')))
            pixel.FPU = pixel.FPU[pixel.FPU>0]
            
            #Create big matrix with rows for original FPU's
            rend_fpu2 = array(NA,dim=c(length(pixel.FPU),29))  
            ind.fpu = match(rend_fpu$FPU,names(pixel.FPU))
            rend_fpu2[ind.fpu,] = as.matrix(rend_fpu[,2:30])
            colnames(rend_fpu2) = colnames(rend_fpu)[2:30]
            
            #create data frame
            rend_fpu3 = data.frame(pixels.original = pixel.FPU,rend_fpu2)
            
            # crear  columnas por variaedades y gcms para identificar mas adelante.
            rend_fpu3$sce<- gcm[g]
            rend_fpu3$var<- variedades[v]
            
            # organizar las columnas
            rend_fpu3<- rend_fpu3[,c("pixels.original.Var1", "pixels.original.Freq",
                                     "sce","var", "num_pixels",paste0("Rend_fpu_20",22:49))]
            
            #Computador personal
            write.csv(rend_fpu3,paste(copyfuture,"RA",'_',variedades[v],'_',gcm[g],'_FPU.csv',sep=''),row.names=T)
            cat(paste("Running SOY ", variedades[v], " of gcm ", gcm[g], " Done!!\n", sep = "" ))
            
      } 
}  

##Historical----------------
##Directorios
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/historical/final/SOY_rainfed_")


##WFD y variedades de trigo
variedades<- c("DONMARIO", "Hutcheson")


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #     #find areas where consistently failing and discard these from aggregation
      #     zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
      #     ind.falla = which(zeros.wfd.r>=14)
      #     
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("Rend_20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"], rend)',sep='')))
      
      #Agregar columnas de producción de 2022 a 2046
      md[,paste0("Prod_20",22:49)]<-md[,"Area"]*md[,paste0("Rend_20",22:49)]
      md[,'ones'] = 1
      #Eliminar las columnas de los rendimientos
      md<-md[,!names(md) %in% (paste0("Rend_20",22:49))]
      
      #     # Descartar pixeles con más de 13 años con fallas en la linea base
      #     if(sum(ind.falla) == 0)
      #     {
      #         md<-md
      #     } else {
      #         md<-md[-ind.falla,]
      #     }
      
      #Agregar producción y area a nivel de fpu
      md_fpu<-aggregate(md[,c("ones","Area",paste0("Prod_20",22:49))],by=list(md[,"FPU"]),
                        FUN= function(x) {sum(x, na.rm=TRUE)} )
      
      #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
      md_fpu[,paste0("Rend_fpu_20",22:49)]<-md_fpu[,paste0("Prod_20",22:49)]/md_fpu[,"Area"]
      
      #Crear un data frame con sólo FPU y rend a nivel de fpu 
      rend_fpu<- md_fpu[,c("Group.1","ones",paste0("Rend_fpu_20",22:49))]
      
      #Asignar nombres apropiados a las columnas
      colnames(rend_fpu)<-c("FPU","num_pixels",paste0("Rend_fpu_20",22:49))
      
      #Count pixels per FPU originalmente
      pixel.FPU = eval(parse(text=paste('table(crop_secano$New_FPU)',sep='')))
      pixel.FPU = pixel.FPU[pixel.FPU>0]
      
      #Create big matrix with rows for original FPU's
      rend_fpu2 = array(NA,dim=c(length(pixel.FPU),29))  #24 años + numero de pixeles en la agregación
      ind.fpu = match(rend_fpu$FPU,names(pixel.FPU))
      rend_fpu2[ind.fpu,] = as.matrix(rend_fpu[,2:30])
      colnames(rend_fpu2) = colnames(rend_fpu)[2:30]
      
      #create data frame
      rend_fpu3 = data.frame(pixels.original = pixel.FPU,rend_fpu2)
      
      #crear  columnas por variaedades y gcms para identificar mas adelante.
      rend_fpu3$sce<- "WFD"
      rend_fpu3$var<- variedades[v]
      
      # Organizar las columnas
      rend_fpu3<- rend_fpu3[,c("pixels.original.Var1", "pixels.original.Freq",
                               "sce","var", "num_pixels",paste0("Rend_fpu_20",22:49))]
      
      #Computador personal
      write.csv(rend_fpu3,paste(copyhistorical,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running SOY ", variedades[v], " of gcm WFD", " Done!!\n", sep = "" ))
      
      
}  


g=gc;rm(list = ls())


