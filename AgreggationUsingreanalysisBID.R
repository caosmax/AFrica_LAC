# Crops irrigated  and rainfed
# Carlos Eduardo Gonzalez

#Directorios
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/DataAll/")
grd<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")
copyall<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/DataAll/")
copymeans<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/graphsMeans/")
time<-c ("Future", "Historical")                 
# cultivos, tratamiento
crops<- c( "Rice") #, "Wheat", "Bean", "Soybean","Maize" )
# treat<- c("Irrigated", "Rainfed")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")
sys_q<- c("IRR", "RA")


#Rice --------------
for(s in 1:length(sys_q)){
    for(t in 1:length(time)){
        
            cat(paste("Processing:", " Rice "," in ", sys_q, "_", time[t], "\n", sep = ""))
        
            c_files <- list.files(path= paste(grd,"Rice","_",sys_q[s],"/","Rice","_",time[t],sep = ""), full.names = T)
            c_files <- lapply(c_files, read.csv)
            c_files <- do.call(rbind,c_files)
            c_files$sys<- sys_q[s]
            c_files$X<-NULL
            c_files<- c_files[,c("pixels.original.Var1","pixels.original.Freq","sce","sys",
                                  "var", "num_pixels",paste0("Rend_fpu_20",22:49))]
            write.csv(c_files,paste(copyall,"RICE",'_',sys_q[s],'_',time[t],".csv",sep=''),row.names=T)
             
    }
  }
               
##apliar arroz     
c_files <- list.files()
c_files <- lapply(c_files, read.csv)
c_files <- do.call(rbind,c_files)
rice<-c_files

#calculo de media
require(plyr)
require(tidyr)
rice<- rice %>%
    gather(year,val,8:35,na.rm = T)  

rice$X<- NULL
rice$num_pixels<- NULL
rice$pixels.original.Freq<- NULL
names(rice)[1]<- "FPU"


rice$year<- sub(pattern ="Rend_fpu_", replacement = "",x = rice$year,  ignore.case = T)

rice$sce<- as.character(rice$sce)
rice$sys<- as.character(rice$sys)
rice$var<-  as.character(rice$var)
# rice<- rice[which(rice$val>2),]

require(plyr)
require(tidyr)
rice<- rice %>% spread(var, val)


# calculo de la media
rice$mean<-apply(rice[,5:7],1,function(x){mean(x,na.rm = T)})

#filtro solo columnas que necesitamos
rice<- rice[,c( 1:4,8)]

#lista por FPUs
by_pots <- list()
rice$FPU<-as.character(rice$FPU)
pots<- unique(rice$FPU)

for(p in 1:length(pots)){
    
    cat(paste("Processing:", " Rice "," in ", pots, "\n", sep = ""))
    by_pots[[p]] <- rice[which(rice$FPU==pots[[p]]),]
           tiff(filename = paste(copymeans,"Rice","_",pots[p],"_","trend",".tiff",sep=""), 
                width = 20, height = 12, units = 'in', res = 100)
           
       require(ggplot2)
              n<- ggplot(data=by_pots[[p]], aes(x=year,y=mean,colour=sce))+ 
                     geom_line(aes(group=sys))+
                     geom_point()+ facet_grid(sce~sys)+
                     ylab("Mean-Yield ") + ggtitle("Scatter plot")+
                     xlab("Years") +
                     theme(axis.text.x=element_text(size=7, angle=90))+
                     guides(color=guide_legend("Scenarios"))
               plot(n)
               dev.off()
 }

#Wheat --------------
for(s in 1:length(sys_q)){
    for(t in 1:length(time)){
        
        cat(paste("Processing:", " Wheat "," in ", sys_q, "_", time[t], "\n", sep = ""))
        
        c_files <- list.files(path= paste(grd,"Wheat","_",sys_q[s],"/","Wheat","_",time[t],sep = ""), full.names = T)
        c_files <- lapply(c_files, read.csv)
        c_files <- do.call(rbind,c_files)
        c_files$sys<- sys_q[s]
        c_files$X<-NULL
        c_files<- c_files[,c("pixels.original.Var1","pixels.original.Freq","sce","sys",
                             "var", "num_pixels",paste0("Rend_fpu_20",22:49))]
        write.csv(c_files,paste(copyall,"WHEAT",'_',sys_q[s],'_',time[t],".csv",sep=''),row.names=T)
        
    }
}

##apliar      
c_files <- list.files(pattern = "WHEAT")
c_files <- lapply(c_files, read.csv)
c_files <- do.call(rbind,c_files)
wheat<-c_files

#calculo de media
require(plyr)
require(tidyr)
wheat<- wheat %>%
    gather(year,val,8:35,na.rm = T)  

wheat$X<- NULL
wheat$num_pixels<- NULL
wheat$pixels.original.Freq<- NULL
names(wheat)[1]<- "FPU"


wheat$year<- sub(pattern ="Rend_fpu_", replacement = "",x = wheat$year,  ignore.case = T)

wheat$sce<- as.character(wheat$sce)
wheat$sys<- as.character(wheat$sys)
wheat$var<-  as.character(wheat$var)

require(plyr)
require(tidyr)
wheat<- wheat %>% spread(var, val)


# calculo de la media
wheat$mean<-apply(wheat[,5:ncol(wheat)],1,function(x){mean(x,na.rm = T)})

#filtro solo columnas que necesitamos
wheat<- wheat[,c( 1:4,ncol(wheat))]

#lista por FPUs
by_pots <- list()
wheat$FPU<-as.character(wheat$FPU)
pots<- unique(wheat$FPU)

for(p in 1:length(pots)){
    
    cat(paste("Processing:", " wheat "," in ", pots, "\n", sep = ""))
    by_pots[[p]] <- wheat[which(wheat$FPU==pots[[p]]),]
    tiff(filename = paste(copymeans,"wheat","_",pots[p],"_","trend",".tiff",sep=""), 
         width = 20, height = 12, units = 'in', res = 100)
    
    require(ggplot2)
    n<- ggplot(data=by_pots[[p]], aes(x=year,y=mean,colour=sce))+ 
        geom_line(aes(group=sys))+
        geom_point()+ facet_grid(sce~sys)+
        ylab("Mean-Yield ") + ggtitle("Scatter plot")+
        xlab("Years") +
        theme(axis.text.x=element_text(size=7, angle=90))+
        guides(color=guide_legend("Scenarios"))
    plot(n)
    dev.off()
}


# write.csv(x = wheat, file = paste("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/", "Wheat.csv", sep = ""))

#Bean --------------
for(s in 1:length(sys_q)){
    for(t in 1:length(time)){
        
        cat(paste("Processing:", " Bean "," in ", sys_q, "_", time[t], "\n", sep = ""))
        
        c_files <- list.files(path= paste(grd,"Bean","_",sys_q[s],"/","Bean","_",time[t],sep = ""), full.names = T)
        c_files <- lapply(c_files, read.csv)
        c_files <- do.call(rbind,c_files)
        c_files$sys<- sys_q[s]
        c_files$X<-NULL
        c_files<- c_files[,c("pixels.original.Var1","pixels.original.Freq","sce","sys",
                             "var", "num_pixels",paste0("Rend_fpu_20",22:49))]
        write.csv(c_files,paste(copyall,"BEAN",'_',sys_q[s],'_',time[t],".csv",sep=''),row.names=T)
        
    }
}

##apliar      
c_files <- list.files(pattern = "BEAN")
c_files <- lapply(c_files, read.csv)
c_files <- do.call(rbind,c_files)
wheat<-c_files

#calculo de media
require(plyr)
require(tidyr)
wheat<- wheat %>%
    gather(year,val,8:35,na.rm = T)  

wheat$X<- NULL
wheat$num_pixels<- NULL
wheat$pixels.original.Freq<- NULL
names(wheat)[1]<- "FPU"


wheat$year<- sub(pattern ="Rend_fpu_", replacement = "",x = wheat$year,  ignore.case = T)

wheat$sce<- as.character(wheat$sce)
wheat$sys<- as.character(wheat$sys)
wheat$var<-  as.character(wheat$var)

require(plyr)
require(tidyr)
wheat<- wheat %>% spread(var, val)


# calculo de la media
wheat$mean<-apply(wheat[,5:ncol(wheat)],1,function(x){mean(x,na.rm = T)})

#filtro solo columnas que necesitamos
wheat<- wheat[,c( 1:4,ncol(wheat))]

#lista por FPUs
by_pots <- list()
wheat$FPU<-as.character(wheat$FPU)
pots<- unique(wheat$FPU)

for(p in 1:length(pots)){
    
    cat(paste("Processing:", " Bean "," in ", pots, "\n", sep = ""))
    by_pots[[p]] <- wheat[which(wheat$FPU==pots[[p]]),]
    tiff(filename = paste(copymeans,"Bean","_",pots[p],"_","trend",".tiff",sep=""), 
         width = 20, height = 12, units = 'in', res = 100)
    
    require(ggplot2)
    n<- ggplot(data=by_pots[[p]], aes(x=year,y=mean,colour=sce))+ 
        geom_line(aes(group=sys))+
        geom_point()+ facet_grid(sce~sys)+
        ylab("Mean-Yield ") + ggtitle("Scatter plot")+
        xlab("Years") +
        theme(axis.text.x=element_text(size=7, angle=90))+
        guides(color=guide_legend("Scenarios"))
    plot(n)
    dev.off()
}

#Maize----
for(s in 1:length(sys_q)){
    for(t in 1:length(time)){
        
        cat(paste("Processing:", " Maize "," in ", sys_q, "_", time[t], "\n", sep = ""))
        
        c_files <- list.files(path= paste(grd,"Maize","_",sys_q[s],"/","Maize","_",time[t],sep = ""), full.names = T)
        c_files <- lapply(c_files, read.csv)
        c_files <- do.call(rbind,c_files)
        c_files$sys<- sys_q[s]
        c_files$X<-NULL
        c_files<- c_files[,c("pixels.original.Var1","pixels.original.Freq","sce","sys",
                             "var", "num_pixels",paste0("Rend_fpu_20",22:49))]
        write.csv(c_files,paste(copyall,"MAIZE",'_',sys_q[s],'_',time[t],".csv",sep=''),row.names=T)
        
    }
}

##apliar      
c_files <- list.files(pattern = "MAIZE")
c_files <- lapply(c_files, read.csv)
c_files <- do.call(rbind,c_files)
wheat<-c_files

#calculo de media
require(plyr)
require(tidyr)
wheat<- wheat %>%
    gather(year,val,8:35,na.rm = T)  

wheat$X<- NULL
wheat$num_pixels<- NULL
wheat$pixels.original.Freq<- NULL
names(wheat)[1]<- "FPU"


wheat$year<- sub(pattern ="Rend_fpu_", replacement = "",x = wheat$year,  ignore.case = T)

wheat$sce<- as.character(wheat$sce)
wheat$sys<- as.character(wheat$sys)
wheat$var<-  as.character(wheat$var)

require(plyr)
require(tidyr)
wheat<- wheat %>% spread(var, val)


# calculo de la media
wheat$mean<-apply(wheat[,5:ncol(wheat)],1,function(x){mean(x,na.rm = T)})

#filtro solo columnas que necesitamos
wheat<- wheat[,c( 1:4,ncol(wheat))]

#lista por FPUs
by_pots <- list()
wheat$FPU<-as.character(wheat$FPU)
pots<- unique(wheat$FPU)

for(p in 1:length(pots)){
    
    cat(paste("Processing:", " MAIZE "," in ", pots, "\n", sep = ""))
    by_pots[[p]] <- wheat[which(wheat$FPU==pots[[p]]),]
    tiff(filename = paste(copymeans,"Maize","_",pots[p],"_","trend",".tiff",sep=""), 
         width = 20, height = 12, units = 'in', res = 100)
    
    require(ggplot2)
    n<- ggplot(data=by_pots[[p]], aes(x=year,y=mean,colour=sce))+ 
        geom_line(aes(group=sys))+
        geom_point()+ facet_grid(sce~sys)+
        ylab("Mean-Yield ") + ggtitle("Scatter plot")+
        xlab("Years") +
        theme(axis.text.x=element_text(size=7, angle=90))+
        guides(color=guide_legend("Scenarios"))
    plot(n)
    dev.off()
}

