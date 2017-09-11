# Analisis graficos a nivel de pixel 


#WFD--------------------------
options(digits=3) 
options(scipen=999)
copyRice<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Rice/")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/")
resum<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/AnalysisDataHistorical/")


#Lista de tipos de sistemas
sys<- c( "IRRI", "RA")
crops<- c("Rice","Bean","Wheat","Maize", "Soybean") 
reg<- read.csv("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ListFPUs.csv",header = T,stringsAsFactors = F )
pots<- reg[,1]


by_pots <- list()

for(c in 1:length(crops)){ #inicio loop
      #cargar files
      c_files <- list.files(path= paste(copy,crops[c],"/",sep=""), pattern = c("WFD") ,full.names = T)
      c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
      c_files <- do.call(rbind,c_files)
      c_files$X<- NULL
      
      
      for(p in 1:length(pots)){ # por fpu
            
            #### Filter
            by_pots[[p]] <- c_files[which(c_files$FPU==pots[[p]]),]
            
            if(nrow(by_pots[[p]]) >= 1){ # Test datos1
                  require(plyr)
                  require(tidyr)
                  by_pots[[p]]<- by_pots[[p]] %>% 
                        gather(year,val, 9:36) 
                  by_pots[[p]]$year<- sub(pattern = "X", replacement = "", x = by_pots[[p]]$year, ignore.case = T)
                  
                  
                  #Hacer graficas
                  png(filename = paste(resum,crops[c],"_",pots[p],"_WFD_","trend",".png",sep=""), 
                      width = 20, height = 12, units = 'in', res = 100)
                  require(ggplot2)
                  n<-ggplot(data=by_pots[[p]], aes(x=year,y=val,colour=pix))+ 
                        geom_line(aes(group=pix))+geom_point()+ facet_grid(v~sys)+ylab("Yield ") +  xlab("Years") +
                        theme(axis.text.x=element_text(size=7, angle=90))+
                        labs(title=paste("Yields by Pixe for each FPU= ",pots[p], " ,Irrigated and Rainfed\n Trend 2020-2049",sep = ""))+
                        theme(strip.text.y = element_text(angle = 0,size = 12))+ theme(legend.position="none")
                  
                  plot(n)
                  dev.off()
                  
            }else{cat(paste(" Cultivo ", crops[c]," para el FPU= " , pots[[p]], " does not have varieties\n", sep = ""))}
      }
      
}


#GCMs-----------------------------------------------------------------------
#cargar files

grdpix<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/")
c=2
p=1
s=1
for(c in 1:length(crops)){
      for(s in 1:length(sys)){
          c_files <- list.files(path= paste(grdpix,crops[c],"/", sep = ""), pattern =sys[s],full.names = T)
          c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
          c_files <- do.call(rbind,c_files)
          c_files$X<- NULL
                                  
                for(p in 1:length(pots)){
                      by_pots <- list()
                      
                      #### filter
                      by_pots[[p]] <- c_files[which(c_files$FPU==pots[[p]]),]
                      
                      if(nrow(by_pots[[p]]) >= 1){
                            ##### reshape
                            require(plyr)
                            require(tidyr)
                            by_pots[[p]]<- by_pots[[p]] %>% 
                                  gather(year,val, 9:36) 
                            #### tratamiento para años
                            by_pots[[p]]$year<- sub(pattern = "X", replacement = "", x = by_pots[[p]]$year, ignore.case = T)
                            
                                              
                            #hacer graficas
                            png(filename = paste(grdpix,crops[c],"/","pix/",crops[c],"_",sys[s],"_",pots[p],"_","trend",".png",sep=""), 
                                width = 20, height = 12, units = 'in', res = 100)
                            require(ggplot2)
                            n<-ggplot(data=by_pots[[p]], aes(x=year,y=val,colour=pix))+ 
                                  geom_line(aes(group=pix))+
                                  geom_point()+ facet_grid(v~sce)+
                                  ylab("Yield ") + ggtitle("Scatter plot")+
                                  xlab("Years") +
                                  theme(axis.text.x=element_text(size=7, angle=90))+
                                  guides(color=guide_legend("Varieties"))+theme(legend.position="none")
                            plot(n)
                            dev.off()
                      } else {
                            cat(paste("Runing in : ", crops[c]," ", pots[[p]], "does not have varieties\n", sep = ""))
          }
                      
                }  
}
      
}

      




