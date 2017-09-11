#MFA RTB
### PCA for RTB crops 
### Carlos Eduardo Gonzalez R. 
### RTB Analysis
g=gc;rm(list = ls())

# librerias------------
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse)) 
suppressMessages(library(modelr)) 
suppressMessages(library(purrr)) 
suppressMessages(library(broom)) 
suppressMessages(library(tidyr)) 
suppressMessages(library(corrplot)) 
suppressMessages(library(FactoMineR)) 
suppressMessages(library(factoextra)) 
suppressMessages(library(cluster)) 
suppressMessages(library(RCurl)) 
suppressMessages(library(ggthemes)) 
suppressMessages(library(tidyquant))
suppressMessages(library(devtools))
suppressMessages(library(mvoutlier))
suppressMessages(library(R.utils))



# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/factoextra")
# 

abbreviate("percentage") 
options(warn = -1); options(scipen = 999) 
options(digits=3) 



############################################################# BIG Regions ####################################################################

rdsFiles<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")
# Big Regions[
r<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA")
s<- c("SSP2-HGEM-HiYld2","SSP2-HGEM-RegYld2","SSP2-HGEM-HiNARS2", "SSP2-HGEM-MMEFF2","SSP2-HGEM2")

# Parametro 2 All Countries.
r2<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA", "Africa","Americas","DVD", "DVG","WLD")
r3<- c("Africa","Americas", "Asia","Europe", "Oceania")
r4<- c("Australia and New Zealand","Caribbean","Central America", "Central Asia","Eastern Africa","Eastern Asia","Eastern Europe","Melanesia",
       "Middle Africa","Northern Africa","Northern America","Northern Europe","South America","South-Eastern Asia","Southern Africa","Southern Asia",
       "Southern Europe","Western Africa","Western Asia", "Western Europe", "Western and Central Asia")
r5<- c("MENg","EAPg")
rall<- c(r2,r3,r4, r5)
jrtb<- c("jbana","jcass", "jpota", "jswpt","jyams","jorat")


t<- c(2010, 2030,2050)
# Vector con los cultivos para RTB incluyendo Bananas
rtb<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Other Roots","R&T-Cassava","F&V-Banana") 



cfiles<-list.files(path = rdsFiles, pattern = "Blue.rds|dataagg.rds|datatotal.rds|precios.rds|TradeFood.rds",full.names = T)
cfiles<- lapply(cfiles, readRDS)
cdata<-cfiles
# primer grupo de variables------------ 

for(i in 1:length(cdata)){
      cdata[[i]]$Scenarios<-  gsub("'",'',cdata[[i]]$Scenarios)
      cdata[[i]]$Commodity<-  gsub("'", '',cdata[[i]]$Commodity)
      cdata[[i]]$Regions<-  gsub("'", '',cdata[[i]]$Regions)
      cdata[[i]]$Year<-  gsub("'",'',cdata[[i]]$Year)
      
      cdata[[i]]$Scenarios<- as.character( cdata[[i]]$Scenarios)
      cdata[[i]]$Commodity<- as.character( cdata[[i]]$Commodity)
      cdata[[i]]$Regions<- as.character( cdata[[i]]$Regions) 
      
      cdata[[i]]<- filter(cdata[[i]], Scenarios %in% s)
      cdata[[i]]<- filter(cdata[[i]], !Regions %in% rall) 
   
      
      cdata[[i]]<- filter(cdata[[i]], Commodity %in% rtb)
      
      cdata[[i]]<-  cdata[[i]]%>% spread(Year, Val) 
      cdata[[i]]$change<- ((cdata[[i]]$`2050`-cdata[[i]]$`2010`)/cdata[[i]]$`2010`)*100
      cdata[[i]]<-  cdata[[i]][,c("Scenarios", "Commodity", "Regions",  "parameter", "change" )]
      
      cdata[[i]]<-  cdata[[i]]%>% spread(Scenarios, change) 
      colnames(cdata[[i]])<- c("Commodity", "Regions","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
      
      print(i)
}

crbind<- do.call(rbind, cdata)


# Por sistema -------- 
jfiles<-list.files(path = rdsFiles, pattern = "datasys.rds|green.rds",full.names = T) #|shock.rds|ipr.rds
jfiles<- lapply(jfiles, readRDS)
jdata<-jfiles

countries <- read.csv(file = paste(rdsFiles,"IPRsLabelsRegions.csv", sep=""), header = T)
colnames(countries)<- c("Regions", "IMPACT.Name")

#i=3
for(i in 1:length(jdata)){
      
      jdata[[i]]$Scenarios<-  gsub("'",'',jdata[[i]]$Scenarios)
      jdata[[i]]$Commodity<-  gsub("'", '',jdata[[i]]$Commodity)
      jdata[[i]]$Regions<-  gsub("'", '',jdata[[i]]$Regions)
      jdata[[i]]$Year<-  gsub("'",'',jdata[[i]]$Year)
      jdata[[i]]$Sys<-  gsub("'",'',jdata[[i]]$Sys)
      jdata[[i]]$parameter<-  gsub("'",'',jdata[[i]]$parameter)
      jdata[[i]]<- filter(jdata[[i]], Scenarios %in% s)
      
      jdata[[i]]<- filter(jdata[[i]], !Regions %in% rall) 
      jdata[[i]]<- filter(jdata[[i]], Commodity %in% rtb)
      jdata[[i]]<- jdata[[i]][,c("Scenarios", "Commodity","Regions","parameter", "Sys", "Year","Val") ]
      jdata[[i]]<-  jdata[[i]]%>% spread(Year, Val) 
      jdata[[i]]$change<- ((jdata[[i]]$`2050`-jdata[[i]]$`2010`)/jdata[[i]]$`2010`)*100
      jdata[[i]]<-  jdata[[i]][,c("Scenarios", "Commodity", "Regions",  "parameter","Sys", "change" )]

      jdata[[i]]<-  jdata[[i]]%>% spread(Scenarios, change) 
      colnames(jdata[[i]])<- c("Commodity", "Regions","parameter","Sys", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
   
      print(i)
}


jrbind<- do.call(rbind, jdata)


# Economic Variables1--------------
efiles<-list.files(path = rdsFiles, pattern = "EcoFood.rds",full.names = T)
efiles<- lapply(efiles, readRDS)
erbind<- do.call(rbind, efiles)

#ajuste y corregir asuntos de texto
erbind$Scenarios<-  gsub("'",'',erbind$Scenarios)
erbind$Regions<-  gsub("'", '',erbind$Regions)
erbind$Year<-  gsub("'",'',erbind$Year)
erbind$Scenarios<- as.character( erbind$Scenarios)
erbind$Regions<- as.character(erbind$Regions) 


erbind<-   filter(erbind, Scenarios %in% s) %>% filter(., !Regions %in% rall) %>% spread(Year, Val) 
erbind$change<- ((erbind$`2050`-erbind$`2010`)/erbind$`2010`)*100
erbind<- erbind[,c("Scenarios","Regions","parameter","change")]

erbind<-  erbind %>% spread(Scenarios, change) 
colnames(erbind)<- c("Regions","parameter","HIGH+NARS","HIGH","RMM", "REGION", "REF")


# Economic Variables2--------------
efiles2<-list.files(path = rdsFiles, pattern = "EcoFood2.rds",full.names = T)
efiles2<- lapply(efiles2, readRDS)
erbind2<- do.call(rbind, efiles2)

#ajuste y corregir asuntos de texto
erbind2$Scenarios<-  gsub("'",'',erbind2$Scenarios)
erbind2$Regions<-  gsub("'", '',erbind2$Regions)
erbind2$Year<-  gsub("'",'',erbind2$Year)
erbind2$Scenarios<- as.character( erbind2$Scenarios)
erbind2$Regions<- as.character(erbind2$Regions) 


erbind2<- erbind2 %>% filter(Scenarios %in% s) %>% 
                                   filter(., !Regions %in% rall) %>% 
                                   spread(Year, Val) 
                             
erbind2$change<- ((log10(erbind2$`2050`)- log10(erbind2$`2010`))/(2050-2010))
erbind2<- erbind2[,c("Scenarios","Regions","parameter","change")]
erbind2<-  erbind2 %>% spread(Scenarios, change) 
colnames(erbind2)<- c("Regions","parameter","HIGH+NARS","HIGH","RMM", "REGION", "REF")

#Economic variables3-------------
efiles3<-list.files(path = rdsFiles, pattern = "EcoFood3.rds",full.names = T)
efiles3<- lapply(efiles3, readRDS)
erbind3<- do.call(rbind, efiles3)

#ajuste y corregir asuntos de texto
erbind3$Scenarios<-  gsub("'",'',erbind3$Scenarios)
erbind3$Regions<-  gsub("'", '',erbind3$Regions)
erbind3$Year<-  gsub("'",'',erbind3$Year)
erbind3$Scenarios<- as.character( erbind3$Scenarios)
erbind3$Regions<- as.character(erbind3$Regions) 


erbind3<- erbind3 %>% filter(Scenarios %in% s) %>% 
      filter(., !Regions %in% rall) %>% 
      spread(Year, Val) 

erbind3$change<- (erbind3$`2050`- erbind3$`2010`)
erbind3<- erbind3[,c("Scenarios","Regions","parameter","change")]
erbind3<-  erbind3 %>% spread(Scenarios, change) 

colnames(erbind3)<- c("Regions","parameter","HIGH+NARS","HIGH","RMM", "REGION", "REF")


############################### Tratamiento y construccion de grupos de variables ######################

#detection and deleted of NA
crbind[is.na(crbind)]<- 0
erbind[is.na(erbind)]<- 0
jrbind[is.na(jrbind)]<- 0
erbind2[is.na(erbind2)]<- 0
erbind3[is.na(erbind3)]<- 0


crbind<- crbind %>% gather(Sce, change, 4:8)
erbind<- erbind %>% gather(Sce, change, 3:7)
erbind2<- erbind2 %>% gather(Sce, change, 3:7)
erbind3<- erbind3 %>% gather(Sce, change, 3:7)
jrbind<- jrbind %>% gather(Sce, change, 5:9)


###################################### Tratamiento de datos economicos #################################
#economic1
ad_erbind<- erbind %>% split(erbind$Sce)
efiles<- list()

for(i in 1:length(ad_erbind)){ 
      efiles[[i]] <- ad_erbind[[i]] %>% spread(parameter, change) %>% 
            gather(Variable, Summary, -(Regions:Sce)) %>%
            unite(Temp, Sce, Variable)%>% spread(Temp, Summary) 

}

#economic2
ad_erbind2<- erbind2 %>% split(erbind2$Sce)
efiles2<- list()

for(i in 1:length(ad_erbind2)){ 
      efiles2[[i]] <- ad_erbind2[[i]] %>% spread(parameter, change) %>% 
            gather(Variable, Summary, -(Regions:Sce)) %>%
            unite(Temp, Sce, Variable)%>% spread(Temp, Summary) 
      
}

#economic3
ad_erbind3<- erbind3 %>% split(erbind3$Sce)
efiles3<- list()

for(i in 1:length(ad_erbind3)){ 
      efiles3[[i]] <- ad_erbind3[[i]] %>% spread(parameter, change) %>% 
            gather(Variable, Summary, -(Regions:Sce)) %>%
            unite(Temp, Sce, Variable)%>% spread(Temp, Summary) 
      
}

######################################## Irrigacion  ###################################################

ad_jrbind<- jrbind %>% split(jrbind$Sce)
jfiles<- list()
#i=1
for(i in 1:length(ad_jrbind)){ 
      jfiles[[i]] <- ad_jrbind[[i]] %>% spread(parameter, change) %>% 
            gather(Variable, Summary, -(Commodity:Sce)) %>%
            unite(Temp, Sys, Variable)%>% spread(Temp, Summary) %>%
            gather(Variable, Summary, -(Commodity:Sce))%>%
            unite(Temp, Sce, Variable)%>% spread(Temp, Summary)
      
}


######################################### Analisis PCA #################################################
ad_crbind<- crbind %>% split(crbind$Sce)
cfiles<- list()
usaid<- c("HIGH","HIGH+NARS","REF","REGION","RMM")
# c=1
# s=1
for(s in 1:length(ad_crbind)){ 
      
      cfiles[[s]]<- ad_crbind[[s]] %>% spread(parameter, change)
      
      ##### By Sce
      cfiles[[s]] <-  cfiles[[s]]  %>%  gather(Variable, Summary, -(Commodity:Sce)) %>%
            unite(Temp, Sce, Variable)%>% spread(Temp, Summary) 
      dfjoin<- left_join(cfiles[[s]],efiles[[s]], by=("Regions"))
      dfjoin<- left_join(dfjoin,efiles2[[s]], by=("Regions"))
      dfjoin<- left_join(dfjoin,efiles3[[s]], by=("Regions"))
      dfjoin<- left_join(dfjoin, jfiles[[s]], by=c("Commodity","Regions"))

      

      ##### By crops
      ad_crops<- dfjoin %>% split(dfjoin$Commodity)
    
      lapply(1:length(ad_crops), function(c){
      
            crop<- unique(as.character(ad_crops[[c]]$Commodity)) 
            sce<- usaid[s]      
            zones<- unique(ad_crops[[c]]$Regions)

            # Creating directories 
            wk_dir <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/RTBAnalysis" 
            if(!dir.exists(paths = paste(wk_dir, "/",crop, sep = ""))){ dir.create(path = paste(wk_dir, "/", crop, sep = "")) }
            
            # Creando data frame 
            dff<- ad_crops[[c]]
            dff<- dff[grep(pattern ="MEN|SSA|SAS|LAC|EAP" ,x = dff$Regions, ignore.case = T),]
            
            #eliminando variables con baja correlación 
            dff<- dff[,-c(5,6,7,8,10,11,12,17,19,21)] 

            dbox<- dff
            dbox$Sce<- usaid[s] 
            write.csv(x = dbox, file = paste(wk_dir, "/", crop,"_", usaid[s],"_boxplot.csv", sep = ""))
            
#            # boxplot
             boxplot(dff[,3:ncol(dff)], horizontal = T)
            
            # Deleting columns with SD = 0 
            rownames(dff) <- dff$Regions
            dff$Regions <- dff$Commodity <- NULL 
            ##Calcular porcentaje de datos faltantes por variable
            miss <- apply(X=dff, MARGIN = 2, FUN = function(x){ y <- sum(is.na(x))/nrow(dff); return(y*100) })
            dff <- dff[,names(which(miss <= 30))]
            dff[is.na(dff)]<- 0
            dff <- Filter(function(x) sd(x) != 0, dff) 

            #detection outluiers, multivariado
            mahal<- mahalanobis(dff,colMeans(dff, na.rm = TRUE),cov(dff), use="pairwise.complete.obs")
            cuotoff<- qchisq(0.999, ncol(dff))
            summary(mahal < cuotoff) # identificar outliners
            outMvar<- dff[mahal>cuotoff,]
            
            if(nrow(outMvar)>1){
                  outMvar<- as.data.frame(outMvar)
                  outMvar$sce<- sce
                  outMvar$crop<- crop
                  outMvar$Regions<- row.names(outMvar)
                  row.names(outMvar)<- NULL
                  outMvar<- outMvar %>% gather(Var, Val, 1:(ncol(outMvar)-3))
                  outMvar$Var<-  gsub("HIGH_", '',outMvar$Var) 
                  outMvar$Var<-  gsub("REF_", '',outMvar$Var) 
                  outMvar$Var<-  gsub("REGION_", '',outMvar$Var) 
                  outMvar$Var<-  gsub("RMM_", '',outMvar$Var) 
                  outMvar$Var<-  gsub("RMM_", '',outMvar$Var) 
                  outMvar$Var<-  gsub("[[:punct:]]", '',outMvar$Var) 
                  outMvar$Var<-  gsub("HIGHNARS",'', outMvar$Var)
                  outMvar$Regions<-  gsub("LAC-", '',outMvar$Regions)
                  outMvar$Regions<-  gsub("FSU-", '',outMvar$Regions)
                  outMvar$Regions<-  gsub("SSA-", '',outMvar$Regions)
                  outMvar$Regions<-  gsub("MEN-", '',outMvar$Regions)
                  outMvar$Regions<-  gsub("SAS-", '',outMvar$Regions)
                  outMvar$Regions<-  gsub("EAP-", '',outMvar$Regions)
                  
                  write.csv(x = outMvar, file = paste(wk_dir, "/", crop,"_",sce,"_OutlierMV.csv", sep = "") )
                  
            }else{cat("no pasa nada de nada")}
           
            
            dff<- dff[mahal< cuotoff,]
            write.csv(x = dff, file = paste(wk_dir, "/", crop,"/",sce,"dataGenesis.csv", sep = "") )

            # Correlation matrix 
            if(!file.exists(paste(wk_dir, "/", crop,"/",sce,"_corrMatrix.png", sep = ""))){ 

                  M<- cor(dff)
                  png(file = paste(wk_dir, "/", crop,"/",sce,"_corrMatrix.png", sep = ""), height = 8, width = 8, units = "in", res = 300) 
                  corrplot.mixed(M) 
                  dev.off() 
            } 
            

            # Principal Component Analysis 
            res.pca <- FactoMineR::PCA(dff, graph = F,scale.unit = T) #quanti.sup =8, ind.sup =12
            if(!file.exists(paste(wk_dir, "/", crop,"/",sce,"_eigenValuesPCA.png", sep = ""))){ 
                  gg <- fviz_eig(res.pca, addlabels = TRUE, hjust = -0.3) + theme_bw() # Visualize eigenvalues/variances 
                  ggsave(filename = paste(wk_dir, "/", crop,"/",sce,"_eigenValuesPCA.png", sep = ""), plot = gg, width = 8, height = 8, units = "in") 
            } 
            
            if(!file.exists(paste(wk_dir, "/", crop,"/",sce,"_varQuality.png", sep = ""))){ 
                  png(paste(wk_dir, "/", crop, "/",sce,"_varQuality.png", sep = ""), height = 8, width = 16, units = "in", res = 300) 
                  par(mfrow = c(1, 3)) 
                  corrplot(res.pca$var$cos2[,1:2], is.corr = FALSE, title = "Representation quality", mar = c(1, 0, 1, 0))        # Representation quality of each variable 
                  corrplot(res.pca$var$contrib[,1:2], is.corr = FALSE, title = "Contribution", mar = c(1, 0, 1, 0))               # Contribution of each variable to dimension 
                  corrplot(res.pca$var$cor[,1:2], method = "ellipse", is.corr = TRUE, title = "Correlation", mar = c(1, 0, 1, 0)) # Correlation of each variable to dimension 
                  dev.off() 
            } 
                  
            # Hierarchical Clustering on Principle Components 
            res.hcpc <- FactoMineR::HCPC(res.pca, nb.clust = -1, graph = F) 
            if(!file.exists(paste(wk_dir, "/", crop, "/",sce,"_biplotPCA.png", sep = ""))){ 
                  gg <- fviz_pca_biplot(res.pca, label = "var", habillage = res.hcpc$data.clust$clust, addEllipses = TRUE, ellipse.level = 0.95) + theme_bw() # Biplot of individuals and variables. Only variables are labelled (var, ind) 
                  ggsave(filename = paste(wk_dir, "/", crop, "/",sce,"_biplotPCA.png", sep = ""), plot = gg, width = 8, height = 8, units = "in") 
            } 

            # Getting coordinates by cluster
            coordClust<- res.hcpc$call$X
            coordClust$Regions<- rownames(coordClust)
            rownames(coordClust)<- NULL
            coordClust$sce<- sce
            coordClust$crop<- crop
            coordClust<- coordClust[,c("Regions","sce","crop","Dim.1","Dim.2","clust")]
            interClus<- coordClust %>% split(coordClust$clust)
            lapply(1: length(interClus), function(i){
                  d<- interClus[[i]]
                  write.csv(x = d,paste(wk_dir, "/",crop,"_",sce,"Inter_clusT",i,".csv" ,sep = ""))
                  
            
            })
               
            
            # Getting countries farthest 
            
            disFar<- res.hcpc$desc.ind$dist
            disClos<- (res.hcpc$desc.ind$para)
            
            #cluster grafico1
            png(paste(wk_dir, "/", crop, "/",sce,"_1ClusterPLOTPCA.png", sep = ""))
            plot(res.hcpc, choice = "3D.map", angle=60)
            dev.off()


            #Cluster grafico2
            gg1 <- fviz_cluster(res.hcpc,ellipse.alpha = 0.1,main = paste("Scenario: ",sce, " ","crop: ",crop, sep = "" ))  #labelsize = T, ggtheme = theme_minimal()
            ggsave(filename = paste(wk_dir, "/", crop, "/",sce,"_2ClusterPLOTPCA.png", sep = ""), plot = gg1, width = 8, height = 8, units = "in") 

            #dendrogram
            gg1<- fviz_dend(res.hcpc,main = paste("Scenario: ",sce, " ","crop: ",crop, sep = "" ),horiz = T ,cex = 0.5, color_labels_by_k = FALSE, rect = TRUE)
            ggsave(filename = paste(wk_dir, "/", crop, "/",sce,"_DendogramPCA.png", sep = ""), plot = gg1, width = 10, height = 8, units = "in") 
            
            
            # Individuals factor map
            if(!file.exists(paste(wk_dir, "/", crop, "/",sce,"_IndividualsFactorPCA.png", sep = ""))){ 
                  gg <- fviz_pca_ind(res.pca, habillage = res.hcpc$data.clust$clust, addEllipses = TRUE, ellipse.level = 0.95) + theme_bw() # Biplot of individuals and variables. Only variables are labelled (var, ind) 
                  ggsave(filename = paste(wk_dir, "/", crop, "/",sce,"_IndividualsFactorPCA.png", sep = ""), plot = gg, width = 8, height = 8, units = "in") 
            } 
            
            #deteccion de datos por cluster
            desClus<- res.hcpc$desc.var$quanti
            
            for(i in 1:length(desClus)){
                   if(!is.null(desClus[[i]])){
                  cfilesClust<- as.data.frame(desClus[[i]])
                  cfilesClust$Var<- rownames(cfilesClust)
                  rownames(cfilesClust) <- NULL
                  cfilesClust$cv <- cfilesClust$`sd in category`/cfilesClust$`Mean in category`
                  cfilesClust$crop<- crop
                  cfilesClust$clust<- i 
                  cfilesClust$Sce<- sce
                  write.csv(x =cfilesClust, file = paste(wk_dir, "/", crop,"/",sce, "cluster_",i,"_desCluster.csv", sep = ""))
                   }else{}
                  }
                 
            # Contribution individual   
            indcontri<- data.frame(res.pca$ind$contrib)
            indcontri$Sce<- sce
            indcontri$crop<- crop
            indcontri$Regions<- rownames(indcontri)
            rownames(indcontri) <- NULL
             indcontri<- indcontri[,c("Sce","crop","Regions","Dim.1","Dim.2","Dim.3","Dim.4","Dim.5")]
            write.csv(x =indcontri, file = paste(wk_dir, "/", crop,"/",sce,"_IndContribution.csv", sep = ""))
            
            # Contribution variable   
            indcontriVar<- data.frame(res.pca$var$contrib)
            indcontriVar$Sce<- sce
            indcontriVar$crop<- crop
            indcontriVar$var<- rownames(indcontriVar)
            rownames(indcontriVar) <- NULL
            indcontriVar<- indcontriVar[,c("Sce","crop","var","Dim.1","Dim.2","Dim.3","Dim.4","Dim.5")]
            write.csv(x =indcontriVar, file = paste(wk_dir, "/", crop,"/",sce,"_VarContribution.csv", sep = ""))
            
            
            # Index based on PCA 
            script <- getURL("https://raw.githubusercontent.com/haachicanoy/r_scripts/master/calculate_index_by_pca.R", ssl.verifypeer = FALSE); eval(parse(text = script)); rm(script) 

            #cluster
            df_cluster <- res.hcpc$data.clust 
            dataCluster<- res.hcpc$data.clust 
            dataCluster$Sce<- sce
            dataCluster$crop<- crop
            dataCluster$Regions<- rownames(dataCluster)
            rownames(dataCluster) <- NULL
            dataCluster<- dataCluster[,c("crop","Regions","Sce","clust")]
            write.csv(x =dataCluster, file = paste(wk_dir, "/", crop,"/",sce,"_DataCluster.csv", sep = ""))
            
         
            cat(paste(crop, " done\n", sep = ""))
      })
      
      cat(paste("terminos  el escenario ", s, " cool it's done\n", sep = ""))
}



############################# Analisis de cluster #############
wk_dir <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/RTBAnalysis" 

for(i in 1:length(rtb)){
      c_clus<- list.files(path =paste(wk_dir,"/",rtb[i],sep = ""), pattern = "DataCluster.csv", full.names = T)
      c_clus<- lapply(c_clus, read.csv)
      
      rr<- do.call(rbind, c_clus)
      rr$X<- NULL
      write.csv(x = rr, file = paste(wk_dir, "/", rtb[i],"/","sumCluster.csv", sep = ""))
      write.csv(x = rr, file = paste(wk_dir, "/", rtb[i],"_sumCluster.csv", sep = ""))
      
      png(filename = paste(wk_dir,"/",rtb[i],"_HeatMapCluster",".png",sep=""), 
          width = 10, height = 10, units = 'in', res = 100)
      gg<- ggplot(rr, aes(Sce, Regions)) + 
            geom_tile(aes(fill = as.factor(clust)),  colour = "white") + 
            scale_fill_brewer(palette = "Set1")+
            labs(x=NULL, y=NULL, title=paste( "Clustering of\n ", rtb[i], " by scenarios",sep = ""))+
            coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme_grey() + labs(x = "",y = "")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
            theme(axis.text.y = element_text(hjust = 1, size = 12))+
            theme(strip.text.x = element_text(size = 12, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 12))+
            guides(fill=guide_legend(title="Cluster"))
      plot(gg)
      dev.off()  
      
      print(i)
      
}

############################################ total subregions 
c_clus<- list.files(path =wk_dir, pattern = "_sumCluster", full.names = T)
c_clus<- lapply(c_clus, read.csv)
rrsubregions<- do.call(rbind, c_clus)
rrsubregions$X<- NULL

write.csv(x = rrsubregions, file = paste(wk_dir, "/","subregionsTotalCluster.csv", sep = ""))


#correr 
rrsubregions$Regions<-  gsub("LAC-", '',rrsubregions$Regions)
rrsubregions$Regions<-  gsub("FSU-", '',rrsubregions$Regions)
rrsubregions$Regions<-  gsub("SSA-", '',rrsubregions$Regions)
rrsubregions$Regions<-  gsub("MEN-", '',rrsubregions$Regions)
rrsubregions$Regions<-  gsub("SAS-", '',rrsubregions$Regions)
rrsubregions$Regions<-  gsub("EAP-", '',rrsubregions$Regions)


rrsubregions$Sce <- factor(rrsubregions$Sce, levels = c("REF","HIGH","HIGH+NARS","REGION","RMM"))

png(filename = paste(wk_dir,"/","HeatMapClustersuper",".png",sep=""), 
    width = 12, height =16 , units = 'in', res = 100)
gg<- ggplot(rrsubregions, aes(Sce, Regions)) + 
      geom_tile(aes(fill = as.factor(clust)),  colour = "white") + 
      scale_fill_brewer(palette = "Set1")+ facet_grid(~crop,scales = "free")+
      labs(x=NULL, y=NULL, title=paste( "RTB crops clustering of\n ", " by scenarios",sep = ""))+
      coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))+
      theme(axis.text.y = element_text(hjust = 1, size = 9))+
      theme(strip.text.x = element_text(size = 9, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 9))+
      guides(fill=guide_legend(title="Cluster"))
plot(gg)
dev.off()  


########################################### groups by cluster

for(i in 1:6){
      c_clusG<- list.files(path =paste(wk_dir,"/",rtb[i],sep = ""), pattern = "desCluster.csv", full.names = T)
      c_clusG<- lapply(c_clusG, read.csv)
      rrG<- do.call(rbind, c_clusG)
      rrG$X<- NULL
      rrG$v.test<- NULL
      rrG<- rrG[,c("crop","clust", "Sce" ,"Var","Mean.in.category", "Overall.mean", "sd.in.category","Overall.sd","p.value", "cv")]
      rrG$crop<- as.character(rrG$crop)
      rrG$Var<- as.character(rrG$Var)
      write.csv(x = rrG, file = paste(wk_dir, "/", rtb[i],"_sum_AllClustReport.csv", sep = ""))
      print(i)

}

c_clusG<- list.files(path =wk_dir, pattern = "_sum_AllClustReport.csv", full.names = T)
c_clusG<- lapply(c_clusG, read.csv)
rrG<- do.call(rbind, c_clusG)
rrG$X<- NULL
rrG$Var<-  gsub("HIGH_", '',rrG$Var) 
rrG$Var<-  gsub("REF_", '',rrG$Var) 
rrG$Var<-  gsub("REGION_", '',rrG$Var) 
rrG$Var<-  gsub("RMM_", '',rrG$Var) 
rrG$Var<-  gsub("RMM_", '',rrG$Var) 
rrG$Var<-  gsub("[[:punct:]]", '',rrG$Var) 
rrG$Var<-  gsub("HIGHNARS",'', rrG$Var)

write.csv(x = rrG, file = paste(wk_dir, "/","TotalCluster.csv", sep = ""))



########################## additional analysis ########################
#tabla con cluster y paises que corresponde a cada cluster
c_distri<- list.files(path =wk_dir, pattern = "boxplot", full.names = T)
c_distri<- lapply(c_distri, read.csv)

for(c in 1:length(c_distri)){
      
      # d<-  c_distri[[c]]
      c_distri[[c]]$X<- NULL
      c_distri[[c]]<- c_distri[[c]] %>% gather(Var, change, 3:(ncol(c_distri[[c]])-1))
      c_distri[[c]]$Var<-  gsub("HIGH_", '',c_distri[[c]]$Var) 
      c_distri[[c]]$Var<-  gsub("REF_", '',c_distri[[c]]$Var) 
      c_distri[[c]]$Var<-  gsub("REGION_", '',c_distri[[c]]$Var) 
      c_distri[[c]]$Var<-  gsub("RMM_", '',c_distri[[c]]$Var) 
      c_distri[[c]]$Var<-  gsub("[[:punct:]]", '',c_distri[[c]]$Var) 
      c_distri[[c]]$Var<-  gsub("HIGHNARS",'', c_distri[[c]]$Var)
      c_distri[[c]]$Regions<-  gsub("LAC-", '',c_distri[[c]]$Regions)
      c_distri[[c]]$Regions<-  gsub("FSU-", '',c_distri[[c]]$Regions)
      c_distri[[c]]$Regions<-  gsub("SSA-", '',c_distri[[c]]$Regions)
      c_distri[[c]]$Regions<-  gsub("MEN-", '',c_distri[[c]]$Regions)
      c_distri[[c]]$Regions<-  gsub("SAS-", '',c_distri[[c]]$Regions)
      c_distri[[c]]$Regions<-  gsub("EAP-", '',c_distri[[c]]$Regions)
      
      print(c)      
}

ccc<- do.call(rbind, c_distri)

colnames(ccc)[1]<- "crop"

testC<- left_join(ccc, rrsubregions, by=c("crop", "Regions", "Sce"))
testXX<- left_join(rrG, testC, by=c("crop", "Sce", "Var","clust"))
testXX<- testXX[,c("crop","clust", "Sce","Var", "Regions","change" )]


cultivation<- unique(testXX$crop)
grupos<- unique(testXX$clust)
testXX$Sce <- factor(testXX$Sce, levels = c("REF","HIGH","HIGH+NARS","REGION","RMM"))


for(c in 1:length(cultivation)){
                        cfiles<- testXX %>% filter(crop==cultivation[c])
                        png(filename = paste(wk_dir,"/",cultivation[c],"_BloxPlot",".png",sep=""), 
                            width = 12, height =16 , units = 'in', res = 100)
                        
                         gg<- ggplot(cfiles, aes(y=change, x=droplevels(Sce),fill=Var)) + 
                               geom_boxplot()  + 
                               facet_grid(clust~Var, scales = "free")+
                               labs(x=NULL, y=NULL, title=paste(cultivation[c]," cluster  boxplot\n ", " by scenarios",sep = ""))+
                               theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                               theme_grey() + labs(x = "",y = "")+
                               theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))+
                               theme(axis.text.y = element_text(hjust = 1, size = 9))+
                               theme(strip.text.x = element_text(size = 9, face = "bold.italic"))+
                               theme(strip.text=element_text(size=8))+
                               theme(strip.text.y = element_text(angle = 0,size = 9))
                         plot(gg)
                         dev.off()  
                         
                  print(i)
 }
    

### Paises con valores extremos

coutfiles<- list.files(path = wk_dir,pattern = "OutlierMV", full.names = T)
coutfiles<- lapply(coutfiles, read.csv, header=T)
coutfiles<- do.call(rbind, coutfiles)
coutfiles$X<- NULL
coutfiles<- coutfiles[,c("Regions","crop","sce", "Var", "Val")]  
coutfiles<- coutfiles %>% spread (sce, Val)
write.csv(x = coutfiles,file = paste(wk_dir,"/","CountriesAtypical.csv", sep = ""))

### intersection across scenarios
it<- list.files(path = wk_dir,pattern = "Inter_clusT", full.names = T)
it<- lapply(it, read.csv)
it<- do.call(rbind, it)
it$X<- NULL


crosInter<- it%>% split(it$crop)
# c=1
intentM<- lapply(1:length(crosInter), function(c){
      df<- crosInter[[c]]
      crop<- droplevels(unique(df$crop))
      df<- df[,c("Regions","sce","crop", "clust")]
      cfcrops<- df %>% split(df$sce)
      a<- left_join(cfcrops[[1]],cfcrops[[2]],by = c("Regions", "crop"))
      b<- left_join(a,cfcrops[[3]],by = c("Regions", "crop"))
      c<- left_join(b,cfcrops[[4]],by = c("Regions", "crop"))
      d<- left_join(c,cfcrops[[5]],by = c("Regions", "crop"))
      
      d$inter <- apply(d[,c(4,6,8,10,12)], 1, function(x)( all(x==1) || all(x==2) || all(x==3) || all(x==4) ))
      d$inter<- ifelse(d$inter=="TRUE",1,0)
#       d<- d[,c("Regions","crop", "inter")]
      rateStab<- d %>% group_by(inter) %>% summarise(n = n())
      write.csv(rateStab, paste(wk_dir,"/" ,crop,"_consisting.csv", sep = ""))
      cat(paste("Cultivation: ", crop, " is complete\n", sep = ""))
        
})

