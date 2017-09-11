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
       "Southern Europe","Western Africa","Western Asia", "Western Europe")
r5<- c("MENg","EAPg")
rall<- c(r2,r3,r4, r5)


t<- c(2010, 2030,2050)
# Vector con los cultivos para RTB incluyendo Bananas
rtb<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Other Roots","R&T-Cassava","F&V-Banana") 


cfiles<-list.files(path = rdsFiles,pattern = ".rds", full.names = T )
cfiles<- lapply(cfiles, readRDS)

cdata<- cfiles
cdata[[5]] <- NULL
cdata[[3]] <- NULL
cdata[[2]] <- NULL

# primer grupo de variables 

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
            cdata[[i]]$Mean<- apply(cdata[[i]][,6:ncol(cdata[[i]])],1,function(x){mean(x,na.rm = T)})
            cdata[[i]]<-  cdata[[i]][,c("Scenarios", "Commodity", "Regions",  "parameter", "Mean" )]
            cdata[[i]]<-  cdata[[i]]%>% spread(Scenarios, Mean) 
            colnames(cdata[[i]])<- c("Commodity", "Regions","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
            
            cdata[[i]]$dpHIGH_NARS<- ((cdata[[i]]$`HIGH+NARS`- cdata[[i]]$REF)/ cdata[[i]]$REF)*100
            cdata[[i]]$dpHIGH<- ((cdata[[i]]$HIGH- cdata[[i]]$REF)/ cdata[[i]]$REF)*100
            cdata[[i]]$dpRMM<- ((cdata[[i]]$RMM - cdata[[i]]$REF)/ cdata[[i]]$REF)*100
            cdata[[i]]$dpREGION<- ((cdata[[i]]$REGION- cdata[[i]]$REF)/ cdata[[i]]$REF)*100
            cdata[[i]]<- cdata[[i]][,c("Commodity","Regions","parameter","dpHIGH_NARS", "dpHIGH","dpRMM","dpREGION")]

            cdata[[i]]<- cdata[[i]] %>% gather(Variable, Summary, -(Commodity:parameter)) %>%
                  unite(temp, parameter, Variable) %>% spread(temp, Summary) 

    print(i)
}

c<- left_join(cdata[[1]], cdata[[2]], by = c("Commodity", "Regions")) %>% left_join(., cdata[[3]],by = c("Commodity", "Regions") )



# segundo de grupo de variables 
jdata<- cfiles
jdata[[6]] <- NULL
jdata[[4]] <- NULL
jdata[[2]] <- NULL
sp <- NULL

sp<- do.call(rbind, jdata[1])

sp$Scenarios<-  gsub("'",'',sp$Scenarios)
sp$Commodity<-  gsub("'", '',sp$Commodity)
sp$Regions<-  gsub("'", '',sp$Regions)
sp$Year<-  gsub("'",'',sp$Year)
# sp$Sys<-  gsub("'",'',sp$Sys)

sp$Scenarios<- as.character( sp$Scenarios)
sp$Commodity<- as.character( sp$Commodity)
sp$Regions<- as.character(sp$Regions) 
      
sp<- filter(sp, Scenarios %in% s) %>% filter(., !Regions %in% rall) %>% filter(., Commodity %in% rtb)
sp<-  sp%>% spread(Year, Val) 


sp$Mean<- apply(sp[,6:ncol(sp)],1,function(x){mean(x,na.rm = T)})
sp<-  sp[,c("Scenarios", "Commodity", "Regions",  "parameter", "Mean" )]
sp<-  sp%>% spread(Scenarios, Mean) 
colnames(sp)<- c("Commodity", "Regions","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )

sp$dpHIGH_NARS<- ((sp$`HIGH+NARS`- sp$REF)/ sp$REF)*100
sp$dpHIGH<- ((sp$HIGH- sp$REF)/ sp$REF)*100
sp$dpRMM<- ((sp$RMM - sp$REF)/ sp$REF)*100
sp$dpREGION<- ((sp$REGION- sp$REF)/ sp$REF)*100
sp<- sp[,c("Commodity","Regions","parameter","dpHIGH_NARS", "dpHIGH","dpRMM","dpREGION")]


sp<- sp %>% gather(Variable, Summary, -(Commodity:parameter)) %>%
      unite(temp, parameter, Variable) %>% spread(temp, Summary) 

#sistemas de produccion
ss<- do.call(rbind, jdata[2])

ss$Scenarios<-  gsub("'",'',ss$Scenarios)
ss$Commodity<-  gsub("'", '',ss$Commodity)
ss$Regions<-  gsub("'", '',ss$Regions)
ss$Year<-  gsub("'",'',ss$Year)
ss$Sys<-  gsub("'",'',ss$Sys)

ss$Scenarios<- as.character( ss$Scenarios)
ss$Commodity<- as.character( ss$Commodity)
ss$Regions<- as.character(ss$Regions) 
ss<- filter(ss,Sys!="gir")
ffiles<-  filter(ss, Scenarios %in% s) %>% filter(., !Regions %in% rall) %>% filter(., Commodity %in% rtb)

ffiles<-  ffiles %>%  spread(Year, Val) 
ffiles$Mean<- apply(ffiles[,7:ncol(ffiles)],1,function(x){mean(x,na.rm = T)})
ffiles<- ffiles[,c("Scenarios","Commodity","Regions","Sys","parameter","Mean")]

ffiles<-  ffiles%>% group_by(Sys) %>% spread(Scenarios, Mean) 
colnames(ffiles)<- c("Commodity", "Regions","Sys", "parameter","HIGH+NARS","HIGH","RMM", "REGION", "REF")

ffiles$dpHIGH_NARS<- ((ffiles$`HIGH+NARS`- ffiles$REF)/ ffiles$REF)*100
ffiles$dpHIGH<- ((ffiles$HIGH- ffiles$REF)/ ffiles$REF)*100
ffiles$dpRMM<- ((ffiles$RMM - ffiles$REF)/ ffiles$REF)*100
ffiles$dpREGION<- ((ffiles$REGION- ffiles$REF)/ ffiles$REF)*100
ffiles<- ffiles[,c("Commodity","Regions","Sys","parameter","dpHIGH_NARS", "dpHIGH","dpRMM","dpREGION")]


ffilesTest <- ffiles %>% 
      gather(Variable, Summary, -(Commodity:parameter))%>%
      unite(temp, parameter,Variable)%>%
      spread(temp, Summary) %>% 
      gather(Variable, Summary, -(Commodity:Sys))%>%
      unite(temp, Sys, Variable)%>%
      spread(temp, Summary)


    
      

#sistemas de produccion
ss1<- do.call(rbind, jdata[3])

ss1$Scenarios<-  gsub("'",'',ss1$Scenarios)
ss1$Regions<-  gsub("'", '',ss1$Regions)
ss1$Year<-  gsub("'",'',ss1$Year)
ss1$Scenarios<- as.character( ss1$Scenarios)
ss1$Regions<- as.character(ss1$Regions) 


ffiles1<-  filter(ss1, parameter!="ShareAtRiskXagg") %>% filter(., Scenarios %in% s) %>% filter(., !Regions %in% rall) %>% spread(Year, Val) 
ffiles1$Mean<- apply(ffiles1[,5:ncol(ffiles1)],1,function(x){mean(x,na.rm = T)})
ffiles1<- ffiles1[,c("Scenarios","Regions","parameter","Mean")]

ffiles1<-  ffiles1 %>% spread(Scenarios, Mean) 
colnames(ffiles1)<- c("Regions","parameter","HIGH+NARS","HIGH","RMM", "REGION", "REF")

ffiles1$dpHIGH_NARS<- ((ffiles1$`HIGH+NARS`- ffiles1$REF)/ ffiles1$REF)*100
ffiles1$dpHIGH<- ((ffiles1$HIGH- ffiles1$REF)/ ffiles1$REF)*100
ffiles1$dpRMM<- ((ffiles1$RMM - ffiles1$REF)/ ffiles1$REF)*100
ffiles1$dpREGION<- ((ffiles1$REGION- ffiles1$REF)/ ffiles1$REF)*100
ffiles1<- ffiles1[,c("Regions","parameter","dpHIGH_NARS", "dpHIGH","dpRMM","dpREGION")]

ffilesTest1 <-  ffiles1 %>% 
      gather(Variable, Summary, -(Regions:parameter)) %>%
      unite(temp, parameter, Variable)%>% spread(temp, Summary) 

# ffilesTest1$PopulationAtRiskXagg_dpHIGH<- NULL; ffilesTest1$PopXAgg_dpHIGH_NARS<- NULL; ffilesTest1$PopulationAtRiskXagg_dpREGION<- NULL
# ffilesTest1$PopXAgg_dpRMM<- NULL

#### Analisis PCA-----------------------


# Dtos para PCA, files

ctotalFiles<- left_join(c,ffilesTest, by=c("Commodity", "Regions"))%>% left_join(.,ffilesTest1, by = c("Regions")) 

# grupos de variables 
gfood<- c("FoodAvailXAgg_dpHIGH","FoodAvailXAgg_dpHIGH_NARS",
          "FoodAvailXAgg_dpREGION","FoodAvailXAgg_dpRMM")
gyield<- c("TYldXAgg_dpHIGH","TYldXAgg_dpHIGH_NARS",
           "TYldXAgg_dpREGION", "TYldXAgg_dpRMM")
gsupply<- c("QSupXAgg_dpHIGH","QSupXAgg_dpHIGH_NARS",
            "QSupXAgg_dpREGION","QSupXAgg_dpRMM")
gdemand<- c("QDXAgg_dpHIGH","QDXAgg_dpHIGH_NARS",
            "QDXAgg_dpREGION","QDXAgg_dpRMM")
garea<- c("TAreaXAgg_dpHIGH", "TAreaXAgg_dpHIGH_NARS",
          "TAreaXAgg_dpREGION","TAreaXAgg_dpRMM")
gnet<- c("QNXAgg_dpHIGH","QNXAgg_dpHIGH_NARS",
         "QNXAgg_dpREGION", "QNXAgg_dpRMM" )
gdemand_feed<- c("QLXAgg_dpHIGH","QLXAgg_dpHIGH_NARS",
                 "QLXAgg_dpREGION", "QLXAgg_dpRMM")
gdemand_bioful<- c("QBFXAgg_dpHIGH","QBFXAgg_dpHIGH_NARS", 
                   "QBFXAgg_dpREGION", "QBFXAgg_dpRMM" )
gdemand_food<- c("QFXAgg_dpHIGH","QBFXAgg_dpHIGH_NARS",
                 "QBFXAgg_dpREGION","QBFXAgg_dpRMM")
gdemand_others<- c("QOTHRXAgg_dpHIGH","QOTHRXAgg_dpHIGH_NARS",
                   "QOTHRXAgg_dpREGION","QOTHRXAgg_dpRMM")
geconomic<- c("GDPXAgg_dpHIGH","GDPXAgg_dpHIGH_NARS", "GDPXAgg_dpREGION", "GDPXAgg_dpRMM")

# Tratamiento a los NA
ctotalFiles[is.na(ctotalFiles)] <- 0 

ad_list <- ctotalFiles %>% split(ctotalFiles$Commodity) 

i=1
# Principal Component Analysis per crop 
lapply(1:length(ad_list), function(i){ 
          
   # Filter data by crop 
   df <- ad_list[[i]] 
    
   # Creating directories 
   wk_dir <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/RTBAnalysis" 
   crop <- unique(as.character(df$Commodity)) 
   if(!dir.exists(paths = paste(wk_dir, "/", crop, sep = ""))){ dir.create(path = paste(wk_dir, "/", crop, sep = "")) } 
    
   # Deleting columns with SD = 0 
   rownames(df) <- df$Regions; df$Regions <- df$Commodity <- NULL 
   df <- Filter(function(x) sd(x) != 0, df) 
    
   # Correlation matrix 
   if(!file.exists(paste(wk_dir, "/", crop, "/corrMatrix.png", sep = ""))){ 
           M <- cor(df) 
           png(file = paste(wk_dir, "/", crop, "/corrMatrix.png", sep = ""), height = 8, width = 8, units = "in", res = 300) 
           corrplot.mixed(M) 
           dev.off() 
         } 
 


   # Principal Component Analysis 
      res.pca <- FactoMineR::PCA(df, graph = F) 
    
      if(!file.exists(paste(wk_dir, "/", crop, "/eigenValuesPCA.png", sep = ""))){ 
           gg <- fviz_eig(res.pca, addlabels = TRUE, hjust = -0.3) + theme_bw() # Visualize eigenvalues/variances 
           ggsave(filename = paste(wk_dir, "/", crop, "/eigenValuesPCA.png", sep = ""), plot = gg, width = 8, height = 8, units = "in") 
         } 
    
      if(!file.exists(paste(wk_dir, "/", crop, "/varQuality.png", sep = ""))){ 
           png(paste(wk_dir, "/", crop, "/varQuality.png", sep = ""), height = 8, width = 16, units = "in", res = 300) 
           par(mfrow = c(1, 3)) 
           corrplot(res.pca$var$cos2[,1:2], is.corr = FALSE, title = "Representation quality", mar = c(1, 0, 1, 0))        # Representation quality of each variable 
           corrplot(res.pca$var$contrib[,1:2], is.corr = FALSE, title = "Contribution", mar = c(1, 0, 1, 0))               # Contribution of each variable to dimension 
           corrplot(res.pca$var$cor[,1:2], method = "ellipse", is.corr = TRUE, title = "Correlation", mar = c(1, 0, 1, 0)) # Correlation of each variable to dimension 
           dev.off() 
         } 
    
      # Hierarchical Clustering on Principle Components 
      res.hcpc <- FactoMineR::HCPC(res.pca, nb.clust = -1, graph = FALSE) 
      if(!file.exists(paste(wk_dir, "/", crop, "/biplotPCA.png", sep = ""))){ 
           gg <- fviz_pca_biplot(res.pca, label = "var", habillage = res.hcpc$data.clust$clust, addEllipses = TRUE, ellipse.level = 0.95) + theme_bw() # Biplot of individuals and variables. Only variables are labelled (var, ind) 
           ggsave(filename = paste(wk_dir, "/", crop, "/biplotPCA.png", sep = ""), plot = gg, width = 8, height = 8, units = "in") 
         } 

#       # Calculate distance between observations 
#       if(!file.exists(paste(wk_dir, "/", crop, "/distances.png", sep = ""))){ 
#            res.dist <- get_dist(df, stand = TRUE, method = "kendall") 
#            gg <- fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) 
#            ggsave(filename = paste(wk_dir, "/", crop, "/distances.png", sep = ""), plot = gg, width = 8, height = 8, units = "in") 
#          } 
# 
#       
#       
      # Index based on PCA 
      script <- getURL("https://raw.githubusercontent.com/haachicanoy/r_scripts/master/calculate_index_by_pca.R", ssl.verifypeer = FALSE); eval(parse(text = script)); rm(script) 
      
      df_cluster <- res.hcpc$data.clust 
      dataCluster<- res.hcpc$data.clust 
      write.csv(x =dataCluster, file = paste(wk_dir, "/", crop, "/DataCluster.csv", sep = ""))
      df_cluster$Index <- index_cal(res.pca); rm(index_cal) 
      df_cluster <- data.frame(df_cluster[,colnames(df_cluster)[which(colnames(df_cluster) != "clust")]], Cluster = df_cluster[,"clust"]) 
      df_cluster$Combination <- rownames(df_cluster) 
      df_cluster <- df_cluster %>% gather(Variable, Value, 1:70) 

      if(!file.exists(paste(wk_dir, "/", crop, "/cluster_boxplot.png", sep = ""))){ 
           gg <- ggplot(df_cluster, aes(x = Cluster, y = Value, fill = Cluster)) + geom_boxplot() 
           gg <- gg + facet_wrap(~ Variable, scales = 'free') 
           gg <- gg + theme_bw() + geom_hline(yintercept = 0) 
           ggsave(filename = paste(wk_dir, "/", crop, "/cluster_boxplot.png", sep = ""), plot = gg, width = 16, height = 8, units = "in") 
         } 

return(cat(paste(crop, " done\n", sep = ""))) 
    
 }) 
 

if(!file.exists(paste("//dapadfs/Workspace_cluster_9/CWR_pre-breeding/Input_data/precense_data/",crop_ggcmi,"/plots/summary_of_missing_data_/",crop_ggcmi," .png"))){
   
