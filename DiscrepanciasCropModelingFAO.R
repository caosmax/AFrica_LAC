### codigo para hacer analisis de discrepancias 



options(warn = -1); options(scipen = 999)
library(reshape)
library(tidyr)
library(dplyr)
library(ggplot2)
#Get FPU-country matching file
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")
fpu_convert <- read.csv("fpudata.csv")
fpu_convert <- fpu_convert[,c("New_FPU","Region_Nam")]
colnames(fpu_convert) <- c("FPU", "Country")

#Get FAO yields
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")
Yield_fao_raw <- read.csv("FAO yields beans maize wheat LAC.csv")
Yield_fao_raw <- Yield_fao_raw[, c("Area", "Item", "Year", "Value")]
Yield_fao_raw$Value <- Yield_fao_raw$Value / 10

Yield_fao_raw <- subset(Yield_fao_raw, Year %in% c(2010:2014))
Yield_fao_raw <- Yield_fao_raw %>% group_by(Area,Item) %>% summarise(mean(Value))
colnames(Yield_fao_raw)[1] <- "Country"
colnames(Yield_fao_raw)[3] <- "FAO yield"
Yield_fao_raw$Country <- as.character(Yield_fao_raw$Country)
Yield_fao_raw$Item <- as.character(Yield_fao_raw$Item)
u <- Yield_fao_raw$Country
Yield_fao_raw$Country[grep("Bolivia", u)] <- "Bolivia"
Yield_fao_raw$Country[grep("Venezuela", u)] <- "Venezuela"
rm(u)
u <- Yield_fao_raw$Item
Yield_fao_raw$Item[grep("Beans", u)] <- "Bean"
rm(u)
#------------------------------
#
cropvec <- c("Wheat", "Maize", "Bean", "Rice"); n_crops <- length(cropvec)
variety_vec_Wheat <- c("TajanBA_WFD_FPU", "Seri82BA_WFD_FPU", "HalconsnaBA_WFD_FPU", "Gerek79BA_WFD_FPU", "DonErnestoBA_WFD_FPU", "BrigadierBA_WFD_FPU")
variety_vec_Maize <- c("FM6_WFD_FPU", "H6_WFD_FPU", "MCCURDY6714_WFD_FPU")
variety_vec_Bean <- c("A193_WFD_FPU", "BAT881_WFD_FPU", "BRSRadiante_WFD_FPU", "Carioca_WFD_FPU", "ICTAOstua_WFD_FPU", "Manitou_WFD_FPU", "Perola_WFD_FPU")
variety_vec_list <- list(variety_vec_Wheat, variety_vec_Maize, variety_vec_Bean)
#------------------------------
#Get modeled yields
#Rainfed
FaoDiscrep_ra <- list(); YieldMod_ra <- list()
for(j in 1:n_crops)
{
    wd_str <- paste0("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/", cropvec[j], "_RA/",cropvec[j],"_Historical/")
    setwd(wd_str)
    Yield_fao <- subset(Yield_fao_raw, Item == cropvec[j])
    variety_vec <- variety_vec_list[[j]]
    YieldModOut <- list(); DiscrepOut <- list();
    for(i in 1:length(variety_vec))
    {
        Yield_mod_raw <- read.csv(paste0("RA_", variety_vec[i], ".csv"))
        Yield_mod_raw[,1] <- NULL
        colnames(Yield_mod_raw)[1] <- "FPU"
        Yield_mod_raw <- merge(Yield_mod_raw,fpu_convert,by = "FPU")
        Yield_mod <- Yield_mod_raw[,c("Country","FPU","Rend_fpu_2022")]
        Yield_mod$FPU <- NULL
        Yield_mod <- Yield_mod %>% group_by(Country) %>% summarise(mean(Rend_fpu_2022))
        colnames(Yield_mod)[2] <- variety_vec[i]
        Yield_fao_mod <- merge(Yield_fao, Yield_mod, by = "Country")
        Yield_fao_mod$discrep <- (Yield_fao_mod[, 4] - Yield_fao_mod$`FAO yield`) / Yield_fao_mod$`FAO yield` * 100
        Discrep_fao_mod <- Yield_fao_mod[,c("Country", "discrep")]
        colnames(Discrep_fao_mod)[2] <- variety_vec[i]
        if(i > 1){Yield_mod$Country <- NULL; Discrep_fao_mod$Country <- NULL}
        if(i == 1){Yield_mod$Crop <- rep(cropvec[j], nrow(Yield_mod)); Discrep_fao_mod$Crop <- rep(cropvec[j], nrow(Discrep_fao_mod))}
        DiscrepOut[[i]] <- Discrep_fao_mod
        YieldModOut[[i]] <- Yield_mod
    }
    x <- as.data.frame(do.call(cbind, DiscrepOut))
    gathercols <- variety_vec
    x <- gather_(x, "Variety", "Discrepancy", gathercols)
    FaoDiscrep_ra[[j]] <- x
    x <- as.data.frame(do.call(cbind, YieldModOut))
    gathercols <- variety_vec
    x <- gather_(x, "Variety", "Yield", gathercols)
    YieldMod_ra[[j]] <- x
    rm(x)
}
FaoDiscrep_ra <- as.data.frame(do.call(rbind, FaoDiscrep_ra))
YieldMod_ra <- as.data.frame(do.call(rbind, YieldMod_ra))
#-----------------------------
#Irrigated
FaoDiscrep_ir <- list(); YieldMod_ir <- list()
for(j in 1:n_crops)
{
    wd_str <- paste0("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/", cropvec[j], "_IRR/",cropvec[j],"_Historical/")
    setwd(wd_str)
    Yield_fao <- subset(Yield_fao_raw, Item == cropvec[j])
    variety_vec <- variety_vec_list[[j]]
    YieldModOut <- list(); DiscrepOut <- list();
    for(i in 1:length(variety_vec))
    {
        Yield_mod_raw <- read.csv(paste0("IRRI_", variety_vec[i], ".csv"))
        Yield_mod_raw[,1] <- NULL
        colnames(Yield_mod_raw)[1] <- "FPU"
        Yield_mod_raw <- merge(Yield_mod_raw,fpu_convert,by = "FPU")
        Yield_mod <- Yield_mod_raw[,c("Country","FPU","Rend_fpu_2022")]
        Yield_mod$FPU <- NULL
        Yield_mod <- Yield_mod %>% group_by(Country) %>% summarise(mean(Rend_fpu_2022))
        colnames(Yield_mod)[2] <- variety_vec[i]
        Yield_fao_mod <- merge(Yield_fao, Yield_mod, by = "Country")
        Yield_fao_mod$discrep <- (Yield_fao_mod[, 4] - Yield_fao_mod$`FAO yield`) / Yield_fao_mod$`FAO yield` * 100
        Discrep_fao_mod <- Yield_fao_mod[,c("Country", "discrep")]
        colnames(Discrep_fao_mod)[2] <- variety_vec[i]
        if(i > 1){Yield_mod$Country <- NULL; Discrep_fao_mod$Country <- NULL}
        if(i == 1){Yield_mod$Crop <- rep(cropvec[j], nrow(Yield_mod)); Discrep_fao_mod$Crop <- rep(cropvec[j], nrow(Discrep_fao_mod))}
        DiscrepOut[[i]] <- Discrep_fao_mod
        YieldModOut[[i]] <- Yield_mod
    }
    x <- as.data.frame(do.call(cbind, DiscrepOut))
    gathercols <- variety_vec
    x <- gather_(x, "Variety", "Discrepancy", gathercols)
    FaoDiscrep_ir[[j]] <- x
    x <- as.data.frame(do.call(cbind, YieldModOut))
    gathercols <- variety_vec
    x <- gather_(x, "Variety", "Yield", gathercols)
    YieldMod_ir[[j]] <- x
    rm(x)
}
FaoDiscrep_ir <- as.data.frame(do.call(rbind, FaoDiscrep_ir))
YieldMod_ir <- as.data.frame(do.call(rbind, YieldMod_ir))
#-------
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Reality check/")
gg <- ggplot(FaoDiscrep_ra, aes(x = Discrepancy, color = Variety)) + geom_density() + xlim(-500, 500) + geom_vline(aes(xintercept = 0))
gg <- gg + facet_wrap(~ Crop, ncol = 2, nrow = 2) + xlab("% Divergence from FAO") + ggtitle("FAO Discrepancy, rainfed")
gg
ggsave("FAO Discrepancy RA.tiff",width = 6, height = 5)
#graph_discrep_ra <- gg
#--
gg <- ggplot(YieldMod_ra, aes(x = Yield, color = Variety)) + geom_density()
gg <- gg + facet_wrap(~ Crop, ncol = 2, nrow = 2) + xlab("Yield (kg. / ha.)") + ggtitle("Modeled yield, rainfed")
gg
ggsave("Baseline Yield RA.tiff",width = 6, height = 5)
graph_yield_ra <- gg
#-------
gg <- ggplot(FaoDiscrep_ir, aes(x = Discrepancy, color = Variety)) + geom_density() + xlim(-500, 500) + geom_vline(aes(xintercept = 0))
gg <- gg + facet_wrap(~ Crop, ncol= 2, nrow = 2) + xlab("% Divergence from FAO") + ggtitle("FAO Discrepancy, irrigated")
gg
ggsave("FAO Discrepancy IR.tiff",width = 6, height = 5)
#graph_discrep_ir <- gg
#--
gg <- ggplot(YieldMod_ir, aes(x = Yield, color = Variety)) + geom_density()
gg <- gg + facet_wrap(~ Crop, ncol = 2, nrow = 2) + xlab("Yield (kg. / ha.)") + ggtitle("Modeled yield, irrigated")
gg
ggsave("Baseline Yield IR.tiff",width = 6, height = 5)
#graph_yield_ir <- gg
#------------------------------

options(warn = -1); options(scipen = 999)
library(reshape)
library(tidyr)
library(dplyr)
library(ggplot2)
#Get FPU-country matching file
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")
fpu_convert <- read.csv("fpudata.csv")
fpu_convert <- fpu_convert[,c("New_FPU","Region_Nam")]
colnames(fpu_convert) <- c("FPU", "Country")
#Get FAO yields
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")
Yield_fao_raw <- read.csv("FAO yields beans maize wheat LAC.csv")
Yield_fao_raw <- Yield_fao_raw[, c("Area", "Item", "Year", "Value")]
Yield_fao_raw$Value <- Yield_fao_raw$Value / 10
Yield_fao_raw <- subset(Yield_fao_raw, Year %in% c(2010:2014))
Yield_fao_raw <- Yield_fao_raw %>% group_by(Area,Item) %>% summarise(mean(Value))
colnames(Yield_fao_raw)[1] <- "Country"
colnames(Yield_fao_raw)[3] <- "FAO yield"
Yield_fao_raw$Country <- as.character(Yield_fao_raw$Country)
Yield_fao_raw$Item <- as.character(Yield_fao_raw$Item)
u <- Yield_fao_raw$Country
Yield_fao_raw$Country[grep("Bolivia", u)] <- "Bolivia"
Yield_fao_raw$Country[grep("Venezuela", u)] <- "Venezuela"
rm(u)
u <- Yield_fao_raw$Item
Yield_fao_raw$Item[grep("Beans", u)] <- "Bean"
rm(u)
#------------------------------
#
cropvec <- c("Wheat", "Maize", "Bean"); n_crops <- length(cropvec)
variety_vec_Wheat <- c("TajanBA_WFD_FPU", "Seri82BA_WFD_FPU", "HalconsnaBA_WFD_FPU", "Gerek79BA_WFD_FPU", "DonErnestoBA_WFD_FPU", "BrigadierBA_WFD_FPU")
variety_vec_Maize <- c("FM6_WFD_FPU", "H6_WFD_FPU", "MCCURDY6714_WFD_FPU")
variety_vec_Bean <- c("A193_WFD_FPU", "BAT881_WFD_FPU", "BRSRadiante_WFD_FPU", "Carioca_WFD_FPU", "ICTAOstua_WFD_FPU", "Manitou_WFD_FPU", "Perola_WFD_FPU")
variety_vec_list <- list(variety_vec_Wheat, variety_vec_Maize, variety_vec_Bean)
#------------------------------
#Get modeled yields
#Rainfed
FaoDiscrep_ra <- list(); YieldMod_ra <- list()
for(j in 1:n_crops)
{
    wd_str <- paste0("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/", cropvec[j], "_RA/",cropvec[j],"_Historical/")
    setwd(wd_str)
    Yield_fao <- subset(Yield_fao_raw, Item == cropvec[j])
    variety_vec <- variety_vec_list[[j]]
    YieldModOut <- list(); DiscrepOut <- list();
    for(i in 1:length(variety_vec))
    {
        Yield_mod_raw <- read.csv(paste0("RA_", variety_vec[i], ".csv"))
        Yield_mod_raw[,1] <- NULL
        colnames(Yield_mod_raw)[1] <- "FPU"
        Yield_mod_raw <- merge(Yield_mod_raw,fpu_convert,by = "FPU")
        Yield_mod <- Yield_mod_raw[,c("Country","FPU","Rend_fpu_2022")]
        Yield_mod$FPU <- NULL
        Yield_mod <- Yield_mod %>% group_by(Country) %>% summarise(mean(Rend_fpu_2022))
        colnames(Yield_mod)[2] <- variety_vec[i]
        Yield_fao_mod <- merge(Yield_fao, Yield_mod, by = "Country")
        Yield_fao_mod$discrep <- (Yield_fao_mod[, 4] - Yield_fao_mod$`FAO yield`) / Yield_fao_mod$`FAO yield` * 100
        Discrep_fao_mod <- Yield_fao_mod[,c("Country", "discrep")]
        colnames(Discrep_fao_mod)[2] <- variety_vec[i]
        if(i > 1){Yield_mod$Country <- NULL; Discrep_fao_mod$Country <- NULL}
        if(i == 1){Yield_mod$Crop <- rep(cropvec[j], nrow(Yield_mod)); Discrep_fao_mod$Crop <- rep(cropvec[j], nrow(Discrep_fao_mod))}
        DiscrepOut[[i]] <- Discrep_fao_mod
        YieldModOut[[i]] <- Yield_mod
    }
    x <- as.data.frame(do.call(cbind, DiscrepOut))
    gathercols <- variety_vec
    x <- gather_(x, "Variety", "Discrepancy", gathercols)
    FaoDiscrep_ra[[j]] <- x
    x <- as.data.frame(do.call(cbind, YieldModOut))
    gathercols <- variety_vec
    x <- gather_(x, "Variety", "Yield", gathercols)
    YieldMod_ra[[j]] <- x
    rm(x)
}
FaoDiscrep_ra <- as.data.frame(do.call(rbind, FaoDiscrep_ra))
YieldMod_ra <- as.data.frame(do.call(rbind, YieldMod_ra))
#-----------------------------
#Irrigated
FaoDiscrep_ir <- list(); YieldMod_ir <- list()
for(j in 1:n_crops)
{
    wd_str <- paste0("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/", cropvec[j], "_IRR/",cropvec[j],"_Historical/")
    setwd(wd_str)
    Yield_fao <- subset(Yield_fao_raw, Item == cropvec[j])
    variety_vec <- variety_vec_list[[j]]
    YieldModOut <- list(); DiscrepOut <- list();
    for(i in 1:length(variety_vec))
    {
        Yield_mod_raw <- read.csv(paste0("IRRI_", variety_vec[i], ".csv"))
        Yield_mod_raw[,1] <- NULL
        colnames(Yield_mod_raw)[1] <- "FPU"
        Yield_mod_raw <- merge(Yield_mod_raw,fpu_convert,by = "FPU")
        Yield_mod <- Yield_mod_raw[,c("Country","FPU","Rend_fpu_2022")]
        Yield_mod$FPU <- NULL
        Yield_mod <- Yield_mod %>% group_by(Country) %>% summarise(mean(Rend_fpu_2022))
        colnames(Yield_mod)[2] <- variety_vec[i]
        Yield_fao_mod <- merge(Yield_fao, Yield_mod, by = "Country")
        Yield_fao_mod$discrep <- (Yield_fao_mod[, 4] - Yield_fao_mod$`FAO yield`) / Yield_fao_mod$`FAO yield` * 100
        Discrep_fao_mod <- Yield_fao_mod[,c("Country", "discrep")]
        colnames(Discrep_fao_mod)[2] <- variety_vec[i]
        if(i > 1){Yield_mod$Country <- NULL; Discrep_fao_mod$Country <- NULL}
        if(i == 1){Yield_mod$Crop <- rep(cropvec[j], nrow(Yield_mod)); Discrep_fao_mod$Crop <- rep(cropvec[j], nrow(Discrep_fao_mod))}
        DiscrepOut[[i]] <- Discrep_fao_mod
        YieldModOut[[i]] <- Yield_mod
    }
    x <- as.data.frame(do.call(cbind, DiscrepOut))
    gathercols <- variety_vec
    x <- gather_(x, "Variety", "Discrepancy", gathercols)
    FaoDiscrep_ir[[j]] <- x
    x <- as.data.frame(do.call(cbind, YieldModOut))
    gathercols <- variety_vec
    x <- gather_(x, "Variety", "Yield", gathercols)
    YieldMod_ir[[j]] <- x
    rm(x)
}
FaoDiscrep_ir <- as.data.frame(do.call(rbind, FaoDiscrep_ir))
YieldMod_ir <- as.data.frame(do.call(rbind, YieldMod_ir))
#-------
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Reality check/")
gg <- ggplot(FaoDiscrep_ra, aes(x = Discrepancy, color = Variety)) + geom_density() + xlim(-500, 500) + geom_vline(aes(xintercept = 0))
gg <- gg + facet_wrap(~ Crop, ncol = 2, nrow = 2) + xlab("% Divergence from FAO") + ggtitle("FAO Discrepancy, rainfed")
gg
ggsave("FAO Discrepancy RA.tiff",width = 6, height = 5)
#graph_discrep_ra <- gg
#--
gg <- ggplot(YieldMod_ra, aes(x = Yield, color = Variety)) + geom_density()
gg <- gg + facet_wrap(~ Crop, ncol = 2, nrow = 2) + xlab("Yield (kg. / ha.)") + ggtitle("Modeled yield, rainfed")
gg
ggsave("Baseline Yield RA.tiff",width = 6, height = 5)
graph_yield_ra <- gg
#-------
gg <- ggplot(FaoDiscrep_ir, aes(x = Discrepancy, color = Variety)) + geom_density() + xlim(-500, 500) + geom_vline(aes(xintercept = 0))
gg <- gg + facet_wrap(~ Crop, ncol= 2, nrow = 2) + xlab("% Divergence from FAO") + ggtitle("FAO Discrepancy, irrigated")
gg
ggsave("FAO Discrepancy IR.tiff",width = 6, height = 5)
#graph_discrep_ir <- gg
#--
gg <- ggplot(YieldMod_ir, aes(x = Yield, color = Variety)) + geom_density()
gg <- gg + facet_wrap(~ Crop, ncol = 2, nrow = 2) + xlab("Yield (kg. / ha.)") + ggtitle("Modeled yield, irrigated")
gg
ggsave("Baseline Yield IR.tiff",width = 6, height = 5)
#graph_yield_ir <- gg
#------------------------------

