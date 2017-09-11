#Por: Carlos Eduardo Gonzalez

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
suppressMessages(library(RColorBrewer))

# manejo de digitos
options(digits=3) 
options(scipen = 999)


#Definition directory work
setwd("C:/Users/CEGONZALEZ/Desktop/IMPACT3-Model-ver3.3/OutputFiles/Aggregation")

# path way to store graphs 
mainDir <- "C:/Users/CEGONZALEZ/Documents/Jamleck_IMPACTClass"
subDir <- "Graphs"

# create folder for graphs 
ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)

# loading data 
cclimate<-read.csv("test.csv",header=T)
ctech<- read.csv("tech.csv",header=T)

# GAMS AND r
library(gdxrrw)

