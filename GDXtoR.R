# Procesamiento y analisis de archivos GDX from GAMS

library(reshape2)
library(gdxrrw)
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID")
grd<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase2/")

# Script--------------------------------------

# con esto obtengo la informacion sobre el gdx, leyendo metadata
info <- gdxInfo(paste(grd."V2_PriceW_allRegions.gdx", sep=""), dump = F, returnDF = T)
info[c("sets", "parameters")]
info <- gdxInfo("YldXAgg.gdx", dump = F, returnDF = T)
info[c("sets", "parameters")]

# # intento 1
# data1<- rgdx(gdxName = "IMPACT3_CCData.gdx",requestList =list(name="CCDelta"))
# val_data<- as.data.frame(data1$val)
# uels <- data1$uels 
# 
# # intento 2 # este codigo traer variebles set en un data frame
# gcm<- rgdx.set(gdxName ="IMPACT3_CCData.gdx", symName ="gcm")
# rgdx.set(gdxName ="IMPACT3_CCData.gdx", symName = "gcm" )

# intento 3 # para obtener parametros
cccdeltaScrip<- rgdx.param(gdxName = "IMPACT3_CCData.gdx",symName ="CCDelta"  )

rm(data1, gcm, info, uels, val_data)


# IMPACT model------------------
setwd("C:/Workspace/IMPACT3-Model-ver3.2/InputFiles/IMPACT3")

# con esto obtengo la informacion sobre el gdx, leyendo metadata
info <- gdxInfo("IMPACT3_CCData.gdx", dump = F, returnDF = T)
info[c("sets", "parameters")]

# # intento 1
# data1<- rgdx(gdxName = "IMPACT3_CCData.gdx",requestList =list(name="CCDelta"))
# val_data<- as.data.frame(data1$val)
# uels <- data1$uels 
# 
# # intento 2 # este codigo traer variebles set en un data frame
# gmc<- rgdx.set(gdxName ="IMPACT3_CCData.gdx", symName ="gcm")
# rgdx.set(gdxName ="IMPACT3_CCData.gdx", symName = "gcm" )

# intento 3 # para obtener parametros
cccdeltaModel<- rgdx.param(gdxName = "IMPACT3_CCData.gdx",symName ="CCDelta"  )


# comparar dos dataframes
identical(cccdeltaScrip, cccdeltaModel)



