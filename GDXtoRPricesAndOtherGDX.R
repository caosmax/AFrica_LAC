# Procesamiento y analisis de archivos GDX from GAMS

library(reshape2)
library(gdxrrw)

setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBIDPhase2/")
#sessionInfo()

#read the data created in GAMS
GAMSdata<- gdxInfo("V2_PriceW_allRegions.gdx")
gdxInfo(gdxName = "compressTest.gdx")

#show some information of the data
head(GAMSdata);
str(GAMSdata);








# Script--------------------------------------

# con esto obtengo la informacion sobre el gdx, leyendo metadata
info <- gdxInfo("Phase2/V2_PriceW_allRegions.gdx", dump = F, returnDF = T)
info[c("sets", "parameters")]
info <- gdxInfo("YldXAgg.gdx", dump = F, returnDF = T)
info[c("sets", "parameters")]

gdxInfo(gdxName = "Phase2/V2_PriceW_BIDRegions.gdx")

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



