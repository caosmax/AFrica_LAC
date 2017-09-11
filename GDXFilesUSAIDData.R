# Tratamiento y visualizacion de USAID Data



# Directorios-------- 
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/GDXfiles/")
# Copiar datos y graficos
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/Graphs/")


# # To find general information of files gdx
info <- gdxInfo(gdxName = "YldXAgg.gdx",dump = F,returnDF = T,returnList =F )
gdxInfo()
# gdxInfo("YldXAgg.gdx",dump=FALSE, returnDF=TRUE)
info[c("sets", "parameters")]
# Standing out gdx files
gdxfiles<- list.files()


# intento 3 # para obtener parametros
yieldgdx<- "YldXAgg.gdx"
yield<- rgdx.param(yieldgdx,"YldXAgg",squeeze = F, compress = T)
y<- rgdx.set(gdxName ="YldXAgg.gdx", symName = "i",ts = T)
yield<- rgdx(gdxName = "YldXAgg.gdx",squeeze = T,useDomInfo = F,followAlias = TRUE)

igdx()

# intento 3 # para obtener parametros
cccdeltaScrip<- rgdx.param(gdxName = "IMPACT3_CCData.gdx",symName ="CCDelta"  )

# # To find general information of files gdx
# info <- gdxInfo("YldXAgg.gdx", dump = T, returnDF = T)
# gdxInfo()
# gdxInfo("YldXAgg.gdx",dump=FALSE, returnDF=TRUE)
# info[c("sets", "parameters")]