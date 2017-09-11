
########################################################### INICIO ##########################################################################
## Autores: Harold A. & Carlos Edo


gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

variedadesWheat<- c("BrigadierBA","DonErnestoBA","Gerek79BA","HalconsnaBA" ,"Seri82BA","TajanBA")
variedadesBean<- c("A193", "BAT881","BRSRadiante","Carioca", "ICTAOstua", "Manitou", "Perola")
variedadesRice<- c("IR8", "IR64","IR72")
variedadesMaize<- c("H6","FM6","MCCURDY6714")
variedadesSoybean<- c("DONMARIO", "Hutcheson")

sys_q<- c("IRR", "RA")
crops<- c( "Rice","Wheat", "Bean", "Soybean","Maize")
Time<- c( "Future", "Historical")


######################################################## Codes Github #######################################################
# Para replicar los codigos aca presentados se debe acceder al github re-running 
setwd("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts")


#_______________________CROP MODEL _______________________

#Parte A. Matriz cultivo + rendimientos modelados--------------------------------------------------
# unimos las coordenadas y los resultados de la modelacion por variedad y GCM
#### Source for each crop, treating individual
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelBean.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelWheat.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelRice.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelMaize.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelSoybean.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelWFD.R")

#Parte B. Seleccion de variedades  por rendimientos max a nivel de  pixel  ----------------------------
### "codigo para calcular los rendimientos maximos por variedad por cada pixel/gcm"
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/SelectYieldByVarietiesAtPixel.R")


#Parte C. Agregación de rendimientos a nivel de FPU, intervencion para Arroz en Chile/Ecuador  --------
### "codigo para calcular los rendimientos maximos por variedad por cada pixel/gcm"
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/AgreggationVersionCarlosRice.R")


#Parte D. Calculo de las tasas de crecimiento -------------------------------------------------------
#Este codigo toma los rendimientos ponderados GCM e WFD y calcula la tasa de crecimiento a nivel de FPU
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/CalculoTasasCrecimientoAnualizadas.R")


#Parte E. transfiere los archivos procesados en R a GAMS para ser incluidos en el modelo IMPACT ------- 
#Presentacion del file en el formato requerido por CCProcessing.gms
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/FormatoLongAppropiateIMPACTRice.R")


#_______________________IMPACT MODEL _______________________
#Parte F.---------------------------------------------------------------------------------------
#Procesamiento de los resultados de IMPACT para presentar reportes y graficos.
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ResultsRegionsBIDPhase2BIDV2.R")

#Parte G.---------------------------------------------------------------------------------------
#Procesamiento de los resultados de IMPACT para presentar reportes y graficos.
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/RasterByChapterBID.R")


#Parte F.---------------------------------------------------------------------------------------
#Este codigo permite graficar los raster tanto del reporte final como del capitulo del libro
#Dentro de el se halla un codigo para calcular los rendimientos promedios entre variedades.
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/SpatialAnalysisResultsBID.R")



#_____________________FOLDERS_____________________________________
# Version 2 o "V2" esta relacionada con los resultados elegidos para presentar
# Phase 2 esta relacionado con los resultados actuales de la modelacion de cultivo e impact
# Resultados generados por impact 
("\\dapadfs\workspace_cluster_6\Socioeconomia\GF_and_SF\BID_2\ResultsIMPACT\AggregationBID\Phase2")

# Folder donde se guardan los archivos para los reportes
("\\dapadfs\workspace_cluster_6\Socioeconomia\GF_and_SF\BID_2\ResultsIMPACT\AggregationBID\Phase2\Test\Tablas&GraficasReporte")

# Tasas de crecimiento diferentes versiones "LongFormat_v2"
("\\dapadfs\workspace_cluster_6\Socioeconomia\GF_and_SF\BID_2\tc_copy")



######################################################## Codigos de pruebas y analisis  ##########################################

#     1 Graficas por Pixel
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/GraphsAtPixelLevel.R")
#     2.Deteccion de outliers and extremes values, definicion de status de los rendimientos a nivel de Pixel 
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/Outliers.R")

#     3.review FPUs aggregation tendencia entre WFD/GCMS, agregaciones de cultivos por FPU, para revisiones mas detalladas
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/AgreggationProofByFPU.R")

#     4.review FPUs aggregation tendencia entre WFD/GCMS
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/comparisonInitiall_UpdatedResultsIMPACTV2.R")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



