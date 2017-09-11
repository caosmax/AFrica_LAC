
#Para administrar proceso de reviision 
github<- "C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/"
setwd("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts")

#Normal------------
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/AgreggationVersionCarlos.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/CalculoTasasCrecimientoAnualizadas.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/FormatoLongAppropiateIMPACT.R")


#Arroz-------------
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/AgreggationVersionCarlosRice.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/CalculoTasasCrecimientoAnualizadasRice.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/FormatoLongAppropiateIMPACTRice.R")

#SinFallos---------

source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/AgreggationVersionCarlosSinFallos.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/CalculoTasasCrecimientoAnualizadas.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/FormatoLongAppropiateIMPACTSinFallos.R")


#AgregacionFPU------
#Toma los archivos de los rendimientos ponderados y los analiza.
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/AgreggationProofByFPU.R") 


#Analisis de comparacion resultados de IMPACT Model------
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/comparisonInitiall_UpdatedResultsIMPACTV1.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/comparisonInitiall_UpdatedResultsIMPACTV2.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/comparisonInitiall_UpdatedResultsIMPACTV3.R")

#Reportando datos para el analisis factorial multiple--------
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/GenerarReportFactorialAnalysisV1.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/GenerarReportFactorialAnalysisV2.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/GenerarReportFactorialAnalysisV3.R")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  END     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


