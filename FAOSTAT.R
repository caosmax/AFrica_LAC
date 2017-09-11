## Para descargar datos de FAOSTAT

install.packages("FAOSTAT")

library(devtools)
install_github(repo = "mkao006/FAOSTATpackage", subdir = "FAOSTAT")
#Vignettes and demos are available and please make use of them:
      
vignette(topic = "FAOSTAT")
demo(topic = "FAOSTATdemo")

library(FAOSTAT)
# Use the interective function  to search the codes
#FAOsearch()
## USe the result of the search to dowloand the data
#test= getFAO(query=. LastSearch)


if(!is.element("FAOSTAT", .packages(all.available = TRUE)))
      install.packages("FAOSTAT")
library(FAOSTAT)

## A demonstration query
FAOquery.df = data.frame(varName = c("arableLand", "cerealExp", "cerealProd"),
                         domainCode = c("RL", "TP", "QC"),
                         itemCode = c(6621, 1944, 1717),
                         elementCode = c(5110, 5922, 5510),
                         stringsAsFactors = FALSE)

## Download the data from FAOSTAT
FAO.lst = with(FAOquery.df,
               getFAOtoSYB(name = varName, domainCode = domainCode,
                           itemCode = itemCode, elementCode = elementCode,
                           useCHMT = TRUE, outputFormat = "wide"))
FAO.lst$entity[, "arableLand"] = as.numeric(FAO.lst$entity[, "arableLand"])



