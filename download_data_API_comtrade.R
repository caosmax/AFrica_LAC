# Using the UN Comtrade data API with R
# Source: https://comtrade.un.org/data/Doc/api/ex/r
# Implemented by: H. Achicanoy, 2016

# R options
options(warn = -1)
options(scipen = 999)

# Extracting codes and descriptions of reporters from the UN Comtrade API
suppressMessages(library(rjson))
suppressMessages(library(dplyr))
suppressMessages(library(plyr))
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters2 <- rjson::fromJSON(file = string)
reporters2 <- as.data.frame(t(sapply(reporters2$results,rbind)))
reporters2 <- reporters2[-1,]
reporters <- data.frame(Code = unlist(reporters2$V1), Country = unlist(reporters2$V2)); rm(reporters2)
reporters$Code <- as.numeric(as.character(reporters$Code))
reporters$Country <- as.character(reporters$Country)

# -------------------------------------------------------------------------- #
# A user-defined function to extract data from the UN Comtrade API
# -------------------------------------------------------------------------- #

get.Comtrade <- function(url = "http://comtrade.un.org/api/get?", maxrec = 50000, type = "C", freq = "A",
                         px = "HS", ps = "now", r, p, rg = "all", cc = "TOTAL", fmt = "json"){
  string<- paste(url
                 ,"max=",maxrec,"&" # maximum no. of records returned
                 ,"type=",type,"&"  # type of trade (c=commodities)
                 ,"freq=",freq,"&"  # frequency
                 ,"px=",px,"&"      # classification
                 ,"ps=",ps,"&"      # time period
                 ,"r=",r,"&"        # reporting area
                 ,"p=",p,"&"        # partner country
                 ,"rg=",rg,"&"      # trade flow (All = "all", imports = 1, exports = 2, re-export = 3)
                 ,"cc=",cc,"&"      # classification code
                 ,"fmt=",fmt        # Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- rjson::fromJSON(file = string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

# Example 1: Using default parameters (i.e., latest annual HS total trade data flows, in json format),
# specifying only reporting and partner areas

s1 <- get.Comtrade(r="842", p="124,484")
s1$data

# Example 2: Same as Example 1 above, but in csv format. Note that in this case, there is no validation
# information included in the data output.

s2 <- get.Comtrade(r="842", p="124,484", fmt="csv")
s2

# Example 3: An example to extract monthly data
s3 <- get.Comtrade(r="842", p="0", ps="201201,201202,201203", freq="M")
s3

# Example 4: My personal tests
s4 <- get.Comtrade(r = "566", p = "124,484", cc = "11081400", ps = "2014", px = "HS", rg = 2, fmt = "csv")
View(s4[[2]])

# -------------------------------------------------------------------------- #
# Downloading all possible data from COMTRADE database
# -------------------------------------------------------------------------- #

# List of years
yearList <- 1962:2015

# Complete matrix all possible combinations
all_comb <- as.data.frame(expand.grid(Reporter = reporters$Country, Partner = reporters$Country))
all <- data.frame(Reporter = unlist(all_comb$Reporter), Partner = unlist(all_comb$Partner)); rm(all_comb)
all$Reporter <- as.character(all$Reporter)
all$Partner <- as.character(all$Partner)

aux <- inner_join(x = all, y = reporters, by = c('Reporter' = 'Country'))
all$Reporter_code <- aux[,ncol(aux)]

aux <- inner_join(x = all, y = reporters, by = c('Partner' = 'Country'))
all$Partner_code <- aux[,ncol(aux)]; rm(aux)

# List of commodities
commList <- c(110811, 110812, 110813, 110814, 110819, 110820)

inList <- list(combinations = all,
               commodities  = commList,
               years        = yearList)
saveRDS(inList, './inputList.rds')
inList <- readRDS("./inputList.rds")

i = 261 # Thailand to World
j = 1 # Wheat, starch
k = 51 # 2012

rbindMat <- lapply(1:nrow(inList$combinations), function(i){
  
  rbindMat <- lapply(1:length(inList$commodities), function(j){
    
    rbindMat <- lapply(1:length(inList$years), function(k){
      
      tryCatch(expr={
        s <- get.Comtrade(r = inList$combinations$Reporter_code[i],
                          p = inList$combinations$Partner_code[i],
                          cc = paste(inList$commodities[j]),
                          ps = paste(inList$years[k]),
                          px = "HS", rg = 2, fmt = "csv")
      },
      error=function(e){
        cat(paste0("Downloading failed in combination, Reporter:", inList$combinations$Reporter[i],
                   ", Partner:", inList$combinations$Partner[i], ", Commodity:", inList$commodities[j],
                   " and Year:", inList$years[k], "\n"))
        return("Done\n")
      })
      
      if(exists('s')){
        s <- as.data.frame(do.call(bind_rows, s))
        s$rr <- inList$combinations$Reporter[i]
        s$pp <- inList$combinations$Partner[i]
        s$cc <- inList$commodities[j]
        s$yy <- inList$years[k]
        return(s)
      }
      
    })
    
    rbindMat <- do.call(bind_rows, rbindMat)
    return(rbindMat)
    
  })
  
  rbindMat <- do.call(bind_rows, rbindMat)
  return(rbindMat)
  
})
rbindMat <- do.call(bind_rows, rbindMat)

# Just only to be run on Windows servers
# rbindMat <- lapply(1:nrow(inList$combinations), function(i){
#   
#   rbindMat <- lapply(1:length(inList$commodities), function(j){
#     
#     rbindMat <- lapply(1:length(inList$years), function(k){
#       
#       tryCatch(expr={
#         s <- get.Comtrade(r = inList$combinations$Reporter_code[i],
#                           p = inList$combinations$Partner_code[i],
#                           cc = paste(inList$commodities[j]),
#                           ps = paste(inList$years[k]),
#                           px = "HS", rg = 2, fmt = "csv")
#       },
#       error=function(e){
#         cat(paste0("Downloading failed in combination, Reporter:", inList$combinations$Reporter[i],
#                    ", Partner:", inList$combinations$Partner[i], ", Commodity:", inList$commodities[j],
#                    " and Year:", inList$years[k], "\n"))
#         return("Done\n")
#       })
#       
#       if(exists('s')){
#         s <- as.data.frame(do.call(rbind.fill, s))
#         s$rr <- inList$combinations$Reporter[i]
#         s$pp <- inList$combinations$Partner[i]
#         s$cc <- inList$commodities[j]
#         s$yy <- inList$years[k]
#         return(s)
#       }
#       
#     })
#     
#     rbindMat <- do.call(rbind.fill, rbindMat)
#     return(rbindMat)
#     
#   })
#   
#   rbindMat <- do.call(rbind.fill, rbindMat)
#   return(rbindMat)
#   
# })
# rbindMat <- do.call(rbind.fill, rbindMat)
