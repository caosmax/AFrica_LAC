##code to download the specific data needed data from comtrade and then save it as a dta (stata) file

##part 1: function to download data
#script to download comtrade data
#reference: http://comtrade.un.org/data/Doc/api/ex/r


library(rjson)
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))


#function

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
      string<- paste(url
                     ,"max=",maxrec,"&" #maximum no. of records returned
                     ,"type=",type,"&" #type of trade (c=commodities)
                     ,"freq=",freq,"&" #frequency
                     ,"px=",px,"&" #classification
                     ,"ps=",ps,"&" #time period
                     ,"r=",r,"&" #reporting area
                     ,"p=",p,"&" #partner country
                     ,"rg=",rg,"&" #trade flow
                     ,"cc=",cc,"&" #classification code
                     ,"fmt=",fmt        #Format
                     ,sep = ""
      )
      
      if(fmt == "csv") {
            raw.data<- read.csv(string,header=TRUE)
            return(list(validation=NULL, data=raw.data))
      } else {
            if(fmt == "json" ) {
                  raw.data<- fromJSON(file=string)
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

##part 2: donwload specific data
#information:

#hs codes:
#Rice: 1006, 100610, 100620, 100630, 100640
#Wheat: 1001, 100190, 100110, 110100
#Soy: 1201, 120100, 120110, 120190
#Maize: 1005, 100510, 100590


#country codes:
#Argentina: 32
#China: 156
#India: 699
#Indonesia: 360
#Kazakhstan: 398
#Russian Federation: 643
#Ukraine: 804
#Viet nam: 704

#note: we can only download less than five countries and years at a time
#note: i am sure there is a more elegant/efficient way to do this
#note: do it line by line, it seems to crash if i try to do it all at once:
#start downloading:

q1a <- get.Comtrade(r="32,156,699,360", p="all", ps="2014,2013,2012,2011,2010", fmt="csv", cc="1006,100610,100620,100630,100640,1001,100190,100110,110100,1201,120100,120110,120190,1005,100510,100590")
q1b <- get.Comtrade(r="398,643,804,704", p="all", ps="2014,2013,2012,2011,2010", fmt="csv", cc="1006,100610,100620,100630,100640,1001,100190,100110,110100,1201,120100,120110,120190,1005,100510,100590")

q2a <- get.Comtrade(r="32,156,699,360", p="all", ps="2009,2008,2007,2006,2005", fmt="csv", cc="1006,100610,100620,100630,100640,1001,100190,100110,110100,1201,120100,120110,120190,1005,100510,100590")
q2b <- get.Comtrade(r="398,643,804,704", p="all", ps="2009,2008,2007,2006,2005", fmt="csv", cc="1006,100610,100620,100630,100640,1001,100190,100110,110100,1201,120100,120110,120190,1005,100510,100590")

q3a <- get.Comtrade(r="32,156,699,360", p="all", ps="2004,2003,2002,2001,2000", fmt="csv", cc="1006,100610,100620,100630,100640,1001,100190,100110,110100,1201,120100,120110,120190,1005,100510,100590")
q3b <- get.Comtrade(r="398,643,804,704", p="all", ps="2004,2003,2002,2001,2000", fmt="csv", cc="1006,100610,100620,100630,100640,1001,100190,100110,110100,1201,120100,120110,120190,1005,100510,100590")

#create data frame for each of this

dq1a <- as.data.frame(do.call(rbind, q1a))
dq1b <- as.data.frame(do.call(rbind, q1b))

dq2a <- as.data.frame(do.call(rbind, q2a))
dq2b <- as.data.frame(do.call(rbind, q2b))

dq3a <- as.data.frame(do.call(rbind, q3a))
dq3b <- as.data.frame(do.call(rbind, q3b))


#part 3: export data
#append all data frames

append=do.call(rbind, list(dq1a, dq1b, dq2a, dq2b, dq3a,dq3b))

##part 3: export files files

#csv file
write.table(append, file = "~/Dropbox/OECD/GG/R/data_all.csv", ,sep=",",row.names=F)

#dta file
library(foreign)
write.dta(append, "~/Dropbox/OECD/GG/R/data_all.dta")

write.dta(append, "~/Dropbox/OECD/GG/R/data_all_b.dta", convert.factors = "string")