# Cassava dried export flows - Preparing for CIAT-blog
# Author: H. Achicanoy, C. Gonzalez
# CIAT, 2016

# R options
options(warn = -1)
options(scipen = 999)

# Load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(networkD3))
suppressMessages(library(jsonlite))
suppressMessages(library(circlize))

# Define directory and files to read
setwd('C:/Users/CEGONZALEZ/Documents/cassava/copyData')
db_dir <- './periods_median/' # Using median as summary measure
db_periods <- list.files(path = db_dir, pattern = '.csv$', full.names = T)
periodList <- c('1986-1992', '1993-1999', '2000-2006', '2007-2013')

# Load datasets correctly
db_time <- lapply(1:length(db_periods), function(i){
  z <- read.csv(db_periods[i])
  z$X <- NULL
  colnames(z) <- c('Reporter', 'Partner', 'mean') # Median
  z$Period <- periodList[i]
  return(z)
})
db_time <- do.call(rbind, db_time)

# Rename and filter countries
db_time$Reporter <- as.character(db_time$Reporter)
db_time$Partner <- as.character(db_time$Partner)

db_time$Reporter <- gsub(pattern = '* \\((.*?)\\)', replacement = '', x = db_time$Reporter)
db_time$Reporter[grep(pattern = "CÃ´te d'Ivoire", x = db_time$Reporter, fixed = TRUE)] <- 'Ivory Coast'
# db_time$Reporter[grep(pattern = "Côte d'Ivoire", x = db_time$Reporter, fixed = TRUE)] <- 'Ivory Coast'
db_time$Reporter[grep(pattern = "China, Hong Kong SAR", x = db_time$Reporter, fixed = TRUE)] <- 'Hong Kong'
db_time$Reporter[grep(pattern = "China, Macao SAR", x = db_time$Reporter, fixed = TRUE)] <- 'Macao'
db_time$Reporter[grep(pattern = "China, mainland", x = db_time$Reporter, fixed = TRUE)] <- 'China'
db_time$Reporter[grep(pattern = "China, Taiwan Province of", x = db_time$Reporter, fixed = TRUE)] <- 'Taiwan'
db_time$Reporter[grep(pattern = "RÃ©union", x = db_time$Reporter, fixed = TRUE)] <- 'Reunion'
# db_time <- db_time[-which(db_time$Reporter=="Belgium-Luxembourg"),]

db_time$Partner <- gsub(pattern = '* \\((.*?)\\)', replacement = '', x = db_time$Partner)
db_time$Partner[grep(pattern = "CÃ´te d'Ivoire", x = db_time$Partner, fixed = TRUE)] <- 'Ivory Coast'
# db_time$Partner[grep(pattern = "Côte d'Ivoire", x = db_time$Partner, fixed = TRUE)] <- 'Ivory Coast'
db_time$Partner[grep(pattern = "China, Hong Kong SAR", x = db_time$Partner, fixed = TRUE)] <- 'Hong Kong'
db_time$Partner[grep(pattern = "China, Macao SAR", x = db_time$Partner, fixed = TRUE)] <- 'Macao'
db_time$Partner[grep(pattern = "China, mainland", x = db_time$Partner, fixed = TRUE)] <- 'China'
db_time$Partner[grep(pattern = "China, Taiwan Province of", x = db_time$Partner, fixed = TRUE)] <- 'Taiwan'
db_time$Partner[grep(pattern = "RÃ©union", x = db_time$Partner, fixed = TRUE)] <- 'Reunion'
db_time <- db_time[-which(db_time$Partner=="Belgium-Luxembourg"),]
# db_time <- db_time[-which(db_time$Partner=="Unspecified"),]

db_time$Reporter <- paste(db_time$Reporter, ' >', sep = '')
db_time$Partner <- paste('> ', db_time$Partner, sep = '')

# Only use countries with some amount of exports
real_exports <- db_time[db_time$mean > 0,]; rownames(real_exports) <- 1:nrow(real_exports)
# real_exports$Mean <- exp(real_exports$Mean)

countryList <- sort(unique(c(as.character(real_exports$Reporter), as.character(real_exports$Partner))))
nodes <- data.frame(name = countryList, node = (1:length(countryList)) - 1); rm(countryList)

# -------------------------------------------------------------------------- #
# Interactive sankey graphs for each period
# -------------------------------------------------------------------------- #
lapply(1:length(periodList), function(j){
  
  # filter data by period
  flows <- real_exports[real_exports$Period==periodList[j],]
  flows$Period <- NULL
  
  # sort by decreasing order
  flows <- flows[order(flows$mean, decreasing = T),]; rownames(flows) <- 1:nrow(flows)
  
  # obtain list of countries and nodes
  virtCountryList <- unique(c(as.character(flows$Reporter), as.character(flows$Partner))) # sort
  virtNodes <- data.frame(name = virtCountryList, node = (1:length(virtCountryList)) - 1); rm(virtCountryList)
  
  # replace text strings by nodes
  for(i in 1:nrow(virtNodes)) {
    flows$Reporter <- gsub(pattern = paste(as.character(virtNodes$name[i]), '$', sep = ''), replacement = virtNodes$node[i], x = flows$Reporter, fixed = F)
    flows$Partner <- gsub(pattern = paste(as.character(virtNodes$name[i]), '$', sep = ''), replacement = virtNodes$node[i], x = flows$Partner, fixed = F)
    # flows$Reporter <- gsub(pattern = paste(as.character(nodes$name[i]), '$', sep = ''), replacement = nodes$node[i], x = flows$Reporter, fixed = F)
    # flows$Partner <- gsub(pattern = paste(as.character(nodes$name[i]), '$', sep = ''), replacement = nodes$node[i], x = flows$Partner, fixed = F)
  }
  
  flows$Reporter <- as.numeric(flows$Reporter)
  flows$Partner <- as.numeric(flows$Partner); rownames(flows) <- 1:nrow(flows)
  # flows <- flows[which(flows$Mean > 0.5),]; rownames(flows) <- 1:nrow(flows) # Omit countries that does not have exports
  # flows$Mean[which(flows$Mean==0.5)] <- 0.0001
  
  # colour links
  flows$gType <- sub(' .*', '', virtNodes[flows$Reporter + 1, 'name'])
  # nodes2 <- nodes[sort(unique(c(flows$Reporter, flows$Partner)))+1,]; rownames(nodes2) <- 1:nrow(nodes2)
  
  sankeyList <- list(links = flows,
                     nodes = virtNodes)
  
  # make and save each plot
  s <- sankeyNetwork(Links = sankeyList$links, Nodes = sankeyList$nodes, Source = "Reporter",
                     Target = "Partner", Value = "mean", NodeID = "name",
                     units = "Ton", LinkGroup = "gType", fontSize = 12, nodeWidth = 30)
  saveNetwork(s, paste("C:/Users/CEGONZALEZ/Documents/cassava/copyData/sankey_", gsub(pattern = "-", replacement = "_", periodList[j]), ".html", sep = ""), selfcontained = T)
  
})

# -------------------------------------------------------------------------- #
# Modifying text in sankeyNetwork
# -------------------------------------------------------------------------- #
# library(networkD3)
# library(data.table)
# set.seed(1999)
# links <- data.table(
#   src = rep(0:4, times=c(1,1,2,3,5)),
#   target = sample(1:11, 12, TRUE),
#   value = sample(100, 12)
# )[src < target, ]  # no loops
# nodes <- data.table(name=LETTERS[1:12])
# 
# ## Need to hover to get counts
# sankeyNetwork(Links=links, Nodes=nodes, Source='src', Target='target',
#               Value='value', NodeID='name', fontSize=16)
# 
# ## Add text to label
# txt <- links[, .(total = sum(value)), by=c('target')]
# nodes[txt$target+1L, name := paste0(name, ' (', txt$total, ')')]
# 
# ## Displays the counts as part of the labels
# sankeyNetwork(Links=links, Nodes=nodes, Source='src', Target='target',
#               Value='value', NodeID='name', fontSize=16, width=600, height=300)
# 
# #################### move leaf node text right ################
# # for this to work
# #   install the newest htmlwidgets
# #   devtools::install_github("ramnathv/htmlwidgets")
# 
# library(htmlwidgets)
# #  add margin left since we'll need extra room
# #   if you are wondering why margin left,
# #   I think we just discovered a bug
# sn <- sankeyNetwork(
#   Links=links, Nodes=nodes, Source='src', Target='target',
#   Value='value', NodeID='name', fontSize=16,
#   width=600, height=300,
#   # give us so room for our newly aligned labels
#   margin = list("left"=100)
# )
# # see how it looks
# sn
# 
# # now let's use the new htmlwidget function
# #  onRender
# onRender(
#   sn,
#   '
#   function(el,x){
#   // select all our node text
#   var node_text = d3.select(el)
#   .selectAll(".node text")
#   //and make them match
#   //https://github.com/christophergandrud/networkD3/blob/master/inst/htmlwidgets/sankeyNetwork.js#L180-L181
#   .attr("x", 6 + x.options.nodeWidth)
#   .attr("text-anchor", "start");
#   }
#   '
# )

# -------------------------------------------------------------------------- #
# Interactive d3/Steven circos of cassava dried exports
# -------------------------------------------------------------------------- #
countryList <- sort(unique(c(as.character(real_exports$Reporter), as.character(real_exports$Partner))))

# Make a transformation over Median in order to show all posible links
# y = ln(x + 10); to return to the original value make: exp(y) - 10
#real_exports$mean <- log(real_exports$mean + 10, base=exp(1))
saveRDS(object = real_exports, file = ('C:/Users/CEGONZALEZ/Documents/cassava/copyData/log_dried_cassava_exports.rds'))
real_exports <- readRDS('C:/Users/CEGONZALEZ/Documents/cassava/copyData//log_dried_cassava_exports.rds')

# All posible flows
flows2json <- lapply(1:length(periodList), function(i){
  
  # Subseting by period and make a square matrix of flows
  subReal <- real_exports %>% dplyr::filter(Period == periodList[i]) %>% as.data.frame()
  subReal <- subReal %>% tidyr::spread(Partner, mean)
  subReal$Period <- NULL
  rownames(subReal) <- subReal$Reporter; subReal$Reporter <- NULL
  
  # Identify countries that don't have exports
  mtch_r <- setdiff(countryList, rownames(subReal))
  mtch_c <- setdiff(countryList, colnames(subReal))
  subReal[nrow(subReal) + 1:length(mtch_r),] <- 0
  subReal[,ncol(subReal) + 1:length(mtch_c)] <- 0
  rownames(subReal)[(nrow(subReal)-length(mtch_r)+1):nrow(subReal)] <- mtch_r
  colnames(subReal)[(ncol(subReal)-length(mtch_c)+1):ncol(subReal)] <- mtch_c
  
  # Arrange by alphabetical order
  subReal[is.na(subReal)] <- 0
  subReal <- subReal[order(rownames(subReal)),]
  subReal <- subReal[,order(colnames(subReal))]
  
  # Formating as Genebank distributions
  subReal2 <- subReal
  rownames(subReal2) <- paste(rownames(subReal2), '_rep', sep='')
  colnames(subReal2) <- rownames(subReal2)
  pos <- seq(1, nrow(subReal)*2, 2)
  pos2 <- seq(2, nrow(subReal)*2, 2)
  subRealFinal <- matrix(data=NA, nrow=nrow(subReal)*2, ncol=nrow(subReal)*2); subRealFinal <- as.data.frame(subRealFinal)
  rownames(subRealFinal)[pos] <- colnames(subRealFinal)[pos] <- rownames(subReal)
  rownames(subRealFinal)[pos2] <- colnames(subRealFinal)[pos2] <- rownames(subReal2)
  subRealFinal[pos, pos] <- subReal
  subRealFinal[pos2, pos2] <- subReal2
  subRealFinal[pos, pos2] <- subReal
  subRealFinal[pos2, pos] <- subReal
  return(subRealFinal)
  
})

### Mucha línea
### csa <- mdwide_tzaCSA[,c("impactparameter", "commodity", paste("X", seq(from=2020, to=2050, by=5), sep = ""), "Cat")] %>% group_by(impactparameter, commodity, Cat) %>% summarise_each(funs(mean))

# {names} for JSON file
mat.labels <- rownames(flows2json[[1]])

# {matrix} for JSON file
matrices <- lapply(flows2json, as.matrix)
names(matrices) <- c('calories','protein','fat','food_weight')

# {regions} for JSON file
regions <- setdiff(1:length(mat.labels),grep(pattern='*_rep$', x=mat.labels)) - 1

# Making JSON file
# Put all elements together in a list, after that apply toJSON function
# Sublist can contain different type of information to show
json.file <- list(names             = mat.labels,
                  labels            = NA,
                  metrics           = NA,
                  regions           = regions,
                  names_description = NA, 
                  matrix            = matrices,
                  help              = NA,
                  description       = NA
)

sink('./_data/_json_files/cassava_dried_exports.json') # redirect console output to a file
toJSON(json.file, pretty=FALSE)
sink()

# -------------------------------------------------------------------------- #
# Static circos of cassava dried exports
# -------------------------------------------------------------------------- #
# Load data
#real_exports <- readRDS('./_data/_cassava_data/periods_median/log_dried_cassava_exports.rds')
real_exports <- readRDS('C:/Users/CEGONZALEZ/Documents/cassava/copyData/log_dried_cassava_exports.rds')

# Making static plots
lapply(1:length(periodList), function(i){
  
  # Subseting by period and make a square matrix of flows
  subReal <- real_exports %>% dplyr::filter(Period == periodList[i]) %>% as.data.frame()
  
  # Initial parameters for static circos
  circos.clear()
  circos.par(start.degree = 90, gap.degree = 1, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
  par(mar = rep(0, 4))
  
  # Save plot
  png(paste('C:/Users/CEGONZALEZ/Documents/cassava/copyData/circos_', periodList[i],'.png', sep = ''), width = 10, height = 10, units = 'in', res = 300)
  
  chordDiagram(x = subReal[subReal$mean > 0.8, ], transparency = 0.25,
               directional = 1,
               direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
               annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
               link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    if(abs(xplot[2] - xplot[1]) < 20) {
      circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                  niceFacing = TRUE, adj = c(0, 0.5))
    } else {
      circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                  niceFacing = TRUE, adj = c(0.5, 0))
    }
  }, bg.border = NA)
  
  dev.off()
  
})

# -------------------------------------------------------------------------- #
# Interactive d3/R chord diagram
# -------------------------------------------------------------------------- #
# Source: http://data-steve.github.io/d3-r-chord-diagram-of-white-house-petitions-data/
# if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("mattflor/chorddiag")
pacman::p_load(dplyr, magrittr, ggplot2, tidyr, curl)

countryList <- sort(unique(c(as.character(real_exports$Reporter), as.character(real_exports$Partner))))

# Making interactive plots
lapply(1:length(periodList), function(i){
  
  # Subseting by period and make a square matrix of flows
  subReal <- real_exports %>% dplyr::filter(Period == periodList[i]) %>% as.data.frame()
  subReal <- subReal %>% tidyr::spread(Partner, mean)
  subReal$Period <- NULL
  rownames(subReal) <- subReal$Reporter; subReal$Reporter <- NULL
  
  # Identify countries that don't have exports
  mtch_r <- setdiff(countryList, rownames(subReal))
  mtch_c <- setdiff(countryList, colnames(subReal))
  subReal[nrow(subReal) + 1:length(mtch_r),] <- 0
  subReal[,ncol(subReal) + 1:length(mtch_c)] <- 0
  rownames(subReal)[(nrow(subReal)-length(mtch_r)+1):nrow(subReal)] <- mtch_r
  colnames(subReal)[(ncol(subReal)-length(mtch_c)+1):ncol(subReal)] <- mtch_c
  
  # Arrange by alphabetical order
  subReal[is.na(subReal)] <- 0
  subReal <- subReal[order(rownames(subReal)),]
  subReal <- subReal[,order(colnames(subReal))]
  cList <- union(x = names(which(rowSums(subReal) != 0)), y = names(which(colSums(subReal) != 0))) # Filtering by values
  subReal <- subReal[cList, cList] # Filtering by values
  ord <- order(rowSums(subReal), decreasing = T)
  subReal <- as.matrix(subReal)
  
  # set number of colors needed
  colorCount <- length(countryList)
  # makes function to create palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
  
  # with the diags means there's a return
  chrd <- chorddiag::chorddiag(subReal[ord, ord], margin = 150, showTicks = FALSE # adjmat[ord, ord]
                               , groupnameFontsize = 11  # have to shrink font for web viewing
                               , groupnamePadding = 5
                               , groupThickness = .05
                               , chordedgeColor = "gray" # getPalette(colorCount)
                               , groupColors = getPalette(colorCount)
                               , showTooltips = FALSE) # Does not show tool box with info
  saveNetwork(chrd, paste("D:/ToBackup/Modelling/global-futures-and-strategic-foresight/_graphics/_interactive_chord_cassava_dried/version_2/chord_", gsub(pattern = "-", replacement = "_", periodList[i]), ".html", sep = ""), selfcontained = T)
  return(cat('Chord diagram done! for:', periodList[i],'\n'))
  
})
