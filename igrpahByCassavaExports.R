# analisis de redes para yuca
# by carlagangas

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% igraph   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#librerias---
library(igraph)
library(igraphdata)
library(igraphinshiny)

#directorios----
setwd("C:/Users/CEGONZALEZ/Documents/cassava/copyData/periods_median")
gr<- c("C:/Users/CEGONZALEZ/Documents/cassava/copyData/igraph/")



# Cargar bases de datos exportaciones cassava
p1<-read.csv("1986-1992.csv",header = T,stringsAsFactors = F)
p1$X<- NULL
p1<- p1[-which(p1$mean==0.5),]
hist(p1$mean)


# convertir 
p1.net<- graph.data.frame(d = p1,directed = T)
V(p1.net) # prints the list of vertices (countries)
E(p1.net) # print the list of edges (relationships)
degree(p1.net) # print the number of edges per vertex  (relationships by country)
plot(p1.net)


# sub set: if want to exclude people who are in the network only tangentially (participate in one or two relationships only)
# we can exclude the by subsetting the graph on the basis of the 'degree':
#p1.netv<-V(p1.net)[degree(p1.net)>3] #identify those vertices part of less than three edges
#p1.net<-delete.vertices(p1.net, p1.netv) #exclude them from the graph
pdf(file=paste(gr,"periodo1.pdf",sep=""))
plot(p1.net, edge.arrow.size=.2, edge.color="orange", vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(p1.net)$media, vertex.label.color="black")
dev.off()



# loop cassva valor medio
files<- list.files()
files<- lapply(files, read.csv)
for(i in 1:length(files)){
      files[[i]]$X<- NULL
      files[[i]]<- files[[i]][-which(files[[i]]$mean==0.5),]
      files[[i]]<- graph.data.frame(d = files[[i]],directed = T)
      V(files[[i]]) 
      V(files[[i]])$size<-degree(files[[i]])/10 
      E(files[[i]])
      degree(files[[i]])
      pdf(file=paste(gr,files[[i]],"_periodo.pdf",sep=""),width = 12,height = 12,paper = "legal")
      plot(files[[i]], edge.arrow.size=.4, edge.color="orange", vertex.color="orange", vertex.frame.color="#ffffff",
           vertex.label=V(files[[i]])$media, vertex.label.color="black")
      dev.off()
      print(i)
}


#useful for highlighting certain people. Works by matching the name attribute of the vertex to the one specified in the 'ifelse' expression
V(p1.net)$color<-ifelse(V(p1.net)$name=='CA', 'blue', 'red') 
# Additional attributes like size can be further specified in an analogous manner, either in advance or when the plot function is called:

#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)
V(p1.net)$size<-degree(p1.net)/10 



# Note that if the same attribute is specified beforehand and inside the function, the former will be overridden.
# And finally the plot itself:
par(mai=c(0,0,1,0)) 			#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
plot(p1.net,				#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Periodo 1986-1992 ',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(p1.net)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
)

# Save and export the plot. The plot can be copied as a metafile to the clipboard, or it can be saved as a pdf or png (and other formats).
# For example, we can save it as a png:
png(filename=paste(E,"periodo 186-1992.png", sep = ""), height=800, width=600) #call the png writer
#run the plot
dev.off() #dont forget to close the device
#And that's the end for now.