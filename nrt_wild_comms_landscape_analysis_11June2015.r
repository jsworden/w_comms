########################################################################
#    Landscape Level Analyses - NRT Wild-CoMMS
#
#
#   last update 5 March 2014 by Jeff Worden
#
#######################################################################


library (adehabitatHR)
library(maptools)
library (raster)
library(rgdal)
library(spatstat)

setwd("C:\\DATA\\NRT\\wild_comms\\annual reports\\Landscape Data Analysis_Feb2014")

s1 <- readShapePoly("C:\\DATA\\GIS\\NRT\\NRT_NorthernConservancies_20Feb_2014_udate1.shp") # has hole in Namunyak - need to fix
s2 <- readShapePoly("C:\\DATA\\NRT\\wild_comms\\annual reports\\NRT_WCoMMS_Conservancies_LL_5Mar2014.shp",delete_null_obj=TRUE) # has hole in Namunyak - need to fix



x1 <- read.csv("Key Species wildlife observation data cleaned_DL3rd march2014.csv", header=T)


ccy <- unique(x1$Conservancy)
spp <- unique(x1$Species)

max1 <- max(x1$X)
min1 <- min(x1$X)
max2 <- max(x1$Y)
min2 <- min(x1$Y)

x2 <- x1
coordinates(x2) <- ~X+Y
x3 <- SpatialPoints(x2)

# calculate utilization distributions


l1 <- list()
l2 <- list()
for(i in 1:length(spp)){
  d1 <- x2[x2$Species==spp[i],]
  #d2 <- SpatialPoints(d1)
  ud1_epa <- kernelUD(d1,kern="epa",grid=1000)
  ud1_vol_epa <- getvolumeUD(ud1_epa)
  l1[[i]] <- ud1_epa
  l2[[i]] <- ud1_vol_epa
}

# does not work as vector too large (276.o mb)
l3 <- list()
l4 <- list()
for(i in 1:length(spp)){
  d1 <- x2[x2$Species==spp[i],]
  #d2 <- SpatialPoints(d1)
  ud1_epa <- kernelUD(d1,kern="epa",grid=5000)
  ud1_vol_epa <- getvolumeUD(ud1_epa)
  l3[[i]] <- ud1_epa
  l4[[i]] <- ud1_vol_epa
}



lat <- c(range(x1$X)) #define our map's ylim - change to lat long
lon <- c(range(x1$Y)) #define our map's xlim

col1 <- grey(c(1:10/10))
col1a <- topo.colors(c(10))
col1b <- c(col1c[1:9],col1[10])
col1c <- heat.colors(10)



windows(record=T)
#plot all species
for( i in 1:length(l2)){
  #plot(x1$X,x1$y,type="n", axes=F, xlab="", ylab="", main=spp[i])
  image(l2[[i]],xlim=lat,ylim=lon,col=col1b)
  plot(s1,add=T,border="grey")
  c1 <- coordinates(s1)
  text(c1,labels=s1@data$Name,cex=.6,offset=.5,pos=1)
  mtext(spp[i], side=3,line=-1, cex=.8, adj=.2)
  mtext("2010 - 2013", side=3, line=-1.5, cex=.5, adj=.2)
 legend()
  #writeGDAL(l2[[i]],fname=paste(spp[i],"kernelUD",sep="_"))
}

library(fields)
windows(record=T)
l3 <- as.matrix(l2[[i]])
l4 <- ((l3-100)*-1)/100 # reverse scale and standardize to 1
col1 <- grey(c(1:10/10))
col1c <- heat.colors(40)
col1d <- c(col1[10],col1c[39:1])

image.plot(l3)


image.plot(l2,nlevel=40,col=col1d,bty="n")
plot(s1,add=T,border="grey")
c1 <- coordinates(s1)
text(c1,labels=s1@data$Name,cex=.6,offset=.5,pos=1)
mtext(spp[i], side=3,line=-1, cex=.8, adj=.2)
mtext("2010 - 2013", side=3, line=-1.5, cex=.5, adj=.2)



# test of adding a legend
layout(matrix(c(1,2,3,0,4,0), nrow=2, ncol=3), widths=c(4,4,1), heights=c(4,1))
layout.show(4)
r1 <- raster(l2[[i]])
r2 <- rasterToPoints(r1)
r2 <- data.frame(r2)
breaks <- seq(min(r2), max(r2),length.out=100)
op <- par
par(mar=c(1,1,1,1))
image(l2[[i]],xlim=lat,ylim=lon,col=col1b)
#image(seq(dim(volcano)[1]), seq(dim(volcano)[2]), volcano, 
 #     col=pal.1(length(breaks)-1), breaks=breaks, xaxt="n", yaxt="n", ylab="", xlab="")
#Add additional graphics
#highest <- which.max(volcano)
#points(highest %% dim(volcano)[1], highest %/% dim(volcano)[1], 
 #      pch=2, lwd=2, cex=2,col="blue")
#Add scale
pal.1=colorRampPalette(c("black", "red", "yellow"), space="rgb")
par(mar=c(3,1,1,1))
image.scale(l2[[i]], col=pal.1(length(breaks)-1), breaks=breaks, horiz=TRUE)
box()










library(fields)
image.plot(matrix(l2), col=terrain.colors(20), add=TRUE,
           transparent.color = "transparent") 



library(ggplot2)

r1 <- raster(l2[[1]])
r2 <- rasterToPoints(r1)
r2 <- data.frame(r2)

b.sight <- seq(min(r2$n),max(r2$n),length.out=5)
p1 <- ggplot()+
  layer(geom="raster",data=r2,mapping=aes(x,y,fill=n)) +
  scale_fill_gradientn(name="Sightings per km2",colours = myColoursAlpha,breaks=b.sight)+
 # scale_x_continuous(name=expression(paste("Longitude (",degree,")")),limits=c(-4,2),expand=c(0,0))+
  #scale_y_continuous(name=expression(paste("Latitude (",degree,")")),limits=c(4,12),expand=c(0,0))+
  coord_equal()
print(p1)



p1 <- ggplot(data=r2, aes(y=y, x=x, fill=n)) +
  geom_raster(aes(fill=n)) +
  #geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  coord_equal() +
  #scale_fill_gradient("MAP (mm/yr)", limits=c(0,2500)) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank())
  

## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
} 

myColours = topo.colors(20)
myColoursAlpha <- c(myColours[1:19],add.alpha(myColours[20], alpha=1))
## "#00000066" "#4682B466" "#FFBB0066" "#66334D66" 








# trends
# individual plots
m1 <- max(x1$Number)
agg1 <- list()

spp1 <- sort(spp)

for(i in 1:length(spp1)){
  agg1[[i]] <- aggregate(x1$Number[x1$Species==spp1[i]], by=list(x1$Year[x1$Species==spp1[i]]), sum)
  #barplot(agg1[[i]]$x, names.arg=agg1[[i]]$Group.1)
  y1 <- agg1[[i]]$Group.1
  d1 <- agg1[[i]]$x
  d2 <- data.frame(d1,y1)
  plot(d2$d1~d2$y1, type="b", axes=F, pch=16, cex=1.2, ylim=c(0,max(d1)*1.1), xlab="Year",
       ylab="Number of Sightings")
  axis(1,at=d2$y1, cex.axis=.8)
  axis(2,cex.axis=.7,las=2)
  box(bty="l")
  mtext(spp1[i], side=3, line=-1,cex=.8, adj=.8)
}

names(agg1) <- spp1

# combined trend plots


windows(8.27,11.69)
op <- par
par(mfcol=c(length(spp1),1),mar=c(1,4,2,3))

for(i in 1:length(spp1)){
  if(i==length(spp1)) {par(mar=c(3,4,1,3))}
  if(i<length(spp1)) {par(mar=c(1.5,4,2.5,3))}  
  agg1[[i]] <- aggregate(x1$Number[x1$Species==spp1[i]], by=list(x1$Year[x1$Species==spp1[i]]), sum)
  #barplot(agg1[[i]]$x, names.arg=agg1[[i]]$Group.1)
  y1 <- agg1[[i]]$Group.1
  d1 <- agg1[[i]]$x
  d2 <- data.frame(d1,y1)
  plot(d2$d1~d2$y1, type="b", axes=F, pch=16, cex=1.2, ylim=c(0,max(d2$d1)*1.1), xlab="Year", ylab="")#ylab="Number of Sightings"
  if(i==length(spp1)) {axis(1,at=d2$y1, cex.axis=.8)}
  axis(2,cex.axis=.7,las=2)
  box(bty="l")
  mtext(spp1[i], side=3, line=-0.5,cex=.7, adj=.7)
}

par <- op















# not working...

plot.new()
plot.window(xlim=c(min1,max1),ylim=c(min2,max2))






# plot vertices
op <- par
par(mfrow=c(4,2),mar=c(.5,.5,.5,.5))

for(i in 1:length(l1)){
  ud1_zb1_50 <-getverticeshr(l1[[i]],lev=50)
  ud1_zb1_75 <-getverticeshr(l1[[i]],lev=75)
  ud1_zb1_90 <-getverticeshr(l1[[i]],lev=90)
  
  plot(s1)
  plot(ud1_zb1_90,add=T, col="red")
  plot(ud1_zb1_75,add=T,col="green")
  plot(ud1_zb1_50,add=T,col="white")
  mtext(spp[i], side=3,line=-1, cex=.8)
  mtext("2010 - 2013", side=3, line=-1.5, cex=.5, adj=.6)
  legend("topright",legend=c("50","75","90"),fill=c("white","green","red"),bty="n")
}
















# test plots

#t1 <- kernelUD(x3,kern="epa",grid=1000, extent=0.5)

ud1 <- kernelUD(x3)
ud1_epa <- kernelUD(x3,kern="epa",grid=1000)
ud1_vol <- getvolumeUD(ud1)
ud1_vol_epa <- getvolumeUD(ud1_epa)

#plot.window(xlim(min1,max1),ylim(min2,max2))
plot.window(range(x1$X),range(x1$Y))

image(ud1_vol_epa,xlim(range(x1$X)),ylim(range(x1$Y)))

plot(s1,add=T,border="grey")

c1 <- coordinates(s1)
text(c1,labels=s1@data$Name,cex=.4,offset=.5,pos=1)





#maps
library(RgoogleMaps)

lat <- c(0.23,2.10) #define our map's ylim - change to lat long
lon <- c(36.8,38.45) #define our map's xlim
center = c(mean(lat), mean(lon))  #tell what point to center on
zoom <- 8  #zoom: 1 = furthest out (entire globe), larger numbers = closer in
terrmap <- GetMap(center=center, zoom=zoom, maptype= "terrain", 
                  destfile = "terrain.png") 


#plots

op <- par
par(mfrow=c(2,2),mar=c(.5,.5,.5,.5))
l1 <- list(c(10,30,50),c(20,40,70),c(30,60,80),c(50,75,90))
for(i in 1:length(l1)){
  ud1_zb1_50 <-getverticeshr(ud1_epa,lev=l1[[i]][1])
  ud1_zb1_75 <-getverticeshr(ud1_epa,lev=l1[[i]][2])
  ud1_zb1_90 <-getverticeshr(ud1_epa,lev=l1[[i]][3])
  
  plot(s1)
  plot(ud1_zb1_90,add=T)
  plot(ud1_zb1_75,add=T,colpol="green")
  plot(ud1_zb1_50,add=T,colpol="white")
  mtext("NRT - All Species 2011-2013",3,-1,cex=.7)
  #plot(ilk1,add=T)
  legend("topright",legend=c(l1[[i]][1],l1[[i]][2],l1[[i]][3]),fill=c("white","green","red"),bty="n")
}

