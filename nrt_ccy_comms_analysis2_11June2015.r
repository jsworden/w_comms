############################################
#  Melako CoMMS Analysis
#
#
#   last updated 23 April 2015 by Jeff Worden
#
###########################################

library(R2wd)
dir1 <- "C:/Users/jeffr_000/Dropbox (NRT Kenya)/w_comms"
dir2 <- "C:/Users/jeffr_000/Dropbox (NRT Kenya)/wildlife_comms_BE_2014"
dir3 <- "C:/Users/jeffr_000/Dropbox (NRT Kenya)/w_comms/clean_data/"

setwd(dir3)

dir()



x <- read.csv("CLEAN1_2015-06-11.csv",header=T)
p <- read.csv("Patrol_effort_Combined_10Mar2014.csv", header=T)

ccy <- unique(x$Conservancy)
ccy_p <- unique(p$ConservancyName)

d <- as.POSIXlt(strptime(x$Date,"%d-%b-%y"))
x$date <- as.POSIXct(d)
x$year <- d$year +1900
x$mon <- d$mon +1

##################
# data cleaning
# isolating incorrect or missing data
x$error <- 0
x$error[which(x$year>2013 | x$year < 2007 | is.na(x$year)==T)] <- 1   # incorrect date

x$spp <- toupper(x$Species)
x$spp[x$spp=="STRIPPED HYAENA"] <- "STRYPED HYAENA"

x1 <- x[x$error==0,]  # use subset of data without errors


p$loc_name <- p$LocationName
p$loc_name[p$loc_name=="lchoro ,leaduma ,lchoro"] <- "Lchoro"
p$loc_name[p$loc_name=="togogicha"] <- "Tokogicha"
p$loc_name[p$loc_name=="Dogogicha"] <- "Tokogicha"
p$loc_name1 <- toupper(p$loc_name)

p$UTMX[which(p$ConservancyName == "Melako" & p$loc_name1=="LCHORO" & is.na(p$UTMX)==T)] <- 367153.0
p$UTMY[which(p$ConservancyName == "Melako" & p$loc_name1=="LCHORO" & is.na(p$UTMY)==T)] <- 157695
p$UTMX[which(p$ConservancyName == "Melako" & p$loc_name1=="TOKOGICHA" & is.na(p$UTMX)==T)] <- 385344.0
p$UTMY[which(p$ConservancyName == "Melako" & p$loc_name1=="TOKOGICHA" & is.na(p$UTMY)==T)] <- 153788

p$error <- 0
p$error[which(p$UTMX<200000 | p$UTMY < 30000 | is.na(p$UTMX)==T)] <- 1   # incorrect date

d1 <- as.POSIXlt(strptime(p$Date,"%d-%b-%y"))
p$date <- as.POSIXct(d1)
p$year <- d1$year +1900
p$mon <- d1$mon+1

p_test <- p[p$error==1,]


############################################
# Melako
x1_mel <- x1[x1$Conservancy=="Melako",]

# remove erroneous species records
x1_mel <- x1_mel[x1_mel$spp!="ELAND" & 
                   x1_mel$spp!="PLAINS ZEBRA" &
                   x1_mel$spp!="JACKAL" &
                   x1_mel$spp!="IMPALA" &
                   x1_mel$spp!="GREATER KUDU" &
                   x1_mel$spp!="THOMSON'S GAZELLE" &
                   x1_mel$spp!="WATER BUCK" &
                   x1_mel$spp!="WILD DOG", ]

sort(unique(x1_mel$spp))

#number of individuals sighted by species and year
agg1 <- aggregate(x1_mel$NumIndiv,by=list(x1_mel$year, x1_mel$spp), sum)

agg1a <- reshape(agg1,v.names= "x", idvar="Group.2", timevar="Group.1",direction="wide")
names(agg1a) <- c("Species","2008","2009","2010","2011","2012","2013")

wdGet()
wdTable(agg1a,caption="Melako - Total Number of Individuals Sighted by Species and Year",
        caption.pos="above",row.names=F)

# number of sightings by species and year
t <- table(x1_mel$spp,x1_mel$year)

t1 <- as.data.frame(t)
t1a <- reshape(t1,v.names= "Freq", idvar="Var1", timevar="Var2",direction="wide")
names(t1a) <- c("Species","2008","2009","2010","2011","2012","2013")
wdGet()
wdTable(t1a,caption="Melako - Number of Sightings by Species and Year",
        caption.pos="above",row.names=F)


# mean group size
t2 <- round(agg1a[,2:7]/t1a[,2:7],2)
t2a <-cbind(t1a$Species,t2)
names(t2a) <- c("Species",names(t2))

#wdGet()
#wdTable(t2a,caption="Melako - Mean Group Size per Sighting by Species and Year",
#        caption.pos="above",row.names=F)

# max and min group size
agg2 <- aggregate(x1_mel$NumIndiv,by=list(x1_mel$year, x1_mel$spp), max)
agg2a <- reshape(agg2,v.names= "x", idvar="Group.2", timevar="Group.1",direction="wide")
names(agg2a) <- c("Species","2008","2009","2010","2011","2012","2013")

#wdGet()
#wdTable(agg2a,caption="Melako - Max Number of Individuals Sighted by Species and Year",
#        caption.pos="above",row.names=F)

# Melako patrol effort
# data checking and correction
p_mel <- p[p$ConservancyName=="Melako",]
p_mel_ok <- p_mel[p_mel$error==0,]

p_mel_loc <- unique(p_mel_ok[,c(14,8:9)])
p_mel_loc <- p_mel_loc[order(p_mel_loc$loc_name1),]

plot(p_mel_loc$UTMX,p_mel_loc$UTMY)


p_mel_agg1 <- table(p_mel_ok$mon,p_mel_ok$year)

wdGet()
wdTable(p_mel_agg1,caption="Melako - Total Patrol Days per Month by Year",
        caption.pos="above",row.names=T)

# species sightings
d_tab <- data.frame(rep(2008:2013,each=12),rep(1:12)) 
names(d_tab) <- c("year","mon")
d_tab$merge <- paste(d_tab$year,d_tab$mon, sep="_")
agg1m <- aggregate(x1_mel$NumIndiv,by=list(x1_mel$mon, x1_mel$year, x1_mel$spp), sum)
agg1m$merge <- paste(agg1m$Group.2,agg1m$Group.1, sep="_")

mel_spp <- sort(unique(x1_mel$spp))

m1 <- agg1m[agg1m$Group.3==mel_spp[1],]
m1a <- merge(d_tab,m1[,3:5],by.all="merge",all.x=T)
m1b <- m1a[order(m1a$year,m1a$mon),]
m1b$pos <- seq(1:72)

plot(m1b$x~m1b$pos,type="h",lwd=3,lend=2,bty="l",axes=F,xlab="Time (months)",
     ylab="Number of Individuals",ylim=c(0,max(na.omit(m1b$x))*1.1))
axis(1,at=m1b$pos,label=m1b$mon,las=2,cex.axis=.4,pos=0)
axis(2,pos=0, cex.axis=.7)
abline(v=c(12.5,24.5,36.5,48.5,60.5),col="grey")
abline(h=0)
mtext(unique(m1b$year),adj=c(.13,.3,.45,.6,.75,.9),cex=.7,line=-1,side=3)
mtext(text=mel_spp[1],side=3,line=0,cex=.8)

# multiple species plots per page
windows(8.5,11, record=T)
op <- par
par(mfcol=c(4,1),mar=c(1.5,1.5,1.5,1.5))

for(i in 1:length(mel_spp)){
  m1 <- agg1m[agg1m$Group.3==mel_spp[i],]
  m1a <- merge(d_tab,m1[,3:5],by.all="merge",all.x=T)
  m1b <- m1a[order(m1a$year,m1a$mon),]
  m1b$pos <- seq(1:72)
  
  plot(m1b$x~m1b$pos,type="h",lwd=3,lend=2,bty="l",axes=F,xlab="",
       ylab="",ylim=c(0,max(na.omit(m1b$x))*1.1))
  axis(1,at=m1b$pos,label=m1b$mon,las=2,cex.axis=.4,pos=0)
  axis(2,pos=0, cex.axis=.7)
  abline(v=c(12.5,24.5,36.5,48.5,60.5),col="grey")
  abline(h=0)
  mtext(unique(m1b$year),adj=c(.13,.3,.45,.6,.75,.9),cex=.7,line=-1,side=3)
  mtext(text=mel_spp[i],side=3,line=0,cex=.8)
}


par <- op



###############
#mapping

library(sp)
library(maptools)

# import maps
s1 <- readShapePoly("C:\\DATA\\GIS\\NRT\\NRT_NorthernConservancies_20Feb_2014_udate1.shp") # has hole in Namunyak - need to fix


ll <- CRS("+proj=longlat +datum=WGS84 +no_defs")
utm1 <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs")
proj4string(s1) <- utm1

s2  <- spTransform(s1,ll)

# melako
mel_shp <- s1[which(s1@data$Name=="Melako"),]

mel_coords <- x1_mel
coordinates(mel_coords) <- ~UTMX+UTMY
proj4string(mel_coords) <- utm1
ov1 <- over(mel_coords,mel_shp)
mel_coords1 <- mel_coords[which(ov1$Name=="Melako"),]

#creat 5 km buffer around extreme points from overlay
minx <- min(mel_coords1@coords[,1]) - 5000
maxx <-max(mel_coords1@coords[,1]) + 5000
miny <- min(mel_coords1@coords[,2])- 5000
maxy <-max(mel_coords1@coords[,2]) + 5000

#select points within the 5 km
mel_coords2 <- mel_coords[mel_coords@coords[,1] > minx &
                            mel_coords@coords[,1] < maxx &
                            mel_coords@coords[,2] > miny &
                            mel_coords@coords[,2] < maxy,]

#adjust the bounding box for the shapefile to display all points
mel_shp@bbox[1,1] <- minx
mel_shp@bbox[1,2] <- maxx
mel_shp@bbox[2,1] <- miny
mel_shp@bbox[2,2] <- maxy

windows(8.5,11, record=F)
#plot(mel_coords2@coords[,1],mel_coords2@coords[,2], type="n", asp=1)
plot(mel_shp, new=F)
#points(mel_coords1)
points(mel_coords2, col="red", cex=.7)

#plot by species and year
# multiple species plots per page
op <- par

yr1 <- unique(mel_coords2@data$year)

for(j in 1:length(yr1)){
  windows(8.5,11, record=T)
  par(mfcol=c(7,2),mar=c(0,0,0,0))
  mel_coords3 <- mel_coords2[mel_coords2@data$year==yr1[j] & mel_coords2@data$NumIndiv > 0,]
  mel_spp1 <- sort(unique(mel_coords3@data$spp))
  for(i in 1:length(mel_spp1)){
    m1_spp1 <- mel_coords3[mel_coords3@data$spp == mel_spp1[i],]
    plot(mel_shp)
    points(m1_spp1,col="red",cex=.85,pch=16)
    mtext(side=3,line=-1,adj=.9, text=mel_spp1[i], cex=.5)
    mtext(side=3,line=-1,adj=.2, text=yr1[j], cex=.5)
    
  }}

par<- op

# annual summaries

mel2 <- cbind(mel_coords2@data, mel_coords2@coords)

mel2_agg1 <- aggregate(mel2$NumIndiv,by=list(mel2$year,mel2$spp),sum)

mel2_spp <- sort(unique(mel2$spp))
d_tab1 <- data.frame(rep(2008:2013,each=1))
names(d_tab1) <- "year"


op <- par
windows(8.5,11, record=T)
par(mfcol=c(7,2),mar=c(2,2.5,2,2))

for(i in 1:length(mel2_spp)){
  mel2_m1 <- mel2_agg1[mel2_agg1$Group.2==mel2_spp[i],]
  mel2_m1a <- merge(d_tab1,mel2_m1,by.x="year", by.y="Group.1",all.x=T)
  mel2_m1b <- mel2_m1a[order(mel2_m1a$year),]
  mel2_m1b$pos <- seq(1:6)
  
  if(length(mel2_m1$x) >1) {
    plot(mel2_m1b$x~mel2_m1b$pos,type="h",lwd=10, col="grey50",lend="butt",bty="l",axes=F,xlab="",
         ylab="",ylim=c(0,max(na.omit(mel2_m1b$x))*1.1))
    axis(1,at=mel2_m1b$pos,label=mel2_m1b$year,las=2,cex.axis=.8,pos=0)
    axis(2,pos=0.5, cex.axis=.7)
    abline(h=0)
    mtext(unique(mel2_m1b$Group.2),adj=c(.1),cex=.5,line=0.5,side=3)
  }
}

par <- op

