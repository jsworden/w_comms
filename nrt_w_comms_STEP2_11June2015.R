
###############################################################
#       
#       Data Cleaning for W-CoMMS  - STEP2
#
#        Correcting missing location information
#
#     last updated by Jeff Worden on 30 April 2014
#
################################################################



# this code still needs work....



##########################################################3
##   Addressing missing values and other issues in block-Loc data
#
#
#


new_loc <- read.csv("C:\\DATA\\NRT\\wild_comms\\_data_cleaning\\results\\block_location_2014-07-10_adjusted.csv",header=T)


ccy_loc_obs <- sort(unique(obs_all$ccy_loc))

# create new data frame to populate with mean values of xy from obs data
utmx_new <- vector("numeric")
utmy_new <- vector("numeric")
cbl <- vector("character")
len <- vector("numeric")
obs_new <- data.frame(cbl,utmx_new,utmy_new,len) 


# populate the new X and Y coordinates with the mean of all non zero coordinates
# for each location -excluding the lower 2 quantiles of the data to remove errors
# associated with misplaced decimal places - look for other alternatives?
list1 <- vector("list")  

for(i in 1:length(ccy_loc_obs)){
  #for(i in 1:5){
  t1 <- ccy_loc_obs[i]
  t1a <- obs_all[which(obs_all$ccy_loc==t1),]
  t1a <- t1a[which(is.na(t1a$UTMX)==F),]
  t1a <- t1a[which(is.na(t1a$UTMY)==F),]
  t1a <- t1a[t1a$UTMX>0|t1a$UTMY>0,]
  list1[[i]] <- t1a
  q1x <- quantile(t1a$UTMX,na.rm=T)
  q1y <- quantile(t1a$UTMY,na.rm=T)
  t1x <- mean(t1a$UTMX,na.rm=T)
  t1y <- mean(t1a$UTMY,na.rm=T)
  t1xa <- mean(t1a$UTMX[which(t1a$UTMX>=q1x[2])],na.rm=T)
  t1ya <- mean(t1a$UTMY[which(t1a$UTMY>=q1y[2])],na.rm=T)
  l1 <- length(t1a$ccy_loc)
  t2 <- data.frame(t1,t1xa,t1ya,l1)
  obs_new <- rbind(obs_new,t2)
}

#test1 <- obs_new

obs_new[1:10,] # there are still some obs (especially those with 1 observation)
# that appear to have misplaced decimal points - need to follow these up...

#write.csv(obs_new,paste("C:\\DATA\\NRT\\wild_comms\\_data_cleaning\\results\\obs_new_",Sys.Date(),".csv",sep=""))




#merge location centroids with location data from sightings observations to compare

loc_obs <- merge(obs_new,new_loc,by.x="t1",by.y="ccy_loc",all=T)
loc_obs1 <- loc_obs[,c("ccy","t1","t1xa","t1ya","l1","UTMX","UTMY","dups","dups_xy","delete")]
#write.csv(loc_obs1,paste("C:\\DATA\\NRT\\wild_comms\\_data_cleaning\\results\\loc_obs1_",Sys.Date(),".csv",sep=""))


loc_obs1_nrt <- loc_obs1[loc_obs1$ccy!="Ndera" & loc_obs1$ccy!="Ishaqbini",]
loc_obs1_nrt$ccy[loc_obs1_nrt$ccy=="WestGate Monitoring Database (BE) 2003.mdb"] <-"West Gate"
loc_obs1_nrt <- loc_obs1_nrt[is.na(loc_obs1_nrt$ccy)==F,]

loc_obs1_nrt$utmx_new <- loc_obs1_nrt$UTMX
loc_obs1_nrt$utmy_new <- loc_obs1_nrt$UTMY
for(i in 1:length(loc_obs1_nrt$UTMX)){
  if(is.na(loc_obs1_nrt$UTMX[i])==T){loc_obs1_nrt$utmx_new[i] <- loc_obs1_nrt$t1xa[i]}
  if(is.na(loc_obs1_nrt$UTMY[i])==T){loc_obs1_nrt$utmy_new[i] <- loc_obs1_nrt$t1ya[i]}
} 

# highlights records with missing XY coordinates
loc_obs1_nrt$missing <- 0
loc_obs1_nrt$missing[which(is.nan(loc_obs1_nrt$utmx_new)==T)] <- 1
loc_obs1_nrt$missing[which(is.na(loc_obs1_nrt$utmx_new)==T)] <- 1
loc_obs1_nrt$missing[which(loc_obs1_nrt$utmx_new==0)] <- 1

#loc_obs2_nrt <- loc_obs1_nrt[which(is.nan(loc_obs1_nrt$utmx_new)==F),]
#loc_obs2_nrt <- loc_obs2_nrt[which(is.na(loc_obs2_nrt$utmx_new)==F),]
#write.csv(loc_obs1_nrt,paste("C:\\DATA\\NRT\\wild_comms\\_data_cleaning\\results\\loc_obs1_nrt_",Sys.Date(),".csv",sep=""))

#when importing to QGIS 58 records are removed because of lack of XY locations
# 0, NA or NaN



loc_fin <- loc_obs1_nrt[which(is.na(loc_obs1_nrt$delete)==T | loc_obs1_nrt$delete==3),]



obs_fin <- merge(obs_all,loc_fin,by.x="ccy_loc",by.y="t1",all.x=T)



############################################
# test plotting these data....

library(maptools)

s1 <- readShapePoly("C:\\DATA\\GIS\\NRT\\NRT_NorthernConservancies_20Feb_2014_udate1.shp") # has hole in Namunyak - need to fix
s1a <- s1[c(1:12,14,16,17),]
s1b <- s1a[c(1,3:5,7:10,13,15),]
s2 <- unionSpatialPolygons(SpatialPolygons(s1a@polygons),IDs=s1a$Name)

plot(s1)
points(obs_fin$utmx_new,obs_fin$utmy_new,cex=.5,col="red")
points(obs_fin$utmx_new[obs_fin$ccy.x=="Sera"],obs_fin$utmy_new[obs_fin$ccy.x=="Sera"],cex=.5,col="blue")
points(obs_fin$utmx_new[obs_fin$ccy.x=="Namunyak"],obs_fin$utmy_new[obs_fin$ccy.x=="Namunyak"],cex=.5,col="green")
points(obs_fin$utmx_new[obs_fin$ccy.x=="West Gate"],obs_fin$utmy_new[obs_fin$ccy.x=="West Gate"],cex=.5,col="purple",pch=1)

points(obs_fin$UTMX.x[obs_fin$ccy.x=="West Gate"],obs_fin$UTMY.x[obs_fin$ccy.x=="West Gate"],cex=.5,col="black",pch=16)


# even with merging there are still 27880 out of 284073 records with no XY coordinates

tt <- table(droplevels(obs_fin$ccy.x[which(is.na(obs_fin$utmx_new)==T)]))
tt1 <- table(obs_fin$spp[which(is.na(obs_fin$utmx_new)==T)])


c1 <- sort(unique(loc_all$ccy_loc[loc_all$ccy=="West Gate"]))
c2 <- sort(unique(obs_all$ccy_loc[obs_all$ccy=="West Gate"]))



#################################
# sorting out location information....

test_obs <- read.csv("C:\\DATA\\NRT\\wild_comms\\_data_cleaning\\results\\NRT_w_comms_data_cleaning_OUTPUT1_unique_2014-07-10_adjusted.csv")

lu <- sort(unique(test_obs$Location))
test_obs[test_obs$Location==lu[865],c("UTMX","UTMY")]


# tests...

obs_all1 <- obs_all
obs_all1$XY <- paste(obs_all1$UTMX,obs_all1$UTMY,sep="_")
test_f <- obs_all1[obs_all1$ccy_loc=="WEST GATE_NAISUNYAI_LPUSI",c("UTMX","UTMY","ccy_loc","XY")]
which.max(table(test_f$UTMX[test_f$UTMX>0]))
which.max(table(test_f$XY[test_f$UTMX>0]))


###########################3
# create shape files for all observations by conservancy
library(maptools)
library(rgdal)
library(sp)

setwd("C:\\DATA\\NRT\\wild_comms\\_data_cleaning\\results\\ccy_obs_shapefiles\\")

c1 <- unique(obs_all$ccy)

for(i in 1:length(c1)){
  c2 <- obs_all[obs_all$ccy==c1[i],]
  c2 <- c2[which(c2$UTMX > 0 & is.na(c2$UTMX)==F & c2$UTMY> 0 & is.na(c2$UTMY)==F),]
  coordinates(c2) <- ~UTMX+UTMY
  proj4string(c2) <- CRS("+init=epsg:32637")
  writeOGR(obj=c2,dsn=".",layer=paste(c1[i],"obs",Sys.Date(),sep="_"),driver="ESRI Shapefile")
}

#######################################
#  Patrol effort
#
#


pat <- vector("list")

for(i in 1:length(f1)){
  ch1 <- odbcConnectAccess(f1[i])
  pat[[i]] <- sqlFetch(ch1,"tblPatrols")
}


#check to see if all the column names are in order
nam <- vector("list")
for(i in 1:length(pat)){
  nam[[i]] <- names(pat[[i]])
}

# if some column names are not correct then change them...

nam1 <- vector("list")
for(i in 1:length(pat)){
  nam1[[i]] <- names(pat[[i]])
}
