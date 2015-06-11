
###############################################################
#       
#       Data Cleaning for W-CoMMS  - STEP1
#
#
#
#     last updated by Jeff Worden on 30 April 2014
#
################################################################


library(RODBC)

dir2 <- "C:/Users/jeffr_000/Dropbox (NRT Kenya)/wildlife_comms_BE_2014"
dir3 <- "C:/Users/jeffr_000/Dropbox (NRT Kenya)/w_comms/clean_data/"

setwd(dir2)

f1 <- list.files(pattern=".mdb", full.names=T)

obs <- vector("list")

for(i in 1:length(f1)){
  ch1 <- odbcConnectAccess(f1[i])
  obs[[i]] <- sqlFetch(ch1,"tblWildlifeObservations")
}


#check to see if all the column names are in order
nam <- vector("list")
for(i in 1:length(obs)){
  nam[[i]] <- names(obs[[i]])
}

# if some column names are not correct then change them...


obs1 <- vector("list")
# create unique IDs for all observations
# add missing conservancy names
# add columns for errors, remove, change
# reorder columns
for(i in 1:length(obs)){
  x <- obs[[i]]
  x$ccy <- unique(x$ConservancyName[1])
  x$Num_ID <- seq(1:length(x$ID))
  x$Unique_ID <- paste(x$Num_ID,
                        substring(x$ConservancyName,1,2),
                        substring(x$BlockName,1,2),
                        substring(x$Location,1,2),
                        substring(x$Conservancy,1,2),
                        substring(x$spp,1,2),
                        x$Date,
                        sep="")
  x$error <- 0
  x$changed <- 0
  x$remove <- 0
  n1 <- names(x)
  obs1[[i]] <- x[sort(n1)]
}


#combine all conservancy level data into a single data.frame

nam1 <- vector("list")
for(i in 1:length(obs1)){
  nam1[[i]] <- names(obs1[[i]])
}

# create a blank data frame
# remove unnecessary columns (Comments, ConservancyName,InputDate,
#     InputPerson,Time)
obs_all <- data.frame(obs1[[1]][NULL,c(-4,-5,-9,-10,-17)])

#obs_all <- data.frame(obs1[[1]][NULL,c(-4,-5,-10,-17)])

#combine all observations into one data.frame
for(i in 1:length(obs1)){
  x <- obs1[[i]][,c(-4,-5,-9,-10,-17)]
  #x <- obs1[[i]][,c(-4,-5,-10,-17)]
  names(x) <- names(obs_all)
  obs_all <-rbind(obs_all,x)
}



# missing and erroneous dates
date1 <- as.POSIXlt(obs_all$Date)
obs_all$year <- date1$year +1900
obs_all$error[is.na(obs_all$Date)==T] <- 1
obs_all$error[which(obs_all$year < 2004)] <- 1
obs_all$error[which(obs_all$year > 2014)] <- 1
# 165 records with missing or erroneous dates

obs_all$Date[obs_all$error==1]
which(obs_all$error==1)  # 165 records with missing or erroneous dates

# indicate rows with species = NA
obs_all$error[which(is.na(obs_all$Species)==T)] <- 1
    length(which(is.na(obs_all$Species)==T))  # 324 missing species names

# missing XY coordinates
obs_all$error[is.na(obs_all$UTMX)==T] <- 1
obs_all$error[is.na(obs_all$UTMY)==T] <- 1
    length(which(is.na(obs_all$UTMX)==T |is.na(obs_all$UTMY)==T ))
      # 12343 observations missing either X or Y or both coordinates

#missing numbers

obs_all$error[is.na(obs_all$NumIndiv)==T] <- 1
obs_all$error[obs_all$NumIndiv==0] <- 1
    length(which(is.na(obs_all$NumIndiv)==T | obs_all$NumIndiv==0))
      # 735 observations with missing or zero(0) individuals recorded

sum(obs_all$error) # total erroneous observations = 13067 out of 199957
                    # for an overall error rate of 0.065 percent


##############################################
# Cleaning species names

spp <- unique(obs_all$Species)
obs_all$spp <- toupper(obs_all$Species)
#obs_all$spp1 <- obs_all$spp
spp1 <- unique(obs_all$spp)
sort(spp1)

# standardize species names
obs_all$spp[obs_all$spp==""] <- NA
obs_all$spp[obs_all$spp=="BUFALLO"] <- "BUFFALO"
obs_all$spp[obs_all$spp=="BUFALO"] <- "BUFFALO"
obs_all$spp[obs_all$spp=="BUSH BACK"] <- "BUSHBUCK"
obs_all$spp[obs_all$spp=="BUSH BUCK"] <- "BUSHBUCK"
obs_all$spp[obs_all$spp=="BUSHBACK"] <- "BUSHBUCK"
obs_all$spp[obs_all$spp=="CHEATAH"] <- "CHEETAH"
obs_all$spp[obs_all$spp=="CLIPSPRINGER"] <- "KLIPSPRINGER"
obs_all$spp[obs_all$spp=="COLOMBUS MONKEY"] <- "COLOBUS MONKEY"
obs_all$spp[obs_all$spp=="DEPRESS MONKEY"] <- "DEBRAZZA'S MONKEY"
obs_all$spp[obs_all$spp=="DE BRAZZA MONKEY"] <- "DEBRAZZA'S MONKEY"
obs_all$spp[obs_all$spp=="GENERUK"] <- "GERENUK"
obs_all$spp[obs_all$spp=="GIRAFEE"] <- "GIRAFFE"
obs_all$spp[obs_all$spp=="GRANT GAZELLE"] <- "GRANT'S GAZELLE"
obs_all$spp[obs_all$spp=="GREATER  KUDU"] <- "GREATER KUDU"
obs_all$spp[obs_all$spp=="GREVY ZEBRA"] <- "GREVY'S ZEBRA"
obs_all$spp[obs_all$spp=="COMMON ZEBRA"] <- "PLAINS ZEBRA" 
obs_all$spp[obs_all$spp=="PLAIN ZEBRA"] <- "PLAINS ZEBRA"
obs_all$spp[obs_all$spp=="HIPPOPOTAMUS"] <- "HIPPO"
obs_all$spp[obs_all$spp=="HIPPOTAMUS"] <- "HIPPO"
obs_all$spp[obs_all$spp=="SPOTTED HYENA"] <- "SPOTTED HYAENA"
obs_all$spp[obs_all$spp=="HYAENA"] <- "SPOTTED HYENA"
obs_all$spp[obs_all$spp=="HYENA"] <- "SPOTTED HYENA"
obs_all$spp[obs_all$spp=="NKOLII"] <- "GRANT'S GAZELLE"
obs_all$spp[obs_all$spp=="NKOLII/IMPALA"] <- "IMPALA"
obs_all$spp[obs_all$spp=="IMPALA/NKOLII"] <- "IMPALA"
obs_all$spp[obs_all$spp=="MANGABAY"] <- "MANGABEY"
obs_all$spp[obs_all$spp=="MANGBAY"] <- "MANGABEY"
obs_all$spp[obs_all$spp=="POTTED HYENA"] <- "SPOTTED HYENA"
obs_all$spp[obs_all$spp=="RED COLLUMBUS"] <- "RED COLOBUS"
obs_all$spp[obs_all$spp=="SPOTTED HYAENA"] <- "SPOTTED HYENA"
obs_all$spp[obs_all$spp=="STRIPPED HYAENA"] <- "STRIPED HYENA" 
obs_all$spp[obs_all$spp=="STRIPPED HYEANA"] <- "STRIPED HYENA" 
obs_all$spp[obs_all$spp=="STRIPPED HYENA"] <- "STRIPED HYENA"
obs_all$spp[obs_all$spp=="STRIPED HYAENA"] <- "STRIPED HYENA"
obs_all$spp[obs_all$spp=="STRIPED HYEANA"] <- "STRIPED HYENA"
obs_all$spp[obs_all$spp=="STRIPPED HAENA"] <- "STRIPED HYENA"
obs_all$spp[obs_all$spp=="STRIPPED HAENA"] <- "STRIPED HYENA"
obs_all$spp[obs_all$spp=="SYKES"] <- "SYKES MONKEY"
obs_all$spp[obs_all$spp=="T/RED COLOBUS"] <- "RED COLOBUS"
obs_all$spp[obs_all$spp=="TANA MANGABAY"] <- "MANGABEY"
obs_all$spp[obs_all$spp=="THOMPSONS GAZELLE"] <- "THOMSON'S GAZELLE"
obs_all$spp[obs_all$spp=="THOMSONS GAZELLE"] <- "THOMSON'S GAZELLE"
obs_all$spp[obs_all$spp=="THOMSON GAZELLE"] <- "THOMSON'S GAZELLE"
obs_all$spp[obs_all$spp=="WATERBUCK "] <- "WATERBUCK"
obs_all$spp[obs_all$spp=="WATER BUCK"] <- "WATERBUCK"
obs_all$spp[obs_all$spp=="WILDOG"] <- "WILD DOG"


spp2 <- unique(obs_all$spp)
sort(spp2)

#standardize the block and location names 
# change all block and location names to upper case
loc_nam_obs <- unique(obs_all$Location)
obs_all$loc <- toupper(obs_all$Location)
loc_nam_obs1 <- sort(unique(obs_all$loc))
obs_all$block <- toupper(obs_all$BlockName)
obs_all$block_loc <- paste(obs_all$block, obs_all$loc, sep="_")
obs_all$ccy_loc <- paste(toupper(obs_all$ccy),obs_all$block,obs_all$loc,sep="_")

ccy_loc_obs <- sort(unique(obs_all$ccy_loc))

#export the results to a .csv file with todays date
#write.csv(obs_all,"C:\\DATA\\NRT\\wild_comms\\_data_cleaning\\results\\NRT_w_comms_data_cleaning_OUTPUT3_unique.csv",row.names=F)
#write.csv(obs_all,paste("C:\\DATA\\NRT\\wild_comms\\_data_cleaning\\results\\NRT_w_comms_data_cleaning_OUTPUT1_unique","_",Sys.Date(),".csv",sep=""),row.names=F)
write.csv(obs_all,paste(dir3,"/","CLEAN1","_",Sys.Date(),".csv",sep=""),row.names=F)

######################################################
#   Import location names and coordinates

loc <- vector("list")

for(i in 1:length(f1)){
  ch1 <- odbcConnectAccess(f1[i])
  loc[[i]] <- sqlFetch(ch1,"tblLocations")
}

#get conservancy names
f2 <- list.files(pattern=".mdb", full.names=F)
x <- strsplit(f2,"_")
ccy_nam <- vector()
for(i in 1:length(x)){
  ccy_nam[i] <- x[[i]][1]
}

for(i in 1:length(f1)){
  loc[[i]]$ccy <- ccy_nam[i]
}

#combine all locations into one data.frame
loc_all <- data.frame(loc[[1]][NULL,])

for(i in 1:length(loc)){
  x <- loc[[i]]
  #names(x) <- names(obs_all)
  loc_all <-rbind(loc_all,x)
}

# change 0 values of X and Y to NA
loc_all$UTMX[which(loc_all$UTMX==0)] <- NA
loc_all$UTMY[which(loc_all$UTMY==0)] <- NA

# change all block and location names to upper case
loc_nam <- unique(loc_all$LocationName)
loc_all$loc <- toupper(loc_all$LocationName)
loc_nam1 <- sort(unique(loc_all$loc))
loc_all$block <- toupper(loc_all$BlockName)
loc_all$block_loc <- paste(loc_all$block, loc_all$loc, sep="_")
loc_all$ccy_loc <- paste(toupper(loc_all$ccy),loc_all$block,loc_all$loc,sep="_")

length(which(duplicated(loc_all$ccy_loc))) #24 duplicated conservancy block location combinations

#give eaching matching set of duplicates a code for sorting
dups1 <- which(duplicated(loc_all$ccy_loc))
loc_all$dups <- 0
for(i in 1:length(dups1)){
  d1 <- loc_all$ccy_loc[dups1[i]]
  loc_all$dups[which(loc_all$ccy_loc==d1)] <- i
}

lad <- loc_all[loc_all$dups>0,]


# check X Y duplication

loc_all$xy <- paste(loc_all$UTMX,loc_all$UTMY, sep="_")
length(which(duplicated(loc_all$xy)))  # 238 duplicated? NAs?

unique(loc_all$xy[which(duplicated(loc_all$xy))])  #29 duplicated xy that are not NA

dups2 <- which(duplicated(loc_all$xy))
loc_all$dups_xy <- 0
for(i in 1:length(dups2)){
  d1 <- loc_all$xy[dups2[i]]
  loc_all$dups_xy[which(loc_all$xy==d1)] <- i
}

loc_all$xy[loc_all$dups_xy==238]

ccy_loc_all <- sort(unique(loc_all$ccy_loc))

#write.csv(loc_all,paste("C:\\DATA\\NRT\\wild_comms\\_data_cleaning\\results\\block_location_",Sys.Date(),".csv",sep=""))





#################################################################
#   Assessing the observation/sightings data
#
#

table(obs_all$ccy[is.na(obs_all$UTMX)==T])
# 12839 missing XY Coordinates
#Biliqo Bulesa     Il Ngwesi     Ishaqbini        Kalama     Lekurruki       Leparua 
#187            72             7          1359             0             0 
#Ltungai        Meibae        Melako    Mpus Kutuk      Naibunga      Namunyak 
#24           105             1             7            11            63 
#Ndera          Ruko          Sera     West Gate      Westgate 
#0           190         10298           515             0 

table(obs_all$spp[is.na(obs_all$UTMX)==T])
# eland = 66
# elephant=1906
# giraffe = 1542
# Grevy's = 593
# lion = 270
# wild dog=19

table(obs_all$spp[is.na(obs_all$UTMX)==T],obs_all$ccy[is.na(obs_all$UTMX)==T])






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

