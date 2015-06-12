
###############################################################
#       
#       Data Cleaning for W-CoMMS  - Patrol Effort
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

pat1 <- vector("list")
pat2 <- vector("list")

for(i in 1:length(f1)){
  ch1 <- odbcConnectAccess(f1[i])
  pat1[[i]] <- sqlFetch(ch1,"tblPatrols")
  pat2[[i]] <- sqlFetch(ch1,"tblPatrolLocations")
}

pat1_temp <- pat1
pat2_temp <- pat2


#check to see if all the column names are in order
nam <- vector("list")
for(i in 1:length(pat1)){
  nam[[i]] <- names(pat1[[i]])
}

nam



#####
# tests

# how to find patrol locations for a given date....

t1 <- sqlTables(ch1)

tnam <- t1$TABLE_NAME

tnam1 <- vector("list")

for(i in 1:length(tnam)){
  tnam1[[i]] <- sqlFetch(ch1,tnam[i])
}


tnam2 <- vector("list")
for(i in 1:length(tnam1)){
  tnam2[[i]] <- names(tnam1[[i]])
}


u1 <- unique(tnam1[[28]]$PatrolID)
u2 <- unique(tnam1[[29]]$ID)  #why is this longer than u1?

length(u1 %in% u2)
length(setdiff(u2,u1))

tnam3 <- merge(tnam1[[28]],tnam1[[29]],by.x="PatrolID",by.y="ID",all.y=T)

t4 <- unique(tnam3$PatrolID)
tnam4 <- vector()
tnam4a <- vector()
tnam4b <- vector()
p_id <- vector()
tnam4c <- .POSIXct(character(10))


#p1 <- data.frame(p_id,tnam4a,tnam4b)

for(i in 1:length(t4)){
  tnam4 <- tnam3[tnam3$PatrolID==t4[i],]
  tnam4a[i] <- length(unique(tnam4$LocationName))
  tnam4b[i] <- length(unique(tnam4$BlockName))
  tnam4c[i] <- unique(tnam3$Date[tnam3$PatrolID==t4[i]])
  p_id[i] <- unique(tnam4$PatrolID)
}

p1 <- data.frame(p_id,tnam4a,tnam4b,tnam4c)
names(p1) <- c("patrol_id","loc_num","block_num","date")

patrol1 <- merge(tnam3,p1,by.x="PatrolID",by.y="patrol_id",all.x=F)


