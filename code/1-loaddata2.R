#################################################################
### Load and prepare data
#################################################################

## Sourcing required packages
source("code/0-packages.R")

### Loading dataset
atm.location <-read.csv("data/atmTrans.CSV")
refugee.location <-read.csv("data/HFDV.CSV")
transaction <-read.csv("data/Transactions.CSV")

admin1 <- st_read('data/admin1.shp')
admin2 <- st_read('data/admin2.shp')

## Test it's well loaded
#plot(admin1) 
#plot(admin2) 

## Fortify shaes
admin1.fortify <- fortify(admin1)
admin2.fortify <- fortify(admin2)

#rename colum to join tables
colnames(admin1.fortify)[5] <- "District"
atm.admin1 = merge(atm.location, admin1.fortify, by="District")

## Convert foreign object to an sf object
atm.admin1.sf <-st_as_sf(atm.admin1)
#str(atm.admin1.sf)

## Setting up projection
#utm10n<-CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
#wgs84<-CRS("+proj=longlat +zone=37 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


############################
####nearest ATM

### Create spatial data frame for ATM location
atm.location.coords <- data.frame  (atm.location$Lat, atm.location$Long, atm.location$Address)
atm.location.sp<-SpatialPointsDataFrame(data.frame(x=atm.location.coords$atm.location.Long,
                                                   y =atm.location.coords$atm.location.Lat),
                                        data=data.frame(ID=1:108, address=atm.location$Address), 
                                        proj4string = CRS("+proj=longlat +zone=37 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
## Project ATM to UTM
atm.location.Proj<-spTransform(atm.location.sp,
                               CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"))

## check
# plot(atm.location.sp)
# plot(atm.location.Proj)


### Create spatial data frame for refugee.location
refugee.location.coords <- data.frame  (refugee.location$Lat, refugee.location$Long, refugee.location$CaseNo)
refugee.location.sp <-SpatialPointsDataFrame(data.frame(x=refugee.location.coords$refugee.location.Long,
                                                      y =refugee.location.coords$refugee.location.Lat),
                                           data=data.frame(refugee.location.coords$refugee.location.CaseNo),
                                           proj4string = CRS("+proj=longlat +zone=37 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
## project Benef to UTM
refugee.location.Proj <-spTransform(refugee.location.sp,
                                 CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"))

# plot(refugee.location.sp)
# plot(refugee.location.Proj)

## Compute distance between refugee location & ATM and merge it to Refugee loc - using UTM projection 
distance<-gDistance(refugee.location.Proj,atm.location.Proj,byid=T)

refugee.location.sp$Nearest_Con <-apply(distance, 2, min)
refugee.location.sp$ID <- as.vector(apply(distance, 2, function(x) which(x==min(x))))

## merge Refugee & ATM
refugee2atm <-merge( refugee.location.sp, refugee.location.sp, by=C("ID")) 
## plot(refugee2atm)

refugee2atm.full <- merge( refugee.location, refugee2atm,  by.y=c("refugee.location.coords.refugee.location.CaseNo"), by.x=c("CaseNo"))

## Coerce to data.table and adjust structure
setDT(refugee2atm.full)

# refugee2atm.full[, Nearest_Dist := distGeo(matrix(c(Long, Lat), ncol = 2), matrix(c(x, y), ncol = 2))]

refugee2atm.full$Lat <- as.numeric(refugee2atm.full$Lat)
refugee2atm.full$Long <- as.numeric(refugee2atm.full$Long)

refugee2atm.full$x <- as.numeric(refugee2atm.full$x)
refugee2atm.full$y<- as.numeric(refugee2atm.full$y)


############################################
#### merge ATM to TRANSACTIONS

transaction.atm.location <- merge(x=transaction, y=atm.location,  by.y=("ATMIP"), by.x=("LocalIP"))
## Coerce to data.table
setDT(transaction.atm.location)

#transaction.atm.location[, actual_Distance := distGeo(matrix(c(Long.x, Lat.x), ncol = 2),  matrix(c(Long.y, Lat.y), ncol = 2))]


transaction.atm.refugee <- merge( x=transaction.atm.location, y=refugee2atm.full,  by=c("CaseNo"))

#transaction.atm.refugee$DIFf <- (round(transaction.atm.refugee$actual_Distance,3)-round(transaction.atm.refugee$Nearest_Dist,3)) 
#transaction.atm.refugee.distance <- (round(transaction.atm.refugee$actual_Distance,3)-round(transaction.atm.refugee$Nearest_Dist,3)) 

#distance.district <- (data.frame(aggregate(transaction.atm.refugee$Nearest_Dist/1000, by=list(transaction.atm.refugee$District), FUN=mean)))
#distance.district$max  <- aggregate(transaction.atm.refugee$Nearest_Dist, by=list(transaction.atm.refugee$District), FUN=max)

