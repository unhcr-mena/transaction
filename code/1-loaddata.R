#################################################################
### Load and prepare data
#################################################################


source("code/0-packages.R")


#setwd("D:/R/")
#getwd()



df<-read.csv("data/atmTrans.CSV")
df2<-read.csv("data/HFDV.CSV")
df3<-read.csv("data/Transactions.CSV")

shapearea <- st_read('data/admin1.shp')
plot(shapearea) 
shapearea2 <- st_read('data/admin2.shp')
plot(shapearea2) 


#setwd("D:/R/HTML")
#getwd()


area.points <- fortify(shapearea)
area.points2 <- fortify(shapearea2)

colors <- brewer.pal(12,"Set3")


#rename colum to join tables
colnames(area.points)[5] <- "District"
df_area.points = merge(df, area.points, by="District")

Jordan_Gov <- df_area.points
Amman_District <-subset(df_area.points, District == "Amman")
Irbid_District <-subset(df_area.points, District == "Irbid")
Balqa_District <-subset(df_area.points, District == "Balqa")
Maan_District <-subset(df_area.points, District == "Ma'an")
Zarqa_District <-subset(df_area.points, District == "Zarqa")
Karak_District <-subset(df_area.points, District == "Karak")
Madaba_District <-subset(df_area.points, District == "Madaba")
Aqaba_District <-subset(df_area.points, District == "Aqaba")
Tafiela_District <-subset(df_area.points, District == "Tafiela")
Ajlun_District <-subset(df_area.points, District == "Ajlun")
Mafraq_District <-subset(df_area.points, District == "Mafraq")
Jarash_District <-subset(df_area.points, District == "Jarash")


Jordan_Gov0<-st_as_sf(Jordan_Gov)
District1<-st_as_sf(Amman_District)
District2<-st_as_sf(Irbid_District)
District3<-st_as_sf(Balqa_District)
District4<-st_as_sf(Maan_District)
District5<-st_as_sf(Zarqa_District)
District6<-st_as_sf(Karak_District)
District7<-st_as_sf(Madaba_District)
District8<-st_as_sf(Aqaba_District)
District9<-st_as_sf(Tafiela_District)
District10<-st_as_sf(Ajlun_District)
District11<-st_as_sf(Mafraq_District)
District12<-st_as_sf(Jarash_District)

utm10n<-CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
wgs84<-CRS("+proj=longlat +zone=37 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

####nearest ATM

df.coords <- data.frame  (df$Lat, df$Long, df$Address)
df.bc<-SpatialPointsDataFrame(data.frame(x=df.coords$df.Long,y =df.coords$df.Lat),data=data.frame(ID=1:108, address=df$Address), proj4string = wgs84)
df.bc.Proj<-spTransform(df.bc,utm10n)


df2.coords <- data.frame  (df2$Lat, df2$Long, df2$CaseNo)
df2.con<-SpatialPointsDataFrame(data.frame(x=df2.coords$df2.Long,y =df2.coords$df2.Lat),data=data.frame(df2.coords$df2.CaseNo), proj4string = wgs84)
df2.con.conProj<-spTransform(df2.con,utm10n)

distance<-gDistance(df2.con.conProj,df.bc.Proj,byid=T)
min.constructionDistance<-apply(distance, 2, min)


df2.con$Nearest_Con<-min.constructionDistance
df2.con$ID<-as.vector(apply(distance, 2, function(x) which(x==min(x))))

df2.conFINAL<-merge( df2.con,df.bc, by=C("ID")) 
df2.conFINAL2<-merge( df2,df2.conFINAL,  by.y=c("df2.coords.df2.CaseNo"), by.x=c("CaseNo"))


setDT(df2.conFINAL2)
df2.conFINAL2[, Nearest_Dist := distGeo(matrix(c(Long, Lat), ncol = 2),
                              matrix(c(x, y), ncol = 2))]

df2.conFINAL2$Lat <- as.numeric(df2.conFINAL2$Lat)
df2.conFINAL2$Long <- as.numeric(df2.conFINAL2$Long)

df2.conFINAL2$x <- as.numeric(df2.conFINAL2$x)
df2.conFINAL2$y<- as.numeric(df2.conFINAL2$y)


####ATM ON TRANSACTIONS

ATM_accessed<-merge( df3,df,  by.y=("ATMIP"), by.x=("LocalIP"))
ATM_accessed2<-ATM_accessed

setDT(ATM_accessed)
ATM_accessed[, actual_Distance := distGeo(matrix(c(Long.x, Lat.x), ncol = 2),
                              matrix(c(Long.y, Lat.y), ncol = 2))]


A<-merge( ATM_accessed,df2.conFINAL2,  by=c("CaseNo"))

A$DIFf<-(round(A$actual_Distance,3)-round(A$Nearest_Dist,3)) 
d<-(round(A$actual_Distance,3)-round(A$Nearest_Dist,3)) 

Gov <-(data.frame(aggregate(A$Nearest_Dist/1000, by=list(A$District), FUN=mean)))
Gov$max  <-aggregate(A$Nearest_Dist, by=list(A$District), FUN=max)

