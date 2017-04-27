

source("code/0-packages.R")


#setwd("D:/R/")
#getwd()

#file1<-"data/atmTrans.CSV"
#file2<-"data/HFDV.CSV"
#file3<-"data/Transactions.CSV"

#load1<-paste0(dir, file1)
#load2<-paste0(dir, file2)
#load3<-paste0(dir, file3)


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
df.bc<-SpatialPointsDataFrame(data.frame(x=df.coords$df.Long,y =df.coords$df.Lat),data=data.frame(ID=1:108, Address=df$Address), proj4string = wgs84)
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




CountTrans1<- sqldf("SELECT LocalIP AS ATMIP, COUNT(DISTINCT TransNo) AS TransCount FROM A GROUP BY LocalIP")
CountTrans2<-merge(CountTrans1, df, by = "ATMIP")

for (i in 1:dim(CountTrans2)[1]){ #will use every line of the attribute table
  pie(CountTrans2$TransCount, init.angle = 90, col=c("red", "yellow"), labels=NA) #this will create the pie chart itself with five collors as we have five columns
  dev.off()
}



pal <- colorNumeric(
  palette = "Greens",
  domain = Gov$x
)

pal2 <- colorNumeric(
  palette = "RdYlBu",
  domain = CountTrans2$TransCount
)


######################################## Distance to the nearest ATM vs actual ATM withdrawal plot ##################################################

p1<- ggplot(A, aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000), colour=(DIFf/1000)))  +geom_point(aes(text = paste('CaseNumber: ', CaseNo, 
                                                                                                                           '</br> Difference(km): ', round(DIFf/1000,2),
                                                                                                                           '</br> Nearest ATM: ', address,
                                                                                                                           '</br> Actual ATM: ', Address,
                                                                                                                           '</br> HVF: ', Form.x,
                                                                                                                           '</br> HVD: ', HVD.x))) + 
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal")+
  geom_hline(aes(yintercept = mean(DIFf/1000)),linetype = 2, label="mean")  +
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))


p2<- p1 +scale_colour_gradient(low="blue", high="red") + scale_y_continuous( breaks = sort(c (seq(0,max(A$Nearest_Dist/1000),50), round(mean(A$DIFf/1000))))) +
  scale_x_continuous( breaks = seq(0,max(A$actual_Distance/1000),50))
plot1<-ggplotly(p2, tooltip = c("text")) %>% config(displayModeBar = F)
#export(plot1, file = "ATM_Plot1.png")
plot1
saveWidget(plot1, file = "out/plot1.html", selfcontained = TRUE)



######################################## Distance to the nearest ATM vs actual ATM withdrawal Hex Plot (all) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEXP1<- ggplot(A, aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Gov")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlot1<-ggplotly(HEXP1, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlot1
saveWidget(HexPlotGov, file = "out/HexPlot1.html", selfcontained = TRUE)


######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Amman) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX1<- ggplot(subset(A, District == "Amman"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Amman")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotAmman<-ggplotly(HEX1, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotAmman
saveWidget(HexPlotAmman, file = "out/HexPlotAmman.html", selfcontained = TRUE)


######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Ajlun) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX2<- ggplot(subset(A, District == "Ajlun"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Ajlun")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))
HexPlotAjlun<-ggplotly(HEX2, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotAjlun
saveWidget(HexPlotAjlun, file = "out/HexPlotAjlun.html", selfcontained = TRUE)


######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Irbid) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX3<- ggplot(subset(A, District == "Irbid"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Irbid")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotIrbid<-ggplotly(HEX3, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotIrbid
saveWidget(HexPlotIrbid, file = "out/HexPlotIrbid.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Balqa) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX4<- ggplot(subset(A, District == "Balqa"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Balqa")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotBalqa<-ggplotly(HEX4, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotBalqa
saveWidget(HexPlotBalqa, file = "out/HexPlotBalqa.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Ma'an) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX5<- ggplot(subset(A, District == "Ma'an"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Ma'an")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotMaan<-ggplotly(HEX5, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotMaan
saveWidget(HexPlotMaan, file = "out/HexPlotMaan.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Zarqa) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX6<- ggplot(subset(A, District == "Zarqa"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Zarqa")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotZarqa<-ggplotly(HEX6, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotZarqa
saveWidget(HexPlotZarqa, file = "out/HexPlotZarqa.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Karak) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX7<- ggplot(subset(A, District == "Karak"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Karak")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotKarak<-ggplotly(HEX7, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotKarak
saveWidget(HexPlotKarak, file = "out/HexPlotKarak.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Madaba) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX8<- ggplot(subset(A, District == "Madaba"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Madaba")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotMadaba<-ggplotly(HEX8, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotMadaba
saveWidget(HexPlotMadaba, file = "out/HexPlotMadaba.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Aqaba) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX9<- ggplot(subset(A, District == "Aqaba"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Aqaba")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotAqaba<-ggplotly(HEX9, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotAqaba
saveWidget(HexPlotAqaba, file = "out/HexPlotAqaba.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Tafiela) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX10<- ggplot(subset(A, District == "Tafiela"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Tafiela")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotTafiela<-ggplotly(HEX10, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotTafiela
saveWidget(HexPlotTafiela, file = "HexPlotTafiela.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Mafraq) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX11<- ggplot(subset(A, District == "Mafraq"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Mafraq")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))



HexPlotMafraq<-ggplotly(HEX11, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotMafraq
saveWidget(HexPlotMafraq, file = "HexPlotMafraq.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal HEX PLOT (Jarash) ##################################################

my_breaks = c(1,10,100,1000,10000)


HEX12<- ggplot(subset(A, District == "Jarash"), aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000)) )+  stat_binhex(aes(fill=..count..), bins=40)+ 
  
  scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) +
  labs(
    x = "Actual Distance (km)",
    y = "Nearest Distance (km)",
    color = "Distance<br />Difference (km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal - Jarash")+
  theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"))

HexPlotJarash<-ggplotly(HEX12, tooltip = c("text")) %>% config(displayModeBar = F)
HexPlotJarash
saveWidget(HexPlotJarash, file = "HexPlotJarash.html", selfcontained = TRUE)

######################################## Distance to the nearest ATM vs actual ATM withdrawal by governorat plot ##################################################


#mean value for Facet:

HlineFacet <- data.frame(District = Gov$Group.1, Mean = Gov$x)
A<-merge(x= HlineFacet, y= A,  by="District")
#HlineFacet$X<- sub("'","",HlineFacet$X)


#mean Facet labeller Function:

plot_labeller <- function(variable,value){
  if (variable=='Amman') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
              
  } else if (variable=='Irbid') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Balqa') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Zarqa') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Karak') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Madaba') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Aqaba') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Ajlun') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Tafiela') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Mafraq') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Jarash') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else {
    return(paste('</br> Maan',
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
  }
}


p3<- ggplot(A, aes(x = (actual_Distance/1000), y = (Nearest_Dist/1000), colour=(DIFf/1000) )) + geom_point(aes(text = paste('CaseNumber: ', CaseNo, 
                                                                                                                       '</br> Difference(km): ', round(DIFf/1000,2),
                                                                                                                       '</br> Nearest ATM: ', address,
                                                                                                                       '</br> Actual ATM: ', Address,
                                                                                                                       '</br> HVF: ', Form.x,
                                                                                                                       '</br> HVD: ', HVD.x))) + 
  
    labs(
    x = "Actual Distance(km)",
    y = "Nearest Distance(km)",
    color = "Distance</br>Difference(km)" ,
    title="Distance to the nearest ATM vs actual ATM withdrawal by governorat") +
    facet_wrap(~District, ncol = 3, scales = "fixed", labeller = as_labeller(plot_labeller),as.table = TRUE) +
    geom_hline(data = HlineFacet, aes(yintercept = Mean), linetype="dashed") +
    scale_y_continuous( breaks = seq(0,max(A$Nearest_Dist/1000),100)) +
    scale_x_continuous( breaks = seq(0,max(A$actual_Distance/1000),100))+
    theme(#plot.margin = unit(c(.7,.7,.7,.7), "cm"),
      strip.text.x = element_text(size = 7, colour = "Black"))

  
p4<- p3 +scale_colour_gradient(low="blue", high="red")
plot2<-ggplotly(p4, tooltip = c("text"))%>%config(displayModeBar = F) 
#export(plot2, file = "ATM_Plot2.png")
plot2
saveWidget(plot2, file = "Plot2.html", selfcontained = TRUE)


########################################  Distance to the nearest ATM vs actual ATM withdrawal by governorat HEX Plot ##################################################


#mean value for Facet:

HlineFacet <- data.frame(District = Gov$Group.1, Mean = Gov$x)
A<-merge(x= HlineFacet, y= A,  by="District")
#HlineFacet$X<- sub("'","",HlineFacet$X)


#mean Facet labeller Function:

plot_labeller <- function(variable,value){
  if (variable=='Amman') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Irbid') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Balqa') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Zarqa') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Karak') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Madaba') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Aqaba') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Ajlun') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Tafiela') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Mafraq') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else if (variable=='Jarash') {
    return(paste('</br>',variable,
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
    
  } else {
    return(paste('</br> Maan',
                 '</br> (mean =', as.character(round(subset(HlineFacet, District == variable)$Mean,2)),')'))
  }
}


HexP2<- ggplot(A, aes((X=actual_Distance/1000), (Y=Nearest_Dist/1000))) + 
  
  labs(
  x = "Actual Distance(km)",
  y = "Nearest Distance(km)",
  color = "Distance</br>Difference(km)" ,
  title="Distance to the nearest ATM vs actual ATM withdrawal by governorat") +
  facet_wrap(~District, ncol = 3, scales = "fixed") +
  
  geom_hex(data = subset(A, District == "Amman"), bins=15) +
  geom_hex(data = subset(A, District == "Irbid"), bins=15)+
  geom_hex(data = subset(A, District == "Balqa"), bins=15)+
  geom_hex(data = subset(A, District == "Ma'an"), bins=15)+
  geom_hex(data = subset(A, District == "Zarqa"), bins=15)+
  geom_hex(data = subset(A, District == "Karak"), bins=15)+
  geom_hex(data = subset(A, District == "Madaba"), bins=15)+
  geom_hex(data = subset(A, District == "Aqaba"), bins=15)+
  geom_hex(data = subset(A, District == "Tafiela"), bins=15)+
  geom_hex(data = subset(A, District == "Ajlun"), bins=15)+
  geom_hex(data = subset(A, District == "Mafraq"), bins=15)+
  geom_hex(data = subset(A, District == "Jarash"), bins=15)+
  
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
    strip.text.x = element_text(size = 7, colour = "Black"))+
  
  scale_fill_gradient(name = "count", trans = "log",
                      breaks = my_breaks, labels = my_breaks, limits=c(1,10000)) 

HexPlot2<-ggplotly(HexP2, tooltip = c("text")) %>% config(displayModeBar = F) 
#export(plot2, file = "ATM_Plot2.png")
HexPlot2
saveWidget(HexPlot2, file = "HexPlot2.html")



########################### map 1 #############################################

pop2<-paste0("<b>Governorate</b>: ",Gov$Group.1,"<br><b>mean</b>: ",round(Gov$x,2), "<br><b>min</b>: ",Gov$min.x, "<br><b></b>")
pop1<-paste0("<b>Distance</b>: ",round(df2.con$Nearest_Con,2),"<br><b>Nearest ATM</b>: ",df2.con$ID, "<br><b>Case Number</b>: ",df2.con$df2.coords.df2.CaseNo)


pop3<-paste('CaseNumber: ', A$CaseNo, 
            '</br> Difference(km): ', round(A$DIFf/1000,2),
            '</br> Nearest ATM: ', A$address,
            '</br> Actual ATM: ', A$Address,
            '</br> HVF: ', A$Form.x,
            '</br> HVD: ', A$HVD.x)

pop4<-paste('ATM Name: ', Jordan_Gov0$ATMPlace, 
            '</br> ATM Address: ', Jordan_Gov0$Address,
            '</br> District: ', Jordan_Gov0$District,
            '</br> ATM IP: ', Jordan_Gov0$ATMIP)
           

map1<-leaflet()%>%addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")%>%setView(36.541, 31.410, zoom = 8) %>%  
 

  addMarkers(data=df_area.points, lng = ~Long, lat = ~Lat, group ='all', label=~ATMPlace, options=markerOptions(opacity=0)) %>%

  addMarkers(data=District1, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District1$ATMPlace, 
                                                                           '</br> ATM Address: ', District1$Address,
                                                                           '</br> District: ', District1$District,
                                                                           '</br> ATM IP: ', District1$ATMIP),  group ="Amman") %>%
  
  addMarkers(data=District2, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District2$ATMPlace, 
                                                                     '</br> ATM Address: ', District2$Address,
                                                                     '</br> District: ', District2$District,
                                                                     '</br> ATM IP: ', District2$ATMIP),  group ="Irbid") %>%
  
  addMarkers(data=District3, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District3$ATMPlace, 
                                                                     '</br> ATM Address: ', District3$Address,
                                                                     '</br> District: ', District3$District,
                                                                     '</br> ATM IP: ', District3$ATMIP),  group ="Balqa") %>%
  
  addMarkers(data=District4, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District4$ATMPlace, 
                                                                     '</br> ATM Address: ', District4$Address,
                                                                     '</br> District: ', District4$District,
                                                                     '</br> ATM IP: ', District4$ATMIP),  group ="Ma'an") %>%
  
  addMarkers(data=District5, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District5$ATMPlace, 
                                                                     '</br> ATM Address: ', District5$Address,
                                                                     '</br> District: ', District5$District,
                                                                     '</br> ATM IP: ', District5$ATMIP),  group ="Zarqa") %>%
  
  addMarkers(data=District6, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District6$ATMPlace, 
                                                                     '</br> ATM Address: ', District6$Address,
                                                                     '</br> District: ', District6$District,
                                                                     '</br> ATM IP: ', District6$ATMIP),  group ="Karak") %>%
  
  addMarkers(data=District7, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District1$ATMPlace, 
                                                                     '</br> ATM Address: ', District1$Address,
                                                                     '</br> District: ', District1$District,
                                                                     '</br> ATM IP: ', District1$ATMIP),  group ="Madaba") %>%
  
  addMarkers(data=District8, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District8$ATMPlace, 
                                                                     '</br> ATM Address: ', District8$Address,
                                                                     '</br> District: ', District8$District,
                                                                     '</br> ATM IP: ', District8$ATMIP),  group ="Aqaba") %>%
  
  addMarkers(data=District9, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District9$ATMPlace, 
                                                                     '</br> ATM Address: ', District9$Address,
                                                                     '</br> District: ', District9$District,
                                                                     '</br> ATM IP: ', District9$ATMIP),  group ="Tafiela") %>%
  
  addMarkers(data=District10, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District10$ATMPlace, 
                                                                      '</br> ATM Address: ', District10$Address,
                                                                      '</br> District: ', District10$District,
                                                                      '</br> ATM IP: ', District10$ATMIP),  group ="Ajlun") %>%
  
  addMarkers(data=District11, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District11$ATMPlace, 
                                                                      '</br> ATM Address: ', District11$Address,
                                                                      '</br> District: ', District11$District,
                                                                      '</br> ATM IP: ', District11$ATMIP),  group ="Mafraq") %>%
  
  addMarkers(data=District12, lng = ~Long, lat = ~Lat,  popup = paste('ATM Name: ', District12$ATMPlace, 
                                                                      '</br> ATM Address: ', District12$Address,
                                                                      '</br> District: ', District12$District,
                                                                      '</br> ATM IP: ', District12$ATMIP),  group ="Jarash") %>%
  


addPolygons(data=District1, color ='black',  fillColor="palegreen", label=~adm1_name, fillOpacity=0.02, weight=1.5, group="Amman") %>%
addPolygons(data=District2, color ='black',fillColor="palegreen", label=~adm1_name, fillOpacity=0.07,  weight=1.5,group="Irbid") %>%
addPolygons(data=District3, color ='black',fillColor="palegreen", label=~adm1_name, fillOpacity=0.07, weight=1.5, group="Balqa") %>%
addPolygons(data=District4, color ='black',fillColor="burlywood", label=~adm1_name, fillOpacity=0.2, weight=1.5, group="Ma'an") %>%
addPolygons(data=District5, color ='black',fillColor="darkseagreen", label=~adm1_name, fillOpacity=0.07, weight=1.5, group="Zarqa") %>%
addPolygons(data=District6, color ='black',fillColor="darkkhaki", label=~adm1_name, fillOpacity=0.1, weight=1.5, group="Karak") %>%
addPolygons(data=District7, color ='black',fillColor="darkseagreen", label=~adm1_name, fillOpacity=0.4, weight=1.5, group="Madaba") %>%
addPolygons(data=District8, color ='black',fillColor="lemonchiffon", label=~adm1_name, fillOpacity=0.1,  weight=1.5,group="Aqaba") %>%
addPolygons(data=District9, color ='black',fillColor="wheat", label=~adm1_name, fillOpacity=0.6, weight=1.5, group="Tafiela") %>%
addPolygons(data=District10, color ='black',fillColor="seagreen", label=~adm1_name, fillOpacity=0.4,  weight=1.5,group="Ajlun") %>%
addPolygons(data=District11, color ='black',fillColor="bisque", label=~adm1_name, fillOpacity=0.1, weight=1.5, group="Mafraq") %>%
addPolygons(data=District12, color ='black',fillColor="lightgreen", label=~adm1_name, fillOpacity=0.4, weight=1.5, group="Jarash") %>%
  
addLayersControl(overlayGroups= c( "Amman", "Irbid", "Balqa", "Ma'an", "Zarqa", "Karak", "Madaba", "Aqaba", "Tafiela","Ajlun", "Mafraq", "Jarash" ), 
options = layersControlOptions(collapsed = TRUE), position = 'topleft') %>%

addSearchMarker( targetGroup = "all",options = searchMarkersOptions(zoom=12, openPopup = TRUE, position = 'topright'))
map1
saveWidget(map1, file = "map1.html", selfcontained = TRUE)

########################### map 2 #############################################

map2<-leaflet()%>%addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")%>%setView(36.541, 31.410, zoom = 8) %>%  
addPolygons(data=Jordan_Gov0, color ='black', fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.5, weight=1.5) %>%
addLegend("bottomright", pal = pal, values = Gov$x, bins=5,
            title = "Actual Distance</br>Mean",
            labFormat = labelFormat(suffix = " Km", digits=3),
            opacity = 1)%>%
  
addCircleMarkers(data=A, radius= 2, color='blue', opacity= 100, lng = ~Long, lat = ~Lat, popup = paste('CaseNumber: ', A$CaseNo, 
                                                                                               '</br> Difference(km): ', round(A$DIFf/1000,2),
                                                                                               '</br> Nearest ATM: ', A$address,
                                                                                               '</br> Actual ATM: ', A$Address,
                                                                                               '</br> HVF: ', A$Form.x,
                                                                                               '</br> HVD: ', A$HVD.x) )%>%
  
addCircleMarkers(data=df_area.points, radius= 2, color='black', opacity= 100, lng = ~Long, lat = ~Lat, popup = paste('ATM Name: ', df_area.points$ATMPlace, 
                                                                                                                     '</br> ATM Address: ', df_area.points$Address,
                                                                                                                     '</br> District: ', df_area.points$District,
                                                                                                                     '</br> ATM IP: ', df_area.points$ATMIP) )%>%  
  
addPolygons(color = heat.colors(NLEV, NULL)[LEVS])


#addMarkers(data=A, lng = ~Long, lat = ~Lat, group ='all',label=~CaseNo,  options=markerOptions(opacity=0)) %>%  
#addSearchMarker( targetGroup = "all",options = searchMarkersOptions(zoom=12, openPopup = TRUE, position = 'topright'))  
map2

saveWidget(map2, file = "map2.html", selfcontained = TRUE)

########################### map 3 ############################################


map3<-leaflet()%>%addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png")%>%setView(36.541, 31.410, zoom = 8) %>%  
  addCircleMarkers(data=CountTrans2,lng = ~Long, lat = ~Lat,  color ='black', fillColor=~pal2(CountTrans2$TransCount),   popup= paste('ATM Name: ', CountTrans2$ATMPlace, 
                                                                                                                                            '</br> ATM Address: ', CountTrans2$Address,
                                                                                                                                     '</br> District: ', CountTrans2$District,
                                                                                                                                            '</br> ATM IP: ', CountTrans2$ATMIP,
                                                                                                                                            '</br> Number of Transactions: ', CountTrans2$TransCount)  , fillOpacity=0.7, weight=0, radius=CountTrans2$TransCount/50 , labelOptions = labelOptions(noHide = T , direction='center', textOnly = T)) %>%
  addLegend("bottomright", pal = pal2, values = CountTrans2$TransCount, bins=3,
            title = "Actual Distance</br>Mean",
            labFormat = labelFormat(suffix = " Trans", digits=3),
            opacity = 1)

pie(c(0,1,2,3), init.angle = 90, col=c("red"), labels=NA) 

map3

mapPies( CountTrans2,nameX=~Long, nameY=~Lat, nameZs=c(names(CountTrans2)[2], names(CountTrans2)[2]),mapRegion='asia', oceanCol = "lightseagreen",
         landCol = "gray50")




map2<-leaflet()%>%addTiles()%>%setView(36.541, 31.410, zoom = 8) %>%  
  
  addCircleMarkers(data=mergedf, weight = 0, lng = ~Long, lat = ~Lat, radius = (mergedf$TransCount), fillOpacity = 0.5, label = mergedf$TransCount, color = ~pal(mergedf$TransCount),  popup = mergedf$TransCount) %>% 
  
  addLegend("bottomleft", pal = pal, values=sort(mergedf$TransCount), title = "Avg.Ratings",labFormat = labelFormat(prefix = ""), opacity = 0.5)

map2








########################### map 3 ############################################



map3<-leaflet()%>%addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")%>%setView(36.541, 31.410, zoom = 8) %>%  
  
addPolygons(data=Amman_District1, color ='black', opacity=0.01, fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.01, weight=1.5, group="Amman") %>%
addPolygons(data=Irbid_District2, color ='black',opacity=0.05,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.05,  weight=1.5,group="Irbid") %>%
addPolygons(data=Balqa_District3, color ='black',opacity=0.05,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.05, weight=1.5, group="Balqa") %>%
addPolygons(data=Maan_District4, color ='black',opacity=0.2,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.2, weight=1.5, group="Ma'an") %>%
addPolygons(data=Zarqa_District5, color ='black',opacity=0.08,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.08, weight=1.5, group="Zarqa") %>%
addPolygons(data=Karak_District6, color ='black',opacity=0.2,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.2, weight=1.5, group="Karak") %>%
addPolygons(data=Madaba_District7, color ='black',opacity=0.2,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.2, weight=1.5, group="Madaba") %>%
addPolygons(data=Aqaba_District8, color ='black',opacity=0.1,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.1,  weight=1.5,group="Aqaba") %>%
addPolygons(data=Tafiela_District9, color ='black',opacity=0.2,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.2, weight=1.5, group="Tafiela") %>%
addPolygons(data=Ajlun_District10, color ='black',opacity=0.2,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.2,  weight=1.5,group="Ajlun") %>%
addPolygons(data=Mafraq_District11, color ='black',opacity=0.2,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.2, weight=1.5, group="Mafraq") %>%
addPolygons(data=Jarash_District12, color ='black',opacity=0.2,fillColor=~pal(Gov$x), label=~adm1_name, fillOpacity=0.2, weight=1.5, group="Jarash") %>%

addCircleMarkers(data=df2, radius= 1, color='red', opacity= 100, lng = ~Long, lat = ~Lat, popup=~pop1 )%>%
addCircleMarkers(data=df_area.points, radius= 1, color='black', opacity= 100, lng = ~Long, lat = ~Lat, popup = ~Address, label=~Address )

for(i in 1:nrow(df2.conFINAL2)){
map3 <- addPolylines(map3, lat = as.numeric(df2.conFINAL2[i, c(4, 11)]), lng = as.numeric(df2.conFINAL2[i, c(5, 10)]),  weight=2)

}
saveWidget(map3, file = "map1.html", selfcontained =TRUE)


