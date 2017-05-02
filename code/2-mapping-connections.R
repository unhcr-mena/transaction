### Mapping connections

## Based on http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
## http://www.injuryepi.org/resources/R/grtCrclTutorial.pdf 

# The objective is to draw a map displaying connection between refugee location and ATm
# based on # of Refugee transaction on each ATM

source("code/1-loaddata2.R")

## needed library
library(maps)
library(geosphere)


### all required data is in transaction.atm.refugee
#names(transaction.atm.refugee)

## here we have coordinaties for ATM
# "Lat.y", "Long.y"
## And coordinates for Refugees
# "Lat.x", "Long.x"

atm.refugee <- transaction.atm.refugee[ ,c("Lat.y", "Long.y","Lat.x", "Long.x", "CaseNo")]
atm.refugee$num <- rownames(atm.refugee)
## Now counting
atm.refugee2 <- aggregate(num ~ Lat.y + Long.y + Lat.x + Long.x,atm.refugee,FUN=NROW)
levels(as.factor(atm.refugee2$num))

### We just order connection from least to greatest # of transactions
atm.refugee2.maxnum <- max(atm.refugee2$num)
atm.refugee2 <- atm.refugee2[order(atm.refugee2$num),]


## We can scale the color of the lines to reflect the number of transaction using the
# colorRampPalette() to create a function that will interpolate a spectrum of color
# based on a base color (here black). 
## setting up a few palette
# pal <- colorRampPalette(c("#f2f2f2", "black"))
# pal <- colorRampPalette(c("#f2f2f2", "red"))
pal <- colorRampPalette(c("#333333", "white", "#1292db"))
colors <- pal(100)

png("out/map.png", width = 1980, height = 1080, bg = "white", res = 300)
# For jordan
xlim <- c(34.178, 39.825)
ylim <- c(29.104, 33.413)
map( "world", 
     col="#f2f2f2", # colour of the regions (usually landmasses);
     fill=TRUE, 
     bg="#191980", # background colour;
     lwd=0.1, # the line width
     resolution = 1000,
     xlim=xlim, ylim=ylim)

## loop around location to get the connecting line
#for (i in 1:nrow(atm.refugee2)) {
for (i in 1:24400) {  
  ### now start building connecting lines using function gcIntermediate
  # Intermediate points on a great circle
  # Pass on the latitude and longitude of the two connecting points, 
  # and gcIntermediate() spits out the coordinates of points on the circle. 
  connectingline <- gcIntermediate(c(atm.refugee2[i , "Long.y" ], atm.refugee2[i , "Lat.y" ]),
                                   c(atm.refugee2[i , "Long.x"], atm.refugee2[i , "Lat.x" ]),
                                   n=100, # The desired number of intermediate points
                                   addStartEnd=TRUE,
                                   breakAtDateLine = T, #breakAtDateLine needs to be set at true: this separates the list 
                                   # when line attempts to cross the shortest path (e.g. east from Australia) and breaks
                                   # to create a straight line across the plot
                                   sp=TRUE # this output a spatial dataframe
  )
  
  colindex <- round( (atm.refugee2[i ,]$num / atm.refugee2.maxnum) * length(colors) )
  
  if (is.list(connectingline)) {
    connectingline1 <- connectingline[[1]] 
    connectingline2 <- connectingline[[2]]
    lines(connectingline1, col=colors[colindex], lwd = 0.05)
    lines(connectingline2, col=colors[colindex], lwd = 0.05)
  } else {
    lines(connectingline, col=colors[colindex], lwd = 0.05)
  }
}

dev.off()

plot(connectingline)
str(connectingline)

#### Now trying to create a similar object ready to be plot with ggplot2
## http://robinlovelace.net/2014/06/22/great-circles-in-rworldmap-ggplot2.html

## Adding great circle lines in ggplot2 is similar, but we must save all of the coordinates of the paths
# in advance before plotting, because ggplot2 like to add all its layers in one function: 
# you cannot iteratively add to the map using a for loop as we did in the base graphics example above.

# To create the for loop, first create a data frame of a single line. 
# The iterate for all zones and use rbind to place one data frame on top of the next:

paths <- gcIntermediate(c(atm.refugee2[1 , "Long.y" ], atm.refugee2[1 , "Lat.y" ]),
                        c(atm.refugee2[1 , "Long.x"], atm.refugee2[1 , "Lat.x" ]))
paths <- data.frame(paths)
paths$group <- 1

## Now create the paths... will take some time...
#for(i in 2:nrow(atm.refugee2)){

for(i in 2:200){
  paths.tmp <- gcIntermediate(c(atm.refugee2[i , "Long.y" ], atm.refugee2[i , "Lat.y" ]),
                              c(atm.refugee2[i , "Long.x"], atm.refugee2[i , "Lat.x" ]),
                              n=5, # The desired number of intermediate points
                              addStartEnd=TRUE)
  paths.tmp <- data.frame(paths.tmp)
  paths.tmp$group <- i
  paths <- rbind(paths, paths.tmp)
}

plot(paths)

# To plot multiple paths, we can use the geom_segment command. 
# Before plotting the lines on the map, it’s sometimes best to first plot them on their own
# to ensure that everything is working. 

#Note the use of the command ggplot(), which initiates an empty ggplot2 instances, 
#ready to be filled with layers. This is more flexible than stating the data at the outset.
# For Amman
xlim <- c(35.5689, 36.6470)
ylim <- c(31.6902, 32.2064)
ggplot() + 
  geom_polygon(data = map_data("world"), aes(x=long, y=lat, group=group), 
               fill = "green", colour="black") + 
  scale_x_continuous(limits = xlim) +
  scale_y_continuous(limits = ylim) +
  geom_path(data = paths, aes(lon, lat , group = group)) +
  geom_point(data = atm.location, aes(x=Long, y=Lat)) +
  theme(panel.background = element_rect(fill = 'lightblue'))


##################################################################
### Theme map style Facebook https://github.com/ricardo-bion/medium_visualization
# ggplot2 map themes 
theme_map <- function(base_size = 12) {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks.length   =   unit(0,"cm"),
      panel.grid.major= element_line(colour="black", size=0.3, linetype=1),
      # panel.grid.minor=element_blank(),
      # panel.grid      =   element_blank(),
      panel.background = element_rect(fill = "white", colour=NA),
      panel.margin    =   unit(0,"lines"),
      plot.title=element_text(vjust=1),
      # strip.background=element_rect(fill="grey90", colour="black", size=0.3),
      # strip.text=element_text(),
      plot.margin     =   unit(c(0,0,0,0),"lines"),
      plot.background = element_blank(),
      legend.justification = c(0, 0),
      legend.position = c(0, 0),
      complete = TRUE
    )
}

# For Amman
xlim <- c(35.5689, 36.6470)
ylim <- c(31.6902, 32.2064)
## Amman center for Projection 
# to see the range of projections available using this method, try ?mapproject
# remove fill as this clearly causes problems:
m <- ggplot() + 
  geom_path(data = map_data("world"), aes(x=long, y=lat, group=group), colour="black") + 
  scale_x_continuous(limits = xlim) +
  scale_y_continuous(limits = ylim) +
  geom_path(data = paths, aes(lon, lat , group = group), colour="red") +
  theme_map()

## testing a few projections.
m + coord_map("bicentric", lon = 0)
m + coord_map("bonne", lat= 0)
m + coord_map("ortho", orientation=c(31.96023, 35.92209, 0)) # for ortho maps centered on Amman

