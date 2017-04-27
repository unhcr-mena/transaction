## install packages if not yet installed

packages <- c( # "leaflet", # We need latest leaflet package from Github, as CRAN package is too old.
                "devtools","viridis",
                "RColorBrewer",
                "plyr",
                "htmlwidgets",
                "htmltools",
               # "leaflet.extras", ## need to be installed from Github
                "rgdal",
                "sf",
                "sp",
                "spatialEco",
                "rgeos",
                "ggplot2",
                "data.table",
                "dplyr",
                "plotly",
                "webshot",
                "colorspace",
                "geosphere")

## identify packages not installed yet
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

rm(packages)

library(devtools)

# We need latest leaflet package from Github, as CRAN package is too old.
#devtools::install_github('rstudio/leaflet')
#devtools::install_github('bhaskarvk/leaflet.extras')


### Load packages

library(leaflet)
library(RColorBrewer)
library(plyr)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
library(rgdal)
library(sf)
library(sp)
library(spatialEco)
library(rgeos)
library(ggplot2)
library(data.table)
library(dplyr)
library(plotly)
library(webshot)
library(colorspace)
library(geosphere)

#install (http://phantomjs.org)