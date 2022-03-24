# data processing
library(foreign) # for reading dbfs
library(dplyr)
library(magrittr)
library(tidyr) 
library(ggplot2)
library(gridExtra) # to arrange grid plots

# spatial
library(raster)
library(rasterVis)
library(rgdal)
library(dismo) #map raster on Google Map

# data location
url<-"http://qgis.org/downloads/data/qgis_sample_data.zip"

mydir<-"D:\\junk"
temp<-tempfile(tmpdir=mydir, fileext=".zip")
download.file(url, temp)
unzip(temp, exdir=mydir)
unlink(temp) #delete the zip file

# Grab the name of the file path
fpath<-list.files(path = mydir, full.names = TRUE, pattern = "qgis_sample_data")
fpath<-gsub("/", "\\\\", fpath)

# Read in landcover raster
landusepath<-paste(fpath, "raster\\landcover.img", sep="\\")
landuse.raw<-raster(landusepath)
plot(landuse.raw, axes=FALSE)

vals<-unique(values(landuse.raw))
recl<-matrix(c(vals, c(0, rep(1, 6), 9, 1,1, 13)),ncol=2)
recl
##       [,1] [,2]
##  [1,]    0    0
##  [2,]   12    1
##  [3,]    9    1
##  [4,]    8    1
##  [5,]   10    1
##  [6,]    7    1
##  [7,]    6    1
##  [8,]    1    9
##  [9,]    5    1
## [10,]    4    1
## [11,]   13   13

landuse<-reclassify(landuse.raw, rcl=recl)
plot(landuse, legend=FALSE, axes=FALSE)

# Regions polygon shapefile
regionpath<-paste(fpath, "shapefiles", sep="\\")
region<-readOGR(dsn=regionpath, layer="regions") 

# we will use ggplot to plot the regions
ggplot()+geom_polygon(data=region,  aes(x=long, y=lat, group=group), 
                      fill="cadetblue", color="grey")+
  coord_equal()+xlim(c(-5000000, 5000000))+ylim(c(1000000, 8000000))

# Create a subset with our regions of interest
myregions<-c( "Anchorage", "Yukon-Koyukuk", "North Slope")
region.sm<-region[region$NAME_2 %in% myregions,]

# crop, rasterize and mask 
cr<-crop(landuse, region.sm)
fr<-rasterize(region.sm, cr)
lr<-mask(x=cr, mask=fr)

# centroids for the labels
centroids<-cbind(coordinates(region.sm), region.sm@data)
names(centroids)[1:2]<-c("x", "y")

# use gplot (not ggplot) from rasterVis
# geom_tile adds the raster, geom_polygon adds the regions
# geom_text adds the labels at the centroids
gplot(lr)+
  geom_tile(aes(fill=factor(value, labels=c("Water", "Green", "Shrubland", "Urban"))), alpha=0.8)#+
#  scale_fill_manual(values = c("steelblue3", "forestgreen", "ghostwhite", "red"),
#                    name= "Land use code")#+
#  geom_polygon(data=region.sm, aes(x=long, y=lat, group=group), 
#               fill=NA,color="grey50", size=1)+
 # geom_text(data=centroids, aes(x=x, y=y, label=NAME_2), fontface="bold")+
  #coord_equal()
