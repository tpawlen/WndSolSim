# Generates plots based on the different change sets
# Output in GWh vs Time Period
# 
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# January 2022; Last revision: January 18, 2022

{library(rgeos)
  library(maptools)
  library(ggmap) 
  library(sp)
  library(rgdal)
  library(raster)
  library(tidyverse)
  library(ggplot2)
  library(dplyr)
  library(plotly) # For interactive charts
  library(odbc)
  library(DBI)
  library("readxl")
  library(nasapower)
  #To get the solar insolation and other meteorological data 
  library(scales)
  #This one allows us to do things like have nice scales for the legends 
  #on the images we create
}

setwd("D:/Documents/Education/Masters Degree/Aurora/R Code")
getwd()

turb_location <- read_excel("Wind_Turbine_Database_FGP.xlsx")
turb_AB <- turb_location %>%
  filter(Province=="Alberta")

#Level 1 shows provinces, while level 2 shows individual counties
can_level1 = getData("GADM", country = "CA", level = 1)
can_level2 = getData("GADM", country = "CA", level =2)

WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
canada_level1_ellipsoid = spTransform(can_level1, WGS84)
canada_level2_ellipsoid = spTransform(can_level2, WGS84)

alberta_ellipsoid1 = 
  canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]
alberta_ellipsoid = 
  canada_level2_ellipsoid[which(canada_level2_ellipsoid$NAME_1 == "Alberta"),]

# Map of Alberta
ggplot() + 
  geom_polygon(data = alberta_ellipsoid1, 
               aes(x = long, y = lat, group = group), 
               fill = "#6B7B83", color = "black") +
  geom_point(data = turb_AB, 
             aes(x = Longitude, y = Latitude), color = "#00a9ac", size = 1) + 
    theme_void()

# Map of Canada
ggplot() + 
  geom_polygon(data = canada_level1_ellipsoid, 
               aes(x = long, y = lat, group = group), 
               fill = "#6B7B83", color = "black") +
  geom_point(data = turb_location, 
             aes(x = Longitude, y = Latitude), color = "#00a9ac", size = 1) + 
  theme_void()
