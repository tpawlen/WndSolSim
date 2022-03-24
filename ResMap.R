# Generates plots based on the different change sets
# Output in GWh vs Time Period
# 
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# January 2022; Last revision: January 18, 2022

{library(rgeos)
  library(maptools)
  library(ggmap) 
  library(ggplot2)
  library(reshape2)
  library(cowplot)
  library(patchwork)
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
  library("ggsci")
  library(rgdal)
  library(raster)
  library(rasterVis)
  library(colorRamps)
#  library("viridis")
#  library(nasapower)
  #To get the solar insolation and other meteorological data 
  library(scales)
  #This one allows us to do things like have nice scales for the legends 
  #on the images we create
}

setwd("D:/Documents/GitHub/AuroraEval")
getwd()

wind_profile <- readRDS("WindAtlas_Data_0.05")
colnames(wind_profile) <- c('Latitude', 'Longitude', 'Wind')

turb_location <- read_excel("Wind_Turbine_Database_FGP.xlsx")
turb_AB <- turb_location %>%
  filter(Province=="Alberta")

res_pot <- read_excel("AESO_Connection_List.xlsx")
turb_pot <- res_pot %>%
  filter(Technology=="Wind")

# Summarizes the wind farms.
plant <- turb_AB %>%
  group_by(`Project name`, `Total project capacity (MW)`) %>%
  summarise(Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
  rename(Capacity = `Total project capacity (MW)`)

#Level 1 shows provinces, while level 2 shows individual counties
can_level1 = getData("GADM", country = "CA", level = 1)
#can_level2 = getData("GADM", country = "CA", level =2)

WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
canada_level1_ellipsoid = spTransform(can_level1, WGS84)
#canada_level2_ellipsoid = spTransform(can_level2, WGS84)

alberta_ellipsoid1 = 
  canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]
#alberta_ellipsoid = 
#  canada_level2_ellipsoid[which(canada_level2_ellipsoid$NAME_1 == "Alberta"),]

# Excludes any points outside the province
#inout = over(
#  SpatialPoints(wind_profile[,c("Longitude","Latitude")],proj4string=CRS(projection(alberta_ellipsoid1))),
#  as(alberta_ellipsoid1,"SpatialPolygons")
#)

# Map of Alberta
ggplot() + 
  geom_tile(data = wind_profile,#[!is.na(inout),], 
            aes(x = Longitude, y = Latitude, fill = Wind)) +
  geom_polygon(data = alberta_ellipsoid1, 
               aes(x = long, y = lat, group = group), 
               fill = "transparent", colour = "black") +
  scale_fill_gradientn(colors = matlab.like2(100),
                       limits=c(3,10),oob=squish) +
#  geom_point(data = turb_pot, 
#             aes(x = Longitude, y = Latitude, size = Capacity, shape = 18), 
#             color = "orangered") +
#  geom_point(data = plant, 
#             aes(x = Longitude, y = Latitude, size = Capacity, shape = 16),
#             color = "limegreen") + 
#  scale_y_continuous(limits = c(49, 60.1)) +
#  scale_color_continuous(name = "Average Wind Speed at 80m",
#                         guide = legend)
#  scale_shape_identity(name = "Status",
#                       breaks = c(18, 16),
#                       labels = c("AESO Planning", "Active"),
#                       guide = "legend") +
#  scale_color_identity(name = "Status",
#                       breaks = c("orangered", "limegreen"),
#                       labels = c("AESO Planning", "Active"),
#                       guide = "legend") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent")) 

ggsave(path = "images", filename = "windfarmlocations.png", bg = "transparent")



# Map of Canada
ggplot() + 
  geom_polygon(data = canada_level1_ellipsoid, 
               aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  geom_point(data = turb_location, 
             aes(x = Longitude, y = Latitude), color = "forestgreen", size = 1) + 
  theme_void()
