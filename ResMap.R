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
#  library(reshape2)
#  library(cowplot)
#  library(patchwork)
  library(sp)
  library(rgdal)
  library(raster)
  library(tidyverse)
#  library(ggplot2)
  library(dplyr)
#  library(plotly) # For interactive charts
#  library(odbc)
#  library(DBI)
  library("readxl")
  library("ggsci")
  library(rgdal)
  library(raster)
#  library(rasterVis)
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

################################################################################
# Load in the data
# Wind Speed data from Canada Wind Atlas 
# http://www.windatlas.ca/nav-en.php?no=46&field=EU&height=80&season=ANU
################################################################################
wind_profile <- readRDS("WindAtlas_Data_0.05")
colnames(wind_profile) <- c('Latitude', 'Longitude', 'Wind')

################################################################################
# Location of operational wind turbines, from Canadian Wind Turbine Database
# https://open.canada.ca/data/en/dataset/79fdad93-9025-49ad-ba16-c26d718cc070
################################################################################
turb_location <- read_excel("Wind_Turbine_Database_FGP.xlsx")
turb_AB <- turb_location %>%
  filter(Province=="Alberta")

################################################################################
# Location of wind farms in AESO queue. Long and Lat were determined manually 
# using Google Maps
# https://www.aeso.ca/grid/projects/connection-project-reporting/
################################################################################
res_pot <- read_excel("AESO_Connection_List.xlsx")
turb_pot <- res_pot %>%
  filter(Technology=="Wind")
turb_pot$Status <- "Planning"
turb_pot <- subset(turb_pot, select = -Technology)

################################################################################
# Potential locations
################################################################################
pot <- read_excel("Potential_Sim.xlsx")

################################################################################
# Summarizes the active wind farms.
################################################################################
plant <- turb_AB %>%
  group_by(`Project name`, `Total project capacity (MW)`) %>%
  summarise(Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
  rename(Capacity = `Total project capacity (MW)`)
plant$Status <- "Active"

################################################################################
# Combine the active and planned wind farms
################################################################################
wind_farm <- rbind(plant,turb_pot)
wind_sim <- rbind(plant,turb_pot,pot)

################################################################################
#Level 1 shows provinces, while level 2 shows individual counties
################################################################################
can_level1 = getData("GADM", country = "CA", level = 1)

WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
canada_level1_ellipsoid = spTransform(can_level1, WGS84)

alberta_ellipsoid1 = 
  canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]

################################################################################
# Excludes any points outside the province
################################################################################
#inout = over(
#  SpatialPoints(wind_profile[,c("Longitude","Latitude")],proj4string=CRS(projection(alberta_ellipsoid1))),
#  as(alberta_ellipsoid1,"SpatialPolygons")
#)

active <- wind_farm %>%
  filter(Status == "Active")

################################################################################
################################################################################
# Map of Alberta with wind speeds
################################################################################
################################################################################

AB <- ggplot() + 
  geom_tile(data = wind_profile, 
            aes(x = Longitude, y = Latitude, fill = Wind)) +
  geom_polygon(data = alberta_ellipsoid1, 
               aes(x = long, y = lat, group = group), 
               fill = "transparent", colour = "black") +
  scale_fill_gradientn(colors = matlab.like2(100),
                       limits=c(3,10),oob=squish, name = "Mean Wind Speed \nat 80m height \n(m/s)") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent")) 

################################################################################
################################################################################
# Map of Alberta with active wind farms
################################################################################
################################################################################
Act_wind <- AB + geom_point(data = active,
                aes(x= Longitude, y = Latitude, size = Capacity), 
                shape = 16, color = "black") +
  ggtitle("Active Wind Farms") +
  theme(plot.title = element_text(size=18, hjust = 0.5, vjust=-5))

################################################################################
################################################################################
# Map of Alberta with active and planned wind farms
################################################################################
################################################################################

AESO_wind <- AB + geom_point(data = wind_farm,
                aes(x= Longitude, y = Latitude, size = Capacity, shape = Status, color = Status)) + 
  scale_shape_manual(values = c(16,18), labels = c("Active","AESO Planning")) +
  scale_color_manual(values = c("black", "grey39"), 
                     labels = c("Active","AESO Planning")) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  ggtitle("Active and Planned Wind Farms") +
  theme(plot.title = element_text(size=18, hjust = 0.5, vjust=-5))

################################################################################
# Save map as png
################################################################################

ggsave(path = "images", filename = "windfarmlocations.png", bg = "transparent")

################################################################################
################################################################################
# Map of Alberta with active and planned wind farms
################################################################################
################################################################################

Sim_wind <- AB + geom_point(data = wind_sim,
                             aes(x= Longitude, y = Latitude, size = Capacity, shape = Status, color = Status)) + 
  scale_shape_manual(values = c(16,18,17), labels = c("Active","AESO Planning","Simulated")) +
  scale_color_manual(values = c("black", "grey39","red4"), 
                     labels = c("Active","AESO Planning","Simulated")) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  ggtitle("Active, Planned, & Potential \nWind Farms") +
  theme(plot.title = element_text(size=18, hjust = 0.5, vjust=-5))

################################################################################
# Save map as png
################################################################################

ggsave(path = "images", filename = "windfarmpotential.png", bg = "transparent")

################################################################################
# Extract the legend
################################################################################

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

################################################################################
################################################################################
# Plot the 2 maps side by side
################################################################################
################################################################################

wnd_plot2 <- ggarrange(ggarrange(Act_wind + theme(legend.position = "none"),
                                NULL,
                                AESO_wind + theme(legend.position = "none"),
                                nrow=1, widths = c(1,0,1)),
                      g_legend(AESO_wind), 
                      ncol=2, widths=c(6,1))

wnd_plot <- annotate_figure(wnd_plot2, 
                           fig.lab = "Source: Canada Wind Atlas, Canada Wind Turbine Database, AESO Data 
                           Graph by Taylor Pawlenchuk",
                           fig.lab.pos = "bottom.right", 
                           fig.lab.face = "italic", 
                           fig.lab.size = 8)

################################################################################
# Save map as png
################################################################################

ggsave(path = "images", filename = "windfarmlocationsdouble.png", bg = "transparent")

################################################################################
################################################################################
# Map of Canada
################################################################################
################################################################################

ggplot() + 
  geom_polygon(data = canada_level1_ellipsoid, 
               aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  geom_point(data = turb_location, 
             aes(x = Longitude, y = Latitude), color = "forestgreen", size = 1) + 
  theme_void()
