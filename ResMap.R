# Generates maps of Alberta showing the wind speeds, with wind farm locations
# identified
# 
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# January 2022; Last revision: October 20, 2022
{
{library(rgeos)
#  library(maptools)
#  library(ggmap) 
  library(ggplot2)
  library(ggpubr)
#  library(reshape2)
  library(cowplot)
#  library(patchwork)
#  library(sp)
#  library(rgdal)
  library(raster)
  library(tidyverse)
  library(dplyr)
#  library(plotly) # For interactive charts
#  library(odbc)
#  library(DBI)
  library("readxl")
#  library("ggsci")
#  library(rgdal)
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
# Set legend text size
################################################################################
legTitle <- 15
legText <- 12

################################################################################
# Load in the data
# Wind Speed data from Canada Wind Atlas 
# http://www.windatlas.ca/nav-en.php?no=46&field=EU&height=80&season=ANU
################################################################################
wind_profile <- readRDS("WindAtlas_Data00_0.05")
colnames(wind_profile) <- c('Latitude', 'Longitude', 'Wind')

{
wind_profile00 <- readRDS("WindAtlas_Data00_0.05")
colnames(wind_profile00) <- c('Latitude', 'Longitude', 'Wind')
wind_profile01 <- readRDS("WindAtlas_Data01_0.05")
colnames(wind_profile01) <- c('Latitude', 'Longitude', 'Wind')
wind_profile02 <- readRDS("WindAtlas_Data02_0.05")
colnames(wind_profile02) <- c('Latitude', 'Longitude', 'Wind')
wind_profile03 <- readRDS("WindAtlas_Data03_0.05")
colnames(wind_profile03) <- c('Latitude', 'Longitude', 'Wind')
wind_profile04 <- readRDS("WindAtlas_Data04_0.05")
colnames(wind_profile04) <- c('Latitude', 'Longitude', 'Wind')
wind_profile10 <- readRDS("WindAtlas_Data10_0.05")
colnames(wind_profile10) <- c('Latitude', 'Longitude', 'Wind')
wind_profile11 <- readRDS("WindAtlas_Data11_0.05")
colnames(wind_profile11) <- c('Latitude', 'Longitude', 'Wind')
wind_profile12 <- readRDS("WindAtlas_Data12_0.05")
colnames(wind_profile12) <- c('Latitude', 'Longitude', 'Wind')
wind_profile13 <- readRDS("WindAtlas_Data13_0.05")
colnames(wind_profile13) <- c('Latitude', 'Longitude', 'Wind')
wind_profile14 <- readRDS("WindAtlas_Data14_0.05")
colnames(wind_profile14) <- c('Latitude', 'Longitude', 'Wind')
wind_profile20 <- readRDS("WindAtlas_Data20_0.05")
colnames(wind_profile20) <- c('Latitude', 'Longitude', 'Wind')
wind_profile21 <- readRDS("WindAtlas_Data21_0.05")
colnames(wind_profile21) <- c('Latitude', 'Longitude', 'Wind')
wind_profile22 <- readRDS("WindAtlas_Data22_0.05")
colnames(wind_profile22) <- c('Latitude', 'Longitude', 'Wind')
wind_profile23 <- readRDS("WindAtlas_Data23_0.05")
colnames(wind_profile23) <- c('Latitude', 'Longitude', 'Wind')
wind_profile24 <- readRDS("WindAtlas_Data24_0.05")
colnames(wind_profile24) <- c('Latitude', 'Longitude', 'Wind')
wind_profile30 <- readRDS("WindAtlas_Data30_0.05")
colnames(wind_profile30) <- c('Latitude', 'Longitude', 'Wind')
wind_profile31 <- readRDS("WindAtlas_Data31_0.05")
colnames(wind_profile31) <- c('Latitude', 'Longitude', 'Wind')
wind_profile32 <- readRDS("WindAtlas_Data32_0.05")
colnames(wind_profile32) <- c('Latitude', 'Longitude', 'Wind')
wind_profile33 <- readRDS("WindAtlas_Data33_0.05")
colnames(wind_profile33) <- c('Latitude', 'Longitude', 'Wind')
wind_profile40 <- readRDS("WindAtlas_Data40_0.05")
colnames(wind_profile40) <- c('Latitude', 'Longitude', 'Wind')
wind_profile44 <- readRDS("WindAtlas_Data44_0.05")
colnames(wind_profile44) <- c('Latitude', 'Longitude', 'Wind')

wind_profile <- rbind(wind_profile00, wind_profile01, wind_profile02, 
                      wind_profile03, wind_profile04, wind_profile10, 
                      wind_profile11, wind_profile12, wind_profile13, 
                      wind_profile14, wind_profile20, wind_profile21, 
                      wind_profile22, wind_profile23, wind_profile24, 
                      wind_profile30, wind_profile31, wind_profile32,
                      wind_profile33, wind_profile34, wind_profile40, 
                      wind_profile41, wind_profile42, wind_profile43,
                      wind_profile44)
}

{
################################################################################
# Location of operational wind turbines, from Canadian Wind Turbine Database
# https://open.canada.ca/data/en/dataset/79fdad93-9025-49ad-ba16-c26d718cc070
################################################################################
#turb_location <- read_excel("Wind_Turbine_Database_FGP.xlsx")
#turb_AB <- turb_location %>%
#  filter(Province=="Alberta")

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
wind_Aurora <- read_excel("Potential_Sim.xlsx")
pot <- read_excel("Potential_Sim.xlsx") %>%
  filter(Status == "Potential")

################################################################################
# Summarizes the active wind farms.
################################################################################
#plant <- turb_AB %>%
#  group_by(`Project name`, `Total project capacity (MW)`) %>%
#  summarise(Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
#  rename(Capacity = `Total project capacity (MW)`)
#plant$Status <- "Active"

################################################################################
# Combine the active and planned wind farms
################################################################################
wind_farm <- rbind(plant,turb_pot)
wind_sim <- rbind(plant,turb_pot,pot)
}

################################################################################
#Level 1 shows provinces, while level 2 shows individual counties
#When getData is removed, use geodata package instead
################################################################################
{can_level1 = getData("GADM", country = "CA", level = 1)

WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
canada_level1_ellipsoid = spTransform(can_level1, WGS84)

alberta_ellipsoid1 = 
  canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]
}
################################################################################
# Excludes any points outside the province
################################################################################
#inout = over(
#  SpatialPoints(wind_profile[,c("Longitude","Latitude")],proj4string=CRS(projection(alberta_ellipsoid1))),
#  as(alberta_ellipsoid1,"SpatialPolygons")
#)

active <- wind_Aurora %>%
  filter(Status == "Active") %>%
  na.omit()
simple <- wind_sim %>%
  filter(Status != "Proposed")

#scale_color_Aurora <- function(...){
#  ggplot2:::manual_scale(
#    'fill', 
#    values = setNames(c("black", "grey39","red4"), 
#                      c("Active","Queue","Potential")), 
#    ...
#  )
#}

#scale_shape_Aurora <- function(...){
#  ggplot2:::manual_scale(
#    'fill', 
#    values = setNames(c(16,18,17), 
#                      c("Active","Queue","Potential")), 
#    ...
#  )
#}

################################################################################
# Calculates the correlation to the rest of the fleet
################################################################################

corm <- "pearson" # Define correlation method ("pearson", "kendall", "spearman")

# Installations since 2019
#post2019 <- c("CRR2","CYP1","CYP2","FMG1","HHW1","HLD1","JNR1","JNR2","JNR3",
#              "RIV1","RTL1","WHE1","WHT1","WHT2","WRW1")

setwd("D:/Documents/Education/Masters Degree/Datasets/Market")
load("nrgstream_gen.RData") 
nrgstream_gen <- nrgstream_gen %>% rename(time=Time)

nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 

corrected <- nrgstream_gen %>%
  filter(is.na(Latitude)) %>%
  mutate(Latitude=case_when(grepl("BRD1",ID) ~ 49.842735,
                            grepl("BUR1",ID) ~ 49.814877,
                            grepl("CLR",ID) ~ 50.032911,
                            grepl("CLY",ID) ~ 49.840967,
                            grepl("CHP1",ID) ~ 50.22189,
                            grepl("COL1",ID) ~ 49.833218,
                            grepl("CRD",ID) ~ 49.807,
                            grepl("CRR2",ID) ~ 49.55891,
                            grepl("FMG1",ID) ~ 49.66334,
                            grepl("KKP",ID) ~ 53.469986,
                            grepl("MON1",ID) ~ 49.833144,
                            grepl("NMK1",ID) ~ 51.026118,
                            grepl("RIV1",ID) ~ 49.53245,
                            grepl("STR",ID) ~ 51.033273,
                            grepl("TVS1",ID) ~ 50.27324,
                            grepl("VCN1",ID) ~ 50.0975,
                            grepl("VXH1",ID) ~ 50.095223,
                            grepl("WEF1",ID) ~ 49.65405,
                            grepl("WHT",ID) ~ 49.64029),
         Longitude=case_when(grepl("BRD1",ID) ~ -111.537891,
                             grepl("BUR1",ID) ~ -111.543323,
                             grepl("CHP1",ID) ~ -110.437106,
                             grepl("CLR",ID) ~ -113.484369,
                             grepl("CLY",ID) ~ -110.356864,
                             grepl("COL1",ID) ~ -112.97448,
                             grepl("CRD",ID) ~ -112.578,
                             grepl("CRR2",ID) ~ -113.983,
                             grepl("FMG1",ID) ~ -111.122,
                             grepl("KKP",ID) ~ -113.61337,
                             grepl("MON1",ID) ~ -112.974231,
                             grepl("NMK1",ID) ~ -113.163017,
                             grepl("RIV1",ID) ~ -113.977,
                             grepl("STR",ID) ~ -113.371296,
                             grepl("TVS1",ID) ~ -112.73059,
                             grepl("VCN1",ID) ~ -112.84841,
                             grepl("VXH1",ID) ~ -112.149936,
                             grepl("WEF1",ID) ~ -111.515812,
                             grepl("WHT",ID) ~ -111.291))

nocorrection <- nrgstream_gen %>%
  filter(!is.na(Latitude))

nrgstream_gen <- rbind(corrected,nocorrection)

sub_samp<-filter(nrgstream_gen, time >= as.Date("2017-01-1"))
rm(nrgstream_gen)

# Load predicted capacity factors
setwd("D:/Documents/GitHub/AuroraEval/WindProfile")
Pot_sites <- readRDS("PotentialSites.RData") %>%
  mutate(year = 2021,
         time = ymd_h(paste0(year,"-",month,"-",day," ",hour)),
         gen = 0,
         Capacity = 0,
         Revenue = 0) %>%
  subset(., select = -c(year,month,day,hour,Outage))

SiteProf <- readRDS("SitesProfiles.RData") %>%
  mutate(time = ymd_h(paste0(year,"-",month,"-",day," ",hour)),
         sim_gen = Capacity * Cap_Fac) %>%
  subset(., select = c(time,ID,Latitude,Longitude,Capacity,sim_gen,Cap_Fac,Installation))

# Filter data for plant_type, calculate total output for fleet for each period
alberta_samp <- sub_samp %>%
  filter(Plant_Type == "WIND") %>%
  subset(., select = -c(Demand,AIL,NRG_Stream,Plant_Fuel,GHG_ID,CO2,Heat.Rate,
                        co2_est,AESO_Name,date,he,Price,Plant_Type)) %>%
  na.omit()

alberta_samp <- rbind(Pot_sites,alberta_samp) %>%
  group_by(time) %>%
  mutate(fleet_gen = sum(gen),
         fleet_cap = sum(Capacity),
         fleet_CF = fleet_gen/fleet_cap) %>%
  ungroup() %>%
  na.omit() %>%
group_by(ID) %>%
  summarize(Capacity = median(Capacity),
            Latitude = median(as.numeric(Latitude)),
            Longitude = median(as.numeric(Longitude)),
            correlation = cor(Cap_Fac,fleet_CF, method=corm),
            Dispatched = sum(gen),
            Revenue = sum(Revenue),
            Capture_Price = Revenue/Dispatched,
            Cap_Fac = mean(Cap_Fac),
            #fleet_cap = mean(fleet_cap),
            #fleet_gen = mean(fleet_gen),
            #fleet_CF = mean(fleet_CF)
            
  ) %>%
  ungroup() %>%
  mutate(Installation=case_when(grepl("CRR2",ID)~"post2019",
                                grepl("CYP",ID)~"post2019",
                                #grepl("CYP2",ID)~"post2019",
                                grepl("FMG1",ID)~"post2019",
                                grepl("GRZ1",ID)~"post2019",
                                grepl("HHW1",ID)~"post2019",
                                grepl("HLD1",ID)~"post2019",
                                grepl("JNR",ID)~"post2019",
                                grepl("RIV1",ID)~"post2019",
                                grepl("RTL1",ID)~"post2019",
                                grepl("WHE1",ID)~"post2019",
                                grepl("WHT",ID)~"post2019",
                                grepl("WRW1",ID)~"post2019",
                                grepl("Anzac",ID)~"Potential",
                                grepl("BisonLake",ID)~"Potential",
                                grepl("ChainLakes",ID)~"Potential",
                                grepl("ClearPrairie",ID)~"Potential",
                                grepl("Falher",ID)~"Potential",
                                grepl("GrandeCache",ID)~"Potential",
                                grepl("Hinton",ID)~"Potential",
                                grepl("JohnDOr",ID)~"Potential",
                                grepl("Kehewin",ID)~"Potential",
                                grepl("LesserSlave",ID)~"Potential",
                                grepl("PigeonLake",ID)~"Potential",
                                grepl("SwanHills",ID)~"Potential",
                                TRUE~"pre2019"
                                ),
         )

alberta_corr <- SiteProf %>%
  group_by(time) %>%
  mutate(fleet_gen = sum(sim_gen),
         fleet_cap = sum(Capacity),
         fleet_CF = fleet_gen/fleet_cap) %>%
  ungroup() %>%
  na.omit() %>%
  group_by(ID) %>%
  summarize(Capacity = median(Capacity),
            Latitude = median(as.numeric(Latitude)),
            Longitude = median(as.numeric(Longitude)),
            correlation = cor(Cap_Fac,fleet_CF, method=corm),
            #Dispatched = sum(gen),
            #Revenue = sum(Revenue),
            #Capture_Price = Revenue/Dispatched,
            #Cap_Fac = mean(Cap_Fac),
            #fleet_cap = mean(fleet_cap),
            #fleet_gen = mean(fleet_gen),
            #fleet_CF = mean(fleet_CF)
            
  ) %>%
  ungroup() %>%
  mutate(Built=case_when(grepl("CRR2",ID)~"post2019",
                                grepl("CYP",ID)~"post2019",
                                #grepl("CYP2",ID)~"post2019",
                                grepl("FMG1",ID)~"post2019",
                                grepl("GRZ1",ID)~"post2019",
                                grepl("HHW1",ID)~"post2019",
                                grepl("HLD1",ID)~"post2019",
                                grepl("JNR",ID)~"post2019",
                                grepl("RIV1",ID)~"post2019",
                                grepl("RTL1",ID)~"post2019",
                                grepl("WHE1",ID)~"post2019",
                                grepl("WHT",ID)~"post2019",
                                grepl("WRW1",ID)~"post2019",
                                grepl("Anzac",ID)~"Potential",
                                grepl("BisonLake",ID)~"Potential",
                                grepl("ChainLakes",ID)~"Potential",
                                grepl("ClearPrairie",ID)~"Potential",
                                grepl("Falher",ID)~"Potential",
                                grepl("GrandeCache",ID)~"Potential",
                                grepl("Hinton",ID)~"Potential",
                                grepl("JohnDOr",ID)~"Potential",
                                grepl("Kehewin",ID)~"Potential",
                                grepl("LesserSlave",ID)~"Potential",
                                grepl("PigeonLake",ID)~"Potential",
                                grepl("SwanHills",ID)~"Potential",
                                TRUE~"pre2019"
  ),
  )

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
                       limits=c(3,10),oob=squish, 
                       name = "Mean wind speed \nat 80m height \n(m/s)") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        legend.text = element_text(size = legText),
        legend.title = element_text(size = legTitle)) 

################################################################################
################################################################################
# Map of Alberta with only usable wind speeds shown
################################################################################
################################################################################

AB1 <- ggplot() + 
  geom_tile(data = wind_profile, 
            aes(x = Longitude, y = Latitude, fill = Wind)) +
  geom_polygon(data = alberta_ellipsoid1, 
               aes(x = long, y = lat, group = group), 
               fill = "transparent", colour = "black") +
  scale_fill_gradientn(colors = matlab.like2(100),
                       limits=c(3.5,12), na.value="white",oob=squish, 
                       name = "Mean wind speed \nat 80m height \n(m/s)") +
#  scale_fill_gradient(low="white", high="white", limits=c(3.5,25), na.value="red",
#                      oob=squish, 
#                      name = "Mean wind speed \nat 80m height \n(m/s)"
#)+
#  scale_fill_gradientn(colors = c("navy","turquoise1","green",
#                                  "yellow","orangered","red4"),
#                       values=c(3.5,5,6.5,7.5,8.5,10),oob=squish, 
#                       name = "Mean wind speed \nat 80m height \n(m/s)") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank()) 

################################################################################
################################################################################
# Map of Alberta with only ideal wind speeds shown
################################################################################
################################################################################

AB2 <- ggplot() + 
  geom_tile(data = wind_profile, 
            aes(x = Longitude, y = Latitude, fill = Wind)) +
  geom_polygon(data = alberta_ellipsoid1, 
               aes(x = long, y = lat, group = group), 
               fill = "transparent", colour = "black") +
  #  scale_fill_gradientn(colors = matlab.like2(100),
  #                       limits=c(3.5,25), na.value="white",#oob=squish, 
  #                       name = "Mean wind speed \nat 80m height \n(m/s)") +
  scale_fill_gradient2(low="deepskyblue", mid="forestgreen", high="yellow", midpoint=13,
                      limits=c(3.5,25), na.value="red",
                      #                      oob=squish, 
                      name = "Mean wind speed \nat 80m height (m/s)"
  )+
  #  scale_fill_gradientn(colors = c("navy","turquoise1","green",
  #                                  "yellow","orangered","red4"),
  #                       values=c(3.5,5,6.5,7.5,8.5,10),oob=squish, 
  #                       name = "Mean wind speed \nat 80m height (m/s)") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        legend.text = element_text(size = legText),
        legend.title = element_text(size = legTitle))  

################################################################################
################################################################################
# Map of Alberta with active wind farms
################################################################################
################################################################################
#ID_labels <- as.list(active$ID)

Act_wind <- ggplot(active, aes(x= Longitude, y = Latitude, #label=ID_labels
                               )) + 
  geom_tile(data = wind_profile, 
            aes(x = Longitude, y = Latitude, fill = Wind)) +
  geom_polygon(data = alberta_ellipsoid1, 
               aes(x = long, y = lat, group = group), 
               fill = "transparent", colour = "black") +
  scale_fill_gradientn(colors = matlab.like2(100),
                       limits=c(3,10),oob=squish, name = "Mean wind speed \nat 80m height \n(m/s)") +
  geom_point(data = active,
                aes(x= Longitude, y = Latitude, size = Capacity, ), 
                shape = 16, color = "black") +
#  ggtitle("Active Wind Farms") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=18, hjust = 0.5, vjust=-5),
        legend.text = element_text(size = legText),
        legend.title = element_text(size = legTitle)) 

################################################################################
# Save map as png
################################################################################

Act_wind
ggsave(path = "images", filename = "windfarmactive.png", bg = "transparent")

################################################################################
################################################################################
# Map of Alberta with active and queued wind farms
################################################################################
################################################################################
{
labs1 <- c("Active","AESO Queue")

AESO_wind <- AB + geom_point(data = wind_farm,
                aes(x= Longitude, y = Latitude, size = Capacity, shape = Status, color = Status)) + 
  scale_shape_manual(values = c(16,18), labels = labs1) +
  scale_color_manual(values = c("black", "grey39"), 
                     labels = labs1) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
#  ggtitle("Active and Queued Wind Farms") +
  theme(plot.title = element_text(size=18, hjust = 0.5, vjust=-5),
        legend.text = element_text(size = legText),
        legend.title = element_text(size = legTitle)) 
}
################################################################################
# Save map as png
################################################################################

AESO_wind
ggsave(path = "images", filename = "windfarmlocations.png", bg = "transparent")

################################################################################
################################################################################
# Map of Alberta with active and queued wind farms with selected locations for
# simulation
################################################################################
################################################################################
{
labs2 <- c("Active","AESO Queue","Simulated")

Sim_wind <- AB + geom_point(data = wind_sim,
                             aes(x= Longitude, y = Latitude, size = Capacity, shape = Status, color = Status)) + 
  scale_shape_manual(values = c(16,18,17), labels = labs2) +
  scale_color_manual(values = c("black", "grey39","red4"), 
                     labels = labs2) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  ggtitle("Active, Queued, & Potential \nWind Farms") +
  theme(plot.title = element_text(size=18, hjust = 0.5, vjust=-5),
        legend.text = element_text(size = legText),
        legend.title = element_text(size = legTitle)) 
}

################################################################################
################################################################################
# Map of Alberta with active wind farms with selected locations for
# simulation
################################################################################
################################################################################
{
  labs3 <- c("Active","Simulated")
  
  Simple_wind <- AB + geom_point(data = simple,
                              aes(x= Longitude, y = Latitude, size = Capacity, shape = Status, color = Status)) + 
    scale_shape_manual(values = c(16,17), labels = labs3) +
    scale_color_manual(values = c("black","red4"), 
                       labels = labs3) +
    guides(shape = guide_legend(override.aes = list(size = 5))) +
    ggtitle("Active, Queued, & Potential \nWind Farms") +
    theme(plot.title = element_text(size=18, hjust = 0.5, vjust=-5),
          legend.text = element_text(size = legText),
          legend.title = element_text(size = legTitle)) 
}

################################################################################
################################################################################
# Map of Alberta with active and queued wind farms in Aurora with selected 
# locations for simulation
################################################################################
################################################################################
{
  labs4 <- c("Active","Simulated","AESO Queue")
  
  Aurora_wind <- AB + geom_point(data = wind_Aurora,
                              aes(x= Longitude, y = Latitude, size = Capacity, 
                                  shape = Status, color = Status)) + 
    scale_shape_manual(values = c(16,17,18), labels = labs4) +
    scale_color_manual(values = c("black", "red4", "grey39"), 
                       labels = labs4) +
    guides(shape = guide_legend(override.aes = list(size = 5))) +
    ggtitle("Active, Queued, & Potential \nWind Farms in Aurora") +
    theme(plot.title = element_text(size=18, hjust = 0.5, vjust=-5),
          legend.text = element_text(size = legText),
          legend.title = element_text(size = legTitle)) 
}

################################################################################
# Save map as png
################################################################################

ggsave(path = "images", filename = "simplewindfarmpotential.png", bg = "transparent")

################################################################################
# Map of Alberta with active and potential farms and their wind profile 
# correlations
################################################################################
labs5 <- c("Built after 2019","Potential","Built before 2019")

wind_corr <- AB + geom_point(data = alberta_samp,
                               aes(x= Longitude, y = Latitude, size = correlation, 
                                   color = Installation),shape=16
                             ) + 
  geom_point(data = alberta_samp,
             aes(x= Longitude, y = Latitude, size = correlation),colour="black",
             shape=1) + 
  #scale_shape_manual(values = c(16,17,18), labels = labs5) +
  scale_color_manual(values = c("gray", "red4", "black"), 
                     labels = labs5) +
  scale_size("Correlation",trans='reverse',range=c(0.5,9)) +
  guides(shape = guide_legend(override.aes = list(size = 7))) +
  ggtitle("Correlation between \nwind farms' profiles in Aurora") +
  theme(plot.title = element_text(size=18, hjust = 0.5, vjust=-5),
        legend.text = element_text(size = legText),
        legend.title = element_text(size = legTitle))

################################################################################
# Map of Alberta with active and potential farms and their wind profile 
# correlations
################################################################################
labs5 <- c("Built after 2019","Potential","Built before 2019")

wind_corr1 <- AB + geom_point(data = alberta_corr,
                             aes(x= Longitude, y = Latitude, size = correlation, 
                                 color = Built),shape=16
) + 
  geom_point(data = alberta_corr,
             aes(x= Longitude, y = Latitude, size = correlation),colour="black",
             shape=1) + 
#  geom_text(data = alberta_corr,
#            aes(x=Longitude, y = Latitude),
#            label=alberta_corr$ID,
#            size = 2,
#            nudge_y = 0.25,
#            ) +
  #scale_shape_manual(values = c(16,17,18), labels = labs5) +
  scale_color_manual(values = c("black", "red4", "gray"), 
                     labels = labs5) +
  scale_size("Wind profile \ncorrelation",trans='reverse',range=c(0.5,7)) +
  guides(shape = guide_legend(override.aes = list(size = 7))) +
#  ggtitle("Correlation between \nwind sites' profiles in Aurora") +
  theme(plot.title = element_text(size=18, hjust = 0.5, vjust=-5),
        legend.text = element_text(size = legText),
        legend.title = element_text(size = legTitle))

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
                      ncol=2, widths=c(5,1))

wnd_plot <- annotate_figure(wnd_plot2, 
                           fig.lab = "Source: Canada Wind Atlas, Canada Wind Turbine Database, AESO Data 
                           Graph by Taylor Pawlenchuk",
                           fig.lab.pos = "bottom.right", 
                           fig.lab.face = "italic", 
                           fig.lab.size = 12)

wnd_plot

################################################################################
# Save map as png
################################################################################

ggsave(path = "images", filename = "windfarmlocationsdouble.png", bg = "transparent")

###############################################################################
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
}
