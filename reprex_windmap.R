#library(raster)
library(ggplot2)
library(colorRamps)
library(scales)

setwd("D:/Documents/GitHub/AuroraEval")

# Read in the data
wind_profile <- readRDS("wind_profile")
wind_profile41 <- readRDS("wind_profile41")

# Try to combine the data
wind_profile <- rbind(wind_profile,wind_profile41)

ggplot() + 
  geom_tile(data = wind_profile, 
            aes(x = Longitude, y = Latitude, fill = Wind)) +
  scale_fill_gradientn(colors = matlab.like2(100),
                       limits=c(3,10),oob=squish, 
                       name = "Mean wind speed \nat 80m height \n(m/s)") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        ) 
