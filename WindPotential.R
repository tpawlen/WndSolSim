# Generates data set of the mean wind speed at 80 m for every available point 
# from the Canada Wind Atlas.
# 
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# March 2022; Last revision: March 21, 2022

library(XML)
library(raster)
{
{
  
  setwd("C:/Users/pawlench/Documents/GitHub/WndSolSim")
  
# Set the resolution that will be used
res <- 0.05

# Set out the basic structure for the webpage address
cons1 <- "http://www.windatlas.ca/rose-en.php?field=E1&height=80&season=ANU&no=41&lat="
cons2 <- "&lon="

# Create blank dataframe with headers
wind_pot <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c('Latitude', 'Longitude')
colnames(wind_pot) <- x
}

# Create variables to define limits of data
{
  maxLat <- 60
  maxLon <- -120
  minLat <- 49.02
  minLon <- -110.04
  
  totLat <- maxLat-minLat
  totLon <- abs(maxLon-minLon)
  
  stepLat <- totLat/res
  stepLon <- totLon/res
}

for(i in 0:stepLat) {
  for(j in 0:stepLon) {
    Lon <- (minLon - (j*res))
    Lat <- (minLat + (i*res))
    
    wind_pot[nrow(wind_pot)+1,] <- c(Lat, Lon)
  }
}

{
# Get province shape
can_level1 = getData("GADM", country = "CA", level = 1)

WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
canada_level1_ellipsoid = spTransform(can_level1, WGS84)

alberta_ellipsoid =
  canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]
}

{
# Excludes any points outside the province
inout = over(
  SpatialPoints(wind_pot[,c("Longitude","Latitude")], proj4string=CRS(projection(alberta_ellipsoid))),
  as(alberta_ellipsoid,"SpatialPolygons")
)

wind_prof <- wind_pot[!is.na(inout),]
wind_prof[,3] <- 0

x <- c('Latitude', 'Longitude', 'Mean Wind Speed (m/s)')

colnames(wind_prof) <- x
row.names(wind_prof) <- 1:nrow(wind_prof)
}
}
# Run for loop to enter a series of coordinates in Canada's Wind Atlas and save
# the Annual Mean Wind Speed. THIS TAKES A LONG TIME AND A LOT OF PROCESSING 
# POWER. At the end, data will be saved to a RDS in a location printed on screen

  # Note the start time
  old <- Sys.time()
  {  
  for(i in 31414:nrow(wind_prof)) {
    tempold <- Sys.time()
    url <- paste(cons1, wind_prof[i,1], cons2, wind_prof[i,2], sep = "")
    wind <- readHTMLTable(url,which=1)
    wind_prof[i,3] <- as.numeric(substr(wind[1,2], 1, 4))
    
    perc <- format(round((i/nrow(wind_prof)*100), 3), nsmall = 3)
    
    dif <- Sys.time() - tempold
    fin <- ((nrow(wind_prof)-i) * dif) + Sys.time()
    
    print(paste(paste(perc,"%", sep = ""),i, fin, sep = ", "))
    #print(paste(wind_prof[i,1],wind_prof[i,2],sep = ","))
  }

     
   # Creates an RDS file with the entire dataset.
  saveRDS(wind_prof, file = paste("WindAtlas_Data23", res, sep = "_"))
  
  # Prints the location of the file.
  getwd()
  
  # Print the elapsed time
  New <- Sys.time() - old
  print(New)
  }
             