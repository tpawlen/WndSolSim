# Generates data set of the mean wind speed at 80 m for every available point 
# from the Canada Wind Atlas.
# 
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# March 2022; Last revision: March 21, 2022

library(XML)

# Set the resolution that will be used
res <- 0.5

# Set out the basic structure for the webpage address
cons1 <- "http://www.windatlas.ca/rose-en.php?field=E1&height=80&season=ANU&no=41&lat="
cons2 <- "&lon="

# Create blank dataframe with headers
{
Wind_pot <- data.frame(matrix(ncol = 3, nrow = 0))

x <- c('Latitude', 'Longitude', 'Mean Wind Speed (m/s)')

colnames(Wind_pot) <- x
}

# Create variables to define limits of data
{
maxLat <- 60
maxLon <- -120

totLat <- maxLat - 49
totLon <- maxLon - (-110)

stepLat <- totLat/res
stepLon <- abs(totLon/res)
}

# Run for loop to enter a series of coordinates in Canada's Wind Atlas and save
# the Annual Mean Wind Speed. THIS TAKES A LONG TIME AND A LOT OF PROCESSING 
# POWER. At the end, data will be saved to a RDS in a location printed on screen
{
  # Note the start time
  old <- Sys.time()
  
for(i in 0:stepLat) {                                             
  for(j in 0:stepLon) {   
    # Creates latitude and longitude to be entered into the WindAtlas
    Lon <- (-110 - (j*res))
    Lat <- (49 + (i*res))
    url <- paste(cons1, Lat, cons2, Lon, sep="")
    
    # Loads the website for the latitude and longitude and reads the annual 
    # mean wind speed.
    wind <- readHTMLTable(url,which=1)
    wnd <- wind[1,2]
    
    # Saves the latitude, longitude, and annual mean wind speed to the dataframe
    Wind_pot[nrow(Wind_pot)+1,] <- c(Lat, Lon, as.numeric(substr(wnd, 1, 4)))
    
    # Print progress
    print(paste(Lat, Lon, sep = ","))
  }
}

  # Creates an RDS file with the entire dataset.
  saveRDS(Wind_pot, file = paste("WindAtlas_Data", res, sep = "_"))
  
  # Prints the location of the file.
  getwd()
  
  # Print the elapsed time
  New <- Sys.time() - old
  print(New)
}
