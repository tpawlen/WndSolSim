# Generates Wind Energy Outage profile to input into Aurora
# 
# The first part of code loads the data from csv file loaded from Canada's  
# windatlas website (http://www.windatlas.ca/series/index-en.php). This 
# information is considered public information and may be distributed or copied. 
#
# When selecting the data, Wind Speed, Wind Direction, and Temperature are 
# selected at 80m, 100m, and 120m.
#
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# February 2022; Last revision: February 8, 2022

# Required library
{library(tidyverse)
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(lutz)
  library(formattable)
}


# Define variables
{{
  location <- "la51970lo248172"  # The location lat and long as it appears on file.
  site <- "GardenPlain(GDP1)" # Name of the site
  hbht <- 100   #Hubheight of wind turbine in meters
  cutin <- 3    #Cut in wind speed (m/s)
  rated <- 12   #Wind speed at maximum capacity (m/s)
  cutout <- 25  #Cut out wind speed (m/s)
}

#FUNCTIONS__________________________________________________________________________________
#Run these functions
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

getSeasonPOSIXct <- function(DATES) {
  WS <- as.POSIXct("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.POSIXct("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.POSIXct("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.POSIXct("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.POSIXct(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

{
# Open the data file
# Identify the location of the downloaded csv file
setwd("D:/Documents/Education/Masters Degree/Datasets/Wind Data/Wind Atlas Data")
TWF<-read.csv(paste0(location,".csv"), header = TRUE,
              na.strings = 999.99, stringsAsFactors = F)

# Date Formatting
TWF$DateTime <- TWF$DateTime.DateHeure %>%
  
  # This step separates the year, month, etc.
  {gsub("^(.{10})(.*)$", "\\1/\\2", .)} %>%
  {gsub("^(.{8})(.*)$", "\\1/\\2", .)} %>%
  {gsub("^(.{6})(.*)$", "\\1/\\2", .)} %>%
  {gsub("^(.{4})(.*)$", "\\1/\\2", .)} %>%
  
  # This step converts to date format
  ymd_hm(.) %>%
  
  # This step changes the time zone to MST
  with_tz(., "MST")

# Adds columns to identify the year, month, week, day, hour, season, and julian
TWF<-TWF %>%  mutate(year=year(DateTime), month=month(DateTime), 
                     week=week(DateTime), day=day(DateTime), hour=hour(DateTime),
                     season=getSeason(as.Date(DateTime)), julian=yday(DateTime))
}

#FIXING THE BLIP, copied and adapted from Natalia's code. 
# Code identifies the applicable hour(s), then the months it applys to, and 
# multiplies by the appropriate factor. All other lines remain the same.
{i=as.integer()
  imax=nrow(TWF)
  #Fixed <- sprintf("FixedWindSpeed_%sm", hbht)
  TWF["FixedWindSpeed_100"]=NA

for(i in 1:imax){
  if (TWF$hour[i] == 7 && 
      TWF$month > 3 && 
      TWF$month < 8){
    
    if(TWF$hour[i] == 7){
      TWF$FixedWindSpeed_100[i]<-TWF$UV_100m[i]*1.05
      
      }else if(TWF$hour[i] == 8){
        TWF$FixedWindSpeed_100[i]<-TWF$UV_100m[i]*1.17
        
        }else if(TWF$hour[i] == 9 || TWF$hour[i] == 10){
          TWF$FixedWindSpeed_100[i]<-TWF$UV_100m[i]*1.10
          
          }else if(TWF$hour[i] == 11){
            TWF$FixedWindSpeed_100[i]<-TWF$UV_100m[i]*1.09
            }}
  else{
    TWF$FixedWindSpeed_100[i]<-TWF$UV_100m[i]
  }
}

# Check if there are any NAs on the file
colSums(is.na(TWF))


# Simulate Output as a percentage of maximum capacity

  i=as.integer()
  j=as.integer()
  imax <- nrow(TWF)
  maxOutput <- 345


# Create the output column with all zeros.
TWF$output<-TWF$FixedWindSpeed_100*0

# The code changes the rows with wind speeds between cutin and cutout.
# If the wind speed is equal to the rated speed or higher, then it is at 100% 
# capacity. Need to look at the coefficients still as Natalia's code assumed a 230 MW farm. 
{
  for (i in 1:imax){
    if(TWF$FixedWindSpeed_100[i] >= cutin &&
       TWF$FixedWindSpeed_100[i] <= cutout)
      if(TWF$FixedWindSpeed_100[i] >= rated &&
         TWF$FixedWindSpeed_100[i] <= cutout){
        TWF$output[i]<- maxOutput
      
      }else {
      TWF$output[i]<- ((( -0.001851229*TWF$FixedWindSpeed_100[i]^7)+
                                (0.1100513*TWF$FixedWindSpeed_100[i]^6) + 
                                (-2.664391*TWF$FixedWindSpeed_100[i]^5) + 
                                (33.82947*TWF$FixedWindSpeed_100[i]^4) + 
                                (-242.5699*TWF$FixedWindSpeed_100[i]^3) +
                                (987.6442*TWF$FixedWindSpeed_100[i]^2) + 
                                (-2111.135*TWF$FixedWindSpeed_100[i])+
                                1827.68))}
  }



for (j in 1:imax){
  if(TWF$output[j]>maxOutput){
    TWF$output[j]<-maxOutput
  }
}

#TWF$output <- TWF$output/maxOutput

#Air Density Adjustment 
n=as.integer()
TWF$Air<-NA
#TWF$TT_100<-TWF$TT_100m #adjustment to make code work for now

for (n in 1:imax){
  if(TWF$output[n]>maxOutput){
    TWF$Air[n]<-maxOutput
  }else {
    TWF$Air[n]<-TWF$output[n]*(293/(TWF$TT_100m[n]+273))
  }
}

n=as.integer()
for (n in 1:imax){
  if (TWF$Air[n]>maxOutput){
    TWF$Air[n]<-maxOutput
  }else if(TWF$Air[n]<0){
    TWF$Air[n]<-0
  }
}
}

# Calculate the percentage of capacity being used (capacity factor)
TWF$CF <- TWF$Air/maxOutput

# Calculate the outage percentage. 
TWF$Outage <- (1-TWF$CF)*100


# Summarize the output to enter into Aurora
Aurora <- TWF %>%
  filter(year==2009) %>%
  group_by(year, month, day, hour) %>%
  summarise(Outage=mean(Outage, na.rm = TRUE), .groups="keep")
}

#SAVE FILE (To save the processed data) This is to be entered into Aurora.
setwd("D:/Documents/GitHub/AuroraEval/WindProfile")
write.csv(Aurora, file=paste0(site,"_",location,".csv"))
#write.csv(Aurora, file=paste0(location,"_",site,".csv"))
}
