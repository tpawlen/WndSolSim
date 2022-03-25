# Generates functions used to plot AESO generation output
#
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# March 2022; Last revision: March 24, 2022

{library(lubridate)
library(dplyr)
}

# Load data
################################################################################
setwd("D:/Documents/GitHub/AuroraEval")

# Data obtained from AESO Market Statistics
# https://public.tableau.com/app/profile/market.analytics/viz/AnnualStatistics_16161854228350/Introduction

# Set header
################################################################################
header <- read.csv('AESO_GenTable.csv', nrows = 2, header = FALSE, stringsAsFactors = FALSE)

# Convert data to numeric
# copy data and add header
################################################################################
char_data <- read.csv('AESO_GenTable.csv', skip = 2, header = F, stringsAsFactors = F)
num_data <- data.frame(data.matrix(char_data))
numeric_columns <- sapply(num_data,function(x){mean(as.numeric(is.na(x)))<0.5})
act_gen <- data.frame(num_data[,numeric_columns], char_data[,!numeric_columns])

colnames(act_gen) <- header[2,]

# Segregate data according to generation type
################################################################################
{
date <- char_data[,1]
Coal <- act_gen[,2:7]
Coal$date <- date
Coal$ID <- "Coal"
Cogen <- act_gen[,8:13]
Cogen$date <- date
Cogen$ID <- "Cogen"
Comb <- act_gen[,14:19]
Comb$date <- date
Comb$ID <- "Comb"
Duel <- act_gen[,20:25]
Duel$date <- date
Duel$ID <- "Duel"
Gas <- act_gen[,26:31]
Gas$date <- date
Gas$ID <- "Gas"
Hydro <- act_gen[,32:37]
Hydro$date <- date
Hydro$ID <- "Hydro"
Other <- act_gen[,38:43]
Other$date <- date
Other$ID <- "Other"
Simple <- act_gen[,44:49]
Simple$date <- date
Simple$ID <- "Simple"
Solar <- act_gen[,50:55]
Solar$date <- date
Solar$ID <- "Solar"
Stor <- act_gen[,56:61]
Stor$date <- date
Stor$ID <- "Storage"
Wind <- act_gen[,62:67]
Wind$date <- date
Wind$ID <- "Wind"
}

# Combine generation into one data frame
################################################################################
gen_table1 <- rbind(Coal, Cogen, Comb, Duel, Gas, Hydro, Other, Simple, Solar, 
                   Stor, Wind)

# Convert date into POSIX date class
# date time conversions doesn't work for all dates right now...
################################################################################
gen_table$date <- as.POSIXct(gen_table1$date, format="%d/%m/%Y %I:%M:%S %p")

# Change factor order
################################################################################
{
gen_table$ID<-fct_relevel(gen_table$ID, "Coal", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Duel", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Comb", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Cogen", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Gas", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Simple", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Hydro", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Other", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Wind", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Solar", after = Inf)
gen_table$ID<-fct_relevel(gen_table$ID, "Storage", after = Inf)
}

# Creates an RDS file with the entire dataset.
################################################################################
#saveRDS(gen_table, file = "AESO_generation_data")

# Filter data by date
################################################################################
date1 <- as.POSIXct("1/1/2020 12:00:00 AM", format="%d/%m/%Y %I:%M:%S %p")
filt_gen <- gen_table %>%
  filter(date >= date1)

################################################################################
################################################################################
# Wind Output
################################################################################

Wind_out <- filt_gen %>%
  filter(ID >= "Wind")

wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
wk_end <- as.POSIXct(paste(day+14, month,year, sep = "/"), format="%d/%m/%Y")

# Select only a single week
################################################################################
wind_WK <- Wind_out %>%
  filter(date >= wk_st & date <= wk_end)

ggplot() +
  geom_line(data = wind_WK, aes(x=date, y=as.numeric(Utilized)))

################################################################################
################################################################################
# Week output
################################################################################



# Function to plot actual data
################################################################################
Week_act <- function(data, days) {
  
  colours = c("black","darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
              "dodgerblue", "darkolivegreen1", "forestgreen", "gold", "cyan")

  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+days,month,year, sep = "/"), format="%d/%m/%Y")
  
  # Select only a single week
  ################################################################################
  WK <- data %>%
    filter(date >= wk_st & date <= wk_end)

  WK$ID<-fct_relevel(WK$ID, "Coal", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Duel", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Comb", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Cogen", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Gas", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Simple", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Hydro", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Other", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Wind", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Solar", after = Inf)
  WK$ID<-fct_relevel(WK$ID, "Storage", after = Inf)
  
  # Plot the data    
  ################################################################################
  ggplot() +
    geom_area(data = WK, aes(x = date, y = Utilized, fill = ID), 
              alpha=0.6, size=.5, colour="black") +
    
    scale_x_datetime(expand=c(0,0)) +
    
    # Set the theme for the plot
    ################################################################################
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "right",
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,6000)) +
    labs(x = "Date", y = "Output (MWh)", fill = "Resource") +
    scale_fill_manual(values = colours)
}
