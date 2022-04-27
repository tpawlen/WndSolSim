# Generates functions used to plot simulation outputs
#
# The entire code can be run once per session, then just the functions can be 
# used for each iteration.
#
# Eval4(month, day, case) plots the output of a single specified week over 4 
#    years for a specified case study with storage separate on top, price in the  
#    middle, and output on the bottom. Basically 4 PrOuts.
# Eval2(month, day, case) plots the output of a single specified week over 2 
#    years for a specified case study with storage separate on top, price in the  
#    middle, and output on the bottom. Basically 2 PrOuts.

# EvalOut(input, case) plots the output averaged over the specified time period 
#    (month, year) on top with the resources built on the bottom.
# EvalPerc(input, case) plots the output of each generation type as a percentage
#     of total generation
# BuildUnits(case,Fuel) plots the units built for a fuel type along with the 
#     available units not built.
# BuildUnits2(case,Fuel) same as BuildUnits, with hypothetical sites highlighted
# PrOut(year, month, day, case) plots the output of a single specified week for
#    a specified case study with storage separate on top, price in the middle, 
#    and output on the bottom.
# PrOut4(year, month, day, case) same as PrOut with limits set for 4 year 
#    comparison
# PrOt(year, month, day, case) same as PrOut minus the storage plot
#
# Week1(year, month, day, case) plots the output of a single specified week for
#    a specified case study.
# Week14(year, month, day, case) same as Week1 with limits set for 4 year
#    comparison
# Stor1(year, month, day, case) plots the storage output of a single specified 
#    week for a specified case study.
# Stor14(year, month, day, case) same as Stor1 with limits set for 4 year
#    comparison
# Week4(month, day, case) plots the output of a single specified week for 
#    specified years for a specified case study.
# week_price(year, month, day, case) plots the price of electricity for a single 
#    specified week for a specified case study.
# week_price4(year, month, day, case) same as week_price with limits set for 4 
#    year comparison
# Eval(input, case) plots the output averaged over the specified time period 
#    (month, year)
# Built(case) plots the resources built over the time span of the study.
# Units(case, Fuel) plots the units of a particular resource the simulation built
# Slack(case, Fuel) plots the units available to build in the simulation that 
#     were not built
#
# imsave("name") Saves the image of the plot to directory
#
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# March 2022; Last revision: March 9, 2022
{
{library(tidyverse)
  library(ggplot2)
  library(grid)
  library(gtable)
  library(gridExtra)
  library(plotly) # For interactive charts
  library(odbc)
  library(ggpubr)
  library(DBI)
  library(lubridate)
  library(cowplot)
  library(scales)
  library(dplyr)
#  library(RMySQL)
  library(RMariaDB)
#  library(plyr)
  }

{
  DB <- "Apr_25_2022"
# Connect to SQL database
################################################################################
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "192.168.0.139,49172",
                 Database = DB,
                 UID = "admin",
                 PWD = "SOB704910",
                 Port = 49172)

  # Connect to MySQL database
  ################################################################################
#  con1 <- dbConnect(RMariaDB::MariaDB(),
#                   user = 'tpawl',
#                   password = 'Aurora2022!',
#                   Driver = "SQL Server",
#                   host='192.168.0.139',
#                   dbname = DB,

#                   port = 3306)
  
  {
# Write data to environment and set variables
################################################################################
Hour <- dbReadTable(con,'ResourceGroupHour1')
Month <- dbReadTable(con,'ResourceGroupMonth1')
Year  <- dbReadTable(con,'ResourceGroupYear1')
#Build <- dbReadTable(con,'LTBuildReport1')
ZoneHour <- dbReadTable(con,'ZoneHour1')
Resource <- dbReadTable(con,'ResourceMonth1')
#LTRes <- dbReadTable(con,'LTResValue1')

setwd("D:/Documents/Education/Masters Degree/Aurora/R Code")

#write.csv(data_raw_Hour, file="data_raw_Hour.csv")
#write.csv(data_ZoneHour, file="data_ZoneHour.csv")

#Hour <- data_raw_Hour #read.csv("data_raw_Hour.csv", header = TRUE)
#Month <- data_raw_Month
#Year  <- data_raw_Year
#Build <- data_raw_Build
#ZoneHour <- data_ZoneHour #read.csv("data_ZoneHour.csv", header = TRUE)
#LTRes <- data_LTResValue
}
  
#  Houra <- Hour
#  Montha <- Month
#  Yeara <- Year
#  ZoneHoura <- ZoneHour

#  Hour <- rbind(Houra, Hourb, Hourc)
#  Month <- rbind(Montha, Monthb, Monthc)
#  Year <- rbind(Yeara, Yearb, Yearc)
#  ZoneHour <- rbind(ZoneHoura, ZoneHourb, ZoneHourc)
  
 # Set variables to identify the case studies
################################################################################
{
  Yr4Sp <- list(2020,2025,2030,2035)
  Yr2Sp <- list(2020,2021)

BC <- "Base Case"
MS  <- "Minimum Solar Constraint"
LCT <- "Low Carbon Tax"
HS <- "Hypothetical Sites"

# Set limits for plots to be consistent
ylimit <- max(Hour$Output_MWH) + max(ZoneHour$Imports)
#prlimit <- max(ZoneHour$Price)

# Set legend variables
colours = c("darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
            "dodgerblue", "forestgreen", "gold", "darkolivegreen1", "cyan")
colours1 = c("darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
             "darkcyan", "dodgerblue", "forestgreen", "gold", "cyan")
colours2 = c("grey", "darkslategrey", "coral4", "goldenrod4", 
            "dodgerblue", "darkcyan", "forestgreen", "gold", "cyan")
colours3 = c("forestgreen", "gold", "coral4", "goldenrod4", "cyan", "dodgerblue")
}

# Converts the date and time and identifies the week when applicable
################################################################################
{
Hour$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",Hour$Time_Period))), 
                        tz = "MST")-(60*60)
Month$Time_Period <- ym(Month$Time_Period)
Year$Time_Period  <- as.Date(as.character(Year$Time_Period), 
                                  format = "%Y")
ZoneHour$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ZoneHour$Time_Period))), 
                            tz = "MST")-(60*60)

# Selects only the required columns
################################################################################

Hour <- Hour %>%
  subset(., select = c(ID, date, Output_MWH, Run_ID))

ZH <- ZoneHour %>%
  filter(Name == "WECC_Alberta") %>%
  filter(Condition == "Average") %>%
  subset(., select = c(date, Price, Baseline_Demand, Demand, Demand_Total,
                       Net_Load, Net_Load_Total, Marginal_Resource, 
                       Smp_Max_Date_Time, Smp_Max_Demand, Smp_Max_Capacity, 
                       Run_ID, Imports, Exports))

ZoneH <- ZoneHour %>%
  filter(Name == "WECC_Alberta") %>%
  subset(., select = c(date, Condition, Price, Demand, Marginal_Resource, Report_Year,
                       Run_ID))

# Select the Import/Export data
Import <- ZH %>%
  subset(., select = c(date, Imports, Run_ID)) %>%
  'colnames<-'(c("date", "Output_MWH", "Run_ID")) %>%
  add_column(ID = "Import")

#Export <- ZH %>%
#  subset(., select = c(date, Exports, Run_ID)) %>%
#  'colnames<-'(c("date", "Output_MWH", "Run_ID")) %>%
#  add_column(ID = "Export")

#Export$Output_MWH <- Export$Output_MWH * -1

#ImEx <- rbind(Import, Export)
}
}

################################################################################
# General functions defined ####################################################
################################################################################

################################################################################
# This function filters for the data that will be evaluated.
################################################################################
{
sim_filt <- function(inputdata) {
  # Filter the data by resource
  {Coal <- inputdata %>%
    filter(ID=="LTO_Coal")
  Coal2Gas  <- inputdata %>%
    filter(ID=="LTO_Coal2Gas")
  Coal2Gas$Output_MWH[Coal2Gas$Output_MWH < 0] <- 0
  Cogen  <- inputdata %>%
    filter(ID=="LTO_Cogen")
  NatGas <- inputdata %>%
    filter(ID=="LTO_NatGas")
  Other <- inputdata %>%
    filter(ID=="LTO_Other")
  Hydro <- inputdata %>%
    filter(ID=="LTO_Hydro")
  Solar <- inputdata %>%
    filter(ID=="LTO_Solar")
  Storage <- inputdata %>%    
    filter(ID=="LTO_Storage")
  Wind <- inputdata %>%
    filter(ID=="LTO_Wind")
  }
  
  # Combine the grouped data
  {case <- rbind(Coal, Coal2Gas, Cogen, NatGas, Hydro, Solar, Wind, Storage, Other)
    case$ID <- factor(case$ID, levels=c("LTO_Coal", "LTO_Coal2Gas", "LTO_Cogen", 
                                        "LTO_NatGas", "LTO_Other", "LTO_Hydro", 
                                        "LTO_Wind", "LTO_Solar", "LTO_Storage"))
    levels(case$ID) <- c("Coal", "Coal2Gas", "Cogen", "NatGas", "Other", "Hydro",
                         "Wind", "Solar", "Storage")
  }
  return(case)
}

{
  sim_filt1 <- function(inputdata) {
    # Filter the data by resource
    {Coal <- inputdata %>%
      filter(ID=="LTO_Coal")
    SCCT  <- inputdata %>%
      filter(ID=="AB_SCCT_noncogen")
    Cogen  <- inputdata %>%
      filter(ID=="LTO_Cogen")
    CCCT <- inputdata %>%
      filter(ID=="AB_CCCT_noncogen")
    Other <- inputdata %>%
      filter(ID=="LTO_Other")
    Hydro <- inputdata %>%
      filter(ID=="LTO_Hydro")
    Solar <- inputdata %>%
      filter(ID=="LTO_Solar")
    Storage <- inputdata %>%    
      filter(ID=="LTO_Storage")
    Wind <- inputdata %>%
      filter(ID=="LTO_Wind")
    }
    
    # Combine the grouped data
    {case <- rbind(Coal, Cogen, SCCT, CCCT, Hydro, Solar, Wind, Storage, Other)
      case$ID <- factor(case$ID, levels=c("LTO_Coal", "AB_CCCT_noncogen", "LTO_Cogen",
                                          "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                          "LTO_Wind", "LTO_Solar", "LTO_Storage"))
      levels(case$ID) <- c("Coal", "NGCC", "Cogen", "SCGT", "Hydro", "Other",
                           "Wind", "Solar", "Storage")
    }
    return(case)
  }
}

# Function to convert the date time for plotting
HrTime <- function(data, year, month, day) {
    subset(data,
           (date >= paste(year,"-", month, "-", day," 00:00:00", sep = "") & 
              date <= 
              paste(year,"-", month, "-", (day+7)," 00:00:00", sep = "")))
}
}
################################################################################
################################################################################
# Plotting functions defined
################################################################################
################################################################################
{
################################################################################
# Functions for weekly evaluation
################################################################################
  Week1 <- function(year, month, day, case) {
    # Filters for the desired case study
    data <- Hour %>%
      sim_filt1(.) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    data$ID <- factor(data$ID, levels=c("Import", "Coal", "Cogen", "SCGT", "NGCC",
                                        "Hydro", "Other", "Wind", "Solar", "Storage"))
    
#    data$date <- as.POSIXct(data$date, tz = "MST")
    
    wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
    wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
    
    # Select only a single week
    ##############################################################################
    WK <- data %>%
      filter(date >= wk_st, date <= wk_end)

    # Select only a single week
#    WK <- HrTime(data,year,month,day)
    ZPrice <- HrTime(ZH,year,month,day)
    Expo <- HrTime(Export,year,month,day)
    WK$MX <- ZPrice$Demand + Expo$Output_MWH
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(WK$MX)), 100, f = ceiling)
    MN <- plyr::round_any(min(WK$Output_MWH), 100, f = floor)

    # Plot the data    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID), 
                alpha=0.6, size=.5, colour="black") +
      
      # Add hourly load line
      geom_line(data = ZPrice, 
                aes(x = date, y = Demand), size=2, colour = "black") +
      scale_x_datetime(expand=c(0,0)) +
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank(),
            legend.position = "right",
      ) +
      theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            text = element_text(size= 15)
      ) +
      scale_y_continuous(expand=c(0,0), limits = c(MN,MX), 
                         breaks = seq(MN, MX, by = MX/4)) +
      labs(x = "Date", y = "Output (MWh)", fill = "Resource") +
      scale_fill_manual(values = colours1)
  }
  
################################################################################
# Functions for weekly evaluation over four years
################################################################################  
  
Week14 <- function(year, month, day, case) {
  # Add imports and exports to data
  
  
  # Filters for the desired case study
  data <- Hour %>%
    sim_filt1(.) %>%
    rbind(.,Import) %>%
    filter(Run_ID == case)
  
  data$ID <- factor(data$ID, levels=c("Import", "Coal", "Cogen", "SCGT", "NGCC",
                                      "Hydro", "Other", "Wind", "Solar", "Storage"))
  
  # Select only a single week
    WK <- HrTime(data,year,month,day)
    ZPrice <- HrTime(ZH,year,month,day)
    Expo <- HrTime(Export,year,month,day)
    data$MX <- ZH$Demand + Export$Output_MWH
    
    # Set the max and min for the plot
    MX1 <- HrTime(data,Yr4Sp[[1]],month,day)
    MX2 <- HrTime(data,Yr4Sp[[2]],month,day)
    MX3 <- HrTime(data,Yr4Sp[[3]],month,day)
    MX4 <- HrTime(data,Yr4Sp[[4]],month,day)
    MXtime <- rbind(MX1, MX2, MX3, MX4)
    
    MX <- plyr::round_any(max(abs(MXtime$MX)), 100, f = ceiling)
    MN <- plyr::round_any(min(MXtime$Output_MWH), 100, f = floor)
  
  # Plot the data    
  ggplot() +
    geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID), 
              alpha=0.6, size=.5, colour="black") +
    
  # Add hourly load line
    geom_line(data = ZPrice, 
              aes(x = date, y = Demand), size=2, colour = "black") +
    scale_x_datetime(expand=c(0,0)) +
  
  # Set the theme for the plot
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "right",
          ) +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 10)
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(MN,MX), 
                       breaks = seq(MN, MX, by = MX/4)) +
    labs(x = "Date", y = "Output (MWh)", fill = "Simulated Data: \nResource") +
    scale_fill_manual(values = colours1)
}

################################################################################
# Generate weekly storage output plot function
################################################################################
################################################################################
Stor1 <- function(year, month, day, case) {
  # Add imports and exports to data
  
  
  # Filters for the desired case study
  data <- Hour %>%
    filter(ID=="LTO_Storage") %>%
    filter(Run_ID == case)
  
  
  # Select only a single week
  WK <- HrTime(data,year,month,day)
  
  # Set the max and min for the plot
  MX <- plyr::round_any(max(abs(WK$Output_MWH)), 10, f = ceiling)
  
  # Plot the data    
  ggplot() +
    geom_area(data = WK, aes(x = date, y = Output_MWH), 
              alpha=0.6, size=.5, colour="black") +
    ggtitle(year)+
    
    # Set the theme for the plot
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(breaks = seq(-MX, MX, by = MX), 
                       limits = c(-MX-1,MX+1),
                       labels = label_number(accuracy = 1)) +
    labs(x = "Date", y = "Storage\n(MWh)", fill = "Resource") +
    scale_fill_manual(values = "cyan")
}

################################################################################
# Generate weekly storage output plot function with axis limits for 4 years
################################################################################

Stor14 <- function(year, month, day, case) {
  # Add imports and exports to data
  
  
  # Filters for the desired case study
  data <- Hour %>%
    filter(ID=="LTO_Storage") %>%
    filter(Run_ID == case)

  
  # Select only a single week
  WK <- HrTime(data,year,month,day)
  
  # Set the max and min for the plot
  MX1 <- HrTime(data,Yr4Sp[[1]],month,day)
  MX2 <- HrTime(data,Yr4Sp[[2]],month,day)
  MX3 <- HrTime(data,Yr4Sp[[3]],month,day)
  MX4 <- HrTime(data,Yr4Sp[[4]],month,day)
  MXtime <- rbind(MX1, MX2, MX3, MX4)
  
  MX <- plyr::round_any(max(abs(MXtime$Output_MWH)), 10, f = ceiling)
  
  # Plot the data    
  ggplot() +
    geom_area(data = WK, aes(x = date, y = Output_MWH), 
              alpha=0.6, size=.5, colour="black") +
    ggtitle(year)+

    # Set the theme for the plot
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(breaks = seq(-MX, MX, by = MX), 
                       limits = c(-MX-1,MX+1),
                       labels = label_number(accuracy = 1)) +
    labs(x = "Date", y = "Storage\n(MWh)", fill = "Resource") +
    scale_fill_manual(values = "cyan")
}

################################################################################
# Function for plotting prices
################################################################################
################################################################################

week_price <- function(year, month, day,case) {
  # Filters for the desired case study
  data <- ZH %>%
    filter(Run_ID == case)
  
  # Select only a single week using function HrTime
  ZPrice <- HrTime(data,year,month,day)
  
  # Set the max and min for the plot
  MX <- plyr::round_any(max(abs(ZPrice$Price)), 10, f = ceiling)
  MN <- plyr::round_any(min(abs(ZPrice$Price)), 10, f = floor)
  
  # Plot the data    
  ggplot() +
    geom_line(data = ZPrice, 
              aes(x = date, y = Price), 
              size = 1.5, colour = "red") +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15)
    ) +
    labs(y = "Pool Price \n$/MWh", fill = "Resource") +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), 
                       limits= c(MN,MX),
                       #                       labels = label_number(accuracy = 1),
                       breaks = seq(MN, MX, by = MX/4)
    )
}

################################################################################
# Function for plotting prices with axis limits for 4 years
################################################################################

week_price4 <- function(year, month, day,case) {
  # Filters for the desired case study
  data <- ZH %>%
    filter(Run_ID == case)
  
  # Select only a single week using function HrTime
  ZPrice <- HrTime(data,year,month,day)

  # Set the max and min for the plot
  MX1 <- HrTime(data,Yr4Sp[[1]],month,day)
  MX2 <- HrTime(data,Yr4Sp[[2]],month,day)
  MX3 <- HrTime(data,Yr4Sp[[3]],month,day)
  MX4 <- HrTime(data,Yr4Sp[[4]],month,day)
  MXtime <- rbind(MX1, MX2, MX3, MX4)
  
  MX <- plyr::round_any(max(abs(MXtime$Price)), 10, f = ceiling)
  MN <- plyr::round_any(min(abs(MXtime$Price)), 10, f = floor)
  
  # Plot the data    
  ggplot() +
    geom_line(data = ZPrice, 
              aes(x = date, y = Price), 
              size = 1.5, colour = "red") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank()
    ) +
    labs(y = "$/MWh", fill = "Resource") +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), 
                       limits= c(MN,MX),
#                       labels = label_number(accuracy = 1),
                       breaks = seq(MN, MX, by = MX/4)
                       )
}

################################################################################
################################################################################
# Function for plotting month/year profiles
################################################################################
################################################################################

Eval <- function(input,case) {
  Imp <- Import %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$date, format="%Y")) %>%
    group_by(Time_Period) %>%
    summarise(Output_MWH = mean(Output_MWH)) %>%
    mutate(ID = "Import")
  
#  Imp$Time_Period  <- as.Date(as.character(Imp$Time_Period), 
#                               format = "%Y")
  
#  Imp <- subset(Imp, Time_Period <= '2040-04-05')
  
  # Filters for the desired case study
  data <- input %>%
    filter(Run_ID == case & Condition == "Average") %>%
    select(ID, Time_Period, Output_MWH) %>%
    sim_filt(.) %>%
    rbind(.,Imp) 
    
  data$ID<-fct_relevel(data$ID, "Import")
  data$Time_Period <- as.Date(data$Time_Period)
  
  data %>%
    ggplot() +
    aes(Time_Period, (Output_MWH/1000), fill = ID) +
    geom_area(alpha=0.6, size=.5, colour="black") +
#    facet_wrap(~ Condition, nrow = 1) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5), 
          legend.justification = c(0,0.5)) +
#    scale_x_date(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_manual(values = colours1) +
    labs(x = "Date", y = "Output (GWh)", fill = "Resource") 
}

Evalcap <- function(input,case) {

  # Filters for the desired case study
  data <- input %>%
    filter(Run_ID == case & Condition == "Average") %>%
    select(ID, Time_Period, Capacity) %>%
    sim_filt(.)

  data %>%
    ggplot() +
    aes(Time_Period, (Capacity/1000), fill = ID) +
    geom_area(alpha=0.6, size=.5, colour="black") +
    #    facet_wrap(~ Condition, nrow = 1) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5), 
          legend.justification = c(0,0.5)) +
    scale_x_date(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_manual(values = colours2) +
    labs(x = "Date", y = "Capacity (GWh)", fill = "Resource") 
}

################################################################################
# Function for plotting month/year profiles as a percentage of total
################################################################################
################################################################################

EvalPerc <- function(input,case) {
  # Filters for the desired case study
  data <- input %>%
    filter(Run_ID == case & Condition == "Average")# %>%
#    group_by(Time_Period, ID) %>%
#    summarise(n = sum(Output_MWH)) %>%
#    mutate(Percentage = n / sum(n))
  
  # Filter the data by resource
  case_Time <- sim_filt(data)
  
  # Remove negative generation (Storage)
  case_Time$Output_MWH[case_Time$Output_MWH < 0] <- NA
  
  case_Time %>%
    ggplot() +
    aes(Time_Period, Output_MWH, fill = ID) +
    geom_area(position = "fill", alpha=0.6, size=.5, colour="black") +
    geom_hline(yintercept = 0.3, linetype = "dashed", color = "forestgreen", size = 1.5) +
    geom_vline(xintercept = as.Date(ISOdate(2035, 1,1)),
               linetype = "dashed", color = "dodgerblue", size = 1.5) +
    #    facet_wrap(~ Condition, nrow = 1) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5), 
          legend.justification = c(0,0.5)) +
    scale_x_date(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0),
                       labels = scales::percent, 
                       breaks = sort(c(seq(0,1,length.out=5),0.3))) +
    scale_fill_manual(values = colours2) +
    labs(x = "Date", y = "Percentage of Generation", fill = "Resource") 
}

################################################################################
# Function for plotting the resources built
################################################################################
################################################################################

# Stacked Area showing totals for Fuel Types
Built <- function(case) {
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period != "Study")%>%
    group_by(Fuel_Type, Time_Period) %>%
    summarise(Units = sum(Units_Built)) 
  
  data$Fuel_Type <- factor(data$Fuel_Type, 
                           levels = c("WND","SUN","Gas0","Gas1", "PS", "OT"))
  
  data %>%
    ggplot() +
    aes(Time_Period, Units, fill = Fuel_Type, group = Fuel_Type) +
    geom_area(alpha=0.6, size=.5, colour="black") +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          legend.justification = c(0,0.5),
          ) +
    labs(x = "Date", y = "# of Units Built", fill = "Fuel Type") +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(max(data$Units)+1))) +
#    scale_x_discrete(expand=c(0,0)) +
    scale_fill_manual(values = colours3)
}

################################################################################
# Function for plotting the resources built as bar chart
################################################################################

# Stacked Area showing totals for Fuel Types
Builtcol <- function(case) {
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period != "Study")%>%
    group_by(Fuel_Type, Time_Period) %>%
    summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
  
  data$Fuel_Type <- factor(data$Fuel_Type, 
                           levels = c("WND","SUN","Gas0","Gas1", "PS", "OT"))
  
  Tot <- data %>%
    group_by(Time_Period) %>%
    summarise(totu = sum(Units), totc = sum(Capacity))
  
  mxu <- max(Tot$totu)
  mxc <- max(Tot$totc)
  
  ggplot(data) +
    aes(Time_Period, Units, fill = Fuel_Type, group = Fuel_Type) +
    geom_bar(position="stack", stat="identity", alpha=0.6, colour = "black") +
    theme_bw() +
    theme(panel.grid = element_blank(),  
          legend.position ="none"
#          legend.justification = c(0,0.5),
#          legend.position = "top"
    ) +
    labs(x = "Date", y = "# of Units Built", fill = "Fuel Type") +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu+1))) +
    #    scale_x_discrete(expand=c(0,0)) +
    scale_fill_manual(values = colours3)
}

################################################################################
# Function for plotting the capacity of resources built
################################################################################

# Stacked Area showing totals for Fuel Types
BuiltMW <- function(case) {
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period != "Study")%>%
    group_by(Fuel_Type, Time_Period) %>%
    summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
  
  data$Fuel_Type <- factor(data$Fuel_Type, 
                           levels = c("WND","SUN","Gas0","Gas1", "PS", "OT"))
  
  Tot <- data %>%
    group_by(Time_Period) %>%
    summarise(totu = sum(Units), totc = sum(Capacity))
  
  mxu <- max(Tot$totu)
  mxc <- max(Tot$totc)
  
  ggplot(data) +
    aes(Time_Period, Capacity, fill = Fuel_Type, group = Fuel_Type) +
    geom_bar(position="stack", stat="identity", alpha=0.6, colour = "black") +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          legend.position ="none"
    ) +
    labs(x = "Date", y = "Capacity Built \n(MW)", fill = "Fuel Type") +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,plyr::round_any(mxc, 100, f = ceiling))) +
    #    scale_x_discrete(expand=c(0,0)) +
    scale_fill_manual(values = colours3)
}

################################################################################
# Unit specific bar chart showing builds
################################################################################
################################################################################

Units <- function(case, Fuel) {
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period == "Study" & Fuel_Type == Fuel) 
  
  data %>%
    ggplot() +
    aes(Name, Units_Built) + 
    geom_col() +
    labs(x = "Plant Name", y = "Units Built") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,(max(data$Units_Built)+1))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#          axis.title.x = element_text(vjust=0.5),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent")) 
}
   
################################################################################
# Unit specific bar chart showing availability not built 
################################################################################
################################################################################

Slack <- function(case, Fuel) {
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period == "Study" & Fuel_Type == Fuel) 
  
  data %>%
    ggplot() +
    aes(Name, Max_Limit_Slack) + 
    geom_col() +
    labs(x = "Plant Name", y = "Units Available") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,(max(data$Max_Limit_Slack)+1))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent"))
}

################################################################################
# Unit specific bar chart showing builds with potential builds highlighted
################################################################################

Units2 <- function(case, Fuel) {
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period == "Study" & Fuel_Type == Fuel) %>%
    mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                              "Hypothetical", "AESO Queue"))
  
  data %>%
    ggplot() +
    aes(Name, Units_Built, fill = Potential) + 
    geom_col() +
    labs(x = "Plant Name", y = "Units Built") +
    scale_fill_manual(
      values = c("Hypothetical"="forestgreen", "AESO Queue"="gray"),
      guide = "none") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,(max(data$Units_Built)+1))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent")) 
}

################################################################################
# Unit specific bar chart showing availability not built with potential sites 
# highlighted
################################################################################

Slack2 <- function(case, Fuel) {
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period == "Study" & Fuel_Type == Fuel) %>%
    mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                              "Hypothetical", "AESO Queue")) 
  
  data %>%
    ggplot() +
    aes(Name, Max_Limit_Slack, fill = Potential) + 
    geom_col() +
    labs(x = "Plant Name", y = "Units Available") +
    scale_fill_manual(
      values = c("Hypothetical"="forestgreen", "AESO Queue"="gray"),
      guide = "none") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,(max(data$Max_Limit_Slack)+1)),
                       breaks = c((max(data$Max_Limit_Slack)+1)/2,(max(data$Max_Limit_Slack)+1))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent"))
}

################################################################################
# Simulation duration curve. 
# The price duration curve represents the percentage of hours in which pool price 
# equaled or exceeded a specified level.
################################################################################

Sim_dur <- function(case) {
  
  tot <- ZoneH %>%
    group_by(Condition, Report_Year)%>%
    mutate(perc = 1-ecdf(Price)(Price))
    
  tot$Report_Year <- as.factor(tot$Report_Year)
  
  ggplot() +
    geom_line(data = tot, 
              aes(x = perc, y = Price, colour = Report_Year), size = 1) +
    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
    ) +
    labs(y = "Pool Price$/MWh", x = "Percentage of Time") +
    scale_color_manual(values = c("goldenrod1", "forestgreen", "cornflowerblue")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

################################################################################
################################################################################
# Combination plotting functions defined
################################################################################
################################################################################

################################################################################
# Function to plot four years for a specific case study
################################################################################

Week4 <- function(month,day,case) {
  ggdraw(add_sub(ggarrange(Week14(Yr4Sp[[1]],month,day,case),
                           Week14(Yr4Sp[[2]],month,day,case),
                           Week14(Yr4Sp[[3]],month,day,case),
                           Week14(Yr4Sp[[4]],month,day,case),
            labels = c(Yr4Sp[[1]],Yr4Sp[[2]]),#,Yr4Sp[[3]],Yr4Sp[[4]]),
            common.legend = TRUE, legend = "right",
            ncol = 2, nrow = 2), DB))
}

################################################################################
# Function to plot Price and Output together
################################################################################

PrOt <- function(year,month,day,case) {
  plot_grid(week_price(year,month,day,case) + 
              theme(axis.title.x=element_blank(),axis.text.x=element_blank()),
            Week1(year,month,day,case)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5))
}

PrOut <- function(year,month,day,case) {
  plot_grid(Stor1(year,month,day,case),
            week_price(year,month,day,case) + theme(axis.title.x=element_blank(),
                                                    axis.text.x=element_blank()),
            Week1(year,month,day,case)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,1,2.5))
}

PrOut4 <- function(year,month,day,case) {
  plot_grid(Stor14(year,month,day,case),week_price4(year,month,day,case),
            Week14(year,month,day,case)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,1,2.5))
}

################################################################################
# Function for plotting the month/year profile with the build
################################################################################

EvalOut <- function(input,case) {
  p1 <- plot_grid(Eval(input,case) + theme(legend.position="top"), 
            Builtcol(case)+theme(legend.position ="none",
                                 axis.title.x = element_blank(),
                                 axis.text.x = element_blank()), 
            BuiltMW(case)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(2.5,0.8,1))
  
  ggdraw(add_sub(p1,paste("Simulation: ",DB, sep = "")))
}

################################################################################
# Function to plot four years for a specific case study of the combined plots
################################################################################

BuildUnits <- function(case, Fuel) {
  p1 <- plot_grid(Units(case,Fuel)+theme(axis.title.x = element_blank(),
                                   axis.text.x = element_blank()),
            Slack(case,Fuel), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,1))
  
  ggdraw(add_sub(p1,paste("Simulation: ",DB, sep = "")))
}

BuildUnits2 <- function(case, Fuel) {
  p1 <- plot_grid(Units2(case,Fuel)+theme(axis.title.x = element_blank(),
                                         axis.text.x = element_blank(),
                                         text = element_text(size= 15)),
                  Slack2(case,Fuel)+theme(text = element_text(size= 15)),
                  ncol = 1, align="v", axis = "l",rel_heights = c(1,1.5))
  
  ggdraw(add_sub(p1,paste("Simulation: ",DB, sep = "")))
}
  
################################################################################
# Function to plot four years for a specific case study of the combined plots
################################################################################

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

Eval2 <- function(month,day,case) {
  ggarrange(arrangeGrob(PrOut(Yr2Sp[[1]],month,day,case)+theme(legend.position ="none"),
                        PrOut(Yr2Sp[[2]],month,day,case)+theme(legend.position ="none"),
                        ncol=2),
            g_legend(Week1(Yr2Sp[[1]],month,day,case)),
            ncol = 2, widths=c(12,1))
}

Eval4 <- function(month,day,case) {
  ggarrange(arrangeGrob(PrOut4(Yr4Sp[[1]],month,day,case)+theme(legend.position ="none"),
                        PrOut4(Yr4Sp[[2]],month,day,case)+theme(legend.position ="none"),
                        PrOut4(Yr4Sp[[3]],month,day,case)+theme(legend.position ="none"),
                        PrOut4(Yr4Sp[[4]],month,day,case)+theme(legend.position ="none"),
                        ncol=4),
            ggdraw(
            add_sub(g_legend(Week1(Yr4Sp[[1]],month,day,case)), 
                    paste("Simulation: \n",DB, sep = ""))),
            ncol = 2, widths=c(7,1))
}
}

subtit <- function(plot) {
  ggdraw(add_sub(plot,paste("Simulation: ",DB, sep = "")))
}

imsave <- function(name) {
  ggsave(path = "images", filename = paste(name,".png", sep = ""), bg = "transparent")
}

################################################################################
################################################################################
# AESO Actual Data
################################################################################
################################################################################

################################################################################
# Load data
################################################################################
{
  setwd("D:/Documents/Education/Masters Degree/Datasets/Market")
  
  load("nrgstream_gen.RData") 
  load("all_merit.RData")
  library(readr)
  merit <- read_csv("student_data_2021_Jul_23_14_09.csv.gz")
#  library(data.table)
#  dt = fread("student_data_2021_Jul_23_14_09.csv.gz")
  merit_filt <- filter(merit, date >= as.Date("2020-01-1"), date <= as.Date("2020-12-31"))
  
  nrgstream_gen <- nrgstream_gen %>% rename(time=Time)
#  merit_data<-merit_data[!is.na(nrgstream_gen$date),] 
  merit_samp <- filter(merit_data, date >= as.Date("2020-01-1"), date <= as.Date("2020-12-31"))

  setwd("D:/Documents/GitHub/AuroraEval")
    
  errors<-nrgstream_gen %>% filter(is.na(Price),date<Sys.Date())
  gen_errors<-nrgstream_gen %>% filter(is.na(gen),date<Sys.Date())
  
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 
  
  sub_samp<-filter(nrgstream_gen, time >= as.Date("2018-01-1"))
  
  demand <- sub_samp %>%
    group_by(time) %>%
    summarise(Demand = median(Demand), 
              Price = median(Price),
              AIL = median(AIL))
  
  trade_excl<-c("AB - WECC Imp Hr Avg MW", "AB - WECC Exp Hr Avg MW","AB - WECC Imp/Exp Hr Avg MW")
  
  df1 <- sub_samp %>% 
    filter(! NRG_Stream %in% trade_excl)%>% 
    group_by(Plant_Type,time) %>% 
    summarise(meancap = mean(Cap_Fac),
              total_gen=sum(gen,na.rm = T),
              total_rev=sum(Revenue,na.rm = T),
              price_mean=mean(Price),
              heatrt_mean=mean(Heat.Rate)) %>% 
    ungroup()
  
  df1$Day <- date(df1$time)
  df1$Year <- as.factor(year(df1$time))
  
  # Identify the Plant Types
  ################################################################################
  gen_set<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND")
  
  df1a <- df1 %>%
    filter(Plant_Type %in% gen_set,year(time)<2022)
  
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "OTHER",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "HYDRO",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "WIND",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "SOLAR",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "IMPORT",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "EXPORT",after=Inf)
  
  # Identify Specific Plant traits
  ################################################################################
  plnt_tr <- function(Asset_ID) {
    plnt <- sub_samp %>%
      filter(ID == Asset_ID) %>%
      subset(., select = c(time, Price, gen, Capacity, Plant_Type, AESO_Name, CO2, Heat.Rate, Revenue, Cap_Fac))
  }
  
  # Establish image theme
  ################################################################################
  slide_theme<-function(){
    return( theme(panel.border = element_blank(),
                  panel.grid = element_blank(),
                  panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
                  axis.line.x = element_line(color = "gray"),
                  axis.line.y = element_line(color = "gray"),
                  axis.text = element_text(size = 16),
                  axis.text.x = element_text(margin = margin(t = 10)),
                  axis.title = element_text(size = 16),
                  #axis.label.x = element_text(size=20,vjust=+5),
                  plot.subtitle = element_text(size = 12,hjust=0.5),
                  plot.caption = element_text(face="italic",size = 12,hjust=0),
                  legend.key.width=unit(2,"line"),
                  legend.position = "bottom",
                  #legend.direction = "horizontal",
                  #legend.box = "horizontal",
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 16),
                  plot.title = element_text(hjust=0.5,size = 20),
                  plot.margin=unit(c(1,1,1.5,1.2),"cm")
    )
    )
  }
}

################################################################################
################################################################################
# Function to plot actual AESO Output data
################################################################################
################################################################################

Week_act <- function(year,month,day) {
  
  colours = c("darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
              "darkcyan", "dodgerblue", "forestgreen", "gold", "cyan")
  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
  
  #wk_st <- hms::as.hms(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  #wk_end <- hms::as.hms(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
  
  df1a$time <- as.POSIXct(df1a$time, tz = "MST")
  
  # Select only a single week
  ##############################################################################
  WKa <- df1a %>%
    filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT") %>%
    filter(time >= wk_st, time <= wk_end)
  
  WKIM <- df1a %>%
    filter(Plant_Type == "IMPORT") %>%
    filter(time >= wk_st & time <= wk_end)
  
  WKIM$total_gen <- WKIM$total_gen * -1
  
  WK <- rbind(WKIM, WKa)
  
  {
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "IMPORT", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COAL", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COGEN", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SCGT", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCC", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "HYDRO", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "OTHER", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "WIND", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SOLAR", after = Inf)
  }
  
  WK$Plant_Type <- factor(WK$Plant_Type, levels=c("IMPORT", "COAL", "COGEN", 
                                                  "SCGT", "NGCC", "HYDRO", 
                                                  "OTHER", "WIND", "SOLAR"))
  
  levels(WK$Plant_Type) <- c("Import","Coal", "Cogen", "SCGT", "NGCC", "Hydro", 
                             "Other", "Wind", "Solar")
  
  dmd <- demand %>%
    filter(time >= wk_st & time <= wk_end)

  # Plot the data    
  ##############################################################################
  ggplot() +
    geom_area(data = WK, aes(x = time, y = total_gen, fill = Plant_Type), 
              alpha=0.6, size=.5, colour="black") +
    
    # Add hourly load line
    geom_line(data = dmd, 
              aes(x = time, y = Demand), size=2, colour = "black") +
    
    scale_x_datetime(expand=c(0,0)) +
    
    # Set the theme for the plot
    ############################################################################
  theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "right",
    ) +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x = "Date", y = "Output (MWh)", fill = "AESO Data: \nResource") +
    scale_fill_manual(values = colours)
}

################################################################################
################################################################################
# Plot the AESO pool price
################################################################################
################################################################################

wkPrice <- function(year,month,day) {
  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+14,month,year, sep = "/"), format="%d/%m/%Y")
  
  price_WK <- demand %>%
    filter(time >= wk_st & time <= wk_end)
  
  ggplot() +
    geom_line(data = price_WK, 
              aes(x=time, y=Price), 
              size = 1.5, color="red") +
    scale_x_datetime(expand=c(0,0)) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x = "Date", y = "Pool Price \n($/MWh)")
}

################################################################################
################################################################################
# Plot combinations of plots
################################################################################
################################################################################

#g_legend<-function(a.gplot){
#  tmp <- ggplot_gtable(ggplot_build(a.gplot))
#  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#  legend <- tmp$grobs[[leg]]
#  return(legend)}

AESO_PrOt <- function(year,month,day) {
  plot_grid(wkPrice(year,month,day) + 
              theme(axis.title.x=element_blank(),axis.text.x=element_blank()),
            Week_act(year,month,day)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5))
}

################################################################################
# Plot comparison between actual and simulated data
################################################################################

AESO_Sim <- function(year,month,day,case) {
  SimP <- week_price(year,month,day,case)
  ActP <- wkPrice(year,month,day)
  SimO <- Week1(year,month,day,case)
  ActO <- Week_act(year,month,day)
  
  MXP <- plyr::round_any(
    max(layer_scales(SimP)$y$range$range,layer_scales(ActP)$y$range$range),
    10, f = ceiling)
  MNP <- plyr::round_any(
    min(layer_scales(SimP)$y$range$range,layer_scales(ActP)$y$range$range),
    10, f = floor)  
  MXO <- plyr::round_any(
    max(layer_scales(SimO)$y$range$range,layer_scales(ActO)$y$range$range),
    100, f = ceiling)
  MNO <- plyr::round_any(
    min(layer_scales(SimO)$y$range$range,layer_scales(ActO)$y$range$range),
    100, f = floor)
  
  legend <- gtable_filter(ggplotGrob(Week1(year,month,day,case)), "guide-box")
  
  ggarrange(arrangeGrob(plot_grid(week_price(year,month,day,case) + 
                                    theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = pretty_breaks(4)),
                                  Week1(year,month,day,case)+
                                    theme(legend.position ="none") + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = pretty_breaks(4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5))+
                          ggtitle(paste("Simulated Data for ",year," (",DB,")", sep = ""))+
                          theme(legend.position ="none",
                                plot.title = element_text(hjust = 0.5)),
                        
                        plot_grid(wkPrice(year,month,day) + 
                                    theme(axis.title=element_blank(),
                                          axis.text.x=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = pretty_breaks(4)),
                                  Week_act(year,month,day)+
                                    theme(legend.position ="none",
                                          axis.title.y=element_blank())+#,
                                     #     axis.text.y=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = pretty_breaks(4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5)) +
                          ggtitle(paste("AESO Data for ",year,sep=""))+
                          theme(legend.position ="none",
                                plot.title = element_text(hjust = 0.5)),
                        ncol=2, widths = c(1.05,1)),
            
            legend,
            
#            arrangeGrob(g_legend(Week1(year,month,day,case)),
 #                       g_legend(Week_act(year,month,day)),
  #                      nrow=2),
            ncol=2, widths =c(5,1))#= unit.c(unit(1, "npc")-legend$width, legend$width))
}

#ggsave(path = "images", filename = "simvsact.png", bg = "transparent")


################################################################################
# Plot difference between simulated and actual pool price
################################################################################

comp_dur <- function(year1, year2, case) {
  
  totSim <- ZoneH %>%
    filter(Report_Year >= year1 & 
           Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    group_by(Condition, Report_Year) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    select(Condition, Report_Year, Price, perc) %>%
    rename(Year = Report_Year) %>%
    ungroup() %>%
    mutate(sit = "Simulated")
  
#  totSim$Report_Year <- as.factor(totSim$Report_Year)
  
  Actual <- na.omit(demand)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Hour <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%H")

  totAct <- Actual %>%
    filter(Year >= year1, 
           Year <= year2,) %>%
    mutate(Condition = if_else(between(Hour, 8, 23), 
                               "On-Peak WECC", "Off-Peak WECC")) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    select(Condition, Year, Price, perc) %>%
    ungroup() %>%
    mutate(sit = "Actual")
  
  total <- rbind(totSim, totAct)
  
#  totAct$Year <- as.factor(totAct$Year)
  
  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Price, colour = Year, linetype = sit), size = 1) +
    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.title = element_blank()
    ) +
    labs(y = "Pool Price$/MWh", 
         x = "Percentage of Time", 
         title = "AESO Data vs Simulation",
         subtitle = DB) +
    scale_color_manual(values = c("goldenrod1", "forestgreen", "cornflowerblue","gray60")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

year_comp <- function(year,case) {
  # Filters for the desired case study
  
#  setwd("D:/Documents/Education/Masters Degree/Aurora/R Code")
#  write.csv(ZH, file="sim_price.csv")
#  sim <- read.csv("sim_price.csv", header = TRUE)

  wk_st <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(01,01,year+1, sep = "/"), format="%d/%m/%Y")
  
  sim <- ZH
#  sim$Date <- as.POSIXct(as.character(sim$date), tz = "MST")
  
  sim_wk <- sim %>%
    filter(date >= wk_st & date <= wk_end & Run_ID == case) %>%
    subset(., select = c(date, Price))

  
  act <- demand
  act$ActPrice <- act$Price
  
  act_wk <- act %>%
    filter(time >= wk_st & time <= wk_end) %>%
    subset(., select = c(time, ActPrice))
  colnames(act_wk) <- c("date","ActPrice")
  
  data <- merge(sim_wk, act_wk, by.x="date", by.y="date")

  data$diff <- (data$Price - data$ActPrice)

  # Plot the data    
  ggplot() +
    geom_line(data = data, 
              aes(x = date, y = diff), 
              size = 1.5, colour = "red") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15)
    ) +
    ggtitle(paste("Simulated Data from "," (",DB,")", sep = ""))+
    labs(x = year,
         y = "Difference in Simulated and \n Actual Pool Price ($/MWh)", 
         fill = "Resource") +
    scale_x_datetime(expand=c(0,0))
}

cap_pf <- function(asset_id){
  data <- plnt_tr(asset_id)
  name <- data[1,6]
  type <- data[1,5]
  data1 <- data %>%
    filter(Cap_Fac > 0)# %>%
#    group_by(Cap_Fac) %>%
#    summarise(avPrice = mean(Price))
  breaks= c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  tags <- c("0-10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","90-100%")
  data1$bins <- cut(data1$Cap_Fac, 
                    breaks= breaks,
                    labels=tags)
  data1 <- na.omit(data1)
  
  data2 <- data1 %>%
    group_by(bins) %>%
    summarise(avPrice = mean(Price))
  
  ggplot(data2, aes(x = bins, y = avPrice, group=bins)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = paste("$",round(avPrice, digits = 0),sep="")), 
                  vjust = -0.3, size = 4)+#, angle = 90) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,750)) +
    ggtitle(paste(name, type, sep = ": "))+
    labs(x = "% of Capacity Dispatched",
         y = "Average Pool Price \n($/MWh)")
  
#  ggplot() +
#    geom_point(data = data1,
#               aes(x = Cap_Fac, y = Price)) +
#    theme_bw() +
#    theme(panel.background = element_rect(fill = "transparent"),
#          panel.grid = element_blank(),
#          plot.background = element_rect(fill = "transparent", color = NA),
#          text = element_text(size= 15),
#          plot.title = element_text(hjust = 0.5)
#    ) +
#    scale_x_continuous(expand=c(0,0),
#                       limits = c(0,1)) +
#    scale_y_continuous(expand=c(0,0),
#                       limits = c(0,(max(data1$Price, na.rm = TRUE)+100))) +
#    ggtitle(paste("Average Capture Price for ", asset_id, " (", type, ")", sep = ""))+
#    labs(x = "% of Capacity Dispatched",
#         y = "Average Pool Price ($/MWh)")
}

#poly <- lm(avPrice~poly(Cap_Fac,4,raw=TRUE), data=data1)
#exp <- lm(avPrice~log(Cap_Fac), data=data1)

Cap3 <- function(plant1,plant2,plant3) {
  ggarrange(cap_pf(plant1), cap_pf(plant2), cap_pf(plant3),
            ncol = 2, nrow = 2)
}

Cap4 <- function(plant1,plant2,plant3,plant4) {
  ggarrange(cap_pf(plant1), cap_pf(plant2), cap_pf(plant3), cap_pf(plant4),
            ncol = 2, nrow = 2)
}
}

year_dif <- function(year,case) {
  # Filters for the desired case study

  wk_st <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(31,12,year, sep = "/"), format="%d/%m/%Y")
  
  sim <- ZH
  sim_wk <- sim %>%
    filter(date >= wk_st & date <= wk_end & Run_ID == case) %>%
    subset(., select = c(date, Price))

  act <- demand
  act_wk <- act %>%
    filter(time >= wk_st & time <= wk_end) %>%
    subset(., select = c(time, Price))
  colnames(act_wk) <- c("date","actPrice")
  
  data <- merge(sim_wk, act_wk, by.x="date", by.y="date")
  
  results <- data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(Price = mean(Price), actPrice = mean(actPrice))
  results$month <- as.Date(results$month)
  
  results$diff <- (results$Price - results$actPrice)
  
  mx <- plyr::round_any(max(results$diff), 20, f = ceiling)
  mn <- plyr::round_any(min(results$diff), 20, f = floor)
  
  # Plot the data    
  ggplot(results, aes(x = month, y = diff)) +
    geom_bar(stat="identity", alpha = 0.7) +
    geom_text(aes(label = paste("$",round(diff, digits = 0),sep="")), 
                  vjust = -0.3, size = 4)+#, angle = 90) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(-60,140), 
                       breaks = pretty_breaks(8)) +
    scale_x_date(date_labels="%B", date_breaks = "months") +
    ggtitle(paste("Monthly Price Average Differences"," (",DB,")", sep = ""))+
    labs(x = year,
         y = "Difference in Simulated and Actual \nAverage Pool Price ($/MWh)",
         colour = element_blank())
}

year_avg <- function(year,case) {
  # Filters for the desired case study

  wk_st <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(31,12,year, sep = "/"), format="%d/%m/%Y")
  
  sim <- ZH
  sim_wk <- sim %>%
    filter(date >= wk_st & date <= wk_end & Run_ID == case) %>%
    subset(., select = c(date, Price))
  
  sim_wk$sit <- "Simulated"
  
  act <- demand
  act$sit <- "Actual"
  
  act_wk <- act %>%
    filter(time >= wk_st & time <= wk_end) %>%
    subset(., select = c(time, Price, sit))
  colnames(act_wk) <- c("date","Price","sit")
  
  data <- rbind(sim_wk, act_wk)

  results <- data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month,sit) %>%
    summarize(avPrice = mean(Price), sd = sd(Price))
  results$month <- as.Date(results$month)
  
  mx <- plyr::round_any(max(results$avPrice), 20, f = ceiling)

  # Plot the data    
  ggplot(results, aes(x = month, y = avPrice, fill=sit, colour = sit)) +
    geom_bar(stat="identity", position = "dodge", alpha = 0.7) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)
    ) +
    scale_fill_manual(values = c("forestgreen","dodgerblue")) +
    scale_color_manual(values = c("forestgreen","dodgerblue")) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,160), 
                       breaks = pretty_breaks(6)) +
    scale_x_date(date_labels="%B", date_breaks = "months") +
    ggtitle(paste("Monthly Price Averages"," (",DB,")", sep = ""))+
    labs(x = year,
         y = "Average Pool Price \n($/MWh)",
         fill = element_blank(),
         colour = element_blank())
}
