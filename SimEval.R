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

{library(tidyverse)
  library(ggplot2)
  library(gridExtra)
  library(plotly) # For interactive charts
  library(odbc)
  library(ggpubr)
  library(DBI)
  library(lubridate)
  library(cowplot)
  library(scales)
  library(dplyr)
  }

{
  DB <- "Apr_1_2022"
# Connect to SQL database
################################################################################
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "192.168.0.139,49172",
                 Database = DB,
                 UID = "admin",
                 PWD = "SOB704910",
                 Port = 49172)

  {
# Write data to environment and set variables
################################################################################
data_raw_Hour <- dbReadTable(con,'ResourceGroupHour1')
data_raw_Month <- dbReadTable(con,'ResourceGroupMonth1')
data_raw_Year  <- dbReadTable(con,'ResourceGroupYear1')
data_raw_Build <- dbReadTable(con,'LTBuildReport1')
data_ZoneHour <- dbReadTable(con,'ZoneHour1')
data_Resource <- dbReadTable(con,'ResourceMonth1')
data_LTResValue <- dbReadTable(con,'LTResValue1')

Hour <- data_raw_Hour
Month <- data_raw_Month
Year  <- data_raw_Year
Build <- data_raw_Build
ZoneHour <- data_ZoneHour
LTRes <- data_LTResValue
}

 # Set variables to identify the case studies
################################################################################
{
  Yr4Sp <- list(2020,2030,2035,2040)
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
            "dodgerblue", "forestgreen", "gold", "cyan")
colours2 = c("grey", "darkslategrey", "coral4", "goldenrod4", 
            "dodgerblue", "forestgreen", "gold", "cyan")
colours3 = c("forestgreen", "gold", "coral4", "goldenrod4", "cyan", "dodgerblue")
}

# Converts the date and time and identifies the week when applicable
################################################################################
{
Hour$date        <- ymd_h(gsub(" Hr ", "_",Hour$Time_Period))
Month$Time_Period <- ym(Month$Time_Period)
Year$Time_Period  <- as.Date(as.character(Year$Time_Period), 
                                  format = "%Y")
ZoneHour$date        <- ymd_h(gsub(" Hr ", "_",ZoneHour$Time_Period))

# Selects only the required columns
################################################################################

Hour_pl <- Hour %>%
  subset(., select = c(ID, date, Output_MWH, Run_ID))

ZH <- ZoneHour %>%
  filter(Name == "WECC_Alberta") %>%
  filter(Condition == "Average") %>%
  subset(., select = c(date, Price, Baseline_Demand, Demand, Demand_Total,
                       Net_Load, Net_Load_Total, Marginal_Resource, 
                       Smp_Max_Date_Time, Smp_Max_Demand, Smp_Max_Capacity, 
                       Run_ID, Imports, Exports))

# Select the Import/Export data
Import <- ZH %>%
  subset(., select = c(date, Imports, Run_ID)) %>%
  'colnames<-'(c("date", "Output_MWH", "Run_ID")) %>%
  add_column(ID = "Import")

Export <- ZH %>%
  subset(., select = c(date, Exports, Run_ID)) %>%
  'colnames<-'(c("date", "Output_MWH", "Run_ID")) %>%
  add_column(ID = "Export")

#Export$Output_MWH <- Export$Output_MWH * -1

ImEx <- rbind(Import, Export)
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
  Cogen  <- inputdata %>%
    filter(ID=="LTO_Cogen")
  NatGas <- inputdata %>%
    filter(ID=="LTO_NatGas")
  Other <- inputdata %>%
    filter(ID=="LTO_Other")
  Solar <- inputdata %>%
    filter(ID=="LTO_Solar")
  Storage <- inputdata %>%    
    filter(ID=="LTO_Storage")
  Wind <- inputdata %>%
    filter(ID=="LTO_Wind")
  }
  
  # Combine the grouped data
  {case <- rbind(Coal, Coal2Gas, Cogen, NatGas, Solar, Wind, Storage, Other)
    case$ID <- factor(case$ID, levels=c("LTO_Coal", "LTO_Coal2Gas", "LTO_Cogen", 
                                        "LTO_NatGas", "LTO_Other", "LTO_Wind", 
                                        "LTO_Solar", "LTO_Storage"))
    levels(case$ID) <- c("Coal", "Coal2Gas", "Cogen", "NatGas", "Other", "Wind",
                         "Solar", "Storage")
  }
  return(case)
}

# Function to convert the date time for plotting
HrTime <- function(data, year, month, day) {
    subset(data,
           (date >= paste(year,"-", month, "-", day," 01:00:00", sep = "") & 
              date <= 
              paste(year,"-", month, "-", (day+7)," 23:00:00", sep = "")))
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
    # Add imports and exports to data
    
    
    # Filters for the desired case study
    data <- Hour_pl %>%
      sim_filt(.) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    data$ID <- factor(data$ID, levels=c("Import", "Coal", "Coal2Gas", "Cogen", 
                                        "NatGas", "Other", "Wind", "Solar", 
                                        "Storage"))
    
    # Select only a single week
    WK <- HrTime(data,year,month,day)
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
      labs(x = "Date", y = "Output (MWh)", fill = "Simulated Data: \nResource") +
      scale_fill_manual(values = colours1)
  }
  
################################################################################
# Functions for weekly evaluation over four years
################################################################################  
  
Week14 <- function(year, month, day, case) {
  # Add imports and exports to data
  
  
  # Filters for the desired case study
  data <- Hour_pl %>%
    sim_filt(.) %>%
    rbind(.,Import) %>%
    filter(Run_ID == case)
  
  data$ID <- factor(data$ID, levels=c("Import", "Coal", "Coal2Gas", "Cogen", 
                                      "NatGas", "Other", "Wind", "Solar", 
                                      "Storage"))
  
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

# Generate weekly storage output plot function
################################################################################
Stor1 <- function(year, month, day, case) {
  # Add imports and exports to data
  
  
  # Filters for the desired case study
  data <- Hour_pl %>%
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

Stor14 <- function(year, month, day, case) {
  # Add imports and exports to data
  
  
  # Filters for the desired case study
  data <- Hour_pl %>%
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
# Function for plotting month/year profiles
################################################################################

Eval <- function(input,case) {
  Imp <- Import %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$date, format="%Y")) %>%
    group_by(Time_Period) %>%
    summarise(Output_MWH = mean(Output_MWH)) %>%
    mutate(ID = "Import")
  
  Imp$Time_Period  <- as.Date(as.character(Imp$Time_Period), 
                               format = "%Y")
  
  Imp <- subset(Imp, Time_Period <= '2040-04-05')
  
  # Filters for the desired case study
  data <- input %>%
    filter(Run_ID == case & Condition == "Average") %>%
    select(ID, Time_Period, Output_MWH) %>%
    sim_filt(.) %>%
    rbind(.,Imp) 
    
  data$ID<-fct_relevel(data$ID, "Import")
  
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
    scale_x_date(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_manual(values = colours1) +
    labs(x = "Date", y = "Output (GWh)", fill = "Resource") 
}

################################################################################
# Function for plotting month/year profiles as a percentage of total
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
    scale_y_continuous(expand=c(0,0)) +
    scale_x_discrete(expand=c(0,0)) +
    scale_fill_manual(values = colours3)
}

# Unit specific bar chart showing builds
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
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent")) 
}
   
# Unit specific bar chart showing availability not built 
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
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent"))
}

# Unit specific bar chart showing builds with potential builds highlighted
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
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent")) 
}

# Unit specific bar chart showing availability not built with potential sites 
# highlighted
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
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
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
            Built(case)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(3,1))
  
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
            nrow = 2, heights=c(12,1))
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

imsave <- function(name) {
  ggsave(path = "images", filename = paste(name,".png", sep = ""), bg = "transparent")
}

# Load data
################################################################################
{
  setwd("D:/Documents/GitHub/AuroraEval")
  
  load("nrgstream_gen.RData") ## which is here *equivalent* to
  
  nrgstream_gen <- nrgstream_gen %>% rename(time=Time)
  
  errors<-nrgstream_gen %>% filter(is.na(Price),date<Sys.Date())
  gen_errors<-nrgstream_gen %>% filter(is.na(gen),date<Sys.Date())
  
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 
  
  sub_samp<-filter(nrgstream_gen, time >= as.Date("2020-01-1"))
  
  demand <- sub_samp %>%
    group_by(time) %>%
    summarise(Demand = median(Demand), 
              Price = median(Price),
              AIL = median(AIL))
  
  df1 <- sub_samp %>% 
    filter(! NRG_Stream %in% trade_excl)%>% 
    group_by(Plant_Type,time) %>% 
    summarise(meancap = mean(Cap_Fac),
              total_gen=sum(gen,na.rm = T),
              total_rev=sum(Revenue,na.rm = T),
              p_mean=mean(Price)) %>% 
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
  
  # Select only a single week
  ##############################################################################
  WK <- df1a %>%
    filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT") %>%
    filter(time >= wk_st & time <= wk_end)
  
  WKIM <- df1a %>%
    filter(Plant_Type == "IMPORT") %>%
    filter(time >= wk_st & time <= wk_end)
  
  WKIM$total_gen <- WKIM$total_gen * -1
  
  WK <- rbind(WKIM, WK)
  
  {
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "IMPORT", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COAL", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCC", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COGEN", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SCGT", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "HYDRO", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "OTHER", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "WIND", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SOLAR", after = Inf)
  }
  
  WK$Plant_Type <- factor(WK$Plant_Type, levels=c("IMPORT", "COAL", "NGCC", 
                                                  "COGEN", "SCGT", "HYDRO", 
                                                  "OTHER", "WIND", "SOLAR"))
  
  levels(WK$Plant_Type) <- c("Import","Coal", "NGCC", "Cogen", "SCGT", "Hydro", 
                             "Other", "Wind", "Solar")
  
  dmd <- demand %>%
    filter(time >= wk_st & time <= wk_end)
  
  MX <- plyr::round_any(rng[2], 100, f = ceiling)
  MN <- plyr::round_any(rng[1], 100, f = floor)
  
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

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

AESO_PrOt <- function(year,month,day) {
  plot_grid(wkPrice(year,month,day) + 
              theme(axis.title.x=element_blank(),axis.text.x=element_blank()),
            Week_act(year,month,day)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5))
}

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
  
  ggarrange(arrangeGrob(plot_grid(week_price(year,month,day,case) + 
                                    theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = seq(MNP, MXP, by = MXP/4)),
                                  Week1(year,month,day,case)+
                                    theme(legend.position ="none") + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = seq(MNO, MXO, by = MXO/4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5))+
                          ggtitle(paste("Simulated Data for ",year," (",DB,")", sep = ""))+
                          theme(legend.position ="none",
                                plot.title = element_text(hjust = 0.5)),
                        
                        plot_grid(wkPrice(year,month,day) + 
                                    theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = seq(MNP, MXP, by = MXP/4)),
                                  Week_act(year,month,day)+
                                    theme(legend.position ="none") + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = seq(MNO, MXO, by = MXO/4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5)) +
                          ggtitle(paste("AESO Data for ",year,sep=""))+
                          theme(legend.position ="none",
                                plot.title = element_text(hjust = 0.5)),
                        ncol=2),
            
            arrangeGrob(g_legend(Week1(year,month,day,case)),
                        g_legend(Week_act(year,month,day)),
                        nrow=2),
            ncol=2, widths = c(5,1))
}

ggsave(path = "images", filename = "simvsact.png", bg = "transparent")