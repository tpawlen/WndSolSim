# Generates functions used to plot simulation outputs
#
# The entire code can be run once per session, then just the functions can be 
# used for each iteration.
#
# Eval4(month, day, case) plots the output of a single specified week over 4 
#    years for a specified case study with storage separate on top, price in the  
#    middle, and output on the bottom. Basically 4 PrOuts.
# EvalOut(input, case) plots the output averaged over the specified time period 
#    (month, year) on top with the resources built on the bottom.
# BuildUnits(case,Fuel) plots the units built for a fuel type along with the 
#     available units not built.
# PrOut(year, month, day, case) plots the output of a single specified week for
#    a specified case study with storage separate on top, price in the middle, 
#    and output on the bottom.
#
# Week1(year, month, day, case) plots the output of a single specified week for
#    a specified case study.
# Stor1(year, month, day, case) plots the storage output of a single specified 
#    week for a specified case study.
# Week4(month, day, case) plots the output of a single specified week for 2020,
#    2025, 2030, & 2035 for a specified case study.
# week_price(year, month, day, case) plots the price of electricity for a single 
#    specified week for a specified case study.
# Eval(input, case) plots the output averaged over the specified time period 
#    (month, year)
# Built(case) plots the resources built over the time span of the study.
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
  }

{
# Connect to SQL database
################################################################################
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "192.168.0.139,49172",
                 Database = "Mar_14_2022",
                 UID = "admin",
                 PWD = "SOB704910",
                 Port = 49172)

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


 # Set variables to identify the case studies
################################################################################

Yr1 <- 2020
Yr2 <- 2021
Yr3 <- 2030
Yr4 <- 2035
  
BC <- "Base Case"
MS  <- "Minimum Solar Constraint"
LCT <- "Low Carbon Tax"


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


# Converts the date and time and identifies the week when applicable
################################################################################

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

Export$Output_MWH <- Export$Output_MWH * -1

ImEx <- rbind(Import, Export)
}

################################################################################
# General functions defined ####################################################
################################################################################

################################################################################
# This function filters for the data that will be evaluated.
################################################################################

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
  }
  return(case)
}

# Filters for storage data
stor_filt <- function(inputdata) {
  # Filter the data by resource
  stor <- inputdata %>%
    filter(ID=="LTO_Storage")
  return(stor)
}

# Function to convert the date time for plotting
HrTime <- function(data, year, month, day) {
    subset(data,
           (date >= paste(year,"-", month, "-", day," 01:00:00", sep = "") & 
              date <= 
              paste(year,"-", month, "-", (day+7)," 23:00:00", sep = "")))
}
  
################################################################################
################################################################################
# Plotting functions defined
################################################################################
################################################################################

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
  
  data$ID <- factor(data$ID, levels=c("Import", "LTO_Coal", 
                                      "LTO_Coal2Gas", "LTO_Cogen", "LTO_NatGas", 
                                      "LTO_Other", "LTO_Wind", "LTO_Solar", 
                                      "LTO_Storage"))
  
  # Select only a single week
    WK <- HrTime(data,year,month,day)
    ZPrice <- HrTime(ZH,year,month,day)
  
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
          legend.position = "bottom",
          ) +
    scale_y_continuous(expand=c(0,0), limits = c(NA,ylimit)) +
#    ylim(NA,ylimit) +
    labs(x = "Date", y = "Output (MWh)", fill = "Resource") +
    scale_fill_manual(values = colours1)
}

# Generate weekly storage output plot function
################################################################################
Stor1 <- function(year, month, day, case) {
  # Add imports and exports to data
  
  
  # Filters for the desired case study
  data <- Hour_pl %>%
    stor_filt(.) %>%
    filter(Run_ID == case)

  
  # Select only a single week
  WK <- HrTime(data,year,month,day)
  MX <- max(WK$Output_MWH)
  
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
                       labels = label_number(accuracy = 1))
  #, limits = c(NA, prlimit)) 
}


################################################################################
# Function for plotting month/year profiles
################################################################################

Eval <- function(input,case) {
  # Filters for the desired case study
  data <- input %>%
    filter(Run_ID == case & Condition == "Average")
  
  # Filter the data by resource
  case_Time <- sim_filt(data)
  
  case_Time %>%
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
    scale_fill_manual(values = colours2) +
    labs(x = "Date", y = "Output (GWh)", fill = "Resource") 
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
          legend.justification = c(0,0.5)) +
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
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) 
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
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1))
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
  ggarrange(Week1(Yr1,month,day,case),Week1(Yr2,month,day,case),
            Week1(Yr3,month,day,case),Week1(Yr4,month,day,case),
            labels = c(Yr1,Yr2),#,Yr3,Yr4),
            common.legend = TRUE, legend = "right",
            ncol = 2, nrow = 2)
}

################################################################################
# Function to plot Price and Output together
################################################################################

PrOut <- function(year,month,day,case) {
  plot_grid(Stor1(year,month,day,case),week_price(year,month,day,case),
            Week1(year,month,day,case)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,1,2.5))
}

################################################################################
# Function for plotting the month/year profile with the build
################################################################################

EvalOut <- function(input,case) {
  plot_grid(Eval(input,case) + theme(legend.position="top"), 
            Built(case)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(3,1))
}

################################################################################
# Function to plot four years for a specific case study of the combined plots
################################################################################

BuildUnits <- function(case, Fuel) {
  plot_grid(Units(case,Fuel)+theme(axis.title.x = element_blank(),
                                   axis.text.x = element_blank()),
            Slack(case,Fuel), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,1))
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
  ggarrange(arrangeGrob(PrOut(Yr1,month,day,case)+theme(legend.position ="none"),
                        PrOut(Yr2,month,day,case)+theme(legend.position ="none"),
                        ncol=2),
            g_legend(Week1(Yr1,month,day,case)),
            nrow = 2, heights=c(12,1))
}

Eval4 <- function(month,day,case) {
  ggarrange(arrangeGrob(PrOut(Yr1,month,day,case)+theme(legend.position ="none"),
                        PrOut(Yr2,month,day,case)+theme(legend.position ="none"),
                        PrOut(Yr3,month,day,case)+theme(legend.position ="none"),
                        PrOut(Yr4,month,day,case)+theme(legend.position ="none"),
                        ncol=4),
            g_legend(Week1(Yr1,month,day,case)),
            nrow = 2, heights=c(12,1))
}

