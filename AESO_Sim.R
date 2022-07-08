# Functions defined
################################################################################
{# Generates functions used to plot simulation outputs
#
# The entire code can be run once per session, then just the functions can be 
# used for each iteration.
#
# AESO_Sim(year, month, day, case) 
{#    compares a single week of AESO data against 
#    the same week of simulated data
}
# comp_dur(year1, year2, case) 
{#    compares duration curves of actual and simulated 
#    data.  
}
#  
# Eval4(month, day, case) 
{#    plots the output of a single specified week over 4 
#    years for a specified case study with storage separate on top, price in the  
#    middle, and output on the bottom. Basically 4 PrOuts.
}
# Eval2(month, day, case) 
{#    plots the output of a single specified week over 2 
#    years for a specified case study with storage separate on top, price in the  
#    middle, and output on the bottom. Basically 2 PrOuts.
}
  
# EvalOut(input, case) 
{#    plots the output averaged over the specified time period 
#    (month, year) on top with the resources built on the bottom.
}
# EvalPerc(input, case) 
{#    plots the output of each generation type as a percentage
#     of total generation
}
# BuildUnits(case,Fuel) 
{#    plots the units built for a fuel type along with the 
#     available units not built.
}
# BuildUnits2(case,Fuel) 
{#   same as BuildUnits, with hypothetical sites highlighted
}
# PrOut(year, month, day, case) 
{#   plots the output of a single specified week for
#    a specified case study with storage separate on top, price in the middle, 
#    and output on the bottom.
}
# PrOut4(year, month, day, case) 
{#   same as PrOut with limits set for 4 year 
#    comparison
}
# PrOt(year, month, day, case) 
{#   same as PrOut minus the storage plot
}
#
{# Week1(year, month, day, case) plots the output of a single specified week for
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
}
#
# imsave("name") Saves the image of the plot to directory
#
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# March 2022; Last revision: April 27, 2022
}

################################################################################
# library
################################################################################

{
  library(tidyverse)
  library(ggplot2)
  library(grid)
  library(gtable)
  library(gridExtra)
  library(odbc)
  library(ggpubr)
  library(DBI)
  library(lubridate)
  library(cowplot)
  library(scales)
  library(dplyr)
  library(reshape2)
  library(zoo)
  library(ggpattern)
  library(patchwork)
#  library(lmtest)
}

################################################################################
# Load AESO Data
################################################################################

{
  setwd("D:/Documents/Education/Masters Degree/Datasets/Market")
  
  load("nrgstream_gen.RData") 
  nrgstream_gen <- nrgstream_gen %>% rename(time=Time)
  #    merit <- read_csv("student_data_2021_Jul_23_14_09.csv.gz")
  #    merit <- read_csv("student_data_2022_May_05_15_19.csv.gz")
  #    saveRDS(merit, file = "Leach_MeritData.RData")
  merit <- readRDS("Leach_MeritData.RData")
  #    load("Leach_MeritData.RData")
  merit_filt <- filter(merit, 
                       date >= as.Date("2017-01-1"))
  rm(merit)
  
  setwd("D:/Documents/GitHub/AuroraEval")
  
  #    errors<-nrgstream_gen %>% filter(is.na(Price),date<Sys.Date())
  #    gen_errors<-nrgstream_gen %>% filter(is.na(gen),date<Sys.Date())
  
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 
  
  demand <- nrgstream_gen %>%
    group_by(time) %>%
    summarise(Demand = median(Demand), 
              Price = median(Price),
              AIL = median(AIL))
  sub_samp<-filter(nrgstream_gen, time >= as.Date("2017-01-1"))
  rm(nrgstream_gen)
  
  trade_excl<-c("AB - WECC Imp Hr Avg MW", 
                "AB - WECC Exp Hr Avg MW",
                "AB - WECC Imp/Exp Hr Avg MW")
  
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
  gen_set<-c("COAL","NGCONV","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND","STORAGE")
  
  df1a <- df1 %>%
    filter(Plant_Type %in% gen_set,year(time)<2022)
  
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "OTHER",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "HYDRO",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "WIND",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "SOLAR",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "IMPORT",after=Inf)
  df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "EXPORT",after=Inf)
}

################################################################################
# Connect to SQL
################################################################################

{
  DB <- "Jul_07_2022"
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


################################################################################
# Load simulation Data
################################################################################

    # Write data to environment and set variables
    ################################################################################
    Hr <- dbReadTable(con,'ResourceGroupHour1')
    Month <- dbReadTable(con,'ResourceGroupMonth1')
    Year  <- dbReadTable(con,'ResourceGroupYear1')
    ZoneHour <- dbReadTable(con,'ZoneHour1')
    ResourceYr <- dbReadTable(con,'ResourceYear1')
    #ResourceHr <- dbReadTable(con,'ResourceHour1')
    #StackHr <- dbReadTable(con,'ResourceStackHour1')
    
    #LTRes <- dbReadTable(con,'LTResValue1')
    #Build <- dbReadTable(con,'LTBuildReport1')
    #Study <- dbReadTable(con,'StudyLog1')
    #Link <- dbReadTable(con,'LinkYear1')
    #Fuel <- dbReadTable(con,'FuelYear1')
    
    setwd("D:/Documents/GitHub/AuroraEval")
    
    source("aeso_sim_comp.R")
    source("sim_eval.R")
    source("aeso_eval.R")
    
    {
      Yr4Sp <- list(2020,2025,2030,2035)
      Yr2Sp <- list(2020,2021)
      
      BC <- "Base Case"
      MS  <- "Minimum Solar Constraint"
      LCT <- "Low Carbon Tax"
      HS <- "Hypothetical Sites"
      IR <- "Incr Heat Rate"
      HREB <- "Heat Rate and Extreme Bid Factors"
      
      # Set limits for plots to be consistent
      ylimit <- max(Hr$Output_MWH) + max(ZoneHour$Imports)
      
      # Set legend variables
      colours = c("darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
                  "dodgerblue", "forestgreen", "gold", "darkolivegreen1", "cyan")
      colours1 = c("darkslateblue", "black", "grey", "darkslategrey", "coral4", "goldenrod4", 
                   "darkcyan", "dodgerblue", "forestgreen", "gold", "cyan")
      colours2 = c("grey", "darkslategrey", "coral4", "goldenrod4", 
                   "dodgerblue", "darkcyan", "forestgreen", "gold", "cyan")
      colours3 = c("forestgreen", "gold", "coral4", "goldenrod4", "cyan", "dodgerblue")

    # Converts the date and time and identifies the week when applicable
    ################################################################################
    {
      Hr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",Hr$Time_Period))), 
                              tz = "MST")-(60*60)
      Month$Time_Period <- ym(Month$Time_Period)
      Year$Time_Period  <- as.Date(as.character(Year$Time_Period), 
                                   format = "%Y")
      ZoneHour$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ZoneHour$Time_Period))), 
                                  tz = "MST")-(60*60)
#      ResourceHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ResourceHr$Time_Period))), 
#                            tz = "MST")-(60*60)
      
      # Selects only the required columns
      ################################################################################
      
      Hour <- Hr %>%
        subset(., select = c(ID, date, Report_Year, Output_MWH, Run_ID, Capacity_Factor))
      
      ZH <- ZoneHour %>%
        filter(Name == "WECC_Alberta") %>%
        filter(Condition == "Average") %>%
        subset(., select = c(date, Price, Baseline_Demand, Demand, Demand_Total,
                             Net_Load, Net_Load_Total, Marginal_Resource, 
                             Smp_Max_Date_Time, Smp_Max_Demand, Smp_Max_Capacity, 
                             Run_ID, Imports, Exports))
      
      ZoneH <- ZoneHour %>%
        filter(Name == "WECC_Alberta") %>%
        subset(., select = c(date, Condition, Price, Demand, Marginal_Resource, 
                             Name,Report_Year, Report_Month,
                             Run_ID))
      
#      RHour <- ResourceHr %>%
#        filter(Zone == "WECC_Alberta") %>%
#        subset(., select = c(ID, Name, Beg_Date, End_Date, date, Capability, Capacity, 
#                             Dispatch_Cost, Incr_Cost, Fixed_Cost, Fuel_Cost, 
#                             Output_MWH, Percent_Marginal, Percent_Committed,
#                             Revenue, Variable_OM_Cost, Capacity_Factor, 
#                             Total_Emission_Cost, Total_Hours_Run, Condition, 
#                             Report_Year, Run_ID, Peak_Capacity, 
#                             Primary_Fuel,Zone))
      
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
    }
   }
}

AESO_Sim(2021,03,01,BC)

price_comp(2020,2021,BC)

gen_comp(2020,2021,BC)

AESOSim(2020,2021,BC)

tot_gen(2020,2021,BC)

comp_dur(2018,2021,BC)

load_dur(2018,2021,BC)
 