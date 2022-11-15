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
# March 2022; Last revision: October 20, 2022
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
  library(timeDate)
  library(rgeos)
  library(raster)
  library(colorRamps)
#  library(lmtest)
}

################################################################################
# Load AESO Data
# Much of this code is from Dr. Leach
################################################################################

{
  # Set location where AESO data is stored
  ################################################################################
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
  
  rm(corrected,nocorrection)
    
  demand <- nrgstream_gen %>%
    group_by(time) %>%
    summarise(Demand = median(Demand), 
              Price = median(Price),
              AIL = median(AIL))
  sub_samp<-filter(nrgstream_gen, time >= as.Date("2017-01-1"))
  rm(nrgstream_gen)
  
  #    merit <- read_csv("student_data_2021_Jul_23_14_09.csv.gz")
  #    merit <- read_csv("student_data_2022_May_05_15_19.csv.gz")
  #    saveRDS(merit, file = "Leach_MeritData.RData")
  merit <- readRDS("Leach_MeritData.RData")
  #    load("Leach_MeritData.RData")
  merit_filt <- filter(merit, 
                       date >= as.Date("2017-01-1"))
  rm(merit)
  load("forecast_data.RData")
  
  setwd("D:/Documents/GitHub/AuroraEval")
  
  #    errors<-nrgstream_gen %>% filter(is.na(Price),date<Sys.Date())
  #    gen_errors<-nrgstream_gen %>% filter(is.na(gen),date<Sys.Date())
  
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
  DB <- "Nov_09_2022"
# Connect to SQLEXPRESS database
################################################################################
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "192.168.0.139,49172",
                   Database = DB,
                   UID = "admin",
                   PWD = "Aurora2022!",
                   Port = 49172)
  
  # Connect to MSSQL database
  ################################################################################
#  con <- dbConnect(odbc(),
#                   Driver = "SQL Server",
#                   Server = "192.168.0.139,1434",
#                   Database = DB,
#                   UID = "admin",
#                   PWD = "MSSQL704910",
#                   Port = 1434)
  
  {  
  # Connect to MySQL database
  ################################################################################
#    con <- dbConnect(RMariaDB::MariaDB(),
#                     user = 'tpawl',
#                     password = 'Aurora2022!',
#                     Driver = "SQL Server",
#                     host='192.168.0.139',
#                     dbname = DB,
#                     port = 3306)


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
    ResourceHr <- dbGetQuery(con,"SELECT Run_ID,Name,Primary_Fuel,Capacity_Factor,
                             Condition,Time_Period,Output_MWH,Capacity,Report_Year FROM ResourceHour1")
     #StackHr <- dbReadTable(con,'ResourceStackHour1')
    StackYr <- dbReadTable(con,'ResourceStackYear1')
    
    #LTRes <- dbReadTable(con,'LTResValue1')
    #LTMarg <- dbReadTable(con,'LTMargResLog1')
    #LTCap <- dbReadTable(con,'LTCapacLog1')
    Build <- dbReadTable(con,'LTBuildReport1') # Run this line if doing LTCE; otherwise skip
    #Study <- dbReadTable(con,'StudyLog1')
    #Link <- dbReadTable(con,'LinkYear1')
    #Fuel <- dbReadTable(con,'FuelYear1')
    
    # Set location where the following codes are found
    ################################################################################
    setwd("D:/Documents/GitHub/AuroraEval")
    #write.csv(Study, file=paste0("StudyLog_",DB,".csv"))
    
    source("aeso_sim_comp.R")
    source("sim_eval.R")
    source("aeso_eval.R")
    
    {
      Yr4Sp <- list(2022,2023,2024,2025)
      Yr2Sp <- list(2020,2021)
      
      # Abbreviations for Case Studies
      BC <- "Base Case"
      MS  <- "Minimum Solar Constraint"
      LCT <- "Low Carbon Tax"
      HS <- "Hypothetical Sites"
      IR <- "Incr Heat Rate"
      HREB <- "Heat Rate and Extreme Bid Factors"
      
      # Set limits for plots to be consistent
      ylimit <- max(Hr$Output_MWH) + max(ZoneHour$Imports)
      
      # Set legend variables
      ################################################################################
      colours = c("darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
                  "dodgerblue", "forestgreen", "gold", "darkolivegreen1", "cyan")
      colours1 = c("darkslateblue", "black", "grey", "darkslategrey", "coral4", 
                   "goldenrod4", "darkcyan", "dodgerblue", "forestgreen", "gold", 
                   "cyan") #11 colours
      colours2 = c("black", "grey", "darkslategrey", "coral4", "goldenrod4", 
                   "dodgerblue", "darkcyan", "forestgreen", "gold", "cyan") #10 colours
      colours3 = c("forestgreen", "gold", "darkslategrey", "goldenrod4", "cyan", 
                   "dodgerblue")
      colours4 = c("forestgreen","gold","cyan","dodgerblue","darkorange1","goldenrod4",
                   "darkslategrey","darkslategrey") #8 colours
      colours4a = c("forestgreen","gold","cyan","dodgerblue","goldenrod4",
                   "darkslategrey","darkslategrey") #7 colours
      colours5 = c("darkslateblue", "black", "grey", "darkslategrey", "coral4", 
                   "goldenrod4", "darkorange1", "darkcyan", "dodgerblue", 
                   "forestgreen", "gold", "cyan") #12 colours
      colours5a = c("black", "grey", "darkslategrey", "coral4", 
                   "goldenrod4", "darkorange1", "darkcyan", "dodgerblue", 
                   "forestgreen", "gold", "cyan") #11 colours
      colours5b = c("black", "grey", "darkslategrey", "coral4", 
                    "goldenrod4", "darkcyan", "dodgerblue", 
                    "forestgreen", "gold", "cyan") #10 colours
      colours5c = c("darkslateblue", "black", "grey", "darkslategrey", "coral4", 
                   "goldenrod4", "darkorange1","lightsalmon", "darkcyan", "dodgerblue", 
                   "forestgreen", "gold", "cyan","blue") #13 colours
      colours5d = c("black", "grey", "darkslategrey", "coral4", 
                    "goldenrod4", "darkorange1","lightsalmon", "darkcyan", "dodgerblue", 
                    "forestgreen", "gold", "cyan","blue") #12 colours
      colours6 = c("black", "grey", "darkslategrey", "coral4", 
                   "goldenrod4", "darkorange1","lightsalmon", "darkcyan", "dodgerblue", 
                   "forestgreen", "gold", "cyan") #12 colours
      colours6a = c("black", "grey", "darkslategrey", "coral4", 
                   "goldenrod4", "darkcyan", "dodgerblue", 
                   "forestgreen", "gold", "cyan") #10 colours
      
      # Custom scale fills to tie colours to specific resources
      ################################################################################
      scale_fill_output <- function(...){
        ggplot2:::manual_scale(
          'fill', 
          values = setNames(c("darkslateblue","black", "grey", "darkslategrey", 
                              "coral4","goldenrod4","darkorange1", "lightsalmon", 
                              "firebrick1", "darkcyan", "dodgerblue", 
                              "chartreuse","forestgreen", "gold", "cyan"), 
                            c("IMPORT","COAL","NGCONV","NGCC","COGEN","SCGT",
                              "CC_BLEND","SC_BLEND","H2",
                              "HYDRO","OTHER","UR","WIND","SOLAR","STORAGE")), 
          ...
        )
      }
      
      scale_fill_built <- function(...){
        ggplot2:::manual_scale(
          'fill', 
          values = setNames(c("black", 
                              "darkslategrey", "darkslategrey", 
                              "coral4","goldenrod4", "goldenrod3",
                              "darkorange1", "lightsalmon", "firebrick1", 
                              "darkcyan", "dodgerblue", 
                              "chartreuse","forestgreen", "gold", "cyan"), 
                            c("COAL", 
                              "Gas", "Gas1", 
                              "Gas0","Gas2","Gas3",
                              "GasB_CC","GasB_SC","H2",
                              "WAT","OT","UR","WND","SUN","PS")), 
          ...
        )
      }

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
      ResourceHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ResourceHr$Time_Period))), 
                            tz = "MST")-(60*60)
      
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
        add_column(ID = "IMPORT")
      
      Export <- ZH %>%
        subset(., select = c(date, Exports, Run_ID)) %>%
        'colnames<-'(c("date", "Output_MWH", "Run_ID")) %>%
        add_column(ID = "EXPORT")
      
      Export$Output_MWH <- Export$Output_MWH * -1
    }
    }
    }
}

# Specific functions I use most often to evaluate a simulation
################################################################################
year_price(2021,BC)

gen_comp(2020,2021,BC)

price_comp(2020,2021,BC)

margin(2020,2021,BC)

Sample_output(2021,03,01,BC)

Fossil_line(2021,03,01,BC)

Sample_output(2021,06,01,BC)

Fossil_line(2021,06,01,BC)

seas_price(2020,2021,BC)

Weekly_line(2021,06,01,BC)

tot_gen(2020,2021,BC)

comp_dur(2018,2021,BC)

load_dur(2018,2021,BC)

cap_fac_difference(2020,2021,"WIND",BC)

capturePrice_diff(2020,2021,"WIND",BC)

capPrice_diff(2020,2021,"WIND",BC)

capacity_factor(2020,"WIND",BC)

capacity_factor(2021,"WIND",BC)

capturePrice(2021,"WIND",BC)

# Functions specific to LTCE
################################################################################

EvalOut(Year,BC)

EvalPerc(Year,BC)

Eval_diffcap(Year,BC)


 