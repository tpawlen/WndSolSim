# Functions defined
################################################################################
{# Generates functions used to plot simulation outputs
#
# The entire code can be run once per session, then just the functions can be 
# used for each iteration.
#
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# March 2022; Last revision: January 3, 2023
}

################################################################################
# library
################################################################################

{
  library(tidyverse)
  library(ggplot2)
  library(ggrepel)
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
  library(viridis)
  library("readxl")
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
  
  # Run this part of the code only after downloading updated data
  ##############################################################################
#  load("nrgstream_gen.RData") 
#  nrgstream_gen <- readRDS("nrgstream_gen_corrected.RData")
#  nrgstream_gen <- nrgstream_gen %>% rename(time=Time)
  
#  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
#  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 

#  corrected <- nrgstream_gen %>%
#    filter(is.na(Latitude)) %>%
#    mutate(Latitude=case_when(grepl("BRD1",ID) ~ 49.842735,
#                              grepl("BUR1",ID) ~ 49.814877,
#                              grepl("CLR",ID) ~ 50.032911,
#                              grepl("CLY",ID) ~ 49.840967,
#                              grepl("CHP1",ID) ~ 50.22189,
#                              grepl("COL1",ID) ~ 49.833218,
#                              grepl("CRD",ID) ~ 49.807,
#                              grepl("CRR2",ID) ~ 49.55891,
#                              grepl("FMG1",ID) ~ 49.66334,
#                              grepl("KKP",ID) ~ 53.469986,
#                              grepl("MON1",ID) ~ 49.833144,
#                              grepl("NMK1",ID) ~ 51.026118,
#                              grepl("RIV1",ID) ~ 49.53245,
#                              grepl("STR",ID) ~ 51.033273,
#                              grepl("TVS1",ID) ~ 50.27324,
#                              grepl("VCN1",ID) ~ 50.0975,
#                              grepl("VXH1",ID) ~ 50.095223,
#                              grepl("WEF1",ID) ~ 49.65405,
#                              grepl("WHT",ID) ~ 49.64029),
#           Longitude=case_when(grepl("BRD1",ID) ~ -111.537891,
#                               grepl("BUR1",ID) ~ -111.543323,
#                               grepl("CHP1",ID) ~ -110.437106,
#                               grepl("CLR",ID) ~ -113.484369,
#                               grepl("CLY",ID) ~ -110.356864,
#                               grepl("COL1",ID) ~ -112.97448,
#                               grepl("CRD",ID) ~ -112.578,
#                               grepl("CRR2",ID) ~ -113.983,
#                               grepl("FMG1",ID) ~ -111.122,
#                               grepl("KKP",ID) ~ -113.61337,
#                               grepl("MON1",ID) ~ -112.974231,
#                               grepl("NMK1",ID) ~ -113.163017,
#                               grepl("RIV1",ID) ~ -113.977,
#                               grepl("STR",ID) ~ -113.371296,
#                               grepl("TVS1",ID) ~ -112.73059,
#                               grepl("VCN1",ID) ~ -112.84841,
#                               grepl("VXH1",ID) ~ -112.149936,
#                               grepl("WEF1",ID) ~ -111.515812,
#                               grepl("WHT",ID) ~ -111.291),
#           Installation_Year=case_when(grepl("CRR2",ID)~2019,
#                                       grepl("CYP",ID)~2022,
#                                       #grepl("CYP2",ID)~"post2019",
#                                       grepl("FMG1",ID)~2022,
#                                       grepl("GDP1",ID)~2022,
#                                       grepl("GRZ1",ID)~2022,
#                                       grepl("HHW1",ID)~2022,
#                                       grepl("HLD1",ID)~2022,
#                                       grepl("JNR",ID)~2022,
#                                       grepl("RIV1",ID)~2019,
#                                       grepl("RTL1",ID)~2021,
#                                       grepl("WHE1",ID)~2022,
#                                       grepl("WHT1",ID)~2019,
#                                       grepl("WHT2",ID)~2021,
#                                       grepl("WRW1",ID)~2021,),
#           Installation_Year=case_when(is.na(Installation_Year)~"pre2019",
#                                         TRUE~"post2019")
#           )
  
#  nocorrection <- nrgstream_gen %>%
#    filter(!is.na(Latitude))
  
#  nrgstream_gen <- rbind(corrected,nocorrection)
  
#  rm(corrected,nocorrection)
#  saveRDS(nrgstream_gen, file = "nrgstream_gen_corrected.RData")
    
#  demand <- nrgstream_gen %>%
#    group_by(time) %>%
#    summarise(Demand = median(Demand), 
#              Price = median(Price),
#              AIL = median(AIL))
  
#  saveRDS(demand, file = "nrgstream_demand.RData")
  #  sub_samp<-filter(nrgstream_gen, time >= as.Date("2017-01-1"))
  #  saveRDS(sub_samp, file = "nrgstream_sub_samp.RData")
  #  rm(nrgstream_gen)
  
  # Loads the filtered and corrected data from the nrgstream file
  demand <- readRDS("nrgstream_demand.RData")
  sub_samp <- readRDS("nrgstream_sub_samp.RData")
  
# Run this part of the code if loading updated data from Dr. Leach
  ##############################################################################
#  merit <- read_csv("student_data_2022_Dec_21_12_36.csv.gz")
#      saveRDS(merit, file = "Leach_MeritData.RData")
#  merit <- readRDS("Leach_MeritData.RData")
#  merit_filt <- filter(merit, 
#                       date >= as.Date("2017-01-1"))
#  saveRDS(merit_filt, file = "Leach_MeritData_filt.RData")
  
# Normally run this code:  
  ##############################################################################
  merit_filt <- readRDS("Leach_MeritData_filt.RData")
  load("forecast_data.RData")
  
  setwd("D:/Documents/GitHub/AuroraEval")
  
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
# Load in custom functions to analyze results
################################################################################
setwd("D:/Documents/GitHub/AuroraEval")
source("aeso_sim_comp.R")
source("sim_eval.R")
source("aeso_eval.R")
source("supporting_Aurora.R")

################################################################################
# Connect to SQL
################################################################################

{     
  DB <- "Dec_13_2022"
# Connect to SQLEXPRESS database
################################################################################
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "192.168.0.139,49172",
                   Database = DB,
                   UID = "admin",
                   PWD = "Aurora2022!",
                   Port = 49172)
{
################################################################################
# Load simulation Data
################################################################################

    # Write data to environment and set variables
    ################################################################################
#  ResourceGroupHour <- dbReadTable(con, 'ResourceGroupHour1')
    Hr <- dbGetQuery(con,"SELECT Run_ID,Name,ID,Capacity_Factor,Energy_Revenue,
                             Time_Period,Output_MWH,Output,Report_Month,Report_Day,
                             Report_Hour,Report_Year,Capacity FROM ResourceGroupHour1")
    Month <- dbReadTable(con,'ResourceGroupMonth1')
    Year  <- dbReadTable(con,'ResourceGroupYear1')
    ZoneHour <- dbGetQuery(con,"SELECT Run_ID,Name,Imports,Exports,Condition,
                           Price,Demand,Marginal_Resource,  
                           Time_Period,Report_Month,Report_Year FROM ZoneHour1")
    Month <- dbReadTable(con,'ResourceGroupMonth1')
    ResourceYr <- dbReadTable(con,'ResourceYear1')
    ResourceHour <- dbGetQuery(con, "SELECT * FROM ResourceHour1")
    ResourceHr <- dbGetQuery(con,"SELECT Run_ID,Name,Primary_Fuel,Capacity_Factor,
                             Time_Period,Output_MWH,Capacity,ID,Condition,Revenue,
                             Report_Year FROM ResourceHour1")
    #StackHr <- dbReadTable(con,'ResourceStackHour1')
    StackYr <- dbReadTable(con,'ResourceStackYear1')
    #Study <- dbReadTable(con,'StudyLog1')
    #Link <- dbReadTable(con,'LinkYear1')
    #Fuel <- dbReadTable(con,'FuelYear1')
    
    #LTRes <- dbReadTable(con,'LTResValue1')
    #LTMarg <- dbReadTable(con,'LTMargResLog1')
    #LTCap <- dbReadTable(con,'LTCapacLog1')
    #Build <- dbReadTable(con,'LTBuildReport1')
    Build <- dbGetQuery(con,"SELECT * FROM LTBuildReport1")
#    Build <- dbGetQuery(con,"SELECT Name,Run_ID,LT_Iteration,Time_Period,Fuel_Type,
#                        Units_Built,Max_Limit_Slack,Capacity_Built
#                        FROM LTBuildReport1")
#    Value <- dbGetQuery(con,"SELECT * FROM LTResValue1")
#    Value <- dbGetQuery(con,"SELECT Run_ID,LT_Iteration,Res_Name,Input_New_Res_ID,
#                        Begin_Year,Fuel_Type,In_System,Capacity,NPV,RLV,
#                        NPV_Energy,RLV_Energy FROM LTResValue1")
    
# Set location where the following codes are found
################################################################################
    setwd("D:/Documents/GitHub/AuroraEval")
    write.csv(ResourceGroupHour, file="ResourceGroupHour_HS")
    write.csv(ResourceHour, file="ResourceHour_HS")
    write.csv(Build, file="BuildReport_NoOff.csv")
    #write.csv(Study, file=paste0("StudyLog_",DB,".csv"))
    
    {
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
        subset(., select = c(date, Price, Demand, Marginal_Resource,
                             #Net_Load, Baseline_Demand, Net_Load_Total, Demand_Total, 
                             #Smp_Max_Date_Time, Smp_Max_Demand, Smp_Max_Capacity, 
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


setwd("D:/Documents/GitHub/AuroraEval")

# Save simulation results to Rdata
################################################################################

# Use this if Standard Zonal
save(ResourceHr,ResourceYr,Hour,Month,Year,ZoneHour,StackYr,Hr,ZoneHour,
     ZoneH,ZH,Export,Import, file = paste0(DB,".RData"))

# Use this if LTCE
save(ResourceHr,ResourceYr,Hour,Month,Year,ZoneHour,Value,StackYr,Hr,ZoneHour,
     ZoneH,ZH,Export,Import,Build, file = paste0(DB,".RData"))

# Load the simulation you want to analyze
load("Dec_08_2022.RData") # LTCE with hypothetical sites, 2nd Week
  DB <- "Dec_08_2022"
load("Dec_12_2022.RData") # LTCE without hypothetical sites, 2nd Week
  DB <- "Dec_12_2022"
load("Dec_13_2022.RData") # LTCE without offsets, with hypothetical sites, 2nd Week
  DB <- "Dec_13_2022"
load("Dec_06_2022.RData") # Standard Zonal without hypothetical sites, everyday
  DB <- "Dec_06_2022"

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

seas_price(2020,2022,BC)

Weekly_line(2021,06,01,BC)

tot_gen(2020,2022,BC)

comp_dur(2019,2022,BC)

load_dur(2019,2022,BC)

cap_fac_difference(2020,2021,"WIND",BC)

capturePrice_diff(2020,2021,"WIND",BC)

capPrice_diff(2020,2021,"WIND",BC)

capacity_factor(2020,"WIND",BC)

capacity_factor(2021,"WIND",BC)

capturePrice(2021,"WIND",BC)

Revenue("Wind",BC)

# Functions specific to LTCE
################################################################################

EvalOut(Year,BC)

EvalPerc(Year,BC)

Eval_diffcap(Year,BC)

BuildUnits2(BC,"WND")
 
CapBuild2(BC,"WND")
CapBuild2(BC,"SUN")
CapBuild2(BC,"PS")
CapBuild2(BC,"Gas0")
CapBuild2(BC,"Gas1","Gas2")
CapBuild2(BC,"Gas3")
CapBuild2(BC,"GasB_SC")
CapBuild2(BC,"GasB_CC")
CapBuild2(BC,"H2")

# Used in thesis
################################################################################

# Chapter 3
correlation(2021,2021)
  imsave("Correlation")
correlation_hypoth(2021,2021)[2]
  imsave("Correlation_hypothetical")
correlation_hypoth(2021,2021)[4]
  imsave("IOD_hy_map")
HypoIRR(2021)
  
# Chapter 4
load_duration(2019,2021)
  imsave("historicdemand")
  
  # Using Dec_06_2022
load_dur(2019,2021,BC)
  imsave("BC_load_duration")
comp_dur(2019,2021,BC)
  imsave("BC_price_duration")
tot_cap(2020,2021,BC)
  imsave("BC_Capacity")
tech_cap(2020,2021,BC)
  imsave("BC_CF")
market_share(2020,2021,BC)
  imsave("BC_marketshare")
tot_gen(2020,2021,BC)
  imsave("BC_total_generation")

comp_dur(2020,2021,BC)
  imsave("BC_price_duration")
price_interval(2020,2022,BC)
  imsave("BC_poolprice")

# Save tables from simulations to compare
################################################################################
  setwd("D:/Documents/GitHub/AuroraEval")

# Standard Zonal without hypothetical sites, everyday (Dec_06_2022)
load("Dec_06_2022.RData") # Standard Zonal without hypothetical sites, everyday

CF_AllR(BC)
  
# LTCE and hypothetical sites (Dec_08_2022)
load("Dec_08_2022.RData")
  HS_ZH <- ZoneH # ZoneH from Dec_08_2022
  HS_ResourceHr <- ResourceHr
  HS_Build <- Build
  
  ZoneH <- HS_ZH
  ResourceHr <- HS_ResourceHr
  Build <- HS_Build
  
CapPot2(BC)
  imsave("Hypo_Build") 
CapPot3(BC)
  imsave("Hypo_Build") 
CF_NR("Wind","Wind",BC) # Need "ResourceHr
  imsave("HS_CF")
Revenue2.0(BC) # Need "ResourceHr"
  imsave("HS_Revenue")
monthly_price(BC)
  imsave("HS_poolprice")
Sim_corr(2035,BC)
  imsave("HS_corr")
Build_Map(BC) # Need "Build"
  mapsave("HS_map")

# LTCE and no hypothetical sites (Dec_12_2022)
load("Dec_12_2022.RData")
  NoHS_ZH <- ZoneH # ZoneH from Dec_12_2022
  NoHS_ResourceHr <- ResourceHr
  NoHS_Build <- Build
  
  ZoneH <- NoHS_ZH
  ResourceHr <- NoHS_ResourceHr
  Build <- NoHS_Build
  
simcomp_monthly_price(BC)
  imsave("NoHS_poolprice")
simcomp_monthly_price3(BC)
  imsave("WorWoutHS_poolprice")
Revenue2.0(BC) # Need "ResourceHr"
  imsave("NoHS_Revenue")
Build_Map(BC)
  mapsave("NoHS_map")

# LTCE and no offsets, with hypothetical sites (Dec_13_2022)
load("Dec_13_2022.RData")
  NoOffset_ZH <- ZoneH # ZoneH from Dec_13_2022
  NoOffset_ResourceHr <- ResourceHr
  NoOffset_Build <- Build
  
  ZoneH <- NoOffset_ZH
  ResourceHr <- NoOffset_ResourceHr
  Build <- NoOffset_Build
    
simcomp_monthly_price2(BC)
  imsave("NoOff_poolprice")
simcomp_monthly_price3(BC)
  imsave("WorWoutOff_poolprice")
Revenue2.0(BC) # Need "ResourceHr"
  imsave("NoOffset_Revenue")
CapPot2(BC) # Need "Build"
  imsave("NoOffset_Build") 
CapPot3(BC) # Need "Build"
  imsave("NoOffset_Build")
Eval_diffcap(Year,BC) +
  theme(plot.title = element_blank())
  imsave("NoOffset_Buildshare")
Build_Map(BC)
  mapsave("NoOffset_map")
  