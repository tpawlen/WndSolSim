# Generates plots showing the division of electricity related emissions in Canada
# 
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# October 2022; Last revision: October 20, 2022

library("readxl")
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(viridis)

# Data used to plot pie chart
################################################################################
# https://unfccc.int/documents/461919
setwd("D:/Documents/Education/Masters Degree/Datasets")

GHG <- read_excel("Greenhouse gas emissions by province (Mt Co2 eq).xlsx")

GHG_all <- GHG %>%
  filter(Year >= 2015) %>%
  mutate(Province = case_when(str_detect(Province, "AB")~"Alberta",
                          str_detect(Province, "BC")~"British Columbia",
                          str_detect(Province, "MB")~"Manitoba",
                          str_detect(Province, "NB")~"New Brunswick",
                          str_detect(Province, "NL")~"Newfoundland",
                          str_detect(Province, "NS")~"Nova Scotia",
                          str_detect(Province, "NT")~"Northwest Territories",
                          str_detect(Province, "NU")~"Nunavut",
                          str_detect(Province, "ON")~"Ontario",
                          str_detect(Province, "PE")~"Prince Edward Island",
                          str_detect(Province, "QC")~"Quebec",
                          str_detect(Province, "SK")~"Saskatchewan",
                          str_detect(Province, "YK")~"Yukon"),
#            e_ghg2005 = e_ghg2005,
#            e_ghg2020 = e_ghg2020,
            #percent = round(Value/sum(e_ghg2005)*100),
            #pct2020 = round(e_ghg2020/sum(e_ghg2020)*100),
         ) %>%
  group_by(Year,Type) %>%
  mutate(Canada_v = sum(Value),
         Canada_Tot = sum(Total)) %>%
  ungroup() %>%
  mutate(#percent_v = round(Value/Canada_v*100),
         #percent_Tot = round(Total/Canada_Tot*100),
         region = case_when(grepl("New",Province)~"Atlantic Canada",
                            grepl("Prince",Province)~"Atlantic Canada",
                            grepl("Nova",Province)~"Atlantic Canada",
                            grepl("Yukon",Province)~"Territories",
                            grepl("North",Province)~"Territories",
                            grepl("Nunavut",Province)~"Territories",
                            TRUE~Province)) %>%
  group_by(region,Year) %>%
  summarize(Value = sum(Value),
            Total = sum(Total),
            Canada_v = median(Canada_v),
            Canada_Tot = median(Canada_Tot),
            percent_v = round(Value/Canada_v*100),
            ) %>%
  ungroup() %>%
  mutate(Region = case_when(percent_v<3~"Other Provinces",
                            TRUE~region)) %>%
  group_by(Region,Year) %>%
  summarize(Value = sum(Value),
            Total = sum(Total),
            Canada_v = median(Canada_v),
            Canada_Tot = median(Canada_Tot),
            percent_v = round(Value/Canada_v*100),
            percent_Tot = round(Total/Canada_Tot*100),)

Province_2005 <- c("lightseagreen",
                   "thistle","steelblue1","peachpuff",
                   "khaki1","orangered","red","palegoldenrod")

Province_2020 <- c("turquoise","lightseagreen",
                      "thistle","steelblue1","peachpuff",
                      "khaki1","orangered","red","palegoldenrod",
                      "steelblue")

# Plot a pie chart showing shares of emissions
################################################################################
pie(GHG$e_ghg2020, labels = paste0(GHG$Province, " ",GHG$pct2020, "%"),
    col = Province_2020,
#    main="Public electricity and heat production greenhouse gas emissions by province and territory in 2020 (kt CO2 eq)"
    )

# Data used to plot faceted bar chart
################################################################################

sz<-15
# Plot the data
ggplot(GHG_all,
#       aes(Province,e_ghg,colour=sit,fill=sit),
       alpha=0.8)+
  geom_area(aes(Year,Value/1000),#colour=sit,fill=sit),
           #size=1.5,position = position_dodge(width = .3),width = 0.9
           )+
  facet_grid(~reorder(Region,-percent_v)) +
  scale_y_continuous(expand=c(0,0),
#                     labels = scales::percent,
                     limits = c(0,52),
                     #breaks = seq(-40,100, by = 20)
  ) +
  scale_x_continuous(breaks = seq(2015,2020, by=1),
                     expand=c(0,0)) +
  labs(x="",y="Greenhouse gas emissions \n(Mt CO2 eq)",
#       title="Electricity generation greenhouse gas emissions by province",
#       subtitle = DB,
#       caption="Source: National inventory report greenhouse gas source and sinks in Canada"
) +
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text = element_text(size = sz),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_text(size = sz),
        plot.subtitle = element_text(size = sz-2,hjust=0.5),
        plot.caption = element_text(face="italic",size = sz-4,hjust=0),
        plot.title = element_text(hjust=0.5,size = sz+2),
        strip.text.x = element_text(size=sz-3),
        #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        
        # For transparent background
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', colour = "transparent"),
  ) 

imsave("GHG")

################################################################################
################################################################################
################################################################################

CanREA <- read_excel("CanREA-Renewable-Project-Data-General-2022-01.xlsx")
colnames(CanREA) <- c('Project','Technology','Province','Year','Wind','Solar',
                   'Storage_Cap','Storage_Dur','Location')

CanREA_wind <- CanREA %>%
  filter(Wind > 0) %>%
  arrange(Province,Year) %>%
  group_by(Province,Year) %>%
  summarise(New = sum(as.numeric(Wind))) %>%
  ungroup() %>%
  arrange(Province,Year) %>%
  group_by(Province) %>%
  mutate(#Capacity = cumsum(New),
         Province = case_when(str_detect(Province, "AB")~"Alberta",
                          str_detect(Province, "BC")~"British Columbia",
                          str_detect(Province, "MB")~"Manitoba",
                          str_detect(Province, "NB")~"New Brunswick",
                          str_detect(Province, "NL")~"Newfoundland and Labrador",
                          str_detect(Province, "NS")~"Nova Scotia",
                          str_detect(Province, "NT")~"Northwest Territories",
                          str_detect(Province, "NU")~"Nunavut",
                          str_detect(Province, "ON")~"Ontario",
                          str_detect(Province, "PE")~"Prince Edward Island",
                          str_detect(Province, "QC")~"Quebec",
                          str_detect(Province, "SK")~"Saskatchewan",
                          str_detect(Province, "YK")~"Yukon")) %>%
  ungroup() %>%
  add_row(Province = "Northwest Territories", Year=2020, New=0)

#CanREA_wind <- CanREA_wind %>%
#  group_by(Year) %>%
#  summarise(Province = "Canada",
#            New = sum(New),
#            Capacity = sum(Capacity)) %>%
#  bind_rows(CanREA_wind,.)

Stats_canada <- read.csv("Table_25-10-0020-01.csv", header = TRUE,
                         #  na.strings = 999.99, stringsAsFactors = F
                         )
colnames(Stats_canada) <- c('Year','Province','DGUID','Class','Technology','UOM',
                            'UOM_ID','SCALAR','VECTOR','SCALAR_ID','COORD','VALUE',
                            'Status','Symbol','Terminal','Decimal')

wind_stats <- Stats_canada %>%
  filter(grepl('utilities',Class),
         Province != "Canada",
         Technology == "Wind power turbine") %>%
  mutate(Actual = VALUE) %>%
  subset(.,select=c(Year,Province,Actual))

CF_canada1 <- merge(wind_stats,CanREA_wind, by = c("Province","Year"), all=TRUE)
CF_canada1[is.na(CF_canada1)] <- 0

CF_canada <- CF_canada1 %>%
  arrange(Province,Year) %>%
  group_by(Province) %>%
  mutate(Capacity = cumsum(New),
         Potential = Capacity*8760,
         CF = Actual/(Capacity * 8760)
         )

lmt <- 1800
################################################################################
{
sum_15yrs <- CF_canada %>%
  filter(Year >= 2005,
         Year < 2021) %>%
  group_by(Province) %>%
  summarise(Actual = sum(Actual),
            Capacity = max(Capacity),
            Potential = sum(Potential), 
            CF = Actual/Potential)

summary_sig15 <- sum_15yrs %>%
  filter(Capacity >= lmt) %>%
  mutate(Time = "Fifteen")

summary_other15 <- sum_15yrs %>%
  filter(Capacity < lmt) %>%
  summarize(Province = "Other Provinces",
            Capacity = sum(Capacity),
            Actual = sum(Actual),
            Potential = sum(Potential),
            CF = Actual/Potential) %>%
  mutate(Time = "Fifteen")

sum_5yrs <- CF_canada %>%
  filter(Year >= 2015,
         Year < 2021) %>%
  group_by(Province) %>%
  summarise(Actual = sum(Actual),
            Capacity = max(Capacity),
            Potential = sum(Potential), 
            CF = Actual/Potential)

summary_sig5 <- sum_5yrs %>%
  filter(Capacity >= lmt) %>%
  mutate(Time = "Five")

summary_other5 <- sum_5yrs %>%
  filter(Capacity < lmt) %>%
  summarize(Province = "Other Provinces",
            Capacity = sum(Capacity),
            Actual = sum(Actual),
            Potential = sum(Potential),
            CF = Actual/Potential) %>%
  mutate(Time = "Five")

summary <- rbind(summary_sig15,summary_other15,summary_sig5,summary_other5)

################################################################################
sum_15yrs <- CF_canada %>%
  filter(Year >= 2005,
         Year < 2021) %>%
  group_by(Province) %>%
  summarise(Actual = sum(Actual),
            Capacity = max(Capacity),
            Potential = sum(Potential), 
            CF15 = Actual/Potential)

summary_sig15 <- sum_15yrs %>%
  filter(Capacity >= lmt)

summary_other15 <- sum_15yrs %>%
  filter(Capacity < lmt) %>%
  summarize(Province = "Other Provinces",
            Capacity = sum(Capacity),
            Actual = sum(Actual),
            Potential = sum(Potential),
            CF15 = Actual/Potential)

sum_5yrs <- CF_canada %>%
  filter(Year >= 2015,
         Year < 2021) %>%
  group_by(Province) %>%
  summarise(Actual = sum(Actual),
            Capacity = max(Capacity),
            Potential = sum(Potential), 
            CF5 = Actual/Potential)

summary_sig5 <- sum_5yrs %>%
  filter(Capacity >= lmt)

summary_other5 <- sum_5yrs %>%
  filter(Capacity < lmt) %>%
  summarize(Province = "Other Provinces",
            Capacity = sum(Capacity),
            Actual = sum(Actual),
            Potential = sum(Potential),
            CF5 = Actual/Potential)

summary15 <- rbind(summary_sig15,summary_other15)
summary5  <- rbind(summary_sig5,summary_other5)

summaryH <- merge(summary15,summary5, by = c("Province","Capacity")) %>%
  subset(., select=c(Province,Capacity,CF5,CF15)) %>%
  mutate(height = abs(CF5-CF15)/2+ifelse(CF5<CF15,CF5,CF15))
}
################################################################################

allsum <- CF_canada %>%
  filter(Year >= 2005,
         Year < 2021) %>%
  group_by(Province) %>%
  summarise(Actual = sum(Actual),
            Capacity = max(Capacity),
            Potential = sum(Potential), 
            CF = Actual/Potential)

summary_sigall <- allsum %>%
  filter(Capacity >= lmt)

summary_otherall <- allsum %>%
  filter(Capacity < lmt) %>%
  summarize(Province = "Other Provinces",
            Capacity = sum(Capacity),
            Actual = sum(Actual),
            Potential = sum(Potential),
            CF = Actual/Potential)

allsum <- rbind(summary_sigall,summary_otherall)

sz<-15
ggplot(allsum,aes(x=Capacity,y=CF)) +
  geom_point(aes(fill=Province,shape=Province),size=4) +#aes(size=Capacity)) +
  #geom_text(data=summaryH,aes(x=Capacity,y=height),label=sum_10yrs$Province, size=4, #nudge_y=0.007,
  #          color="black",
  #          hjust="inward",position=position_jitter(width=10,height=0.007)) +
#  geom_text_repel(data=allsum, aes(x=Capacity,y=height),
#                  label=summaryH$Province, 
#                  size=4, color="black", #angle=90,
#                  box.padding=2
#            ) +
  #geom_line(aes(group=Province)) +
  scale_y_continuous(expand=c(0,0),
                     labels = scales::percent,
                     limits = c(0,0.41),
                     #breaks = seq(-40,100, by = 20)
  ) +
  scale_x_continuous(expand=c(0,0),
                     limits = c(0,5500),) +
  labs(x="Capacity in 2020 (MW)",y="Average Capacity Factor \nfrom 2005 to 2020 (%)",
       )+
  scale_fill_manual(values = c("Alberta"="black","Ontario"="grey20","Quebec"="grey50",
  "Other Provinces"="grey80"),
  #                   labels = c("2005 - 2020","2015 - 2020")
  ) +
  scale_shape_manual(values = c("Alberta"=21,"Ontario"=22,"Quebec"=23,"Other Provinces"=24)) +
  #scale_color_viridis(discrete = TRUE) +
  guides(color = guide_legend(override.aes = list(size=5)))+
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text = element_text(size = sz),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        axis.title = element_text(size = sz),
        plot.subtitle = element_text(size = sz-2,hjust=0.5),
        plot.caption = element_text(face="italic",size = sz-4,hjust=0),
        plot.title = element_text(hjust=0.5,size = sz+2),
        strip.text.x = element_text(size=sz-3),
        #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        
        # For transparent background
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(),
        legend.text = element_text(size=sz),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', colour = "transparent"),
  )

setwd("D:/Documents/GitHub/AuroraEval")
ggsave(path = "thesis_images", filename = "Provincial_CF.png", bg = "transparent")

################################################################################
################################################################################
################################################################################

library(ggthemes)
library(zoo)
library(curl)
library(httr)
library(janitor)
library(viridis)
library(scales)
library(XML)
library(lubridate)
library(readxl)
library(cansim)
library(tidyverse)

options(scipen=999)

get_data<-function() {
  file_path<-"nir_electricity.xlsx"
  download.file("https://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/C-Tables-Electricity-Canada-Provinces-Territories/EN_Electricity_Can_Prov_Terr.xlsx",file_path,mode="wb")
  #get the listing of sheets
  sheet_names<-excel_sheets(file_path)
  index_val<-1
  prov_list<-list()
  for(target_sheet in sheet_names){
    #target_sheet<-sheet_names[7]
    prov<-read_excel(file_path,target_sheet,range = "B1",col_names = FALSE)%>%
      rename(province=1)%>%
      mutate(province=gsub("Table A11-13: Electricity Generation and GHG Emission Details for the Nunavut","Nunavut",province),
             province=gsub("Electricity Generation and GHG Emission Details for ","",province))%>%
      as.character()
    print(prov)
    col_names<-read_excel(file_path,target_sheet,range = "B3:T3",col_names = FALSE)
    col_names[1]<-"fuel"
    col_names<-gsub("a","",col_names)
    elec_data<-read_excel(file_path,target_sheet,range = "B6:T11",col_names = FALSE,
                          na = c("N/A", "n/a","x","**"))
    names(elec_data)<-as.character(col_names)
    
    elec_data <-elec_data %>% 
      mutate(fuel=row_number(),
             fuel=factor(fuel),
             fuel=fct_recode(fuel,"Combustion"="1",
                             "Coal"="2",
                             "Natural Gas"="3",
                             "Other Fuel"="4",
                             "Other Generation"="5",
                             "Overall Total"="6"
             ),
             prov=prov           )%>%
      pivot_longer(-c(fuel,prov),names_to = "year",values_to = "ghg")%>%
      mutate(year=as.numeric(year))
    
    gen_data<-read_excel(file_path,target_sheet,range = "B15:T23",col_names = FALSE,
                         na = c("N/A", "n/a","x","**"))
    names(gen_data)<-as.character(col_names)
    gen_data <-gen_data %>% 
      mutate(fuel=row_number(),
             fuel=factor(fuel),
             fuel=fct_recode(fuel,"Combustion"="1",
                             "Coal"="2",
                             "Natural Gas"="3",
                             "Other Fuel"="4",
                             "Nuclear"="5",
                             "Hydro"="6",
                             "Other Renewables"="7",
                             "Other Generation"="8",
                             "Overall Total"="9"
             ),
             #fuel=fct_collapse(fuel,
             #                 "Other Generation" = c("Nuclear", "Hydro","Other Renewables","Other Generation")),
             
             prov=prov           )%>%
      pivot_longer(-c(fuel,prov),names_to = "year",values_to = "gen")%>%
      mutate(year=as.numeric(year))%>%
      group_by(fuel,year,prov)%>%
      summarize(gen=sum(gen,na.rm = T),.groups = "drop")
    
    elec_data<-elec_data %>% full_join(gen_data)%>% #only joining emissions associated with 
      mutate(ei=ghg/gen)
    
    
    
    prov_list[[index_val]]<-elec_data
    index_val<-index_val+1
  }  
  
  
  
  
  prov_data<-bind_rows(prov_list)
  
  prov_data<-prov_data%>%
    mutate(prov=fct_recode(prov,"AB"="Alberta",
                           "BC"="British Columbia",
                           "NL"="Newfoundland and Labrador",
                           "MB"="Manitoba",
                           "SK"="Saskatchewan",
                           "NS"="Nova Scotia",
                           "ON"="Ontario",
                           #"NT"="Northwest Territories",
                           "NT"="the Northwest Territories",
                           "QC"="Quebec",
                           "NU"="Nunavut",
                           "NB"="New Brunswick",
                           "YT"="Yukon",
                           "PE"="Prince Edward Island",
                           #"NT & NU"="Northwest Territories and Nunavut"
                           ),
           prov=fct_collapse(prov,
                             "OTHER" = c("NT", "NU","YT",#"NT & NU",
                                         "NL", "NB",
                                         "NS","PE","QC","BC","MB")
                             #"TERR" = c("NT", "NU","YT","NT & NU"),
                             #"ATL" = c("NL", "NB","NS","PE")
                             ),
           fuel=fct_other(fuel,drop=c("Other Fuel","Other Generation"))
    )%>%
    group_by(prov,year,fuel)%>%
    summarize(ghg=sum(ghg,na.rm=T),gen=sum(gen,na.rm=T))%>%
    ungroup()%>%
    mutate(prov=factor(prov,levels=c("Canada" ,"AB","ON","SK", "OTHER", 
                                     "MB", "BC", "QC", "ATL", "TERR"  
                                     )))
  
  prov_data <- prov_data %>%
    mutate(fuel=fct_recode(fuel,"Non-hydro Renewables"="Other Renewables"),
           fuel=fct_relevel(fuel,"Nuclear",after=0),
           fuel=fct_relevel(fuel,"Non-hydro Renewables",after=0),
           fuel=fct_relevel(fuel,"Hydro",after=0)
    )
  
  prov_data
}

prov_data<-get_data() 

prov_db <- prov_data %>%
  filter(year >= 2005,
         fuel == "Coal" | fuel == "Natural Gas" | fuel == "Other",
         #ghg > 0,
         prov != "Canada"
         ) %>%
  mutate(year = as.Date(paste(year,01,01,sep="-")))

prov_db1 <- prov_data %>%
  filter(year >= 2005,
         fuel == "Combustion" | fuel == "Overall Total",
         ghg > 0,
         prov != "Canada"
  ) %>%
  mutate(year = as.Date(paste(year,01,01,sep="-")))

ggplot(prov_db)+
 # geom_area(aes(year,ghg/1000,fill=fuel),alpha=0.6,
            #size=1.5,position = position_dodge(width = .3),width = 0.9
#  )+
  geom_line(data=prov_db1,aes(x=year,y=ghg/1000,color=fuel), size = 2) +
  facet_grid(~reorder(prov,-ghg)) +
  scale_y_continuous(expand=c(0,0),
                     #                     labels = scales::percent,
                     #limits = c(0,52),
                     #breaks = seq(-40,100, by = 20)
  ) +
  scale_fill_viridis("",discrete=TRUE,option="B",direction = 1) +
  scale_x_date(date_breaks = "5 year",
               date_minor_breaks = "1 year",
               date_labels = "%Y",
               limits = as.Date(c("2005-01-01","2020-01-01")),
                     expand=c(0,0)) +
  labs(x="",y="Electricity generating greenhouse \ngas emissions (Mt CO2 eq)",
       #       title="Electricity generation greenhouse gas emissions by province",
       #       subtitle = DB,
       #       caption="Source: National inventory report greenhouse gas source and sinks in Canada"
  ) +
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text = element_text(size = sz),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_text(size = sz),
        plot.subtitle = element_text(size = sz-2,hjust=0.5),
        plot.caption = element_text(face="italic",size = sz-4,hjust=0),
        plot.title = element_text(hjust=0.5,size = sz+2),
        strip.text.x = element_text(size=sz-3),
        #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        
        # For transparent background
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', colour = "transparent"),
  ) 

imsave("GHG")

################################################################################
################################################################################
################################################################################

library(ggthemes)
library(zoo)
library(curl)
library(httr)
library(janitor)
library(viridis)
library(scales)
library(XML)
library(lubridate)
library(readxl)
library(cansim)
library(tidyverse)

#Environment Canada Emissions Projection Data
file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Current-Projections-Actuelles/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="data/ec_projections_2022.csv",mode = "wb")


#2022 Projections Data
proj_data_2022<-read.csv("data/ec_projections_2022.csv",skip = 0,na = "-",
                         fileEncoding = "Latin1", check.names = F) %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),
         year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2022)<-c("region","scenario","sector","subsector_level_1",
                         "subsector_level_2","subsector_level_3","unit","year",
                         "emissions")



#2020 projections data
file_loc<-"https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2020/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="data/ec_projections_2020.csv",mode = "wb")

proj_data_2020<-read.csv("data/ec_projections_2020.csv",skip = 0,na = "-",fileEncoding = "Latin1") %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2020)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


#load 2018 detailed projections data

file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2018/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="data/ec_projections_2018.csv",mode = "wb")
proj_data_2018<-read.csv("data/ec_projections_2018.csv",skip = 0,na = "-",fileEncoding = "Latin1") %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))

names(proj_data_2018)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


#load 2019 detailed projections data
file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2019/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="data/ec_projections_2019.csv",mode = "wb")


proj_data_2019<-
  read_csv("data/ec_projections_2019.csv",skip = 0,na = "-",col_types = cols(.default = "c")) %>%
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  mutate_if(is.double,as.character()) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2019)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


proj_data<-proj_data_2019 %>% bind_rows(proj_data_2022,proj_data_2020,proj_data_2018)%>%
  filter(scenario!="NIR 2018",scenario!="NIR 2019",scenario!="NIR 2021",sector!="Total")

proj_data<-proj_data %>% filter(!sector%in%c("International Emissions","n/a","WCI Credits"))%>%
  mutate(prov=as.factor(region),
         prov=fct_recode(prov,"AB"="Alberta",
                         "BC"="British Columbia",
                         "NL"="Newfoundland and Labrador",
                         "MB"="Manitoba",
                         "SK"="Saskatchewan",
                         "NS"="Nova Scotia",
                         "ON"="Ontario",
                         "NT"="Northwest Territory",
                         "QC"="Quebec",
                         "NU"="Nunavut",
                         "NB"="New Brunswick",
                         "YT"="Yukon Territory",
                         "PE"="Prince Edward Island",
                         "NU"="Nunavut"
         ),
         prov=fct_collapse(prov,
                           "TERR" = c("NT", "NU","YT","NT & NU")
                           ,"ATL" = c("NL", "NB","NS","PE")
                           #,"OTHER ATL" = c("NL", "NB","PE")
         ))%>%
  select(year,prov,region,scenario,emissions,sector)%>% group_by(year,prov,sector,scenario)%>%summarize(emissions=sum(emissions,na.rm = T)) %>%ungroup()


#strip out nir from this csv, build new NIR

proj_data<-proj_data%>% 
  filter(scenario!="NIR 2018",scenario!="NIR 2019",scenario!="NIR 2020",scenario!="NIR 2021")%>%
  bind_rows(test=NIR_data %>%
              filter(sector %in% main_sectors)%>%
              mutate(sector=fct_other(sector,
                                      keep=c("Agriculture","Buildings","Electricity","Heavy Industry","Oil and Gas","Transportation"),                        
                                      other_level = "Waste and Others"
              ))%>%
              select(sector,year=Year,prov=Prov,emissions=GHGs)%>%
              group_by(year,prov,sector) %>% summarise(emissions=sum(emissions))%>% ungroup()%>%
              mutate(scenario="NIR 2022")
  )



#set terr agriculture equal to zero in years it doesn't appear


terr_ag_fix<-as_tibble(x=seq(1990,2020,1))%>% rename(year=value) %>% left_join(proj_data %>% filter(prov=="TERR",sector=="Agriculture",scenario=="NIR 2022")%>%
                                                                                 select(year,prov,sector,emissions))%>%
  mutate(emissions=na.fill(emissions,0),prov="TERR",sector="Agriculture",scenario="NIR 2022")


proj_data<-proj_data %>% filter(!((prov=="TERR")&(sector=="Agriculture") &(scenario=="NIR 2022"))) %>% bind_rows(terr_ag_fix)


inventory_provs<-ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2022") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.6)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks(n=5))+
  
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.85,end=0.01)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'))
inventory_provs

setwd("D:/Documents/GitHub/AuroraEval")
source("leachandrew_NIR_graphs.R")
