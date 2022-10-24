# Generates plots showing the division of electricity related emissions in Canada
# 
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# October 2022; Last revision: October 20, 2022

library("readxl")
library(tidyverse)
#library(ggplot2)

# Data used to plot pie chart
################################################################################
setwd("D:/Documents/Education/Masters Degree/Datasets")

GHG <- read_excel("Greenhouse gas emissions by province (Mt Co2 eq).xlsx")

colnames(GHG) <- c('Province','Yr2005','Yr2015','Yr2016','Yr2017','Yr2018',
                   'Yr2019','Yr2020','e_ghg2020','CO2_2020','e_ghg2005')

GHG_all <- GHG %>%
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
                          str_detect(Province, "YT")~"Yukon"),
#            e_ghg2005 = e_ghg2005,
#            e_ghg2020 = e_ghg2020,
            pct2005 = round(e_ghg2005/sum(e_ghg2005)*100),
            pct2020 = round(e_ghg2020/sum(e_ghg2020)*100),
         )

GHG_sig <- GHG_all %>%
  filter(pct2020 >= 2)

GHG_other <- GHG_all %>%
  filter(pct2020 < 2) %>%
  summarize(Province = "Other",
            e_ghg2005 = sum(e_ghg2005),
            pct2005 = sum(pct2005),
            e_ghg2020 = sum(e_ghg2020),
            pct2020 = sum(pct2020))

GHG <- rbind(GHG_sig,GHG_other)

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
setwd("D:/Documents/Education/Masters Degree/Datasets")

ghg <- read_excel("GHG emissions by province (Mt Co2 eq).xlsx") %>%
#  filter(Year >= 2015) %>%
  mutate(Prov = case_when(str_detect(Province, "AB")~"Alberta",
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
                              str_detect(Province, "YT")~"Yukon"),
         Year = as.factor(Year)) 

Canada <- max(ghg$e_ghg)

ghg_all <- ghg %>%
  group_by(Prov) %>%
  mutate(total = max(e_ghg),
         perc = total/Canada*100)

ghg_sig <- ghg_all %>%
  filter(perc >= 2)

ghg_other <- ghg_all %>%
  filter(perc < 2) %>%
  group_by(Year) %>%
  summarize(Prov = "Other Provinces",
            e_ghg = sum(e_ghg),
            perc = sum(perc))

ghg <- rbind(ghg_sig,ghg_other)

sz<-15
# Plot the data
ggplot(ghg,
#       aes(Province,e_ghg,colour=sit,fill=sit),
       alpha=0.8)+
  geom_col(aes(Year,e_ghg),#colour=sit,fill=sit),
           size=1.5,position = position_dodge(width = .3),width = 0.9)+
  facet_grid(~reorder(Prov,-perc)) +
  scale_y_continuous(expand=c(0,0),
#                     labels = scales::percent,
                     limits = c(0,52000),
                     #breaks = seq(-40,100, by = 20)
  ) +
#  scale_x_continuous(breaks = seq(2015,2020, by=1),
#                     expand=c(0,0)) +
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
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', colour = "transparent"),
  ) 
