# Functions to compare simulations to AESO data

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
  # Single week output, demand, and pool price
  plot_grid(wkPrice(year,month,day) + 
              theme(axis.title.x=element_blank(),axis.text.x=element_blank()),
            Week_act(year,month,day)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5))
}

################################################################################
# Plot difference between simulated and actual pool price
################################################################################

Weekly <- function(year, month, day, case) {
  # Plots hourly generation output of actual beside simulation
  
  # Set duration to the length of time being compared
  duration <- 14
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+duration,month,year, sep = "/"), format="%d/%m/%Y")
  
  # Filters for the desired case study
  Sim <- Hour %>%
    
    sim_filt1(.) %>%
    subset(., select=-c(Report_Year,Capacity_Factor)) %>%
    rbind(.,Import) %>%
    filter(Run_ID == case, 
           date >= wk_st, 
           date <= wk_end) %>%
    subset(., select=-c(Run_ID)) %>%
    mutate(sit = paste0("Simulation ",DB))
  
  # Select only a single week
  #    WK <- HrTime(data,year,month,day)
  ZPrice <- ZH %>%
    filter(Run_ID == case, 
           date >= wk_st, 
           date <= wk_end) %>%
    subset(., select = c(date, Demand)) %>%
    mutate(sit = paste0("Simulation ",DB))
  #Expo <- HrTime(Export,year,month,day)
  #Sim$MX <- ZPrice$Demand - Expo$Output_MWH
  
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
  
  Act <- rbind(WKIM, WKa) %>%
    subset(., select = c(time, total_gen, Plant_Type)) %>%
    rename(date = time, Output_MWH = total_gen, ID = Plant_Type) %>%
    mutate(sit = "NRG Stream data")
  
  WK <- rbind(Sim,Act)
  
  # Reorder the factor levels
  {
    WK$ID<-fct_relevel(WK$ID, "IMPORT", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "COAL", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "NGCONV", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "COGEN", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "SCGT", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "NGCC", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "HYDRO", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "OTHER", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "WIND", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "SOLAR", after = Inf)
    WK$ID<-fct_relevel(WK$ID, "STORAGE", after = Inf)
  }
  
  # Set the factor levels
  WK$ID <- factor(WK$ID, levels=c("IMPORT", "COAL", "NGCONV", "COGEN", 
                                                  "SCGT", "NGCC", "HYDRO", 
                                                  "OTHER", "WIND", "SOLAR", "STORAGE"))
  
  # Rename the factor levels
  levels(WK$ID) <- c("Import","Coal", "NGConv", "Cogen", "SCGT", "NGCC", "Hydro", 
                             "Other", "Wind", "Solar", "Storage")
  
  # Select the demand data
  dmd <- demand %>%
    filter(time >= wk_st & time <= wk_end) %>%
    rename(date = time) %>%
    subset(., select = c(date,Demand)) %>%
    mutate(sit = "NRG Stream data")
  
  line <- rbind(dmd,ZPrice)
  
  # Set the max and min for the plot
#  MX <- plyr::round_any(max(abs(Act$MX)), 100, f = ceiling)
#  MN <- plyr::round_any(min(Act$Output_MWH), 100, f = floor)
  
  # Plot the data    
  ggplot() +
    geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID), 
              alpha=0.6, size=.5, colour="black") +
    
    # Add hourly load line
    geom_line(data = line, 
              aes(x = date, y = Demand), size=2, colour = "black") +
    
    facet_grid(~sit) +
    
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
    scale_y_continuous(expand=c(0,0), 
                       #limits = c(MN,MX), 
                       #breaks = seq(MN, MX, by = MX/4)
                       ) +
    labs(x = "Date", y = "Output (MWh)", fill = "Resource") +
    scale_fill_manual(values = colours1)
}

Weekly_price <- function(year, month, day,case) {
  # Plot the pool price actual verses simulation
  
  # Set duration to the length of time being compared
  duration <- 14
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+duration,month,year, sep = "/"), format="%d/%m/%Y")
  
  # Filters for the desired case study
  Sim <- ZH %>%
    filter(Run_ID == case, 
           date >= wk_st, 
           date <= wk_end) %>%
    subset(., select = c(date, Price)) %>%
    mutate(sit = paste0("Simulation ",DB))
  
  Act <- demand %>%
    filter(time >= wk_st & time <= wk_end) %>%
    rename(date = time) %>%
    subset(., select = c(date, Price)) %>%
    mutate(sit = "NRG Stream data")
  
  wk_Price <- rbind(Sim,Act)
  
  # Set the max and min for the plot
  MX <- plyr::round_any(max(abs(wk_Price$Price)), 10, f = ceiling)
  MN <- plyr::round_any(min(abs(wk_Price$Price)), 10, f = floor)
  
  # Plot the data    
  ggplot() +
    geom_line(data = wk_Price, 
              aes(x = date, y = Price), 
              size = 1.5, colour = "red") +
    
    facet_grid(~sit) +
    
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

price_interval <- function(year1, year2, case) {
  # Based on code from Dr. Leach with simulation data overlaid ontop.
  # Plots the average price with bands showing the range between on and off peak
  # prices.
  
  source("DrLeach_Code.R")
  
  dataAct <- merit_filt %>%
    filter(year >= year1 & year <= year2) %>%
    mutate(time = as.POSIXct(paste0(year,"-",month,"-",day," ",he,":00:00"), "%Y-%m-%d %H:%M:%S", tz = "UTC"))%>%
    group_by(date,year,he,time)%>%
    summarize(actual_ail = median(actual_ail),
              actual_posted_pool_price = median(actual_posted_pool_price))%>%
    ungroup()
  
  dataSim <- ZH %>%
    mutate(year = year(date),
           time = date) %>%
    filter(year >= year1 & year <= year2,
           Run_ID == case)
  
  peak_data_Sim<-dataSim %>%
    #    filter(!is.na(actual_posted_pool_price),!is.na(actual_ail))%>%
    assign_date_time_days()%>%
    assign_peaks()%>%
    group_by(year,month) %>%
    summarize(ail=mean(Demand,na.rm = T),peak_ail=max(Demand),trough_ail=min(Demand),
              q75_price=quantile(Price, probs=c(.95)),
              q25_price=quantile(Price, probs=c(.05)),
              q75_ail=quantile(Demand, probs=c(.95)),
              q25_ail=quantile(Demand, probs=c(.05)),
              mean_peak_price=sum(Price*Demand*(on_peak==TRUE),
                                  na.rm = T)/sum(Demand*(on_peak==TRUE),
                                                 na.rm = T),
              mean_off_peak_price=sum(Price*Demand*(on_peak==FALSE),
                                      na.rm = T)/sum(Demand*(on_peak==FALSE),
                                                     na.rm = T),
              mean_peak_ail=sum(Demand*(on_peak==TRUE),
                                na.rm = T)/sum((on_peak==TRUE),
                                               na.rm = T),
              mean_off_peak_ail=sum(Demand*(on_peak==FALSE),
                                    na.rm = T)/sum((on_peak==FALSE),
                                                   na.rm = T),
              mean_price=sum(Price*Demand,
                             na.rm = T)/sum(Demand,na.rm = T),
              peak_price=max(Price),
              trough_price=min(Price)
    )%>%  
    mutate(date=ymd(paste(year,month,1,sep="-")),
           sit = paste0("Simulation ",DB))
  
  peak_data_Act<-forecast_data %>%
    mutate(year = year(time))%>%
    filter(!is.na(actual_posted_pool_price),!is.na(actual_ail),
           year >= year1 & year <= year2)%>%
    assign_date_time_days()%>%
    assign_peaks()%>%
    group_by(year,month) %>%
    summarize(ail=mean(actual_ail,na.rm = T),peak_ail=max(actual_ail),trough_ail=min(actual_ail),
              q75_price=quantile(actual_posted_pool_price, probs=c(.95)),
              q25_price=quantile(actual_posted_pool_price, probs=c(.05)),
              q75_ail=quantile(actual_ail, probs=c(.95)),
              q25_ail=quantile(actual_ail, probs=c(.05)),
              mean_peak_price=sum(actual_posted_pool_price*actual_ail*(on_peak==TRUE),
                                  na.rm = T)/sum(actual_ail*(on_peak==TRUE),
                                                 na.rm = T),
              mean_off_peak_price=sum(actual_posted_pool_price*actual_ail*(on_peak==FALSE),
                                      na.rm = T)/sum(actual_ail*(on_peak==FALSE),
                                                     na.rm = T),
              mean_peak_ail=sum(actual_ail*(on_peak==TRUE),
                                na.rm = T)/sum((on_peak==TRUE),
                                               na.rm = T),
              mean_off_peak_ail=sum(actual_ail*(on_peak==FALSE),
                                    na.rm = T)/sum((on_peak==FALSE),
                                                   na.rm = T),
              mean_price=sum(actual_posted_pool_price*actual_ail,
                             na.rm = T)/sum(actual_ail,na.rm = T),
              peak_price=max(actual_posted_pool_price),
              trough_price=min(actual_posted_pool_price)
    )%>%  
    mutate(date=ymd(paste(year,month,1,sep="-")),
           sit = "Actual")
  
  peak_data <- rbind(peak_data_Sim,peak_data_Act)
  
  top_panel<-ggplot(peak_data) +
    geom_line(aes(date,mean_price,linetype="A",color=sit),size=.85)+#,color="black")+
    geom_line(aes(date,mean_off_peak_price,linetype="B",color=sit),size=.85)+#,color="blue")+
    geom_ribbon(aes(date,ymax=q75_price,ymin=q25_price,fill=sit,color=sit),alpha=.5)+
    #  facet_grid(rows = vars(sit)) +
    #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
    scale_color_manual("",values = c("black","royalblue4"))+
    scale_fill_manual("",values = c("grey50","royalblue"),
                      labels=c("Two-tailed 90th percentile\nrange Actual",
                               "Two-tailed 90th percentile\nrange Simulation"))+
    scale_linetype_manual("",values = c("solid","11"),
                          labels=c("Peak \nperiod average","Off-peak \nperiod average"))+
    scale_x_date(expand=c(0,0),breaks="3 month",labels = date_format("%b\n%Y",tz="America/Denver"))+
    scale_y_continuous(expand=c(0,0))+
    #  paper_theme()+
    expand_limits(y=0)+ #make sure you get the zero line
    guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
    theme_bw() +
    theme(legend.position="right",
          #        axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 15)
          #legend.margin=margin(c(0,0,0,0),unit="cm")
          #legend.text = element_text(colour="black", size = 12, face = "bold")
          #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
    )+
    labs(y="Pool Prices ($/MWh)",x="",
         #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
         #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
         NULL) + 
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  top_panel
  
}

Weekly_line <- function(year, month, day, case) {
  # Simple line chart showing hourly generation output with actual beside 
  # simulation
  
  # Select only a single week
  ##############################################################################
  # Set duration to the length of time being compared
  duration <- 14
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+duration,month,year, sep = "/"), format="%d/%m/%Y")
  
  # Filters Simulated data for the desired case study
  ##############################################################################
  Sim <- Hour %>%
    sim_filt1(.) %>%
    subset(., select=-c(Report_Year,Capacity_Factor)) %>%
    rbind(.,Import) %>%
    filter(Run_ID == case,
           date >= wk_st, date <= wk_end) %>%
    subset(., select = -c(Run_ID)) %>%
    mutate(sit = paste0("Simulation ",DB))
  
  # Filters NRG Stream data for the desired case study
  ##############################################################################
  df1a$time <- as.POSIXct(df1a$time, tz = "MST")
  
  WKa <- df1a %>%
    filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT") %>%
    filter(time >= wk_st, time <= wk_end)
  
  WKIM <- df1a %>%
    filter(Plant_Type == "IMPORT") %>%
    filter(time >= wk_st & time <= wk_end)
  
  WKIM$total_gen <- WKIM$total_gen * -1
  
  Act <- rbind(WKIM, WKa) %>%
    rename(date = time, ID = Plant_Type, Output_MWH = total_gen) %>%
    subset(., select = c(ID, date, Output_MWH)) %>%
    mutate(sit = "NRG Stream data")
  
  total <- rbind(Act,Sim)
  
  {
    total$ID<-fct_relevel(total$ID, "IMPORT", after = Inf)
    total$ID<-fct_relevel(total$ID, "COAL", after = Inf)
    total$ID<-fct_relevel(total$ID, "NGCONV", after = Inf)
    total$ID<-fct_relevel(total$ID, "COGEN", after = Inf)
    total$ID<-fct_relevel(total$ID, "SCGT", after = Inf)
    total$ID<-fct_relevel(total$ID, "NGCC", after = Inf)
    total$ID<-fct_relevel(total$ID, "HYDRO", after = Inf)
    total$ID<-fct_relevel(total$ID, "OTHER", after = Inf)
    total$ID<-fct_relevel(total$ID, "WIND", after = Inf)
    total$ID<-fct_relevel(total$ID, "SOLAR", after = Inf)
    total$ID<-fct_relevel(total$ID, "STORAGE", after = Inf)
  }
  
  total$ID <- factor(total$ID, levels=c("IMPORT", "COAL", "NGCONV", "COGEN", 
                                       "SCGT", "NGCC", "HYDRO", "OTHER",
                                       "WIND", "SOLAR", "STORAGE"))
  
  levels(total$ID) <- c("Import","Coal", "NGConv", "Cogen", "SCGT", "NGCC", "Hydro", 
                             "Other", "Wind", "Solar", "Storage")
  
  # Set the max and min for the plot
  MX <- plyr::round_any(max(abs(total$Output_MWH)), 100, f = ceiling)
  MN <- plyr::round_any(min(total$Output_MWH), 100, f = floor)
  
  # Plot the data    
  ggplot() +
    geom_line(data = total, aes(x = date, y = Output_MWH, color = ID), 
              #                alpha=0.8, 
              size=2) +
    
    facet_grid(~sit) +
    
    # Add hourly load line
    #      geom_line(data = ZPrice, 
    #                aes(x = date, y = Demand), size=2, colour = "black") +
    scale_x_datetime(expand=c(0,0),
                     breaks = "3 days") +
    
    # Set the theme for the plot
    theme_bw() +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right",
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0), 
                       limits = c(MN,MX), 
                       breaks = seq(MN, MX, by = MX/4)
    ) +
    labs(x = "Date", y = "Output (MWh)", color = "Resource") +
    scale_color_manual(values = colours1)
}

Fossil_line <- function(year, month, day, case) {
  # Same as Weekly_line, but with only the fossil fuel resources
  
  # Select only a single week
  ##############################################################################
  # Set duration to the length of time being compared
  duration <- 14
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+duration,month,year, sep = "/"), format="%d/%m/%Y")
  
  # Filters Simulated data for the desired case study
  ##############################################################################
  Sim <- Hour %>%
    sim_filt1(.) %>%
    subset(., select=-c(Report_Year,Capacity_Factor)) %>%
    rbind(.,Import) %>%
    filter(Run_ID == case,
           date >= wk_st, date <= wk_end
           ) %>%
    subset(., select = -c(Run_ID)) %>%
    mutate(sit = paste0("Simulation ",DB))
  
  # Filters NRG Stream data for the desired case study
  ##############################################################################
  df1a$time <- as.POSIXct(df1a$time, tz = "MST")
  
  WKa <- df1a %>%
    filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT") %>%
    filter(time >= wk_st, time <= wk_end)
  
  WKIM <- df1a %>%
    filter(Plant_Type == "IMPORT") %>%
    filter(time >= wk_st & time <= wk_end)
  
  WKIM$total_gen <- WKIM$total_gen * -1
  
  Act <- rbind(WKIM, WKa) %>%
    rename(date = time, ID = Plant_Type, Output_MWH = total_gen) %>%
    subset(., select = c(ID, date, Output_MWH)) %>%
    mutate(sit = "NRG Stream data")
  
  total <- rbind(Act,Sim) %>%
    filter(ID == "COAL" | ID == "NGCONV" | ID == "COGEN" |
             ID == "SCGT" | ID == "NGCC")
  
  {
    total$ID<-fct_relevel(total$ID, "COAL", after = Inf)
    total$ID<-fct_relevel(total$ID, "NGCONV", after = Inf)
    total$ID<-fct_relevel(total$ID, "COGEN", after = Inf)
    total$ID<-fct_relevel(total$ID, "SCGT", after = Inf)
    total$ID<-fct_relevel(total$ID, "NGCC", after = Inf)
  }
  
  total$ID <- factor(total$ID, levels=c("IMPORT", "COAL", "NGCONV", "COGEN", 
                                        "SCGT", "NGCC", "HYDRO", "OTHER",
                                        "WIND", "SOLAR", "STORAGE"))
  
  levels(total$ID) <- c("Import","Coal", "NGConv", "Cogen", "SCGT", "NGCC", "Hydro", 
                        "Other", "Wind", "Solar", "Storage")
  
  # Set the max and min for the plot
  MX <- plyr::round_any(max(abs(total$Output_MWH)), 100, f = ceiling)
  MN <- plyr::round_any(min(total$Output_MWH), 100, f = floor)
  
  # Plot the data    
  ggplot() +
    geom_line(data = total, aes(x = date, y = Output_MWH, color = ID), 
              #                alpha=0.8, 
              size=2) +
    
    facet_grid(~sit) +
    
    # Add hourly load line
    #      geom_line(data = ZPrice, 
    #                aes(x = date, y = Demand), size=2, colour = "black") +
    scale_x_datetime(expand=c(0,0),
                     breaks = "3 days") +
    
    # Set the theme for the plot
    theme_bw() +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right",
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0), 
                       limits = c(MN,MX), 
                       breaks = seq(MN, MX, by = MX/4)
    ) +
    labs(x = "Date", y = "Output (MWh)", color = "Resource") +
    scale_color_manual(values = c("black", "grey", "darkslategrey", "coral4", "goldenrod4"))
}

rev_dur <- function(year1, year2, type, case) {
  # Plots revenue duration plot by plant type, comparing simulated and AESO
  
  totZone <- ZoneH %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    subset(.,select=c(date,Condition,Price))
  
  typeH <- sim_filt1(Hour)

  typeH <- Hour %>%
    sim_filt1(.) %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           ID == type,
           Run_ID == case) 

  data1 <- left_join(totZone, typeH, by=c("date")) 
  
  totSim <- data1 %>%
    group_by(Condition, Report_Year) %>%
    mutate(Revenue = Price*Output_MWH/1000, perc = 1-ecdf(Revenue)(Revenue)) %>%
    select(Condition, Report_Year, Revenue, perc) %>%
    rename(Year = Report_Year) %>%
    ungroup() %>%
    mutate(sit = "Simulated")
  
  #  totSim$Report_Year <- as.factor(totSim$Report_Year)
  
  Actual <- na.omit(sub_samp)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Hour <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  
  totAct <- Actual %>%
    filter(Year >= year1, 
           Year <= year2,
           Plant_Type == type) %>%
    mutate(Condition = if_else(between(Hour, 08, 23), 
                               "On-Peak WECC", "Off-Peak WECC"),
           Revenue = Revenue/1000) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(Revenue)(Revenue)) %>%
    select(Condition, Year, Revenue, perc) %>%
    ungroup() %>%
    mutate(sit = "Actual")
  
  total <- rbind(totSim, totAct)
  sz <- 15

  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Revenue, colour = Year, linetype = sit), size = 1) +
    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Revenue ($ in thousands)", 
         x = "Percentage of Time", 
         title = paste("AESO ", type, "Data vs Simulation"),
         subtitle = DB) +
    scale_color_manual(values = c("goldenrod1", "forestgreen", "cornflowerblue",
                                  "firebrick","gray60")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

year_comp <- function(year,case) {
  # Plots the difference in Pool Price between AESO and Sim
  
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

year_dif <- function(year,case) {
  # Bar plot showing the difference between AESO and Sim
  
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
  # Bar chart comparing monthly average pool prices 
  
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

tot_gen <- function(year1, year2, case) {
  # Plots the year-end generation by technology for AESO and Sim
  
  Act <- sub_samp %>%
    filter(! NRG_Stream %in% trade_excl,
           year(time) >= year1,
           year(time) <= year2,
           Plant_Type == "COAL" | Plant_Type == "NGCONV" | Plant_Type == "COGEN" | Plant_Type == "NGCC" |
             Plant_Type == "SCGT" | Plant_Type == "HYDRO" | Plant_Type == "OTHER" |
             Plant_Type == "WIND" | Plant_Type == "SOLAR" | Plant_Type == "STORAGE" |
             Plant_Type == "EXPORT" | Plant_Type == "IMPORT"
    ) %>%
    mutate(Year = as.factor(year(time))) %>%
    group_by(Year, Plant_Type) %>%
    summarise(gen = sum(gen)) %>%
    mutate(sit = "Actual")
  
  Act <- na.omit(Act)
  
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "HYDRO",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "WIND",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "SOLAR",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "OTHER",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "STORAGE",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "EXPORT",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "IMPORT",after=Inf)
  
  Imp <- ZH %>%
    mutate(Year = year(date)) %>%
    filter(Year >= year1,
           Year <= year2,
    ) %>%
    group_by(Year) %>%
    summarise(gen = -sum(Imports)) %>%
    mutate(Plant_Type = "IMPORT", sit = "Simulation")
  
  Exp <- ZH %>%
    mutate(Year = year(date)) %>%
    filter(Year >= year1,
           Year <= year2,
    ) %>%
    group_by(Year) %>%
    summarise(gen = sum(Exports)) %>%
    mutate(Plant_Type = "EXPORT", sit = "Simulation")
  
  Sim <- Hour %>%
    filter(Run_ID == case,
           Report_Year >= year1,
           Report_Year <= year2,
           ID == "LTO_Coal" | ID == "AB_NGCONV" | ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | 
             ID == "AB_SCCT_noncogen" | ID == "LTO_Hydro" | ID == "LTO_Other" | 
             ID == "LTO_Wind" | ID == "LTO_Solar" | ID == "LTO_Storage"
    ) %>%
    mutate(Plant_Type = ID, Year = Report_Year) %>%
    group_by(Year, Plant_Type) %>%
    summarise(gen = sum(Output_MWH)) %>%
    mutate(sit = "Simulation")
  
  Sim <- rbind(Sim, Imp, Exp)
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("LTO_Coal", "AB_NGCONV", "AB_CCCT_noncogen", "LTO_Cogen",
                                                    "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                                    "LTO_Wind", "LTO_Solar", "LTO_Storage", "EXPORT", "IMPORT"))
  
  levels(Sim$Plant_Type) <- c("COAL", "NGCONV", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                              "WIND", "SOLAR", "STORAGE", "EXPORT", "IMPORT")
  
  Sim$Year <- as.factor(Sim$Year)
  
  total <- rbind(Sim,Act)
  total$gen <- total$gen/1000
  
  sz <- 15
  
  ggplot() +
    geom_col(data = total, position = "dodge", alpha = 0.8, width = 0.7,
             aes(x = Plant_Type, y = gen, fill = sit, linetype = sit)) +
    #    geom_text(total, aes(label = gen), vjust = -0.5, angle = 90) +
    facet_grid(~Year) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          #plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Total Yearly Generation (GWh)", 
         #title = "Yearly total generation AESO Data vs Simulation",
         #subtitle = DB
         ) +
    scale_fill_manual(values = c("grey50","black")) +
    #    scale_x_continuous(expand=c(0,0), 
    #                       limits = c(0,1.1),
    #                       labels = percent) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(-12000,40000)
                       #                       breaks = seq(0,1, by = 0.2)
    )
}

market_share <- function(year1, year2, case) {
  # Plots the year-end generation by technology for AESO and Sim
  
  Act <- sub_samp %>%
    filter(! NRG_Stream %in% trade_excl,
           year(time) >= year1,
           year(time) <= year2,
           Plant_Type == "COAL" | Plant_Type == "NGCONV" | Plant_Type == "COGEN" | Plant_Type == "NGCC" |
             Plant_Type == "SCGT" | Plant_Type == "HYDRO" | Plant_Type == "OTHER" |
             Plant_Type == "WIND" | Plant_Type == "SOLAR" | Plant_Type == "STORAGE" |
             Plant_Type == "IMPORT" 
    ) %>%
    mutate(Year = as.factor(year(time))) %>%
    group_by(Year, Plant_Type) %>%
    summarise(gen = abs(sum(gen))) %>%
    ungroup() %>%
    group_by(Year)%>%
    summarise(share = gen/sum(gen),
              Plant_Type = Plant_Type) %>%
    mutate(sit = "Actual")
  
  Act <- na.omit(Act)
  
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "IMPORT",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "HYDRO",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "WIND",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "SOLAR",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "OTHER",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "STORAGE",after=Inf)
  
  
  Imp <- ZH %>%
    mutate(Year = year(date)) %>%
    filter(Year >= year1,
           Year <= year2,
    ) %>%
    group_by(Year) %>%
    summarise(gen = sum(Imports)) %>%
    mutate(Plant_Type = "IMPORT", sit = "Simulation")
  
  Sim <- Hour %>%
    filter(Run_ID == case,
           Report_Year >= year1,
           Report_Year <= year2,
           ID == "LTO_Coal" | ID == "AB_NGCONV" | ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | 
             ID == "AB_SCCT_noncogen" | ID == "LTO_Hydro" | ID == "LTO_Other" | 
             ID == "LTO_Wind" | ID == "LTO_Solar" | ID == "LTO_Storage"
    ) %>%
    mutate(Plant_Type = ID, Year = Report_Year) %>%
    group_by(Year, Plant_Type) %>%
    summarise(gen = abs(sum(Output_MWH)))
  
  Sim <- rbind(Sim, Imp)%>%#, Exp) %>%
    group_by(Year)%>%
    summarise(share = gen/sum(gen),
              Plant_Type = Plant_Type) %>%
    mutate(sit = "Simulation")
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("IMPORT","LTO_Coal", "AB_NGCONV", "AB_CCCT_noncogen", "LTO_Cogen",
                                                    "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                                    "LTO_Wind", "LTO_Solar", "LTO_Storage"))#"EXPORT", 
  
  levels(Sim$Plant_Type) <- c("IMPORT", "COAL", "NGCONV", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                              "WIND", "SOLAR", "STORAGE")#"EXPORT", 
  
  Sim$Year <- as.factor(Sim$Year)
  
  total <- rbind(Sim,Act)
  
  sz <- 15
  
  # Plot the data
  ggplot(total,
         aes(Year,share,colour=sit,fill=sit),
         alpha=0.8)+
    geom_col(aes(Plant_Type,share,colour=sit,fill=sit),
             size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","black"))+
    scale_fill_manual("",values=c("grey50","black"))+
    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       labels = scales::percent,
                       limits = c(0,0.4),
                       breaks = seq(0,0.5,by = 0.1)
    ) +
    labs(x="",y="Percentage of Market",
         #title=paste0("Market Share by Technology (%, ",year1,"-",year2,")"),
         #subtitle = DB,
         #caption="Source: AESO Data, accessed via NRGStream"
         ) +
    theme(#panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          #panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          #plot.subtitle = element_text(size = sz-2,hjust=0.5),
          #plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          #plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
  
}

tot_intertie <- function(year1, year2, case) {
  # Plots the year-end imports and exports by technology for AESO and Sim
  
  Act <- sub_samp %>%
    filter(! NRG_Stream %in% trade_excl,
           year(time) >= year1,
           year(time) <= year2,
           Plant_Type == "EXPORT" | Plant_Type == "IMPORT"
    ) %>%
    mutate(Year = as.factor(year(time))) %>%
    group_by(Year, Plant_Type) %>%
    summarise(gen = sum(gen)) %>%
    mutate(sit = "Actual")
  
  Act <- na.omit(Act)
  
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "EXPORT",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "IMPORT",after=Inf)
  
  Imp <- ZH %>%
    mutate(Year = year(date)) %>%
    filter(Year >= year1,
           Year <= year2,
    ) %>%
    group_by(Year) %>%
    summarise(gen = -sum(Imports)) %>%
    mutate(Plant_Type = "IMPORT", sit = "Simulation")
  
  Exp <- ZH %>%
    mutate(Year = year(date)) %>%
    filter(Year >= year1,
           Year <= year2,
    ) %>%
    group_by(Year) %>%
    summarise(gen = sum(Exports)) %>%
    mutate(Plant_Type = "EXPORT", sit = "Simulation")
  
  Sim <- rbind(Imp, Exp)
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("LTO_Coal", "AB_CCCT_noncogen", "LTO_Cogen",
                                                    "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                                    "LTO_Wind", "LTO_Solar", "LTO_Storage", "EXPORT", "IMPORT"))
  
  levels(Sim$Plant_Type) <- c("COAL", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                              "WIND", "SOLAR", "STORAGE", "EXPORT", "IMPORT")
  
  Sim$Year <- as.factor(Sim$Year)
  
  total <- rbind(Sim,Act)
  total$gen <- total$gen/1000
  
  sz <- 15
  
  ggplot() +
    geom_col(data = total, position = "dodge", alpha = 0.8, width = 0.7,
             aes(x = Plant_Type, y = gen, fill = sit, linetype = sit)) +
    #    geom_text(total, aes(label = gen), vjust = -0.5, angle = 90) +
    facet_grid(~Year) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Total Yearly Generation (GWh)", 
         title = "Yearly total intertie \nAESO Data vs Simulation",
         subtitle = DB) +
    scale_fill_manual(values = c("grey50","royalblue")) +
    #    scale_x_continuous(expand=c(0,0), 
    #                       limits = c(0,1.1),
    #                       labels = percent) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(-12000,6000)
                       #                       breaks = seq(0,1, by = 0.2)
    )
}

capture_price <- function(year1, year2, case) {
  # Plots the difference between the average capture price realized by each 
  # generation technology and the mean price for the same time period. 
  
  # Based on a plot designed by Dr. Andrew Leach

  # Filters data set for required timeframe
  Act <- df1 %>% 
    filter(year(time) >= year1,
           year(time) <= year2,
           Plant_Type %in% gen_set) %>%
    group_by(Plant_Type,Year) %>% 
    summarise(capture = sum(total_rev)/sum(total_gen),
              avg_rev = sum(total_rev)/sum(total_gen),
              p_mean=mean(price_mean, na.rm = TRUE)) %>%
    mutate(sit = "Actual", Year = as.factor(Year))

  # Filters data set for required timeframe for simulated data
  SampleSimZ <- ZH  %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Run_ID == case,
    ) %>%
    subset(., select = c(date, Price, Imports, Exports))
  
  SampleSim <- Hr %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Output_MWH >= 0,
           Run_ID == case,
           ID == "LTO_Coal" | ID == "AB_NGCONV" | ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | 
             ID == "AB_SCCT_noncogen" | ID == "LTO_Hydro" | ID == "LTO_Other" | 
             ID == "LTO_Wind" | ID == "LTO_Solar" | ID == "LTO_Storage"
    ) %>%
    mutate(Year = as.factor(Report_Year)) %>%
    subset(., select = c(date, ID, Output_MWH, Energy_Revenue, Year)) 
  
  SamSim <- merge(SampleSimZ, SampleSim, by = "date") %>%
    subset(., select = -c(Imports,Exports))
  
  # This section calculates the achieved prices for imports and exports
    Imp <- SampleSimZ %>%
      mutate(Energy_Revenue = Price*Imports/1000, 
             Year = as.factor(year(date)), 
             ID = "IMPORT",
             Output_MWH = Imports) %>%
      subset(., select = -c(Imports,Exports))
  
    Exp <- SampleSimZ %>%
      mutate(Energy_Revenue = Price*Exports/1000, 
             Year = as.factor(year(date)), 
             ID = "EXPORT",
             Output_MWH = Exports) %>%
      subset(., select = -c(Imports,Exports))
  
  Sim <- rbind(SamSim,Imp,Exp) %>%
    group_by(ID,Year,date) %>%
    summarise(total_rev = sum(Energy_Revenue*1000), 
              total_gen = sum(Output_MWH),
              price_mean=mean(Price)) %>%
    ungroup() %>%
    mutate(Plant_Type = ID) %>%
    group_by(Plant_Type,Year) %>%
    summarise(capture = sum(total_rev)/sum(total_gen),
              avg_rev = sum(total_rev)/sum(total_gen),
              p_mean=mean(price_mean, na.rm = TRUE)) %>%
    mutate(sit = "Simulation")
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("LTO_Coal", "AB_NGCONV", "AB_CCCT_noncogen", 
                                                    "LTO_Cogen","AB_SCCT_noncogen",
                                                    "LTO_Hydro","LTO_Other", 
                                                    "LTO_Wind","LTO_Solar",
                                                    "LTO_Storage","EXPORT","IMPORT"))
  
  levels(Sim$Plant_Type) <- c("COAL", "NGCONV", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                              "WIND", "SOLAR", "STORAGE", "EXPORT", "IMPORT")
  
  total <- rbind(Sim,Act)
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(Year,capture-p_mean,colour=sit,fill=sit),
         alpha=0.8)+
    geom_col(aes(Plant_Type,capture-p_mean,colour=sit,fill=sit),
             size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","royalblue"))+
    scale_fill_manual("",values=c("grey50","royalblue"))+
    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
#                       limits = c(-50,100),
#                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Revenue Relative to \nMean Price ($/MWh)",
         title=paste0("Energy Price Capture Differential ($/MWh, ",year1,"-",year2,")"),
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
#          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          ) 
}

capturePrice <- function(year, plant_type, case) {
  # Plots the difference between the average capture price realized by each 
  # generation technology and the mean price for the same time period. 
  
  # Based on a plot designed by Dr. Andrew Leach
  
  # Filters data set for required timeframe
  Act <- sub_samp %>% 
    filter(year(time) == year,
           #year(time) <= year,
           Plant_Type %in% gen_set) %>%
    mutate(Year = year(time)) %>%
    group_by(ID,Year,Plant_Type) %>% 
    summarize(capture = sum(Revenue)/sum(gen),
              #avg_rev = sum(Revenue)/sum(gen),
              p_mean=mean(Price, na.rm = TRUE)) %>%
    mutate(sit = "Actual", Year = as.factor(Year)) #%>%
    #subset(., select = c(time,Price,ID,AESO_Name,Plant_Type,Cap_Fac,
    #                     Year,capture,avg_rev,p_mean,sit))
  
  ResourceHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",
                                                        ResourceHr$Time_Period))), 
                        format="%Y-%m-%d %H:%M:%S",tz = "MST")-(60*60)
  
  # Filters data set for required timeframe for simulated data
  SampleSimZ <- ZH  %>%
    filter(year(date) == year,
#           year(date) <= year2,
           Run_ID == case,
    ) %>%
    subset(., select = c(date, Price, Imports, Exports))
  
  SampleSim <- ResourceHr %>%
    subset(., select = c(date,ID,Name,Output_MWH,Capacity_Factor,Primary_Fuel,
                         Revenue,Run_ID,Report_Year,#Beg_Date,End_Date,
                         Condition)) %>%
    filter(Report_Year == year,
#           Report_Year <= year2,
           Output_MWH >= 0,
           Run_ID == case,
    ) %>%
    mutate(Year = as.factor(Report_Year),
           Aurora_ID = ID,
           ID = str_extract(Name, "(?<=\\().*(?=\\))"),
           Plant_Type = case_when(str_detect(Primary_Fuel, "Coal")~"COAL",
                                  str_detect(Primary_Fuel, "NaturalGas")~"NGCC",
                                  str_detect(Primary_Fuel, "-Peaking")~"SCGT",
                                  str_detect(Primary_Fuel, "Water")~"HYDRO",
                                  str_detect(Primary_Fuel, "COGEN")~"COGEN",
                                  str_detect(Primary_Fuel, "Wind")~"WIND",
                                  str_detect(Primary_Fuel, "Other")~"OTHER",
                                  str_detect(Primary_Fuel, "Solar")~"SOLAR",
                                  str_detect(Primary_Fuel, "Storage")~"STORAGE",
                                  str_detect(Primary_Fuel, "Trade")~"IMPORT",
                                  str_detect(Primary_Fuel, "100-Hydrogen")~"H2",
                                  str_detect(Primary_Fuel, "Simple Cycle")~"SC_BLEND",
                                  ),
           sit="Simulation")

  SamSim <- merge(SampleSimZ, SampleSim, by = "date") %>%
    subset(., select = -c(Imports,Exports,Run_ID,Report_Year,Primary_Fuel)) %>%
    mutate(Revenue = Price * Output_MWH)
  
  
  SamSim <- SamSim %>%
    group_by(ID,Year,sit,Plant_Type) %>%
    summarize(capture = sum(Revenue)/sum(Output_MWH),
           #avg_rev = sum(Revenue)/sum(Output_MWH),
           p_mean=mean(Price, na.rm = TRUE),
           #time = date,
           #AESO_Name = Name,
           #Cap_Fac=Capacity_Factor
           ) #%>%
    #subset(., select=c(time,Price,ID,AESO_Name,Plant_Type,Cap_Fac,Year,capture,avg_rev,
    #                    p_mean,sit))
  
  # This section calculates the achieved prices for imports and exports
#  Imp <- SampleSimZ %>%
#    mutate(Energy_Revenue = Price*Imports/1000, 
#           Year = as.factor(year(date)), 
#           ID = "IMPORT",
#           Output_MWH = Imports) %>%
#    subset(., select = -c(Imports,Exports))
  
#  Exp <- SampleSimZ %>%
#    mutate(Energy_Revenue = Price*Exports/1000, 
#           Year = as.factor(year(date)), 
#           ID = "EXPORT",
#           Output_MWH = Exports) %>%
#    subset(., select = -c(Imports,Exports))
  
#  Sim <- rbind(SamSim,Imp,Exp) #%>%
#    group_by(ID,Year,date) %>%
#    mutate(total_rev = sum(Energy_Revenue*1000), 
#              total_gen = sum(Output_MWH),
#              price_mean=mean(Price)) %>%
#    ungroup() %>%
#    mutate(Plant_Type = ID) %>%
#    group_by(Plant_Type,Year) %>%
#    mutate(capture = sum(total_rev)/sum(total_gen),
#           avg_rev = sum(total_rev)/sum(total_gen),
#           p_mean=mean(price_mean, na.rm = TRUE),
#           sit = "Simulation",
#           time = date) %>%
#    subset(., select = -c(date,ID,total_gen,total_rev,price_mean,Output_MWH))
  
#  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("LTO_Coal", "AB_NGCONV", 
#                                                    "AB_CCCT_noncogen", "LTO_Cogen",
#                                                    "AB_SCCT_noncogen", "AB_CCCT_Blended", 
#                                                    "AB_SCCT_Blended", "LTO_H2", "LTO_Hydro", 
#                                                    "LTO_Other", "LTO_Nuclear",
#                                                    "LTO_Wind", "LTO_Solar", "LTO_Storage"))
  
#  levels(Sim$Plant_Type) <- c("COAL", "NGCONV", "NGCC", "COGEN", "SCGT", "CC_BLEND",
#                              "SC_BLEND", "H2", "HYDRO", "OTHER", "NUCLEAR",
#                              "WIND", "SOLAR", "STORAGE")
  
  total <- rbind(SamSim,Act) %>%
    filter(Plant_Type == plant_type) %>%
    na.omit()
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(Year,capture-p_mean,colour=sit,fill=sit),
         alpha=0.8)+
    geom_col(aes(ID,capture-p_mean,colour=sit,fill=sit),
             size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","royalblue"))+
    scale_fill_manual("",values=c("grey50","royalblue"))+
    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       #                       limits = c(-50,100),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Revenue Relative to \nMean Price ($/MWh)",
         title=paste0("Energy Price Capture Differential ($/MWh, ",year,")"),
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
}

capturePrice_diff <- function(year1, year2, plant_type, case) {
  # Plots the difference between the average capture price realized by each 
  # generation technology and the mean price for the same time period. 
  
  # Based on a plot designed by Dr. Andrew Leach
  
  # Filters data set for required timeframe
  Act <- sub_samp %>% 
    filter(year(time) >= year1,
           year(time) <= year2,
           Plant_Type %in% gen_set) %>%
    mutate(Year = year(time)) %>%
    group_by(ID,Plant_Type,Year) %>% 
    summarize(capture = sum(Revenue)/sum(gen),
           p_mean=mean(Price, na.rm = TRUE),
           diff_Act = capture-p_mean) %>%
    mutate(Year = as.factor(Year),
           ) %>%
    filter(Plant_Type == plant_type) %>%
    subset(., select = c(ID,Year,diff_Act))
  
#  ResourceHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",
#                                                        ResourceHr$Time_Period))), 
#                                format="%Y-%m-%d %H:%M:%S",tz = "MST")-(60*60)
  
  # Filters data set for required timeframe for simulated data
  SampleSimZ <- ZH  %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Run_ID == case,
    ) %>%
    subset(., select = c(date, Price, Imports, Exports))
  
  SampleSim <- ResourceHr %>%
    subset(., select = c(date,ID,Name,Output_MWH,Capacity_Factor,Primary_Fuel,
                         Revenue,Run_ID,Report_Year,#Beg_Date,End_Date,
                         Condition)) %>%
    filter(Report_Year >= year1,
           Report_Year <= year2,
           Output_MWH >= 0,
           Run_ID == case,
    ) %>%
    mutate(Year = as.factor(Report_Year),
           ID = str_extract(Name, "(?<=\\().*(?=\\))"),
           Plant_Type = case_when(str_detect(Primary_Fuel, "Coal")~"COAL",
                                  str_detect(Primary_Fuel, "NaturalGas")~"NGCC",
                                  str_detect(Primary_Fuel, "-Peaking")~"SCGT",
                                  str_detect(Primary_Fuel, "Water")~"HYDRO",
                                  str_detect(Primary_Fuel, "COGEN")~"COGEN",
                                  str_detect(Primary_Fuel, "Wind")~"WIND",
                                  str_detect(Primary_Fuel, "Other")~"OTHER",
                                  str_detect(Primary_Fuel, "Solar")~"SOLAR",
                                  str_detect(Primary_Fuel, "Storage")~"STORAGE",
                                  str_detect(Primary_Fuel, "Trade")~"IMPORT",
                                  str_detect(Primary_Fuel, "100-Hydrogen")~"H2",
                                  str_detect(Primary_Fuel, "Simple Cycle")~"SC_BLEND",
           ),
           sit="Simulation")
  
  Sim <- merge(SampleSimZ, SampleSim, by = "date") %>%
    subset(., select = -c(Imports,Exports,Run_ID,Report_Year,Primary_Fuel)) %>%
    mutate(Revenue = Price * Output_MWH) %>%
    group_by(ID,Plant_Type,Year) %>%
    summarize(capture = sum(Revenue)/sum(Output_MWH),
           p_mean=mean(Price, na.rm = TRUE),
           diff_Sim = capture-p_mean) %>%
    filter(Plant_Type == plant_type) %>%
    subset(., select=c(ID,Year,diff_Sim))
  
  total <- merge(Act,Sim,by = c("ID","Year"))  %>%
    mutate(CP_diff = diff_Act - diff_Sim)
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(Year,CP_diff,colour=Year,fill=Year),
         alpha=0.8)+
    geom_col(aes(ID,CP_diff,colour=Year,fill=Year),
             size=1.5,position = position_dodge(width = .75),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=AESO_colours)+
    scale_fill_manual("",values=AESO_colours)+
#    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(-50,50),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Difference in Revenue Relative to Mean Price 
    between Actual and Simulation ($/MWh)",
         title=paste0("Energy Price Capture Differential ($/MWh, ",year1,"-",
                      year2," (%))"),
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
}

capPrice_diff <- function(year1, year2, plant_type, case) {
  # Plots the difference between the average capture price realized by each 
  # generation technology and the mean price for the same time period. 
  
  # Based on a plot designed by Dr. Andrew Leach
  
  # Filters data set for required timeframe
  Act <- sub_samp %>% 
    filter(year(time) >= year1,
           year(time) <= year2,
           Plant_Type %in% gen_set) %>%
    mutate(Year = year(time)) %>%
    group_by(ID,Plant_Type,Year) %>% 
    summarize(capture_Act = sum(Revenue)/sum(gen)) %>%
    mutate(Year = as.factor(Year),
    ) %>%
    filter(Plant_Type == plant_type) %>%
    subset(., select = c(ID,Year,capture_Act))
  
  # Filters data set for required timeframe for simulated data
  SampleSimZ <- ZH  %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Run_ID == case,
    ) %>%
    subset(., select = c(date, Price, Imports, Exports))
  
  SampleSim <- ResourceHr %>%
    subset(., select = c(date,ID,Name,Output_MWH,Capacity_Factor,Primary_Fuel,
                         Revenue,Run_ID,Report_Year,
                         Condition)) %>%
    filter(Report_Year >= year1,
           Report_Year <= year2,
           Output_MWH >= 0,
           Run_ID == case,
    ) %>%
    mutate(Year = as.factor(Report_Year),
           ID = str_extract(Name, "(?<=\\().*(?=\\))"),
           Plant_Type = case_when(str_detect(Primary_Fuel, "Coal")~"COAL",
                                  str_detect(Primary_Fuel, "NaturalGas")~"NGCC",
                                  str_detect(Primary_Fuel, "-Peaking")~"SCGT",
                                  str_detect(Primary_Fuel, "Water")~"HYDRO",
                                  str_detect(Primary_Fuel, "COGEN")~"COGEN",
                                  str_detect(Primary_Fuel, "Wind")~"WIND",
                                  str_detect(Primary_Fuel, "Other")~"OTHER",
                                  str_detect(Primary_Fuel, "Solar")~"SOLAR",
                                  str_detect(Primary_Fuel, "Storage")~"STORAGE",
                                  str_detect(Primary_Fuel, "Trade")~"IMPORT",
                                  str_detect(Primary_Fuel, "100-Hydrogen")~"H2",
                                  str_detect(Primary_Fuel, "Simple Cycle")~"SC_BLEND",
           ),
           sit="Simulation")
  
  Sim <- merge(SampleSimZ, SampleSim, by = "date") %>%
    subset(., select = -c(Imports,Exports,Run_ID,Report_Year,Primary_Fuel)) %>%
    mutate(Revenue = Price * Output_MWH) %>%
    group_by(ID,Plant_Type,Year) %>%
    summarize(capture_Sim = sum(Revenue)/sum(Output_MWH)) %>%
    filter(Plant_Type == plant_type) %>%
    subset(., select=c(ID,Year,capture_Sim))
  
  total <- merge(Act,Sim,by = c("ID","Year"))  %>%
    mutate(CP_diff = capture_Act - capture_Sim)
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(Year,CP_diff,colour=Year,fill=Year),
         alpha=0.8)+
    geom_col(aes(ID,CP_diff,colour=Year,fill=Year),
             size=1.5,position = position_dodge(width = .75),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=AESO_colours)+
    scale_fill_manual("",values=AESO_colours)+
    #    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(-50,50),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Difference in Revenue between 
         Actual and Simulation ($/MWh)",
         title=paste0("Energy Price Capture Differential ($/MWh, ",year1,"-",
                      year2," (%))"),
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
}

year_price <- function(year,case) {
  # Line plot of the pool price with simulation shown in red over the historical
  
  # Filters for the desired case study
  data <- ZH %>%
    filter(Run_ID == case,
           date >= paste(year,"-01-01 00:00:00", sep = "") & 
             date <= 
             paste(year,"-12-31 00:00:00", sep = "")) %>%
    subset(., select = c(date,Price)) %>%
    mutate(sit = "Simulation")
  
  yr_st <- as.POSIXct(paste("01","01",year, sep = "/"), format="%d/%m/%Y")
  yr_end <- as.POSIXct(paste("31","12",year, sep = "/"), format="%d/%m/%Y")
  
  price_YR <- demand %>%
    filter(time >= yr_st & time <= yr_end) %>%
    mutate(date = time) %>%
    subset(., select = c(date,Price)) %>%
    mutate(sit = "Actual")
  
  Price <- rbind(data,price_YR)
  
  # Set the max and min for the plot
  MX <- plyr::round_any(
    max(Price$Price),
    10, f = ceiling)
  MN <- plyr::round_any(
    min(Price$Price),
    10, f = floor)
  
  sz = 15
  
  # Plot the data    
  ggplot() +
    geom_line(data = Price, 
              aes(x = date, y = Price, color = sit), 
              size = 1.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(size = sz+2),
          plot.subtitle = element_text(size = sz-2),
          axis.title.x = element_blank(),
          aspect.ratio = 2/6
    ) +
    labs(y = "Pool Price \n$/MWh", color = "Data Set",
         title = "Yearly total intertie \nAESO Data vs Simulation",
         subtitle = DB) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), 
                       limits= c(MN,MX),
                       #                       labels = label_number(accuracy = 1),
                       breaks = seq(MN, MX, by = MX/4)
    ) + 
    scale_color_manual(values = c("black", "red"))
}

capacity_factor <- function(year, plant_type, case) {
  # Plots the difference between the average capture price realized by each 
  # specific plant of a generation technology and the mean price for the same 
  # time period. 
  
  # Based on a plot designed by Dr. Andrew Leach
  
  # Filters data set for required timeframe
  Act <- sub_samp %>% 
    filter(year(time) == year,
           Plant_Type %in% gen_set) %>%
    mutate(Year = year(time)) %>%
    group_by(ID,Plant_Type,Year) %>%
    summarize(Cap_Fac = mean(Cap_Fac)) %>%
    mutate(sit = "Actual", Year = as.factor(Year))
  
  #  ResourceHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",
  #                                                        ResourceHr$Time_Period))), 
  #                                format="%Y-%m-%d %H:%M:%S",tz = "MST")-(60*60)
  
  # Filters data set for required timeframe for simulated data
  #  SampleSimZ <- ZH  %>%
  #    filter(year(date) == year,
  #           #           year(date) <= year2,
  #           Run_ID == case,
  #    ) %>%
  #    subset(., select = c(date, Price, Imports, Exports))
  
  SampleSim <- ResourceHr %>%
    subset(., select = c(date,ID,Name,Output_MWH,Capacity_Factor,Primary_Fuel,
                         Run_ID,Report_Year,#Beg_Date,End_Date,
                         Condition)) %>%
    filter(Report_Year == year,
           #           Report_Year <= year2,
           Output_MWH >= 0,
           Run_ID == case,
    ) %>%
    mutate(Year = as.factor(Report_Year),
           #           Aurora_ID = ID,
           ID = str_extract(Name, "(?<=\\().*(?=\\))"),
           Plant_Type = case_when(str_detect(Primary_Fuel, "Coal")~"COAL",
                                  str_detect(Primary_Fuel, "NaturalGas")~"NGCC",
                                  str_detect(Primary_Fuel, "-Peaking")~"SCGT",
                                  str_detect(Primary_Fuel, "Water")~"HYDRO",
                                  str_detect(Primary_Fuel, "COGEN")~"COGEN",
                                  str_detect(Primary_Fuel, "Wind")~"WIND",
                                  str_detect(Primary_Fuel, "Other")~"OTHER",
                                  str_detect(Primary_Fuel, "Solar")~"SOLAR",
                                  str_detect(Primary_Fuel, "Storage")~"STORAGE",
                                  str_detect(Primary_Fuel, "Trade")~"IMPORT",
                                  str_detect(Primary_Fuel, "100-Hydrogen")~"H2",
                                  str_detect(Primary_Fuel, "Simple Cycle")~"SC_BLEND",
           )) %>%
    group_by(ID,Plant_Type,Year) %>%
    summarize(Cap_Fac = mean(Capacity_Factor)) %>%
    mutate(sit = "Simulation") %>%
    na.omit()
  
  total <- rbind(SampleSim,Act) %>%
    filter(Plant_Type == plant_type) 
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(Year,Cap_Fac,colour=sit,fill=sit),
         alpha=0.8)+
    geom_col(aes(ID,Cap_Fac,colour=sit,fill=sit),
             size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","royalblue"))+
    scale_fill_manual("",values=c("grey50","royalblue"))+
    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       labels = scales::percent,
                       #                       limits = c(-50,100),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Capacity Factor (%)",
         title=paste0("Capacity Factor for ",plant_type," in ",year," (%)"),
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
}

cap_fac_diff <- function(year, plant_type, case) {
  # Plots the difference between the average capture price realized by each plant
  # of a specific generation technology and the mean price for the same time period. 
  
  # Based on a plot designed by Dr. Andrew Leach
  
  # Filters data set for required timeframe
  Act <- sub_samp %>% 
    filter(year(time) == year,
           Plant_Type %in% gen_set,
           Plant_Type == plant_type) %>%
    #    mutate(Year = year(time)) %>%
    group_by(ID,
             #Plant_Type,
             #Year
    ) %>%
    summarize(Act_Cap_Fac = mean(Cap_Fac)) %>%
    mutate(
      #sit = "Actual", 
      #Year = as.factor(Year)
    )#%>%
  #filter(Plant_Type == plant_type) 
  
  #  ResourceHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",
  #                                                        ResourceHr$Time_Period))), 
  #                                format="%Y-%m-%d %H:%M:%S",tz = "MST")-(60*60)
  
  # Filters data set for required timeframe for simulated data
  #  SampleSimZ <- ZH  %>%
  #    filter(year(date) == year,
  #           #           year(date) <= year2,
  #           Run_ID == case,
  #    ) %>%
  #    subset(., select = c(date, Price, Imports, Exports))
  
  SampleSim <- ResourceHr %>%
    subset(., select = c(date,ID,Name,Output_MWH,Capacity_Factor,Primary_Fuel,
                         Run_ID,Report_Year,#Beg_Date,End_Date,
                         Condition)) %>%
    filter(Report_Year == year,
           #           Report_Year <= year2,
           Output_MWH >= 0,
           Run_ID == case,
    ) %>%
    mutate(#Year = as.factor(Report_Year),
      #           Aurora_ID = ID,
      ID = str_extract(Name, "(?<=\\().*(?=\\))"),
      Plant_Type = case_when(str_detect(Primary_Fuel, "Coal")~"COAL",
                             str_detect(Primary_Fuel, "NaturalGas")~"NGCC",
                             str_detect(Primary_Fuel, "-Peaking")~"SCGT",
                             str_detect(Primary_Fuel, "Water")~"HYDRO",
                             str_detect(Primary_Fuel, "COGEN")~"COGEN",
                             str_detect(Primary_Fuel, "Wind")~"WIND",
                             str_detect(Primary_Fuel, "Other")~"OTHER",
                             str_detect(Primary_Fuel, "Solar")~"SOLAR",
                             str_detect(Primary_Fuel, "Storage")~"STORAGE",
                             str_detect(Primary_Fuel, "Trade")~"IMPORT",
                             str_detect(Primary_Fuel, "100-Hydrogen")~"H2",
                             str_detect(Primary_Fuel, "Simple Cycle")~"SC_BLEND",
      )) %>%
    filter(Plant_Type == plant_type)  %>%
    group_by(ID,
             #Plant_Type,
             #Year
    ) %>%
    summarize(Sim_Cap_Fac = mean(Capacity_Factor))%>%
    
    #    mutate(sit = "Simulation") %>%
    na.omit()
  
  total <- merge(Act,SampleSim,by = "ID") %>%
    mutate(CF_diff = Act_Cap_Fac - Sim_Cap_Fac)
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(#Year,
           CF_diff,#colour=sit,fill=sit
         ),
         alpha=0.8)+
    geom_col(aes(ID,CF_diff,#colour=sit,fill=sit
    ),
    size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","royalblue"))+
    scale_fill_manual("",values=c("grey50","royalblue"))+
    #    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       labels = scales::percent,
                       limits = c(-0.2,0.2),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Difference in Capacity Factor between /nActual and Simulation (%)",
         title=paste0("Difference in Capacity Factor for ",plant_type," in ",year," (%)"),
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
}

cap_fac_difference <- function(year1, year2, plant_type, case) {
  # Plots the difference between the average capture price realized by each 
  # generation technology and the mean price for the same time period. 
  
  # Based on a plot designed by Dr. Andrew Leach
  
  # Filters data set for required timeframe
  Act <- sub_samp %>% 
    filter(year(time) >= year1,
           year(time) <= year2,
           Plant_Type %in% gen_set,
           Plant_Type == plant_type) %>%
    mutate(Year = year(time)) %>%
    group_by(ID,
             #Plant_Type,
             Year
    ) %>%
    summarize(Act_Cap_Fac = mean(Cap_Fac)) %>%
    mutate(
      #sit = "Actual", 
      Year = as.factor(Year)
    )
  
  Sim <- ResourceHr %>%
    subset(., select = c(date,ID,Name,Output_MWH,Capacity_Factor,Primary_Fuel,
                         Run_ID,Report_Year,#Beg_Date,End_Date,
                         Condition)) %>%
    filter(Report_Year >= year1,
           Report_Year <= year2,
           Output_MWH >= 0,
           Run_ID == case,
    ) %>%
    mutate(Year = as.factor(Report_Year),
           ID = str_extract(Name, "(?<=\\().*(?=\\))"),
           Plant_Type = case_when(str_detect(Primary_Fuel, "Coal")~"COAL",
                                  str_detect(Primary_Fuel, "NaturalGas")~"NGCC",
                                  str_detect(Primary_Fuel, "-Peaking")~"SCGT",
                                  str_detect(Primary_Fuel, "Water")~"HYDRO",
                                  str_detect(Primary_Fuel, "COGEN")~"COGEN",
                                  str_detect(Primary_Fuel, "Wind")~"WIND",
                                  str_detect(Primary_Fuel, "Other")~"OTHER",
                                  str_detect(Primary_Fuel, "Solar")~"SOLAR",
                                  str_detect(Primary_Fuel, "Storage")~"STORAGE",
                                  str_detect(Primary_Fuel, "Trade")~"IMPORT",
                                  str_detect(Primary_Fuel, "100-Hydrogen")~"H2",
                                  str_detect(Primary_Fuel, "Simple Cycle")~"SC_BLEND",
           )) %>%
    filter(Plant_Type == plant_type)  %>%
    group_by(ID,
             #Plant_Type,
             Year
    ) %>%
    summarize(Sim_Cap_Fac = mean(Capacity_Factor))%>%
    
    #    mutate(sit = "Simulation") %>%
    na.omit()
  
  total <- merge(Act,Sim,by = c("ID","Year")) %>%
    mutate(CF_diff = Act_Cap_Fac - Sim_Cap_Fac)
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(#Year,
           CF_diff,#colour=sit,fill=sit
         ),
         alpha=0.8)+
    geom_col(aes(ID,CF_diff,colour=Year,fill=Year
    ),
    size=1.5,position = position_dodge(width = 0.75),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=AESO_colours)+
    scale_fill_manual("",values=AESO_colours)+
    #    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       labels = scales::percent,
                       limits = c(-0.2,0.2),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Difference in Capacity Factor between Actual and Simulation (%)",
         title=paste0("Difference in Capacity Factor for ",plant_type," in ",
                      year1,"-",year2," (%)"),
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
}

################################################################################
# Plot charts shown in AESO 2021 Market Report
################################################################################

AESO_colours <- c("goldenrod1", "gray60", "yellowgreen", "cornflowerblue",
                  "#001933")

year_pool <- function(year1, year2,case) {
  # A function to plot the Monthly average pool price 
  # (Like in the AESO Market Report 2021 Figure 1)

  # Filter and prepare Simulation data
  Sim <- ZoneH %>%
    filter(
           Run_ID == case,
           Condition != "Average") %>%
    group_by(Report_Year,Report_Month) %>%
    summarise(
      Price = mean(Price)
      ) %>%
    mutate(Date = as.Date(paste(Report_Year,Report_Month,"01"), "%Y %m %d"),
           type = "MonAve",
           case = "Simulation"
           ) 
  
  # Calculate rolling average
  SimMA <- Sim %>%
    mutate(Price = 
             rollapplyr(Price, width = 12, FUN = mean, partial = TRUE),
           type = "RollAve")
  
  # Combine sim data with rolling average
  Sim <- rbind(Sim,SimMA) %>%
    filter(Report_Year >= year1 &
             Report_Year <= year2) %>%
    subset(.,select = c(Date,
                        Price,
                        case,
                        type
    ))
  
  # Prepare AESO data by creating Year and Month columns
  Actual <- na.omit(demand)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Month <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%m")

  # Filter and prepare AESO data
  AESO <- Actual %>%
    group_by(Year,Month) %>%
    summarise(
      Price = mean(Price),
              ) %>%
    ungroup() %>%
    mutate(Date = as.Date(paste(Year,Month,"01"), "%Y %m %d"), 
           type = "MonAve",
           case = "AESO",
           )
  
  # Calculate rolling average
  ActMA <- AESO %>%
    mutate(Price = 
             rollapplyr(Price, width = 12, FUN = mean, partial = TRUE),
           type = "RollAve")
  
  # Combine AESO data with rolling average
  AESO <- rbind(AESO,ActMA) %>%
    filter(Year >= year1 &
             Year <= year2) %>%
    subset(.,select = c(Date,
                        Price,
                        case,
                        type
                        ))
  
  # Combine simulation and AESO data
  total <- rbind(Sim,AESO)
  
  # Set font size for plot
  sz <- 15
  
  ggplot() +
    geom_line(data = total,
              aes(x = Date, y = Price, linetype = case, colour = type), 
              size = 1) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Average Pool Price \n($/MWh)", 
         title = "AESO Data vs Simulation Monthly Average Pool Price",
         subtitle = DB) +
    scale_linetype_discrete(labels = c("Actual","Simulated"))+
    scale_color_manual(values = c("midnightblue", "orange"),
                       labels = c("Monthly Ave","12-Month Rolling")) +
    scale_x_date(date_labels = "%b-%Y",
      expand=c(0,0), 
      date_breaks = "3 months"
      ) +
    scale_y_continuous(expand=c(0,0),
                       n.breaks = 3
    )
}

seas_price <- function(year1, year2, case) {
  # Plots the Pool Price duration vs percentile for AESO and Sim for a specific season
  # Like AESO Market Report 2021 Figures 2 and 3
  
  # Load and filter Simulation data, 
  # Calculate the percentage of time
  # Create column 'sit' to indicate Simulation
  
  WS1 <- as.Date(paste0(year1,"-1-01"), format = "%Y-%m-%d") # Winter Solstice
  SE1 <- as.Date(paste0(year1,"-3-15"),  format = "%Y-%m-%d") # Spring Equinox
  SS1 <- as.Date(paste0(year1,"-6-15"),  format = "%Y-%m-%d") # Summer Solstice
  FE1 <- as.Date(paste0(year1,"-9-15"),  format = "%Y-%m-%d") # Fall Equinox  
  
  
  WS2 <- as.Date(paste0(year2-1,"-12-15"), format = "%Y-%m-%d") # Winter Solstice
  SE2 <- as.Date(paste0(year2,"-3-15"),  format = "%Y-%m-%d") # Spring Equinox
  SS2 <- as.Date(paste0(year2,"-6-15"),  format = "%Y-%m-%d") # Summer Solstice
  FE2 <- as.Date(paste0(year2,"-9-15"),  format = "%Y-%m-%d") # Fall Equinox  
  WS3 <- as.Date(paste0(year2,"-12-15"), format = "%Y-%m-%d") # Winter Solstice
  
  totSim <- ZoneH %>%
    filter(Report_Year >= (year1) & 
             Report_Year <= year2,
           Name == "WECC_Alberta",
           Run_ID == case, 
           Condition != "Average") %>%
    group_by(Condition, Report_Year) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    subset(., select=c(date, Condition, Report_Year, Price, perc)) %>%
    rename(Year = Report_Year) %>%
    ungroup() %>%
    mutate(sit = "Simulated")
  
  # Load and filter AESO data, 
  # Calculate the percentage of time
  # Create column 'sit' to indicate Actual AESO data
  Actual <- na.omit(demand)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Hour <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  
  totAct <- Actual %>%
    filter(Year >= (year1), 
           Year <= year2,) %>%
    mutate(Condition = if_else(between(Hour, 08, 23), 
                               "On-Peak WECC", "Off-Peak WECC")) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    subset(., select=c(time, Condition, Year, Price, perc)) %>%
    ungroup() %>%
    mutate(sit = "Actual") %>%
    rename(date = time)
  
  # Combine Actual and Simulation data
  total <- rbind(totSim,totAct)
  
  WinterTot <- total %>%
    filter(date >= WS1 & date <= SE1 |
             date >= WS2 & date <= SE2) %>%
    mutate(Season = "Winter")
  
  SpringTot <- total %>%
    filter(date >= SE1 & date <= SS1 |
             date >= SE2 & date <= SS2) %>%
    mutate(Season = "Spring")
  
  SummerTot <- total %>%
    filter(date >= SS1 & date <= FE1 |
             date >= SS2 & date <= FE2) %>%
    mutate(Season = "Summer")
  
  FallTot <- total %>%
    filter(date >= FE1 & date <= WS2 |
             date >= FE2 & date <= WS3) %>%
    mutate(Season = "Fall")
  
  total <- rbind(WinterTot, SpringTot, SummerTot, FallTot) #%>%
  #      filter(Season == season)
  
  # Set font size for plot
  sz <- 15
  
  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Price, colour = Year, linetype = sit), size = 1) +
    facet_grid(Condition ~ Season) +#cols = vars(Condition)) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          
          # For transparent background
          panel.grid = element_blank(),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          aspect.ratio = 2/3
    ) +
    labs(y = "Pool Price$/MWh", 
         x = "Percentage of Time", 
         title = "Seasonal AESO Data vs Simulation",
         subtitle = DB) +
    scale_color_manual(values = c("goldenrod1", "forestgreen", "cornflowerblue",
                                  "firebrick","gray60", "orange")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    ) #+
  #coord_fixed(ratio = 10)
}

comp_dur <- function(year1, year2, case) {
  # Plots the Pool Price duration vs percentile for AESO and Sim
  # Like AESO Market Report 2021 Figures 2 and 3
  
  # Load and filter Simulation data, 
    # Calculate the percentage of time
    # Create column 'sit' to indicate Simulation
  totSim <- ZoneH %>%
    filter(Report_Year >= (year1-2) & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    group_by(Condition, Report_Year) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    subset(., select=c(Condition, Report_Year, Price, perc)) %>%
    rename(Year = Report_Year) %>%
    ungroup() %>%
    mutate(sit = "Simulated")
  
  # Load and filter AESO data, 
    # Calculate the percentage of time
    # Create column 'sit' to indicate Actual AESO data
  Actual <- na.omit(demand)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Hour <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  
  totAct <- Actual %>%
    filter(Year >= (year1-3), 
           Year <= year2,) %>%
    mutate(Condition = if_else(between(Hour, 08, 23), 
                               "On-Peak WECC", "Off-Peak WECC")) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    subset(., select=c(Condition, Year, Price, perc)) %>%
    ungroup() %>%
    mutate(sit = "Actual")
  
  # Combine Actual and Simulation data
  total <- rbind(totSim, totAct)
  
  # Set font size for plot
  sz <- 15
  
  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Price, colour = Year, linetype = sit), size = 1) +
    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          
          # For transparent background
          panel.grid = element_blank(),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Pool Price ($/MWh)", 
         x = "Percentage of Time", 
         #title = "AESO Data vs Simulation",
         #subtitle = DB
         ) +
    scale_color_manual(values = c("gray60", "forestgreen", "cornflowerblue",
                                  "goldenrod1", "firebrick","orange")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    ) +
    scale_linetype_manual(values = c("dotted","solid"))
}

load_dur <- function(year1, year2, case) {
  # Plots the load duration vs percentile for AESO and Sim
  # Like AESO Market Report 2021 Figures 7 and 8
  
  # Load and filter Simulation data, 
    # Calculate the percentage of time
    # Create column 'sit' to indicate Simulation
  totSim <- ZoneH %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    group_by(Condition, Report_Year) %>%
    mutate(perc = 1-ecdf(Demand)(Demand)) %>%
    dplyr::select(Condition, Report_Year, Demand, perc) %>%
    rename(Year = Report_Year) %>%
    ungroup() %>%
    mutate(sit = "Simulated")
  
  # Load and filter AESO data, 
    # Calculate the percentage of time
    # Use AIL as demand
    # Create column 'sit' to indicate Actual AESO data
  Actual <- na.omit(demand)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Hour <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  
  totAct <- Actual %>%
    filter(Year >= year1, 
           Year <= year2,) %>%
    mutate(Condition = if_else(between(Hour, 08, 23), 
                               "On-Peak WECC", "Off-Peak WECC")) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(AIL)(AIL)) %>%
    dplyr::select(Condition, Year, AIL, perc) %>%
    ungroup() %>%
    mutate(sit = "Actual", Demand = AIL) %>%
    dplyr::select(Condition, Year, Demand, perc,sit)
  
  # Combine Actual and Simulation data
  total <- rbind(totSim, totAct)
  
  # Set font size for plot
  sz <- 15
  
  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Demand, colour = Year, linetype = sit), size = 1.25) +
    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Hourly Alberta Internal Load (MW)", 
         x = "Percentage of Time", 
         #title = "AESO Data vs Simulation",
         #subtitle = DB
         ) +
    scale_color_manual(values = c("gray60", "forestgreen", "cornflowerblue",
                                  "goldenrod1", "firebrick")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    ) +
    scale_linetype_manual(values = c("dotted","solid"))
}

tot_cap <- function(year1, year2, case) {
  # Plots the year-end capacity by technology for AESO and Sim
  # Like AESO Market Report 2021 Figure 11
  
  Act <- sub_samp %>%
    filter(! NRG_Stream %in% trade_excl,
           year(time) >= year1,
           year(time) <= year2,
           month(time) == 12,
           day(time) == 31,
           hour(time) == 23,
           #           Plant_Type != "STORAGE"
    ) %>%
    group_by(time, Plant_Type) %>%
    summarise(Cap = sum(Capacity)) %>%
    #    subset(., select=c(time, Cap, perc)) %>%
    mutate(sit = "Actual", Year = as.factor(year(time))) %>%
    subset(., select=c(Year, Plant_Type, Cap, sit))
  
  Act <- na.omit(Act)
  
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "HYDRO",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "WIND",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "SOLAR",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "OTHER",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "STORAGE",after=Inf)
  
  Sim <- Hr %>%
    filter(Run_ID == case,
           Report_Year >= year1,
           Report_Year <= year2,
           Report_Month == 12,
           Report_Day == 31,
           Report_Hour == 24,
           ID == "LTO_Coal" | ID == "AB_NGCONV" | ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | 
             ID == "AB_SCCT_noncogen" | ID == "LTO_Hydro" | ID == "LTO_Other" | 
             ID == "LTO_Wind" | ID == "LTO_Solar") %>%
    group_by(Report_Year, ID) %>%
    subset(., select=c(Report_Year, ID, Capacity)) %>%
    #    summarise(Cap = mean(Capacity_Factor)) %>%
    mutate(sit = "Simulation")
  
  colnames(Sim) <- c("Year", "Plant_Type", "Cap", "sit")
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("LTO_Coal", "AB_NGCONV", "AB_CCCT_noncogen", "LTO_Cogen",
                                                    "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                                    "LTO_Wind", "LTO_Solar"))
  levels(Sim$Plant_Type) <- c("COAL", "NGCONV", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                              "WIND", "SOLAR")
  
  Sim$Year <- as.factor(Sim$Year)
  
  total <- rbind(Sim,Act)
  
  sz <- 15
  
  ggplot() +
    geom_col(data = total, position = "dodge", alpha = 0.8, width = 0.7,
             aes(x = Plant_Type, y = Cap, fill = sit, linetype = sit)) +
    facet_grid(~Year) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Installed Generation Capacity (MW)", 
         #title = "Year-end generation capacity AESO Data vs Simulation",
         #subtitle = DB
         ) +
    scale_fill_manual(values = c("grey50","black")) +
    #    scale_x_continuous(expand=c(0,0), 
    #                       limits = c(0,1.1),
    #                       labels = percent) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,6000),
                       #                       breaks = seq(0,1, by = 0.2)
    )
}

tech_cap <- function(year1, year2, case) {
  # Plots the capacity factor by technology for AESO and Sim
  # Like AESO Market Report 2021 Figure 15
  
  Act <- df1 %>%
    filter(Plant_Type != "STORAGE") %>%
    group_by(Year, Plant_Type) %>%
    summarise(Cap = mean(meancap)) %>%
    mutate(sit = "Actual")
  
  Act$Year <- as.numeric(as.character(Act$Year))
  
  Act <- na.omit(Act)
  
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "HYDRO",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "WIND",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "SOLAR",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "OTHER",after=Inf)
#  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "STORAGE",after=Inf)
  
  Sim <- Hour %>%
    filter(Run_ID == case,
           ID == "LTO_Coal" | ID == "AB_NGCONV" | ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | 
           ID == "AB_SCCT_noncogen" | ID == "LTO_Hydro" | ID == "LTO_Other" | 
           ID == "LTO_Wind" | ID == "LTO_Solar") %>%
    group_by(Report_Year, ID) %>%
    summarise(Cap = mean(Capacity_Factor)) %>%
    mutate(sit = "Simulation")
  
  colnames(Sim) <- c("Year", "Plant_Type", "Cap", "sit")

  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("LTO_Coal", "AB_NGCONV", "AB_CCCT_noncogen", "LTO_Cogen",
                                      "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                      "LTO_Wind", "LTO_Solar"))
  levels(Sim$Plant_Type) <- c("COAL", "NGCONV", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                       "WIND", "SOLAR")
  
  total <- rbind(Sim,Act)
  
  total <- total %>%
    filter(Year >= year1,
           Year <= year2)
  
  sz <- 15
  
  ggplot() +
    geom_col(data = total, position = "dodge", alpha = 0.8, width = 0.7,
              aes(x = Plant_Type, y = Cap, fill = sit, linetype = sit)) +
    facet_grid(~Year) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Capacity Factor", 
         #title = "AESO Data vs Simulation",
         #subtitle = DB
         ) +
    scale_fill_manual(values = c("grey50","black"
#      "goldenrod1", "forestgreen"
#                                 "darkseagreen", "cornflowerblue",
#                                  "firebrick","gray60", "forestgreen"
                                 )) +
#    scale_x_continuous(expand=c(0,0), 
#                       limits = c(0,1.1),
#                       labels = percent) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1),
                       breaks = seq(0,1, by = 0.2)
    )
}

ach_pool <- function(year1, year2, case) {
  # Plots the achieved premium-to-pool price realized by each generation 
  # technology. 
  #
  #The ratio of the achieved margin to the average pool price
  # Achieved price represents the average price realized in the wholesale energy 
  #
  # market for electricity delivered to the grid and is calculated as the 
  # weighted average of the hourly pool price, where the price in each settlement 
  # interval is weighted by the net-to-grid generation in that interval.
  #
  # Like AESO Market Report 2021 Figure 18
  
  # Filters data set for required timeframe
  Sample <- sub_samp %>%
    filter(! NRG_Stream %in% trade_excl,
           year(time) >= year1,
           year(time) <= year2,
           Plant_Type == "COAL" | Plant_Type == "COGEN" | Plant_Type == "NGCC" |
             Plant_Type == "SCGT" | Plant_Type == "HYDRO" | Plant_Type == "OTHER" |
             Plant_Type == "WIND" | Plant_Type == "SOLAR" | Plant_Type == "STORAGE" |
             Plant_Type == "EXPORT" | Plant_Type == "IMPORT"
    )
  
  # This section calculates the annual average pool prices
  Avg <- Sample %>%
    group_by(time) %>%
    summarise(Price = median(Price)) %>%
    ungroup() %>%
    mutate(Year = as.factor(year(time))) %>%
    group_by(Year) %>%
    summarise(Pool = mean(Price))
  
  # This section calculates the achieved prices
  Ach <- Sample %>%
    group_by(time, Plant_Type) %>%
    summarise(Price = median(Price), gen = sum(gen)) %>%
    mutate(WeighPrice = Price*gen) %>%
    ungroup() %>%
    mutate(Year = as.factor(year(time))) %>%
    group_by(Year, Plant_Type) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(gen)) %>%
    mutate(AchPrice = WeighPrice/gen)
  
  # Combine the two
  Act <- merge(Avg, Ach, by = "Year")
  
  # Calculate the achieved margin and the achieved premium-to-pool price
  Act <- Act %>%
    mutate(Margin = AchPrice-Pool, Ratio = Margin/Pool, sit = "Actual")
  
  # Reorder the factors for plotting
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "HYDRO",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "WIND",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "SOLAR",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "STORAGE",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "OTHER",after=Inf)
  
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "EXPORT",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "IMPORT",after=Inf)
  
  # Filters data set for required timeframe for simulated data
  SampleSimZ <- ZH  %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Run_ID == case,
    ) %>%
    subset(., select = c(date, Price, Imports, Exports))
  
  SampleSim <- Hr %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Run_ID == case,
           ID == "LTO_Coal" | ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | 
             ID == "AB_SCCT_noncogen" | ID == "LTO_Hydro" | ID == "LTO_Other" | 
             ID == "LTO_Wind" | ID == "LTO_Solar" | ID == "LTO_Storage"
    ) %>%
    subset(., select = c(date, ID, Output))
  
  # Combine the two
  SampleSim <- merge(SampleSimZ, SampleSim, by = "date")
  SampleSim <- SampleSim %>%
    mutate(Year = as.factor(year(date)))
  
  # This section calculates the annual average pool prices for simulated data
  AvgSim <- SampleSim %>%
    group_by(Year) %>%
    summarise(Pool = mean(Price))
  
  # This section calculates the achieved prices for imports and exports
  Imp <- SampleSimZ %>%
    mutate(WeighPrice = Price*Imports, Year = as.factor(year(date))) %>%
    group_by(Year) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(Imports)) %>%
    mutate(AchPrice = WeighPrice/gen, Plant_Type = "IMPORT")
  
  Exp <- SampleSimZ %>%
    mutate(WeighPrice = Price*Exports, Year = as.factor(year(date))) %>%
    group_by(Year) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(Exports)) %>%
    mutate(AchPrice = WeighPrice/gen, Plant_Type = "EXPORT")
  
  # This section calculates the achieved prices
  AchSim <- SampleSim %>%
    mutate(WeighPrice = Price*Output, Plant_Type = ID) %>%
    #    ungroup() %>%
    #    mutate(Year = as.factor(year(time))) %>%
    group_by(Year, Plant_Type) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(Output)) %>%
    mutate(AchPrice = WeighPrice/gen)
  
  AchSim <- rbind(AchSim, Imp, Exp)
  
  # Combine the two
  Sim <- merge(AvgSim, AchSim, by = "Year")
  
  # Calculate the achieved margin and the achieved premium-to-pool price
  Sim <- Sim %>%
    mutate(Margin = AchPrice-Pool, Ratio = Margin/Pool, sit = "Simulation")
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("LTO_Coal", "AB_CCCT_noncogen", "LTO_Cogen",
                                                    "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                                    "LTO_Wind", "LTO_Solar", "LTO_Storage", "EXPORT", "IMPORT"))
  
  levels(Sim$Plant_Type) <- c("COAL", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                              "WIND", "SOLAR", "STORAGE", "EXPORT", "IMPORT")
  
  #Sim$Year <- as.factor(Sim$Year)
  
  total <- rbind(Sim,Act)
  
  sz <- 12
  
  # Plot the data
  ggplot() +
    geom_col(data = total, position = "dodge", alpha = 0.8, width = 0.7,
             aes(x = Plant_Type, y = Ratio, fill = sit, linetype = sit)) +
    #    geom_text(total, aes(label = gen), vjust = -0.5, angle = 90) +
    facet_grid(~Year) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Achieved Premium to Pool Price", 
         title = "Annual achieved premium to pool price \nAESO Data vs Simulation",
         subtitle = DB) +
    scale_fill_manual(values = AESO_colours) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(-0.5,1.1),
                       breaks = c(-0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 0.7, 0.9, 1.1),
                       labels = percent
    )
}

margin <- function(year1, year2, case) {
  # Plots the marginal price-setting technology for AESO and Sim
  # Like AESO Market Report 2021 Figure 19
  
#  MargSim <- Year %>%
#    filter(Run_ID == case,
#           Condition == "Average",
#           ID == "LTO_Coal" | ID == "AB_NGCONV" | ID == "AB_SCCT_noncogen" |
#           ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | ID == "LTO_Other" |
#           ID == "LTO_Hydro" | ID == "LTO_Solar" | ID == "LTO_Storage" | 
#           ID == "LTO_Wind" | ID == "Intertie") %>%
#    subset(.,select=c(ID,Report_Year,Condition,Percent_Marginal))
#    filter(Condition )
    
  cogen <- c("ALS1", "APS1", "BCR2", "BCRK", "BFD1", "CL01", "CNR5", "COD1", 
             "DOWG", "EC04", "FH1", "HMT1", "HRT1", "IOR1", "IOR2", "JOF1",
             "MEG1", "MKR1", "MKRC", "MUL1", "NX02", "PR1", "PW01", "RL1", 
             "SCL1", "SCR1", "SCR5", "SCR6", "SDH1", "TC01", "TC02", "TLM2", 
             "UOC1", "UOA1", "IOR3", "IOR4", "SHCG", "PEC1", "CRG1")
  
  ngcc <- c("CAL1", "CMH1", "EC01", "EGC1", "FNG1", "NX01", "Cascade")
  
  scgt <- c("ALP1", "ALP2", "ANC1", "BHL1", "CRS1", "CRS2", "CRS3", "DRW1", 
            "ENC1", "ENC2", "ENC3", "GEN5", "GEN6", "HSM1", "ME02", "ME03", 
            "ME04", "MFG1", "NAT1", "NPC1", "NPC2", "NPC3", "NPP1", "PH1", "PMB1",
            "RB5", "SET1",  "VVW1", "VVW2", "WCD1"
            )
  
  coal <- c("BR3", "BR4", "BR5", "GN1", "GN2", "GN3", "HRM", "KH1", "KH2", 
            "KH3", "SD2", "SD3", "SD4", "SD5", "SD6", "SH1", "SH2")
  
  hydro <- c("BIG", "BOW1", "BRA", "CHIN", "DKSN", "ICP1", "OMRH", "RYMD", "TAY1")
  
  ngconv <- c("Retrofit")
  
  other <- c("AFG1", "BON1", "CCMH", "DAI1", "DV1", "EAGL", "GPEC", "GOC1", "NRG3", 
             "SLP1", "SRL1", "WEY1", "WST1", "WWD1")
  
  solar <- c("BSC1", "HUL1", "INF1", "VXH1", "BRD1", "BUR1", "CLR1", "CLR2", 
             "SUF1", "WEF1", "JER1", "HYS1", "BRK1", "BRK2", "COL1", "CRD2", 
             "CRD1", "MON1", "NMK1", "STR1", "STR2", "TVS1", "EPS1", "VCN1", 
             "KKP1", "KKP2", "MIC1", "CLY1", "CLY2", "TRH1", "Tilley", "Coulee",
             "Enchant", "Stavely")
  
  wind <- c("CRE3", "CR1", "AKE1", "IEW1", "KHW1", "SCR2", "TAB1", "GWW1", "SCR3", 
            "BTR1", "OWF1", "IEW2", "SCR4", "CRR2", "NEP1", "HAL1", "BUL1", 
            "BUL2", "BSR1", "CRR1", "RIV1", "WHT1", "ARD1", "WRW1", "WHT2", 
            "RTL1", "WHE1", "FMG1", "JNR1", "JNR2", "JNR3", "HHW1", "Garden", 
            "Cypress", "Buffalo", "Grizzly", "Lanfine")
  
  storage <- c("*CRS", "*GN1&2", "ERV1", "ERV2", "ERV3", "SUM1")
  
  Sim <- ZoneH %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    mutate(Name = Marginal_Resource,
           Year = Report_Year,
#           Plant_Type = "Group"
           ) %>%
    subset(.,select=c(date,Name,Price,Year)) %>%
    mutate(Plant_Type = case_when(
      grepl(paste(cogen,collapse="|"), x=Name) ~ "COGEN",
      grepl(paste(ngcc,collapse="|"), x=Name) ~ "NGCC",
      grepl(paste(scgt,collapse="|"), x=Name) ~ "SCGT",
      grepl(paste(coal,collapse="|"), x=Name) ~ "COAL",
      grepl(paste(hydro,collapse="|"), x=Name) ~ "HYDRO",
      grepl(paste(ngconv,collapse="|"), x=Name) ~ "NGCONV",
      grepl(paste(other,collapse="|"), x=Name) ~ "OTHER",
      grepl(paste(solar,collapse="|"), x=Name) ~ "SOLAR",
      grepl(paste(wind,collapse="|"), x=Name) ~ "WIND",
      grepl(paste(storage,collapse="|"), x=Name) ~ "STORAGE",
      grepl("Intertie", x=Name) ~ "INTERTIE"
    )) %>%
    group_by(Plant_Type,Year) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    group_by(Year) %>%
    mutate(tot = sum(freq),
           perc = freq/tot*100,
           sit = "Simulation") %>%
    ungroup() 
    
  Sim <- na.omit(Sim)  
   
#  totHour <- RHour %>%
#    filter(Report_Year >= year1 & 
#             Report_Year <= year2,
#           Percent_Marginal == 100,
#           Run_ID == case) %>%
#    subset(.,select=c(date,Name,Dispatch_Cost,Incr_Cost,Primary_Fuel,Percent_Marginal,Zone))
  
#  Sim <- Hr %>%
#    filter(Report_Year >= year1 & 
#             Report_Year <= year2,
#           Run_ID == case, 
#           Name != "Alberta",
#           Percent_Marginal == 100) %>%
#    subset(.,select=c(Name,Time_Period,ID,date))
  
#  SimComb <- merge(Sim,totZone, by=c("date",))

#  data1 <- left_join(totZone, totHour, by=c("date","Name")) 
  
#  data1 <- data1 %>%
#    group_by(Name, Report_Year) %>%
#    mutate(perc = 1-ecdf(Price)(Price)) #%>%
  
  Sample <- merit_filt %>%
    filter(year >= year1,
           year <= year2) %>%
    subset(.,select=c(date,year,he,asset_id,AESO_Name,Plant_Type,price,
                      actual_posted_pool_price,merit,dispatched_mw)) %>%
    mutate(Year = year)
  
  Act <- Sample %>%
    filter(dispatched_mw != 0) %>%
    group_by(date, he) %>%
    slice_max(n=1,merit) %>%
    ungroup() %>%
    group_by(Year,Plant_Type) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    group_by(Year) %>%
    mutate(tot = sum(freq), 
           perc = freq/tot*100,
           sit = "Actual")
  
  total <- rbind(Sim,Act)
  
  # Reorder the factors for plotting
  total$Plant_Type<-fct_relevel(total$Plant_Type, "NGCC",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "SCGT",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "COGEN",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "INTERTIE",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "NGCONV",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "STORAGE",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "HYDRO",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "OTHER",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "WIND",after=Inf)
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(Plant_Type,perc,colour=sit,fill=sit),
         alpha=0.8)+
    geom_col(aes(Plant_Type,perc,colour=sit,fill=sit),
             size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","royalblue"))+
    scale_fill_manual("",values=c("grey50","royalblue"))+
    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       #                       limits = c(-50,100),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Percentage of Time",
         title="Annual marginal price-setting technology",
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
  
}

margin_season <- function(year1, year2, case) {
  # Plots the marginal price-setting technology for AESO and Sim
  # Like AESO Market Report 2021 Figure 19
  
  #  MargSim <- Year %>%
  #    filter(Run_ID == case,
  #           Condition == "Average",
  #           ID == "LTO_Coal" | ID == "AB_NGCONV" | ID ==  "AB_SCCT_noncogen" |
  #           ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | ID == "LTO_Other" |
  #           ID == "LTO_Hydro" | ID == "LTO_Solar" | ID == "LTO_Storage" | 
  #           ID == "LTO_Wind" | ID == "Intertie") %>%
  #    subset(.,select=c(ID,Report_Year,Condition,Percent_Marginal))
  #    filter(Condition )
  
  cogen <- c("ALS1", "APS1", "BCR2", "BCRK", "BFD1", "CL01", "CNR5", "COD1", 
             "DOWG", "EC04", "FH1", "HMT1", "HRT1", "IOR1", "IOR2", "JOF1",
             "MEG1", "MKR1", "MKRC", "MUL1", "NX02", "PR1", "PW01", "RL1", 
             "SCL1", "SCR1", "SCR5", "SCR6", "SDH1", "TC01", "TC02", "TLM2", 
             "UOC1", "UOA1", "IOR3", "IOR4", "SHCG", "PEC1", "CRG1")
  
  ngcc <- c("CAL1", "CMH1", "EC01", "EGC1", "FNG1", "NX01", "Cascade")
  
  scgt <- c("ALP1", "ALP2", "ANC1", "BHL1", "CRS1", "CRS2", "CRS3", "DRW1", 
            "ENC1", "ENC2", "ENC3", "GEN5", "GEN6", "HSM1", "ME02", "ME03", 
            "ME04", "MFG1", "NAT1", "NPC1", "NPC2", "NPC3", "NPP1", "PH1", "PMB1",
            "RB5", "SET1",  "VVW1", "VVW2", "WCD1"
  )
  
  coal <- c("BR3", "BR4", "BR5", "GN1", "GN2", "GN3", "HRM", "KH1", "KH2", 
            "KH3", "SD2", "SD3", "SD4", "SD5", "SD6", "SH1", "SH2")
  
  hydro <- c("BIG", "BOW1", "BRA", "CHIN", "DKSN", "ICP1", "OMRH", "RYMD", "TAY1")
  
  ngconv <- c("Retrofit")
  
  other <- c("AFG1", "BON1", "CCMH", "DAI1", "DV1", "EAGL", "GPEC", "GOC1", "NRG3", 
             "SLP1", "SRL1", "WEY1", "WST1", "WWD1")
  
  solar <- c("BSC1", "HUL1", "INF1", "VXH1", "BRD1", "BUR1", "CLR1", "CLR2", 
             "SUF1", "WEF1", "JER1", "HYS1", "BRK1", "BRK2", "COL1", "CRD2", 
             "CRD1", "MON1", "NMK1", "STR1", "STR2", "TVS1", "EPS1", "VCN1", 
             "KKP1", "KKP2", "MIC1", "CLY1", "CLY2", "TRH1", "Tilley", "Coulee",
             "Enchant", "Stavely")
  
  wind <- c("CRE3", "CR1", "AKE1", "IEW1", "KHW1", "SCR2", "TAB1", "GWW1", "SCR3", 
            "BTR1", "OWF1", "IEW2", "SCR4", "CRR2", "NEP1", "HAL1", "BUL1", 
            "BUL2", "BSR1", "CRR1", "RIV1", "WHT1", "ARD1", "WRW1", "WHT2", 
            "RTL1", "WHE1", "FMG1", "JNR1", "JNR2", "JNR3", "HHW1", "Garden", 
            "Cypress", "Buffalo", "Grizzly", "Lanfine")
  
  storage <- c("*CRS", "*GN1&2", "ERV1", "ERV2", "ERV3", "SUM1")
  
  Sim <- ZoneH %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    mutate(Name = Marginal_Resource,
           Year = Report_Year,
           #           Plant_Type = "Group"
    ) %>%
    subset(.,select=c(date,Name,Price,Year)) %>%
    mutate(Plant_Type = case_when(
      grepl(paste(cogen,collapse="|"), x=Name) ~ "COGEN",
      grepl(paste(ngcc,collapse="|"), x=Name) ~ "NGCC",
      grepl(paste(scgt,collapse="|"), x=Name) ~ "SCGT",
      grepl(paste(coal,collapse="|"), x=Name) ~ "COAL",
      grepl(paste(hydro,collapse="|"), x=Name) ~ "HYDRO",
      grepl(paste(ngconv,collapse="|"), x=Name) ~ "NGCONV",
      grepl(paste(other,collapse="|"), x=Name) ~ "OTHER",
      grepl(paste(solar,collapse="|"), x=Name) ~ "SOLAR",
      grepl(paste(wind,collapse="|"), x=Name) ~ "WIND",
      grepl(paste(storage,collapse="|"), x=Name) ~ "STORAGE",
      grepl("Intertie", x=Name) ~ "INTERTIE"
    )) %>%
    group_by(Plant_Type,Year) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    group_by(Year) %>%
    mutate(tot = sum(freq),
           perc = freq/tot*100,
           sit = "Simulation") %>%
    ungroup() 
  
  Sim <- na.omit(Sim)  
  
  #  totHour <- RHour %>%
  #    filter(Report_Year >= year1 & 
  #             Report_Year <= year2,
  #           Percent_Marginal == 100,
  #           Run_ID == case) %>%
  #    subset(.,select=c(date,Name,Dispatch_Cost,Incr_Cost,Primary_Fuel,Percent_Marginal,Zone))
  
  #  Sim <- Hr %>%
  #    filter(Report_Year >= year1 & 
  #             Report_Year <= year2,
  #           Run_ID == case, 
  #           Name != "Alberta",
  #           Percent_Marginal == 100) %>%
  #    subset(.,select=c(Name,Time_Period,ID,date))
  
  #  SimComb <- merge(Sim,totZone, by=c("date",))
  
  #  data1 <- left_join(totZone, totHour, by=c("date","Name")) 
  
  #  data1 <- data1 %>%
  #    group_by(Name, Report_Year) %>%
  #    mutate(perc = 1-ecdf(Price)(Price)) #%>%
  
  Sample <- merit_filt %>%
    filter(year >= year1,
           year <= year2) %>%
    subset(.,select=c(date,year,he,asset_id,AESO_Name,Plant_Type,price,
                      actual_posted_pool_price,merit,dispatched_mw)) %>%
    mutate(Year = year)
  
  Act <- Sample %>%
    filter(dispatched_mw != 0) %>%
    group_by(date, he) %>%
    slice_max(n=1,merit) %>%
    ungroup() %>%
    group_by(Year,Plant_Type) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    group_by(Year) %>%
    mutate(tot = sum(freq), 
           perc = freq/tot*100,
           sit = "Actual")
  
  total <- rbind(Sim,Act)
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(Plant_Type,perc,colour=sit,fill=sit),
         alpha=0.8)+
    geom_col(aes(Plant_Type,perc,colour=sit,fill=sit),
             size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","royalblue"))+
    scale_fill_manual("",values=c("grey50","royalblue"))+
    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       #                       limits = c(-50,100),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Percentage of Time",
         title="Annual marginal price-setting technology",
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
  
}

margin_onoff <- function(year1, year2, case) {
  # Plots the marginal price-setting technology for AESO and Sim
  # Like AESO Market Report 2021 Figure 19
  
  cogen <- c("ALS1", "APS1", "BCR2", "BCRK", "BFD1", "CL01", "CNR5", "COD1", 
             "DOWG", "EC04", "FH1", "HMT1", "HRT1", "IOR1", "IOR2", "JOF1",
             "MEG1", "MKR1", "MKRC", "MUL1", "NX02", "PR1", "PW01", "RL1", 
             "SCL1", "SCR1", "SCR5", "SCR6", "SDH1", "TC01", "TC02", "TLM2", 
             "UOC1", "UOA1", "IOR3", "IOR4", "SHCG", "PEC1", "CRG1")
  
  ngcc <- c("CAL1", "CMH1", "EC01", "EGC1", "FNG1", "NX01", "Cascade")
  
  scgt <- c("ALP1", "ALP2", "ANC1", "BHL1", "CRS1", "CRS2", "CRS3", "DRW1", 
            "ENC1", "ENC2", "ENC3", "GEN5", "GEN6", "HSM1", "ME02", "ME03", 
            "ME04", "MFG1", "NAT1", "NPC1", "NPC2", "NPC3", "NPP1", "PH1", "PMB1",
            "RB5", "SET1",  "VVW1", "VVW2", "WCD1"
  )
  
  coal <- c("BR3", "BR4", "BR5", "GN1", "GN2", "GN3", "HRM", "KH1", "KH2", 
            "KH3", "SD2", "SD3", "SD4", "SD5", "SD6", "SH1", "SH2")
  
  hydro <- c("BIG", "BOW1", "BRA", "CHIN", "DKSN", "ICP1", "OMRH", "RYMD", "TAY1")
  
  ngconv <- c("Retrofit")
  
  other <- c("AFG1", "BON1", "CCMH", "DAI1", "DV1", "EAGL", "GPEC", "GOC1", "NRG3", 
             "SLP1", "SRL1", "WEY1", "WST1", "WWD1")
  
  solar <- c("BSC1", "HUL1", "INF1", "VXH1", "BRD1", "BUR1", "CLR1", "CLR2", 
             "SUF1", "WEF1", "JER1", "HYS1", "BRK1", "BRK2", "COL1", "CRD2", 
             "CRD1", "MON1", "NMK1", "STR1", "STR2", "TVS1", "EPS1", "VCN1", 
             "KKP1", "KKP2", "MIC1", "CLY1", "CLY2", "TRH1", "Tilley", "Coulee",
             "Enchant", "Stavely")
  
  wind <- c("CRE3", "CR1", "AKE1", "IEW1", "KHW1", "SCR2", "TAB1", "GWW1", "SCR3", 
            "BTR1", "OWF1", "IEW2", "SCR4", "CRR2", "NEP1", "HAL1", "BUL1", 
            "BUL2", "BSR1", "CRR1", "RIV1", "WHT1", "ARD1", "WRW1", "WHT2", 
            "RTL1", "WHE1", "FMG1", "JNR1", "JNR2", "JNR3", "HHW1", "Garden", 
            "Cypress", "Buffalo", "Grizzly", "Lanfine")
  
  storage <- c("*CRS", "*GN1&2", "ERV1", "ERV2", "ERV3", "SUM1")
  
  Sim <- ZoneH %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    mutate(Name = Marginal_Resource,
           Year = Report_Year,
           #           Plant_Type = "Group"
    ) %>%
    subset(.,select=c(date,Name,Price,Year,Condition)) %>%
    mutate(Plant_Type = case_when(
      grepl(paste(cogen,collapse="|"), x=Name) ~ "COGEN",
      grepl(paste(ngcc,collapse="|"), x=Name) ~ "NGCC",
      grepl(paste(scgt,collapse="|"), x=Name) ~ "SCGT",
      grepl(paste(coal,collapse="|"), x=Name) ~ "COAL",
      grepl(paste(hydro,collapse="|"), x=Name) ~ "HYDRO",
      grepl(paste(ngconv,collapse="|"), x=Name) ~ "NGCONV",
      grepl(paste(other,collapse="|"), x=Name) ~ "OTHER",
      grepl(paste(solar,collapse="|"), x=Name) ~ "SOLAR",
      grepl(paste(wind,collapse="|"), x=Name) ~ "WIND",
      grepl(paste(storage,collapse="|"), x=Name) ~ "STORAGE",
      grepl("Intertie", x=Name) ~ "INTERTIE"
    )) %>%
    group_by(Plant_Type,Year,Condition) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    group_by(Year,Condition) %>%
    mutate(tot = sum(freq),
           perc = freq/tot*100,
           sit = "Simulation") %>%
    ungroup() 
  
  Sim <- na.omit(Sim)  
  
  Sample <- merit_filt %>%
    filter(year >= year1,
           year <= year2,
           dispatched_mw != 0) %>%
    mutate(Year = year,
           Condition = case_when(
#             he<=7 ~ "Off-Peak WECC",
#             he>=23 ~ "Off-Peak WECC",
             (hour<=06 | hour>=23) ~ "Off-Peak WECC",
#             (he>=08 & he<=22) ~ "On-Peak WECC",
             TRUE ~ "On-Peak WECC"
#             (he<=07 | he>=23) ~ "Off-Peak WECC",
             )
          ) %>%
    subset(.,select=c(date,Year,hour,asset_id,Plant_Type,merit,dispatched_mw,
                      Condition))
  
  Act <- Sample %>%
#    filter(dispatched_mw != 0) %>%
    group_by(date, hour) %>%
    slice_max(n=1,merit) %>%
    ungroup() %>%
    group_by(Year,Plant_Type,Condition) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    group_by(Year,Condition) %>%
    mutate(tot = sum(freq), 
           perc = freq/tot*100,
           sit = "Actual")
  
  total <- rbind(Sim,Act)
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(Plant_Type,perc,colour=sit,fill=sit),
         alpha=0.8)+
    geom_col(aes(Plant_Type,perc,colour=sit,fill=sit),
             size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","royalblue"))+
    scale_fill_manual("",values=c("grey50","royalblue"))+
    facet_grid(Condition~Year) +
    scale_y_continuous(expand=c(0,0),
                       #                       limits = c(-50,100),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Percentage of Time",
         title="Annual marginal price-setting technology",
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
  
}

wind_cap <- function(year1, year2, case) {
  # Plots the annual wind capacity factor duration curve for AESO and Sim
  # Like AESO Market Report 2021 Figure 25
  
  # Load and filter Simulation data, 
  # Calculate the percentage of time
  # Create column 'sit' to indicate Simulation
  Sim <- Hour %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case,
           ID == "LTO_Wind") %>%
    group_by(Report_Year) %>%
    mutate(perc = 1-ecdf(Capacity_Factor)(Capacity_Factor)) %>%
    mutate(sit = "Simulated", Cap_Fac = Capacity_Factor) %>%
    subset(., select=c(Report_Year, Cap_Fac, perc, sit)) %>%
    rename(Year = Report_Year) %>%
    ungroup() 
  
  # Load and filter AESO data, 
  # Calculate the percentage of time
  # Create column 'sit' to indicate Actual AESO data
  Actual <- sub_samp 
#  Actual <- df1
#  Actual$Year <- as.numeric(as.character(Actual$Year))
#  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual <- Actual %>%
  filter(year(time) >= year1,
         year(time) <= year2,
         Plant_Type == "WIND") #%>%
 #   mutate(Cap_Fac = meancap)
  Actual <- na.omit(Actual)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  
#  Act <- Actual %>%
#    group_by(Year,time) %>%
#    mutate(perc = 1-ecdf(Cap_Fac)(Cap_Fac)) %>%
#    subset(., select=c(Year, Cap_Fac, perc)) %>%
#    ungroup() %>%
#    mutate(sit = "Actual")
  
  Act <- Actual %>%
    group_by(Year,time) %>%
    summarise(Cap_Fac = mean(Cap_Fac)) %>%
    mutate(perc = 1-ecdf(Cap_Fac)(Cap_Fac)) %>%
    subset(., select=c(Year, Cap_Fac, perc)) %>%
    ungroup() %>%
    mutate(sit = "Actual")
  
  # Combine Actual and Simulation data
  total <- rbind(Sim, Act)
  
  # Set font size for plot
  sz <- 15
  
  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Cap_Fac, colour = Year, linetype = sit), size = 1) +
#    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          
          # For transparent background
          panel.grid = element_blank(),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Wind Capacity Factor", 
         x = "Percentage of Time", 
         title = "AESO Data vs Simulation",
         subtitle = DB) +
    scale_color_manual(values = AESO_colours) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

################################################################################
# Plot combination of plots
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
  
  sz <- 15
  
  ggarrange(arrangeGrob(plot_grid(week_price(year,month,day,case) + 
                                    labs(title = paste0("Simulated Data for ",year),
                                         subtitle = paste0("(",DB,")")) +
                                    theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank(),
                                          legend.position ="none",
                                          plot.title = element_text(hjust = 0.5, size = sz),
                                          plot.subtitle = element_text(hjust = 0.5, size = sz-2, face="italic")) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = pretty_breaks(4)),
                                  Week1(year,month,day,case)+
                                    theme(legend.position ="none") + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = pretty_breaks(4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5)),
                        
                        plot_grid(wkPrice(year,month,day) + 
                                    labs(title = paste0("AESO Data for ",year),
                                         subtitle = "NRGStream Data") +
                                    theme(axis.title=element_blank(),
                                          axis.text.x=element_blank(),
                                          legend.position ="none",
                                          plot.title = element_text(hjust = 0.5, size = sz),
                                          plot.subtitle = element_text(hjust = 0.5, size = sz-2, face="italic")) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = pretty_breaks(4)),
                                  Week_act(year,month,day)+
                                    theme(legend.position ="none",
                                          axis.title.y=element_blank())+
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = pretty_breaks(4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5)),
                        ncol=2, widths = c(1.05,1)),
            
            legend,
            ncol=2, widths =c(5,1))
}

AESOSim <- function(year1,year2,case) {

  sz <- 16
  
  p.c <- comp_dur(year1,year2,case) +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz-2),
          axis.title.x = element_blank())
  p.y <- year_pool(year1,year2,case) + 
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          
          legend.text = element_text(size = sz-2),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right")
  p.t <- tech_cap(year1,year2,case) + 
    theme(axis.text = element_text(size = sz-2),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz-2),
          
          legend.text = element_text(size = sz-2),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          legend.position = "right")
  
  p.c + p.y + p.t + plot_layout(design = "A
                                B
                                C") &
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent"))
  
#  plot_grid(p.c,p.y,p.t,
#            ncol = 1, align = "v", axis = "l",
#            rel_heights = c(2,1.5,2))
}

Sample_output <- function(year,month,day,case) {
  sz <- 14
  
  pool <- Weekly_price(year,month,day,case) +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_blank(),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz-2),
          axis.title.x = element_blank())
  stacked <- Weekly(year,month,day,case)  +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz-2),
          axis.title.x = element_blank())
  
  pool + stacked + plot_layout(design = "A
                                B",
                               heights = c(1,3)) &
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent"))
}

price_comp <- function(year1,year2,case) {
  
  sz <- 14
  
  p.c <- comp_dur(year1,year2,case) +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz-2),
          axis.title.x = element_blank())
  p.y <- price_interval(year1,year2,case) + 
    scale_fill_manual("",values = c("grey50","royalblue"),
                      labels=c("Actual",
                               "Simulation"))
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          
          legend.text = element_text(size = sz-2),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right")
  p.t <- capture_price(year1,year2,case) +
    scale_y_continuous(expand=c(0,0),
#                       limits = c(-50,100),
#                       breaks = seq(-50,100, by = 50)
    ) +
    theme(axis.text = element_text(size = sz-2),
          axis.title = element_text(size = sz),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = sz-2),
          
          legend.text = element_text(size = sz-2),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption = element_blank(),
          legend.position = "right")
  
  p.c + p.y + p.t + plot_layout(design = "A
                                B
                                C") &
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent"))
  
  #  plot_grid(p.c,p.y,p.t,
  #            ncol = 1, align = "v", axis = "l",
  #            rel_heights = c(2,1.5,2))
}

gen_comp <- function(year1,year2,case) {
  
  sz <- 14
  
  p.c <- tot_intertie(year1,year2,case) +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz-2),
          axis.title.x = element_blank())
  p.y <- tech_cap(year1,year2,case) + 
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          
          legend.text = element_text(size = sz-2),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right")
  p.t <- market_share(year1,year2,case) +
#    scale_y_continuous(expand=c(0,0),
#                       limits = c(-50,200),
#                       breaks = seq(-50,200, by = 50)
#    ) +
    theme(axis.text = element_text(size = sz-2),
          axis.title = element_text(size = sz),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = sz-2),
          
          legend.text = element_text(size = sz-2),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption = element_blank(),
          legend.position = "right")
  
  p.c + p.y + p.t + plot_layout(design = "A
                                B
                                C") &
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent"))
  
  #  plot_grid(p.c,p.y,p.t,
  #            ncol = 1, align = "v", axis = "l",
  #            rel_heights = c(2,1.5,2))
}

week_cycling <- function(year,month,day,case) {
  
  sz <- 15
  
  Sim <- Week_line(year,month,day,case)  + 
    labs(title = paste0("Simulated Data for ",year),
         subtitle = paste0("(",DB,")")) +
    theme(axis.title.x=element_blank(),
          legend.position ="none",
          plot.title = element_text(hjust = 0.5, size = sz),
          plot.subtitle = element_text(hjust = 0.5, size = sz-2, face="italic"),
          aspect.ratio = 5/2)
  Act <- Week_aline(year,month,day) + 
    labs(title = paste0("AESO Data for ",year),
         subtitle = "NRGStream Data") +
    theme(axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5, size = sz),
          plot.subtitle = element_text(hjust = 0.5, size = sz-2, face="italic"),
          aspect.ratio = 5/2)
  
  MX <- plyr::round_any(
    max(layer_scales(Sim)$y$range$range,layer_scales(Act)$y$range$range),
    100, f = ceiling)
  MN <- plyr::round_any(
    min(layer_scales(Sim)$y$range$range,layer_scales(Act)$y$range$range),
    100, f = floor)
  
  Simulation <- Sim + scale_y_continuous(expand=c(0,0), limits = c(MN,MX), 
                                         breaks = pretty_breaks(4))
  
  Actual <- Act + scale_y_continuous(expand=c(0,0), limits = c(MN,MX), 
                                     breaks = pretty_breaks(4))
  
  Simulation + Actual + plot_layout(ncol = 2, widths = c(1,1.1), guides = 'collect')
}
