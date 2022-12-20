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
      NGConv <- inputdata %>%
        filter(ID=="AB_NGCONV")
      SCCT  <- inputdata %>%
        filter(ID=="AB_SCCT_noncogen")
      Cogen  <- inputdata %>%
        filter(ID=="LTO_Cogen")
      CCCT <- inputdata %>%
        filter(ID=="AB_CCCT_noncogen")
      CC_Blended <- inputdata %>%
        filter(ID=="AB_CCCT_Blended")
      SC_Blended <- inputdata %>%
        filter(ID=="AB_SCCT_Blended")
      H2 <- inputdata %>%
        filter(ID=="LTO_H2")
      Other <- inputdata %>%
        filter(ID=="LTO_Other")
      Hydro <- inputdata %>%
        filter(ID=="LTO_Hydro")
      Nuclear <- inputdata %>%
        filter(ID=="LTO_Nuclear")
      Solar <- inputdata %>%
        filter(ID=="LTO_Solar")
      Storage <- inputdata %>%    
        filter(ID=="LTO_Storage")
      Wind <- inputdata %>%
        filter(ID=="LTO_Wind")
      }
      
      # Combine the grouped data
      {case <- rbind(Coal, NGConv, Cogen, SCCT, CCCT, CC_Blended, SC_Blended,
                     H2, Hydro, Nuclear, Solar, Wind, Storage, Other)
        case$ID <- factor(case$ID, levels=c("LTO_Coal", "AB_NGCONV", 
                                            "AB_CCCT_noncogen", "LTO_Cogen",
                                            "AB_SCCT_noncogen", "AB_CCCT_Blended", 
                                            "AB_SCCT_Blended", "LTO_H2", "LTO_Hydro", 
                                            "LTO_Other", "LTO_Nuclear",
                                            "LTO_Wind", "LTO_Solar", "LTO_Storage"))
        levels(case$ID) <- c("COAL", "NGCONV", "NGCC", "COGEN", "SCGT", "CC_BLEND",
                             "SC_BLEND", "H2", "HYDRO", "OTHER", "NUCLEAR",
                             "WIND", "SOLAR", "STORAGE")
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
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    data$ID <- factor(data$ID, levels=c("IMPORT", "COAL", "NGCONV", "COGEN", 
                                        "SCGT", "NGCC", "HYDRO", "OTHER",
                                        "WIND", "SOLAR", "STORAGE"))
    
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
    WK$MX <- ZPrice$Demand - Expo$Output_MWH
    
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
  
  Week_line <- function(year, month, day, case) {
    # Filters for the desired case study
    data <- Hour %>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    data$ID <- factor(data$ID, levels=c("Import", "COAL", "NGCONV", "COGEN", 
                                        "SCGT", "NGCC", "HYDRO", "OTHER",
                                        "WIND", "SOLAR", "STORAGE"))
    
    #    data$date <- as.POSIXct(data$date, tz = "MST")
    
    wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
    wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
    
    # Select only a single week
    ##############################################################################
    WK <- data %>%
      filter(date >= wk_st, date <= wk_end)
    
    # Select only a single week
    #    WK <- HrTime(data,year,month,day)
#    ZPrice <- HrTime(ZH,year,month,day)
#    Expo <- HrTime(Export,year,month,day)
#    WK$MX <- ZPrice$Demand - Expo$Output_MWH
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(WK$Output_MWH)), 100, f = ceiling)
    MN <- plyr::round_any(min(WK$Output_MWH), 100, f = floor)
    
    # Plot the data    
    ggplot() +
      geom_line(data = WK, aes(x = date, y = Output_MWH, color = ID), 
#                alpha=0.8, 
                size=2) +
      
      # Add hourly load line
#      geom_line(data = ZPrice, 
#                aes(x = date, y = Demand), size=2, colour = "black") +
      scale_x_datetime(expand=c(0,0)) +
      
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
  
  Month1 <- function(year, month, case) {
    # Filters for the desired case study
    data <- Hour %>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    data$ID <- factor(data$ID, levels=c("IMPORT", "COAL", "NGCONV", "COGEN", 
                                        "SCGT", "CC_BLEND", "SC_BLEND", "H2",
                                        "NGCC", "HYDRO", "OTHER", "UR",
                                        "WIND", "SOLAR", "STORAGE"))
    
    #    data$date <- as.POSIXct(data$date, tz = "MST")
    
    mth_st <- as.POSIXct(paste(01,month,year, sep = "/"), format="%d/%m/%Y")
    mth_end <- as.POSIXct(paste(01,month+1,year, sep = "/"), format="%d/%m/%Y")
    
    # Select only a single week
    ##############################################################################
    data <- data %>%
      filter(date >= mth_st, date < mth_end)
    
    # Select only a single week
    #    data <- HrTime(data,year,month,day)
    ZPrice <- ZH %>%
      subset(.,
             (date >= mth_st & 
                date < mth_end))
    Expo <- Export %>%
      subset(.,
             (date >= mth_st & 
                date < mth_end))
    data$MX <- ZPrice$Demand - Expo$Output_MWH
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(data$MX)), 100, f = ceiling)
    MN <- plyr::round_any(min(data$Output_MWH), 100, f = floor)
    
    # Plot the data    
    ggplot() +
      geom_area(data = data, aes(x = date, y = Output_MWH, fill = ID), 
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
      scale_fill_output()
#      scale_fill_manual(values = colours1)
  }
  
  ################################################################################
  # Functions for weekly evaluation over four years
  ################################################################################  
  
  Week14 <- function(year, month, day, case) {
    # Add imports and exports to data
    
    
    # Filters for the desired case study
    data <- Hour %>%
      sim_filt1(.) %>%
      select(-Report_Year) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    data$ID <- factor(data$ID, levels=c("Import", "COAL", "COGEN", "SCGT", "NGCC", 
                                        "HYDRO", "OTHER",
                                        "WIND", "SOLAR", "STORAGE"))
    
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
  
  Month14 <- function(year, month, case) {
    # Filters for the desired case study
    data <- Hour %>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    data$ID <- factor(data$ID, levels=c("IMPORT", "COAL", "NGCONV", "COGEN", 
                                        "SCGT", "CC_BLEND", "SC_BLEND", "H2",
                                        "NGCC", "HYDRO", "OTHER", "UR",
                                        "WIND", "SOLAR", "STORAGE"))
    
    #    data$date <- as.POSIXct(data$date, tz = "MST")
    
    if(month < 12) {
      month1 <- month+1
      day1 <- 01
    } else {
      month1 <- month
      day1 <- 31
    }
    
    mth_st <- as.POSIXct(paste(01,month,year, sep = "/"), format="%d/%m/%Y")
    mth_end <- as.POSIXct(paste(day1,month1,year, sep = "/"), format="%d/%m/%Y")
    
    # Select only a single week
    ##############################################################################
    mth <- data %>%
      filter(date >= mth_st, date < mth_end)
    
    # Select only a single week
    #    data <- HrTime(data,year,month,day)
    ZPrice <- ZH %>%
      subset(.,
             (date >= mth_st & 
                date < mth_end))
#    Expo <- Export %>%
#      subset(.,
#             (date >= mth_st & 
#                date < mth_end))
    data$MX <- ZH$Demand - Export$Output_MWH
    
    # Set the max and min for the plot
    MX1 <- data %>%
      subset(.,
             (date >= as.POSIXct(paste(01,month,Yr4Sp[[1]], sep = "/"), 
                                 format="%d/%m/%Y") & 
                  date < as.POSIXct(paste(day1,month1,Yr4Sp[[1]], sep = "/"), 
                                    format="%d/%m/%Y")))
                
    MX2 <- data %>%
      subset(.,
             (date >= as.POSIXct(paste(01,month,Yr4Sp[[2]], sep = "/"), 
                                 format="%d/%m/%Y") & 
                date < as.POSIXct(paste(day1,month1,Yr4Sp[[2]], sep = "/"), 
                                  format="%d/%m/%Y")))
    MX3 <- data %>%
      subset(.,
             (date >= as.POSIXct(paste(01,month,Yr4Sp[[3]], sep = "/"), 
                                 format="%d/%m/%Y") & 
                date < as.POSIXct(paste(day1,month1,Yr4Sp[[3]], sep = "/"), 
                                  format="%d/%m/%Y")))
    MX4 <- data %>%
      subset(.,
             (date >= as.POSIXct(paste(01,month,Yr4Sp[[4]], sep = "/"), 
                                 format="%d/%m/%Y") & 
                date < as.POSIXct(paste(day1,month1,Yr4Sp[[4]], sep = "/"), 
                                  format="%d/%m/%Y")))
    MXtime <- rbind(MX1, MX2, MX3, MX4)
    
    MX <- plyr::round_any(max(abs(MXtime$MX)), 100, f = ceiling)
    MN <- plyr::round_any(min(MXtime$Output_MWH), 100, f = floor)
    
    # Plot the data    
    ggplot() +
      geom_area(data = mth, aes(x = date, y = Output_MWH, fill = ID), 
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
      labs(x = year, y = "Output (MWh)", fill = "Resource") +
      scale_fill_output()
    #      scale_fill_manual(values = colours1)
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
  
  monthly_price <- function(case) {
    # Filters for the desired case study
    
    source("DrLeach_Code.R")

    data <- ZoneH %>%
      mutate(year = year(date),
             time = date) %>%
      filter(Run_ID == case,
             year >= 2020 & year <= 2035,
             Condition != "Average") %>%
      subset(.,select=-c(Condition,Marginal_Resource,date,Run_ID,Name,Report_Year))
    
    peak_data_Sim<-data %>%
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
    
    top_panel<-ggplot(peak_data_Sim) +
      geom_line(aes(date,mean_price,linetype="A"),size=.85)+#,color="black")+
      geom_line(aes(date,mean_off_peak_price,linetype="B"),size=.85)+#,color="blue")+
      geom_ribbon(aes(date,ymax=q75_price,ymin=q25_price,fill=sit),alpha=.5)+
      geom_hline(yintercept=0) +
      scale_color_manual("",values = c("black","royalblue4"))+
      scale_fill_manual("",values = c("grey50","royalblue"),
                        labels="Two-tailed 90th\npercentile range")+
      scale_linetype_manual("",values = c("solid","11"),
                            labels=c("Peak \nperiod average","Off-peak \nperiod average"))+
      scale_x_date(expand=c(0,0),breaks="1 year",labels = date_format("%Y",tz="America/Denver"))+
      scale_y_continuous(expand=c(0,0))+
      expand_limits(y=0)+ #make sure you get the zero line
      guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
      theme_bw() +
      theme(legend.position="right",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
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
  
  simcomp_monthly_price <- function(case) {
    # Filters for the desired case study
    
    source("DrLeach_Code.R")
    
    dataNo <- NoHS_ZH %>%
      mutate(year = year(date),
             time = date) %>%
      filter(Run_ID == case,
             year >= 2020 & year <= 2035,
             Condition != "Average") %>%
      subset(.,select=-c(Condition,Marginal_Resource,date,Run_ID,Name,Report_Year))
    
    peak_data_no<-dataNo %>%
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
             sit = "No")
    
    dataHS <- HS_ZH %>%
      mutate(year = year(date),
             time = date) %>%
      filter(Run_ID == case,
             year >= 2020 & year <= 2035,
             Condition != "Average") %>%
      subset(.,select=-c(Condition,Marginal_Resource,date,Run_ID,Name,Report_Year))
    
    peak_data_HS<-dataHS %>%
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
             sit = "HS")
    
    total <- rbind(peak_data_HS,peak_data_no)
    
    top_panel<-ggplot(total%>%arrange(desc(sit))) +
      geom_line(aes(date,mean_price,linetype="A",color=sit),size=.75)+
      geom_line(aes(date,mean_off_peak_price,linetype="B",color=sit),size=.75)+
      geom_ribbon(aes(date,ymax=q75_price,ymin=q25_price,fill=sit),
                  alpha=.3)+
      geom_hline(yintercept=0) +
      scale_color_manual("",values = c("royalblue4","black"))+
      scale_fill_manual("",values = c("royalblue","grey40"),
                        labels=c("With hypothetical sites",
                                 "Without hypothetical sites")
                        )+
      scale_linetype_manual("",values = c("solid","11"),
                            labels=c("Peak \nperiod average",
                                     "Off-peak \nperiod average")
                            )+
      scale_x_date(expand=c(0,0),breaks="1 year",
                   labels = date_format("%Y",tz="America/Denver"))+
      scale_y_continuous(expand=c(0,0))+
      expand_limits(y=0)+ #make sure you get the zero line
      guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
      theme_bw() +
      theme(legend.position="top",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
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
  
  simcomp_monthly_price2 <- function(case) {
    # Filters for the desired case study
    
    source("DrLeach_Code.R")
    
    dataNo <- NoHS_ZH %>%
      mutate(year = year(date),
             time = date) %>%
      filter(Run_ID == case,
             year >= 2020 & year <= 2035,
             Condition != "Average") %>%
      subset(.,select=-c(Condition,Marginal_Resource,date,Run_ID,Name,Report_Year))
    
    peak_data_no<-dataNo %>%
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
             sit = "No")
    
    dataHS <- HS_ZH %>%
      mutate(year = year(date),
             time = date) %>%
      filter(Run_ID == case,
             year >= 2020 & year <= 2035,
             Condition != "Average") %>%
      subset(.,select=-c(Condition,Marginal_Resource,date,Run_ID,Name,Report_Year))
    
    peak_data_HS<-dataHS %>%
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
             sit = "HS")
    
    dataOff <- NoOffset_ZH %>%
      mutate(year = year(date),
             time = date) %>%
      filter(Run_ID == case,
             year >= 2020 & year <= 2035,
             Condition != "Average") %>%
      subset(.,select=-c(Condition,Marginal_Resource,date,Run_ID,Name,Report_Year))
    
    peak_data_Off<-dataOff %>%
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
             sit = "Off")
    
    total <- rbind(peak_data_HS,peak_data_no,peak_data_Off)
    
    shades <- c("HS"="mediumblue","No"="black","Off"="orangered")
    
    top_panel<-ggplot(total%>%arrange(desc(sit))) +
      geom_line(aes(date,mean_price,linetype="A",color=sit),size=.75)+
      geom_line(aes(date,mean_off_peak_price,linetype="B",color=sit),size=.75)+
      geom_ribbon(aes(date,ymax=q75_price,ymin=q25_price,fill=sit),
                  alpha=.3)+
      geom_hline(yintercept=0) +
      scale_color_manual("",values = shades)+
      scale_fill_manual("",values = shades,
                        labels=c("HS"="With hypothetical sites",
                                 "No"="Without hypothetical sites",
                                 "Off"="No Offsets")
      )+
      scale_linetype_manual("",values = c("solid","11"),
                            labels=c("Peak \nperiod average",
                                     "Off-peak \nperiod average")
      )+
      scale_x_date(expand=c(0,0),breaks="1 year",
                   labels = date_format("%Y",tz="America/Denver"))+
      scale_y_continuous(expand=c(0,0))+
      expand_limits(y=0)+ #make sure you get the zero line
      guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),
             color="none")+
      theme_bw() +
      theme(legend.position="top",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
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
    
      Imp$Time_Period  <- as.Date(as.character(Imp$Time_Period), 
                                   format = "%Y")
    
    #  Imp <- subset(Imp, Time_Period <= '2040-04-05')
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      subset(., select=c(ID, Time_Period, Output_MWH)) %>%
      sim_filt1(.) %>%
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
          scale_x_date(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_output() +
#      scale_fill_manual(values = colours5c) +
      labs(x = "Date", y = "Output (GWh)", fill = "Resource") 
  }
  
  Evalcap <- function(input,case) {
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Capacity) %>%
      sim_filt1(.)
    
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
      scale_fill_output() +
#      scale_fill_manual(values = colours6) +
      labs(x = "Date", y = "Capacity (GWh)", fill = "Resource") 
  }
  
  Eval_diffUnits <- function(input,case) {
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Capacity) %>%
      sim_filt1(.) %>%
      group_by(ID) %>%
      arrange(Time_Period) %>%
      mutate(diff = Capacity - lag(Capacity, default = first(Capacity)))
    
    data$Time_Period <- as.Date(data$Time_Period)
    
    Tot <- data %>%
      group_by(Time_Period) %>%
      summarise(maxy = sum(diff[which(diff>0)]), miny = sum(diff[which(diff<0)]))
    
    mny <- plyr::round_any(min(Tot$miny),1000, f=floor)
    mxy <- plyr::round_any(max(Tot$maxy),1000, f=ceiling)
    
    mnx <- min(Month$Time_Period)
    mxx <- max(Month$Time_Period)
    
    data %>%
      ggplot() +
      aes(Time_Period, (diff), fill = ID) +
      geom_col(alpha=0.6, size=.5, colour="black") +
      #    facet_wrap(~ Condition, nrow = 1) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.justification = c(0,0.5)) +
      scale_x_date(expand=c(0,0),
                   limits = as.Date(c(mnx,mxx))) +
      scale_y_continuous(expand=c(0,0),
                         limits = c((mny+100),(mxy+100))) +
      scale_fill_output() +
#      scale_fill_manual(values = colours5d) +
      labs(x = "Date", y = "Yearly change in capacity (MW)", fill = "Resource",
           title = paste("Simulation: ",DB, sep = "")) 
  }
  
  Eval_diffcap <- function(input,case) {
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      subset(.,select=c(ID, Time_Period, Capacity)) %>%
      sim_filt1(.) %>%
      group_by(ID) %>%
      arrange(Time_Period) %>%
      mutate(diff = Capacity - lag(Capacity, default = first(Capacity)))
    
    data$Time_Period <- as.factor(format(data$Time_Period, format="%Y"))
    
    Tot <- data %>%
      group_by(Time_Period) %>%
      summarise(maxy = sum(diff[which(diff>0)]), miny = sum(diff[which(diff<0)]))
    
    mny <- plyr::round_any(min(Tot$miny),1000, f=floor)
    mxy <- plyr::round_any(max(Tot$maxy),1000, f=ceiling)
    
    mnx <- format(min(Month$Time_Period), format="%Y")
    mxx <- format(max(Month$Time_Period), format="%Y")
    
    data %>%
      ggplot() +
      aes(Time_Period, (diff), fill = ID) +
      geom_col(alpha=0.6, size=.5, colour="black") +
      #    facet_wrap(~ Condition, nrow = 1) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.justification = c(0,0.5),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),) +
      scale_x_discrete(expand=c(0,0),
                       limits = as.character(mnx:mxx)) +
#      scale_x_date(expand=c(0,0),
#                   limits = as.Date(c(mnx,mxx))
#                   ) +
      scale_y_continuous(expand=c(0,0),
                         limits = c((mny),(mxy))) +
      scale_fill_output() +
#      scale_fill_manual(values = colours5d) +
      labs(x = "Date", y = "Yearly change in capacity (MW)", fill = "Resource",
           title = paste("Simulation: ",DB, sep = "")) 
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
    case_Time <- sim_filt1(data)
    
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
            legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            legend.justification = c(0,0.5)) +
      scale_x_date(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0),
                         labels = scales::percent, 
                         breaks = sort(c(seq(0,1,length.out=5),0.3))) +
#      scale_fill_manual(values = colours6) +
      scale_fill_output() +
      labs(x = "Date", y = "Percentage of Generation", fill = "Resource",
           title = paste("Simulation: ",DB, sep = "")) 
  }
  
  ################################################################################
  # Function for plotting the resources built
  ################################################################################
  ################################################################################
  
  # Stacked Area showing totals for Fuel Types
  Built <- function(case) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == 0 & 
               Time_Period != "Study")%>%
      group_by(Fuel_Type, Time_Period) %>%
      summarise(Units = sum(Units_Built)) 
    
    data$Fuel_Type <- factor(data$Fuel_Type, 
                             levels = c("WND","SUN","PS","UR","WAT","OT","H2",
                                        "GasB","Gas2","Gas1","Gas0","Gas","COAL"))
    
#    data$Time_Period <- as.Date(data$Time_Period)
    
#    mnx <- min(Month$Report_Year)
#    mxx <- max(Month$Report_Year)
    
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
                         #limits = c(0,(max(data$Units)+1))
                         ) +
#      scale_x_continuous(expand=c(0,0),
#                       limits = c(mnx,mxx)
#                       ) +
      scale_fill_built()
#      scale_fill_manual(values = colours3)
  }
  
  ################################################################################
  # Function for plotting the resources built as bar chart
  ################################################################################
  
  # Stacked Area showing totals for Fuel Types
  Builtcol <- function(case) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == 0 & 
               Time_Period != "Study")%>%
      group_by(Fuel_Type, Time_Period) %>%
      summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
    
    data$Time_Period <- as.factor(data$Time_Period)
    #data$Time_Period <- anytime::anydate(data$Time_Period)
    
    data$Fuel_Type <- factor(data$Fuel_Type, 
                             levels = c("WND","SUN","PS","UR","WAT","OT","H2",
                                        "GasB_CC","GasB_SC","Gas3","Gas2","Gas1",
                                        "Gas0","Gas","COAL"
                                        ))
    
#    data$Fuel_Type[is.na(data$Fuel_Type)] <- "GasB"
    
    Tot <- data %>%
      group_by(Time_Period) %>%
      summarise(totu = sum(Units), totc = sum(Capacity))
    
    mxu <- max(Tot$totu)
    mxc <- max(Tot$totc)
    
    mnx <- format(min(Month$Time_Period), format="%Y")
    mxx <- format(max(Month$Time_Period), format="%Y")
    
    ggplot(data) +
      aes(Time_Period, Units, fill = Fuel_Type, group = Fuel_Type) +
      geom_col(alpha=0.6, colour = "black",
               size=0.5) +
      theme_bw() +
      theme(panel.grid = element_blank(),  
            #legend.position ="none"
            #          legend.justification = c(0,0.5),
            #          legend.position = "top"
      ) +
      labs(x = "Date", y = "# of Units Built", fill = "Fuel Type") +
      scale_x_discrete(expand=c(0,0),
                       limits = as.character(mnx:mxx)) +
      #      scale_x_date(expand=c(0,0),
      #                   limits = as.Date(c(mnx,mxx))
      #                   ) +
      scale_y_continuous(expand=c(0,0),
                         limits = c(0,(mxu+1))) +
      scale_fill_built()
  #    scale_fill_manual(values = colours4)
  }
  
  ################################################################################
  # Function for plotting the capacity of resources built
  ################################################################################
  
  # Stacked Area showing totals for Fuel Types
  BuiltMW <- function(case) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == 0 & 
               Time_Period != "Study")%>%
      group_by(Fuel_Type, Time_Period) %>%
      summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
    
    data$Fuel_Type <- factor(data$Fuel_Type, 
                             levels = c("WND","SUN","PS","UR","WAT","OT","H2",
                                        "GasB_CC","GasB_SC","Gas2","Gas1","Gas0",
                                        "Gas3","COGEN","COAL"))
    
    data$Time_Period <- as.Date(ISOdate(data$Time_Period,1,1))
    
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
            #legend.position ="none"
      ) +
      labs(x = "Date", y = "Capacity Built \n(MW)", fill = "Fuel Type") +
      scale_x_date(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0),
                         limits = c(0,plyr::round_any(mxc, 100, f = ceiling))) +
      #    scale_x_discrete(expand=c(0,0)) +
      scale_fill_built()
#      scale_fill_manual(values = colours4)
  }
  
  ################################################################################
  # Function for plotting the locations of the resources built on a map
  ################################################################################
  
  Build_Map <- function(case) {
    library(raster)
    
    {can_level1 = getData("GADM", country = "CA", level = 1)
    
    WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    canada_level1_ellipsoid = spTransform(can_level1, WGS84)
    
    alberta_ellipsoid1 = 
      canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]
    }
    
    Built <- Build %>%
      filter(Run_ID == case,
             Fuel_Type == "WND",
             LT_Iteration == 0,
             Capacity_Built != 0,
             Time_Period <= 2035) %>%
      mutate(Latitude=case_when(grepl("Buffalo Atlee",Name) ~ 50.74551,
                                grepl("Buffalo Plains",Name) ~ 50.38399,
                                grepl("Buffalo Trail North",Name) ~ 49.87802,
                                grepl("Buffalo Trail South",Name) ~ 49.77304,
                                grepl("Bull Trail",Name) ~ 49.81,
                                grepl("Castle Meridian",Name) ~ 49.47619,
                                grepl("EDPR Sharp Hills",Name) ~ 51.75,
                                grepl("Enerfin Winnifred",Name) ~ 49.93573,
                                grepl("Forty Mile Maleb",Name) ~ 49.61156,
                                grepl("Forty Mile",Name) ~ 49.11795,
                                grepl("Invenergy Schuler",Name) ~ 50.38994,
                                grepl("Lanfine North",Name) ~ 51.32123,
                                grepl("Lone Pine",Name) ~ 51.80457,
                                grepl("Northern Lights",Name) ~ 54.7587,
                                grepl("Old Elm",Name) ~ 49.31,
                                grepl("Oyen",Name) ~ 51.45441,
                                grepl("Paintearth",Name) ~ 52.18275,
                                grepl("Renewable Energy Service",Name) ~ 49.48438,
                                grepl("Riplinger",Name) ~ 49.21956,
                                grepl("Stirling",Name) ~ 49.57666,
                                
                                grepl("Fort Saskatchewan",Name) ~ 53.67672,
                                grepl("Clear Prairie",Name) ~ 56.724,
                                grepl("Lesser Slave Lake",Name) ~ 55.435,
                                grepl("John D'Or",Name) ~ 58.794,
                                grepl("Anzac",Name) ~ 56.34,
                                grepl("Grande Cache",Name) ~ 54.443,
                                grepl("Pigeon Lake",Name) ~ 53.086,
                                grepl("Kehewin",Name) ~ 54.066,
                                grepl("Chain Lakes",Name) ~ 50.25,
                                grepl("Falher",Name) ~ 55.73,
                                grepl("Bison Lake",Name) ~ 57.382,
                                grepl("Hinton",Name) ~ 53.341
                                ),
             Longitude=case_when(grepl("Buffalo Atlee",Name) ~ -111.024,
                                 grepl("Buffalo Plains",Name) ~ -112.774,
                                 grepl("Buffalo Trail North",Name) ~ -110.522,
                                 grepl("Buffalo Trail South",Name) ~ -110.419,
                                 grepl("Bull Trail",Name) ~ -110.22,
                                 grepl("Castle Meridian",Name) ~ -114.018,
                                 grepl("EDPR Sharp Hills",Name) ~ -110.58,
                                 grepl("Enerfin Winnifred",Name) ~ -111.089,
                                 grepl("Forty Mile Maleb",Name) ~ -111.144,
                                 grepl("Forty Mile",Name) ~ -111.207,
                                 grepl("Invenergy Schuler",Name) ~ -110.222,
                                 grepl("Lanfine North",Name) ~ -110.567,
                                 grepl("Lone Pine",Name) ~ -113.598,
                                 grepl("Northern Lights",Name) ~ -115.567,
                                 grepl("Old Elm",Name) ~ -112.93,
                                 grepl("Oyen",Name) ~ -110.459,
                                 grepl("Paintearth",Name) ~ -112.062,
                                 grepl("Renewable Energy Service",Name) ~ -113.834,
                                 grepl("Riplinger",Name) ~ -113.681,
                                 grepl("Stirling",Name) ~ -112.387,
                                  
                                 grepl("Fort Saskatchewan",Name) ~ -113.17,
                                 grepl("Clear Prairie",Name) ~ -119.496,
                                 grepl("Lesser Slave Lake",Name) ~ -115.081,
                                 grepl("John D'Or",Name) ~ -114.97,
                                 grepl("Anzac",Name) ~ -111.265,
                                 grepl("Grande Cache",Name) ~ -119.341,
                                 grepl("Pigeon Lake",Name) ~ -114.187,
                                 grepl("Kehewin",Name) ~ -110.802,
                                 grepl("Chain Lakes",Name) ~ -114.176,
                                 grepl("Falher",Name) ~ -117.177,
                                 grepl("Bison Lake",Name) ~ -115.807,
                                 grepl("Hinton",Name) ~ -117.472),
             Status = case_when(grepl("Potential",Name) ~ "Simulated",
                                TRUE ~ "Queue"),
             Name = gsub("\\s*\\([^\\)]+\\)", "",Name),
             Capacity = Capacity_Built
             ) %>%
      group_by(Name,Latitude,Longitude,Status) %>%
      summarise(Capacity = sum(Capacity))
     # subset(., select=c(Name,Capacity,Latitude,Longitude,Status))
    
#    Active <- ResourceYr %>%
#      filter(Run_ID == case,
#             Primary_Fuel == "Wind",
#             Capacity > 0,
#             !grepl('New Resource',Name)) %>%
#      group_by(Name) %>%
#      summarise(Capacity = mean(Capacity)
#                ) %>%
#      mutate(Latitude=case_when(grepl("AKE1",Name) ~ 49.60344,
#                                grepl("ARD1",Name) ~ 49.55403,
#                                grepl("BSR1",Name) ~ 50.13716,
#                                grepl("BTR1",Name) ~ 49.6537,
#                                grepl("BUL",Name) ~ 52.50809,
#                                grepl("CR1",Name) ~ 49.50669,
#                                grepl("CRE3",Name) ~ 49.56245,
#                                grepl("CRR1",Name) ~ 49.55893,
#                                grepl("CRR2",Name) ~ 49.55891,
#                                grepl("CYP",Name) ~ 49.84097,
#                                grepl("FMG1",Name) ~ 49.66334,
#                                grepl("Garden Plain",Name) ~ 51.97348,
#                                grepl("GRZ1",Name) ~ 53.21519,
#                                grepl("GWW1",Name) ~ 49.51095,
#                                grepl("HAL1",Name) ~ 52.27372,
#                                grepl("HHW1",Name) ~ 51.5661,
#                                grepl("HLD1",Name) ~ 50.55398,
#                                grepl("IEW",Name) ~ 49.60701,
#                                grepl("JNR1",Name) ~ 50.83577,
#                                grepl("JNR2",Name) ~ 50.82,
#                                grepl("JNR3",Name) ~ 50.77712,
#                                grepl("KHW1",Name) ~ 49.51343,
#                                grepl("Lanfine",Name) ~ 51.32123,
#                                grepl("NEP1",Name) ~ 51.89861,
#                                grepl("OWF1",Name) ~ 49.57529,
#                                grepl("RIV1",Name) ~ 49.53245,
#                                grepl("RTL1",Name) ~ 49.91059,
#                                grepl("SCR2",Name) ~ 49.38739,
#                                grepl("SCR3",Name) ~ 49.68462,
#                                grepl("SCR4",Name) ~ 51.2111,
#                                grepl("TAB1",Name) ~ 49.71317,
#                                grepl("WHE1",Name) ~ 51.2375,
#                                grepl("WHT",Name) ~ 49.64029,
#                                grepl("WRW1",Name) ~ 49.47014
#                                ),
#             Longitude=case_when(grepl("AKE1",Name) ~ -113.485,
#                                 grepl("ARD1",Name) ~ -113.432,
#                                 grepl("BSR1",Name) ~ -112.891,
#                                 grepl("BTR1",Name) ~ -113.467,
#                                 grepl("BUL",Name) ~ -110.06,
#                                 grepl("CR1",Name) ~ -114.042,
#                                 grepl("CRE3",Name) ~ -114.108,
#                                 grepl("CRR1",Name) ~ -113.969,
#                                 grepl("CRR2",Name) ~ -113.983,
#                                 grepl("CYP",Name) ~ -110.357,
#                                 grepl("FMG1",Name) ~ -111.122,
#                                 grepl("Garden Plain",Name) ~ -111.833,
#                                 grepl("GRZ1",Name) ~ -111.095,
#                                 grepl("GWW1",Name) ~ -113.506,
#                                 grepl("HAL1",Name) ~ -112.063,
#                                 grepl("HHW1",Name) ~ -112.254,
#                                 grepl("HLD1",Name) ~ -110.124,
#                                 grepl("IEW",Name) ~ -113.777,
#                                 grepl("JNR1",Name) ~ -111.104,
#                                 grepl("JNR2",Name) ~ -111.07,
#                                 grepl("JNR3",Name) ~ -111.046,
#                                 grepl("KHW1",Name) ~ -113.816,
#                                 grepl("Lanfine",Name) ~ -110.567,
#                                 grepl("NEP1",Name) ~ -113.365,
#                                 grepl("OWF1",Name) ~ -113.853,
#                                 grepl("RIV1",Name) ~ -113.977,
#                                 grepl("RTL1",Name) ~ -111.06,
#                                 grepl("SCR2",Name) ~ -112.955,
#                                 grepl("SCR3",Name) ~ -112.324,
#                                 grepl("SCR4",Name) ~ -112.558,
#                                 grepl("TAB1",Name) ~ -111.933,
#                                 grepl("WHE1",Name) ~ -112.426,
#                                 grepl("WHT",Name) ~ -111.291,
#                                 grepl("WRW1",Name) ~ -113.983),
#      Status = "Active")
    
#    comb_sites <- rbind(Active,Built) #%>%
    #  arrange(match(Status, c("Active","Queue","Potential")),
    #          desc(Status))
      
    
    setwd("D:/Documents/GitHub/AuroraEval")
    
    labs <- c("Queue"="AESO Queue","Simulated"="Hypothetical")#"Active"="Existing",
    Simcolor <- c("Queue"="grey39","Simulated"="red4")#"Active"="black",
    Simshape <- c("Queue"=18,"Simulated"=17)#"Active"=16,
    
    wind_profileAA <- readRDS("WindAtlas_Data00_0.05")
    colnames(wind_profileAA) <- c('Latitude', 'Longitude', 'Wind')
    
    ggplot() + 
      geom_raster(data = wind_profileAA, 
                  aes(x = Longitude, y = Latitude, fill = Wind)) + 
      geom_polygon(data = alberta_ellipsoid1, 
                   aes(x = long, y = lat, group = group), 
                   fill = "transparent", colour = "black") +
      geom_point(data = Built, aes(x=Longitude, y=Latitude, size=Capacity,
                                  color=Status, shape=Status)) +
      scale_fill_gradientn(colors = matlab.like2(100),
                           limits=c(3,10),oob=squish, 
                           name = "Mean wind speed \nat 80m height \n(m/s)") +
      scale_shape_manual(values = Simshape, labels = labs) + 
      scale_color_manual(values = Simcolor, 
                         labels = labs) +
      guides(shape = guide_legend(override.aes = list(size = 5), order = 1)) +
      guides(color = guide_legend(override.aes = list(size = 5), order = 1)) +
      guides(size = guide_legend(order = 2)) +
      coord_fixed(ratio=5/3) +
      geom_label(data=subset(Built, Status == "Simulated"),
                 aes(x=Longitude,y=Latitude, 
                     label = Name),
                 #label=Built$Name,
                 size = 4, fontface='bold',
                 nudge_y = 0.35, #hjust="inward"
      ) +
      #labs(subtitle = DB) +
      theme(panel.background = element_rect(fill = "transparent"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(0,0,0,0), "cm"),
            plot.background = element_rect(fill = "transparent", color = NA),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust=0.5,vjust=-1),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent", color = "transparent"),
            legend.key=element_rect(fill = "transparent"),
            #legend.text = element_text(size = legText),
            #legend.title = element_text(size = legTitle),
            rect = element_rect(fill="transparent")
            )
  }
  
  ################################################################################
  # Unit specific bar chart showing builds
  ################################################################################
  ################################################################################
  
  Units <- function(case, Fuel) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == 0 & 
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
      filter(Run_ID == case & LT_Iteration == 0 & 
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
  
  Units2 <- function(case, Fuel1, Fuel2) {
    # Test to verify both Fuels were provided
    if(missing(Fuel2)) {
      data <- Build %>%
        filter(Run_ID == case & LT_Iteration == 0 & 
                 Time_Period == "Study" & Fuel_Type == Fuel1) %>%
        mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                  "Hypothetical", "AESO Queue"))
      fuel <- Fuel1
    } else {
      data <- Build %>%
        filter(Run_ID == case & LT_Iteration == 0 & 
                 Time_Period == "Study" & 
                 (Fuel_Type == Fuel1 | Fuel_Type == Fuel2)) %>%
        mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                  "Hypothetical", "AESO Queue"))
      fuel <- paste0(Fuel1," & ",Fuel2)
    }
    
    data %>%
      ggplot() +
      aes(Name, Units_Built, fill = Potential) + 
      geom_col() +
      labs(x = fuel, y = "Units Built") +
      scale_fill_manual(
        values = c("Hypothetical"="forestgreen", "AESO Queue"="gray"),
        guide = "none") +
      scale_y_continuous(expand = c(0,0),
                         #limits = c(0,(max(data$Units_Built)+1))
                         ) +
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
  
  CapBuild2 <- function(case, Fuel1, Fuel2) {
    # Test to verify both Fuels were provided
    if(missing(Fuel2)) {
      data <- Build %>%
        filter(Run_ID == case & LT_Iteration == 0 & 
                 Time_Period == "Study" & Fuel_Type == Fuel1) %>%
        mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                  "Hypothetical", "AESO Queue"))
      fuel <- Fuel1
    } else {
      data <- Build %>%
        filter(Run_ID == case & LT_Iteration == 0 & 
                 Time_Period == "Study" & 
                 (Fuel_Type == Fuel1 | Fuel_Type == Fuel2)) %>%
        mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                  "Hypothetical", "AESO Queue"))
      fuel <- paste0(Fuel1," & ",Fuel2)
    }
    
    data <- data %>%
      mutate(max = case_when(Max_Limit_Slack == 0 & 
                               Potential == "AESO Queue" ~ "YesAESO",
                             Max_Limit_Slack == 0 & 
                               Potential == "Hypothetical" ~ "YesHypo",
                             Max_Limit_Slack != 0 & 
                               Potential == "Hypothetical" ~ "NoHypo",
                             Max_Limit_Slack != 0 & 
                               Potential == "AESO Queue"~ "NoAESO"))
    
    data %>%
      ggplot() +
      aes(Name, Capacity_Built, fill = max) + 
      geom_col() +
      labs(x = fuel, y = "Capacity Built (MW)") +
      scale_fill_manual(
        values = c("YesHypo"="forestgreen","YesAESO"="grey0","NoHypo"="palegreen",
                   "NoAESO"="gray"),
        #values = c("Hypothetical"="forestgreen", "AESO Queue"="gray"),
        guide = "none") +
      scale_y_continuous(expand = c(0,0),
                         #limits = c(0,(max(data$Units_Built)+1))
      ) +
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
  
  CapPot <- function(case) {
    # Test to verify both Fuels were provided
    data <- Build %>%
        filter(Run_ID == case,
               LT_Iteration == 0,
               Time_Period != "Study",
               Fuel_Type == "WND",
               Capacity_Built != 0) %>%
        mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                  "Hypothetical", "AESO Queue"),
               Name = gsub("\\s*\\([^\\)]+\\)", "",Name)) %>%
      group_by(Name) %>%
      mutate(Total = sum(Capacity_Built),
             Name = gsub("Joss Wind","",Name),
             Name = gsub(" Wind","",Name),
             Name = gsub(" Farm","",Name)) #%>%
#      filter(Potential == "Hypothetical")
    
    sz <- 15
    
    data %>%
      ggplot() +
      aes(fct_reorder(Name,-Total), Capacity_Built, fill=Time_Period) + 
      geom_bar(position="stack",stat="identity") +
      labs(x = "", y = "Capacity Built (MW)") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max(ceiling(data$Total))+50)),
                         breaks = seq(0,3000, by=200)
      ) +
      scale_fill_viridis(discrete=TRUE,
      ) +
      guides(fill=guide_legend(title="Year Built")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text = element_text(size = sz),
            axis.title = element_text(size = sz),
            panel.background = element_rect(fill = "transparent"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.title = element_text(size = sz-1),
            legend.text = element_text(size = sz-2),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            panel.border = element_rect(colour = "black", fill = "transparent")) 
  }
  
  CapPot2 <- function(case) {
    # Test to verify both Fuels were provided
    data <- Build %>%
      filter(Run_ID == case,
             LT_Iteration == 0,
             Capacity_Built !=0,
             Time_Period <= 2035,
             Fuel_Type == "WND") %>%
      mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                "Hypothetical", "AESO Queue"),
             Name = gsub("\\s*\\([^\\)]+\\)", "",Name)) %>%
      group_by(Name) %>%
      mutate(Total = sum(Capacity_Built)) %>%
      filter(Potential == "Hypothetical")
    
    
    sz <- 15
    
    data %>%
      ggplot() +
      aes(fct_reorder(Name,-Total), Capacity_Built, fill=Time_Period) + 
      geom_bar(position="stack",stat="identity") +
      labs(x = "", y = "Capacity Built (MW)") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max(ceiling(data$Total))+50)),
                         breaks = seq(0,1050, by=100)
      ) +
      scale_fill_viridis(discrete=TRUE,
                         ) +
      guides(fill=guide_legend(title="Year Built")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text = element_text(size = sz),
            axis.title = element_text(size = sz),
            panel.background = element_rect(fill = "transparent"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.title = element_text(size = sz-1),
            legend.text = element_text(size = sz-2),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            panel.border = element_rect(colour = "black", fill = "transparent")) 
  }
  
  ################################################################################
  # Unit specific bar chart showing availability not built with potential sites 
  # highlighted
  ################################################################################
  
  Slack2 <- function(case, Fuel1, Fuel2) {
    # Test to verify both Fuels were provided
    if(missing(Fuel2)) {
      data <- Build %>%
        filter(Run_ID == case & LT_Iteration == 0 & 
                 Time_Period == "Study" & Fuel_Type == Fuel1) %>%
        mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                  "Hypothetical", "AESO Queue")) 
      fuel <- Fuel1
    } else {
      data <- Build %>%
        filter(Run_ID == case & LT_Iteration == 0 & 
                 Time_Period == "Study" & 
                 (Fuel_Type == Fuel1 | Fuel_Type == Fuel2)) %>%
        mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                  "Hypothetical", "AESO Queue")) 
      fuel <- paste0(Fuel1," & ",Fuel2)
    }
    
    data %>%
      ggplot() +
      aes(Name, Max_Limit_Slack, fill = Potential) + 
      geom_col() +
      labs(x = fuel, y = "Units Not Built") +
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
  
  CapSlack2 <- function(case, Fuel1, Fuel2) {
    # Test to verify both Fuels were provided
    if(missing(Fuel2)) {
      data <- Build %>%
        filter(Run_ID == case & LT_Iteration == 0 & 
                 Time_Period == "Study" & Fuel_Type == Fuel1) %>%
        mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                  "Hypothetical", "AESO Queue")) 
      fuel <- Fuel1
    } else {
      data <- Build %>%
        filter(Run_ID == case & LT_Iteration == 0 & 
                 Time_Period == "Study" & 
                 (Fuel_Type == Fuel1 | Fuel_Type == Fuel2)) %>%
        mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                  "Hypothetical", "AESO Queue")) 
      fuel <- paste0(Fuel1," & ",Fuel2)
    }
    
    data <- data %>%
      mutate(max = case_when(Max_Limit_Slack == 0 & 
                               Potential == "AESO Queue" ~ "YesAESO",
                             Max_Limit_Slack == 0 & 
                               Potential == "Hypothetical" ~ "YesHypo",
                             Max_Limit_Slack != 0 & 
                               Potential == "Hypothetical" ~ "NoHypo",
                             Max_Limit_Slack != 0 & 
                               Potential == "AESO Queue"~ "NoAESO"))
    
    data %>%
      ggplot() +
      aes(Name, Max_Limit_Slack, fill = max) + 
      geom_col() +
      labs(x = fuel, y = "Units Not Built") +
      scale_fill_manual(#,
        values = c("YesHypo"="forestgreen","YesAESO"="grey0","NoHypo"="palegreen",
                   "NoAESO"="gray"),
        #values = c("Hypothetical"="forestgreen", "AESO Queue"="gray"),
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
  # Capacity factors for new resources built
  ################################################################################
  
  CF_NR <- function(fuel,type,case) {
    
    #ResourceHr <- dbReadTable(con,'ResourceHour1')
    
    data <- ResourceHr %>%
      filter(Run_ID == case,
             Report_Year <= 2035,
             grepl('New Resource',Name),
             grepl(type,Name),
             #Output != 0,
             Primary_Fuel == fuel,
             !is.na(Capacity_Factor)
             ) %>%
      mutate(Name = gsub("^.*?from ","",Name),
             Name = gsub("^.*? ","",Name),
             #Name = case_when(str_detect(Name,"Pot")~paste("Hypothetical:",Name),
            #                  TRUE~Name),
             #Name = gsub("\\s*\\([^\\)]+\\)", "",Name)
             #time = as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",Time_Period))), 
              #                 tz = "MST")-(60*60)
             ) %>%
      #na.omit() %>%
      subset(.,select=c(Name,Time_Period,Output_MWH,Capacity,
                        #Percent_Marginal,Capacity,Capability,ID,Primary_Fuel,
                        Capacity_Factor,
                        #Beg_Date,End_Date,
                        Report_Year)) %>%
      group_by(Report_Year,Name) %>%
      summarise(Capacity = sum(Capacity),
                Output_MWH = sum(Output_MWH),
                Cap_Fac = Output_MWH/Capacity,
                #CF = mean(Capacity_Factor),
                #diff = Cap_Fac-CF
                ) %>%
      mutate(sit = case_when(str_detect(Name,"Pot")~"Hypo",
                             TRUE~"AESO"),
             Name = gsub("\\s*\\([^\\)]+\\)", "",Name),
             Name = case_when(str_detect(Name,"John D")~"John DOr",
                              TRUE~Name))
    
    ggplot() +
      geom_line(data = data, 
                aes(x = Report_Year, y = Cap_Fac,colour=Name,linetype=sit,
                    size = sit), 
                ) +
      theme(panel.background = element_rect(fill = "transparent"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            #axix.title.x = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            text = element_text(size= 15),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
      ) +
      labs(x = "", y = "Capacity Factor (%)", fill = "Potential New Resource") +
      scale_x_continuous(expand=c(0,0)) +
      scale_color_manual("Hypothetical Site",
                         values = c("steelblue","seagreen","tan",
                                    "saddlebrown","khaki","firebrick",
                                    "goldenrod","black"),
                          limits = c('Anzac','Bison Lake','Hinton','John DOr',
                                     'Kehewin',
                                     'Lesser Slave Lake','Pigeon Lake')) +
      scale_linetype_manual("",values = c("AESO"="dotdash","Hypo"="solid"),
                            labels=c("Wind farm from AESO queue",
                                     "Wind farm at hypothetical site")) +
      scale_size_manual("",values = c(0.5,2),
                        labels=c("Wind farm from AESO queue",
                                 "Wind farm at hypothetical site")) +
      guides(color = guide_legend(override.aes = list(size = 3)),
      )
  }
  
  CF_AllR <- function(case) {
    
    #ResourceHr <- dbReadTable(con,'ResourceHour1')
    
    data <- ResourceHr %>%
      filter(Run_ID == case,
             #Output != 0,
             Primary_Fuel == "Wind",
             !is.na(Capacity_Factor)
      ) %>%
      mutate(Name = gsub("^.*?from ","",Name),
             #Name = gsub("^.*? ","",Name),
             #Name = case_when(str_detect(Name,"Pot")~paste("Hypothetical:",Name),
             #                  TRUE~Name),
             #Name = gsub("\\s*\\([^\\)]+\\)", "",Name)
             #time = as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",Time_Period))), 
             #                 tz = "MST")-(60*60)
      ) %>%
      #na.omit() %>%
      subset(.,select=c(Name,Time_Period,Output_MWH,Capacity,
                        #Percent_Marginal,Capacity,Capability,ID,Primary_Fuel,
                        Capacity_Factor,
                        #Beg_Date,End_Date,
                        Report_Year)) %>%
      group_by(Report_Year,Name) %>%
      summarise(Capacity = sum(Capacity),
                Output_MWH = sum(Output_MWH),
                Cap_Fac = Output_MWH/Capacity,
                #CF = mean(Capacity_Factor),
                #diff = Cap_Fac-CF
      ) %>%
      mutate(sit = case_when(str_detect(Name,"Pot")~"Hypo",
                             TRUE~"AESO"),
             Name = gsub("\\s*\\([^\\)]+\\)", "",Name),
             Name = case_when(str_detect(Name,"John D")~"John DOr",
                              TRUE~Name))
    
    ggplot() +
      geom_line(data = data, 
                aes(x = Report_Year, y = Cap_Fac,colour=Name,linetype=sit,
                    size = sit), 
      ) +
      theme(panel.background = element_rect(fill = "transparent"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            #axix.title.x = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            text = element_text(size= 15),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
      ) +
      labs(x = "", y = "Capacity Factor (%)", fill = "Potential New Resource") +
      scale_x_continuous(expand=c(0,0)) +
      scale_color_manual("Hypothetical Site",
                         values = c("steelblue","seagreen","tan",
                                    "saddlebrown","khaki","firebrick",
                                    "goldenrod","black"),
                         limits = c('Anzac','Bison Lake','Hinton','John DOr',
                                    'Kehewin',
                                    'Lesser Slave Lake','Pigeon Lake')) +
      scale_linetype_manual("",values = c("AESO"="dotdash","Hypo"="solid"),
                            labels=c("Wind farm from AESO queue",
                                     "Wind farm at hypothetical site")) +
      scale_size_manual("",values = c(0.5,2),
                        labels=c("Wind farm from AESO queue",
                                 "Wind farm at hypothetical site")) +
      guides(color = guide_legend(override.aes = list(size = 3)),
      )
  }
  
  ################################################################################
  # Annual revenue for resources
  ################################################################################
  
  Revenue <- function(case) {
    data1 <- ResourceHr %>%
      filter(Run_ID == case,
             #grepl('New Resource',Name),
             #grepl("Pot",Name),
             Primary_Fuel == "Wind",
             !is.na(Capacity_Factor),
             #Report_Year >= 2024,
             Report_Year <= 2035
      ) %>%
      mutate(#Year = (Report_Year),
             Year = as.factor(Report_Year),
      ) %>%
      group_by(Year) %>%
      mutate(
             Fleet_AveRev = sum(Revenue*1000)/sum(Output_MWH),
      ) %>%
      ungroup() %>%
      filter(grepl('New Resource',Name),
             #grepl("Pot",Name),
             ) %>%
      mutate(Name = gsub("\\s*\\([^\\)]+\\)", "",Name),
             Name = gsub("^.*?P","",Name),
             Name = gsub("[0-9] *","",Name),
             Name = gsub("^.*?_","",Name),
             Name = gsub("Joss Wind","",Name)
      ) %>%
      group_by(Name,Year) %>% #Year
      summarise(Capacity = sum(Capacity),
                Fleet_AveRev = median(Fleet_AveRev),
                Output_MWH = sum(Output_MWH),
                Cap_Fac = Output_MWH/Capacity,
                Revenue = sum(Revenue)*1000,
                AveRev = Revenue/Output_MWH,
                diff = AveRev-Fleet_AveRev,
      ) %>%
      ungroup() %>%
      group_by(Name) %>%
      summarise(#Capacity = sum(Capacity),
                #Output_MWH = sum(Output_MWH),
                Cap_Fac = mean(Cap_Fac),
                #CF = Output_MWH/Capacity,
                Revenue = mean(Revenue),
                AveRev = mean(AveRev),
                diff = mean(diff)) %>%
      mutate(sit = case_when(grepl("Anzac",Name)~"Hypo",
                             grepl("Bison Lake",Name)~"Hypo",
                             grepl("Hinton",Name)~"Hypo",
                             grepl("John D'Or",Name)~"Hypo",
                             grepl("Kehewin",Name)~"Hypo",
                             grepl("Lesser",Name)~"Hypo",
                             grepl("Pigeon",Name)~"Hypo",
                             TRUE~"Active"))
    
    sz <- 12
    CF_color<-"grey40"
    AR_color<-"black"
    
#    revmin <- min(floor(data1$AveRev),0)
#    CFmax <- max(data1$Cap_Fac)
#    revmax <- max(ceiling(data1$AveRev))
#    multifact <- CFmax/revmax
    
    ggplot(data1,
           aes(x=fct_reorder(Name,AveRev)))+
      geom_col(aes(y=Cap_Fac/0.013,colour=sit,fill=sit),#fill=CF_color,
               width=0.7,color=CF_color,alpha = 0.6,#color="black"
      ) +
      geom_col(aes(y=AveRev,fill=sit),
                width=0.4, alpha=1, color="black" #position = position_dodge(width = .85),width = .6
               #fill=AR_color
               )+
      
      geom_hline(yintercept=0) +
      #geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
      scale_color_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
                         labels = c("Active"="AESO Queue",
                                    "Hypo"="Hypothetical Site"))+
      scale_fill_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
                        labels = c("Active"="AESO Queue",
                                   "Hypo"="Hypothetical Site"))+
      #    facet_grid(~Year) +
      scale_y_continuous(expand=c(0,0),
                         limits = c(-20,45),
                         breaks = seq(-15,45, by = 10),
                         sec.axis = sec_axis(trans=~.*(0.013*100), 
                                             name ="Average Annual Capacity Factor (%)",
                                             breaks = seq(0,60,5))
                         ) +
      labs(x="",y="Average Annual Energy Revenue ($/MWh)") +
      theme(axis.text = element_text(size = sz),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y.right = element_text(color = CF_color),
            axis.title.y = element_text(size = sz,color = AR_color,face="bold"),
            axis.title.y.right = element_text(size = sz,color = CF_color,
                                              face="bold",
                                              margin=unit(c(0,0,1,0.3), "cm")
                                              ),
            
            # For transparent background
            panel.background = element_rect(fill = "transparent"),
            panel.grid = element_blank(),
            panel.spacing = unit(1.5, "lines"),
            panel.border = element_rect(colour = "black", fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.position = "top",
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
      ) 
    
  }
  
  Revenue2.0 <- function(case) {
    data1 <- ResourceHr %>%
      filter(Run_ID == case,
             #grepl('New Resource',Name),
             #grepl("Pot",Name),
             Primary_Fuel == "Wind",
             !is.na(Capacity_Factor),
             #Report_Year >= 2024,
             Report_Year <= 2035
      ) %>%
      mutate(#Year = (Report_Year),
        Year = as.factor(Report_Year),
        Status = case_when(grepl('New Resource',Name)~'New',
                           TRUE~'Old'),
        Name = gsub("\\s*\\([^\\)]+\\)", "",Name),
        Name = gsub("^.*?P","",Name),
        Name = gsub("[0-9] *","",Name),
        Name = gsub("^.*?_","",Name),
        Name = gsub("Joss Wind","",Name)
      ) %>%
      group_by(date,Name,Year,Status) %>%
      summarise(Capacity_Factor = sum(Output_MWH)/sum(Capacity),
                Output_MWH = sum(Output_MWH),
                Capacity = sum(Capacity),
                Revenue = sum(Revenue),
                ) %>%
      ungroup () %>%
#      group_by(Year) %>%
#      mutate(
#        Fleet_AveRev = sum(Revenue*1000)/sum(Output_MWH),
#      ) %>%
#      ungroup() %>%
      group_by(date) %>%
      mutate(#fleet_CF = sum(Output_MWH)/sum(Capacity),
             other_CF = (sum(Output_MWH)-Output_MWH)/(sum(Capacity)-Capacity),
             Fleet_Rev = (sum(Revenue)-Revenue)*1000,
             Fleet_gen = sum(Output_MWH)-Output_MWH,
             deviance_CF = abs(Capacity_Factor-other_CF)
             ) %>%
      filter(#grepl('New Resource',Name),
             Status == "New"
      ) %>%
      ungroup() %>%
      group_by(Name,Year) %>% #Year
      mutate(Capacity = sum(Capacity),
                Output_MWH = sum(Output_MWH),
                Revenue = sum(Revenue)*1000,
                Cap_Fac = Output_MWH/Capacity,
                #IOD = sqrt(mean(deviance_CF)),
                AveRev = Revenue/Output_MWH,
             Fleet_AveRev = sum(Fleet_Rev)/sum(Fleet_gen),
             diff = AveRev-Fleet_AveRev
      ) %>%
      ungroup() %>%
      group_by(Name) %>%
      summarise(Cap_Fac = mean(Cap_Fac),
                Revenue = mean(Revenue),
                AveRev = mean(AveRev),
                IOD = sqrt(mean(deviance_CF)),
                Start = min(as.numeric(as.character(Year))),
                diff = sum(diff)
                ) %>%
      mutate(sit = case_when(grepl("Anzac",Name)~"Hypo",
                             grepl("Bison Lake",Name)~"Hypo",
                             grepl("Hinton",Name)~"Hypo",
                             grepl("John D'Or",Name)~"Hypo",
                             grepl("Kehewin",Name)~"Hypo",
                             grepl("Lesser",Name)~"Hypo",
                             grepl("Pigeon",Name)~"Hypo",
                             TRUE~"Active"))
    
    dataA <- data1 %>%
      mutate(metric = "IOD",
             value = IOD) %>%
      subset(., select=-c(IOD,diff,Revenue))
    
    dataB <- data1 %>%
      mutate(metric = "CF",
             value = Cap_Fac) %>%
      subset(., select=-c(IOD,diff,Revenue))
    
    data2 <- rbind(dataA,dataB)
    
    sz <- 12
    CF_color<-"grey40"
      AR_color<-"black"
        
      #    revmin <- min(floor(data1$AveRev),0)
      #    CFmax <- max(data1$Cap_Fac)
      #    revmax <- max(ceiling(data1$AveRev))
      #    multifact <- CFmax/revmax
      
#      Rev <- ggplot(data1,
#             aes(x=fct_reorder(Name,AveRev)))+
        #geom_col(aes(y=IOD/0.013,colour=sit,fill=sit),#fill=CF_color,
        #         width=0.7,color=CF_color,alpha = 0.6,#color="black"
        #) +
#        geom_col(aes(y=AveRev,fill=sit),
#                 width=0.75, alpha=1, color="black" #position = position_dodge(width = .85),width = .6
                   #fill=AR_color
#        )+
        
#        geom_hline(yintercept=0) +
#        scale_color_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
#                           labels = c("Active"="AESO Queue",
#                                      "Hypo"="Hypothetical Site"))+
#        scale_fill_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
#                          labels = c("Active"="AESO Queue",
#                                     "Hypo"="Hypothetical Site"))+
        #    facet_grid(~Year) +
#        scale_y_continuous(expand=c(0,0),
#                           limits = c(-16.5,40),
#                           breaks = seq(-15,40, by = 10),
                           #sec.axis = sec_axis(trans=~.*(0.013*100), 
                          #                     name ="Average Annual Capacity Factor (%)",
                          #                     breaks = seq(0,60,5))
#        ) +
#        labs(x="",y="Energy Revenue \n($/MWh)") +
#        theme(axis.text = element_text(size = sz),
#              axis.text.x = element_blank(),
              #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#              axis.ticks.x = element_blank(),
#              axis.text.y.right = element_text(color = CF_color),
#              axis.title.y = element_text(size = sz,color = AR_color,face="bold"),
#              axis.line.y = element_line(color="black"),
              
              # For transparent background
#              panel.background = element_rect(fill = "transparent"),
#              panel.grid = element_blank(),
#              panel.spacing = unit(1.5, "lines"),
#              panel.border = element_rect(colour = "transparent", fill = "transparent"),
#              plot.background = element_rect(fill = "transparent", color = NA),
#              legend.position = "none",# "top",
              #legend.key = element_rect(colour = "transparent", fill = "transparent"),
              #legend.background = element_rect(fill='transparent'),
              #legend.box.background = element_rect(fill='transparent', colour = "transparent"),
#        ) 
      
#      iod <- ggplot(data1,
#                    aes(x=fct_reorder(Name,AveRev),
#                        y=IOD))+
#        geom_point(aes(color=sit),
                 #width=0.75, 
#                 alpha=1, #color="black" #position = position_dodge(width = .85),width = .6
                   #fill=AR_color
#        )+
#        scale_color_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
#                           labels = c("Active"="AESO Queue",
#                                      "Hypo"="Hypothetical Site"))+
#        scale_fill_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
#                          labels = c("Active"="AESO Queue",
#                                     "Hypo"="Hypothetical Site"))+
#        scale_y_continuous(expand=c(0,0),
#                           limits = c(0.45,0.57),
#                           breaks = seq(0.45,0.55, by = 0.05),
#        ) +
#        labs(x="",y="Index of \nDeviation") +
#        theme(axis.text = element_text(size = sz),
#              axis.text.x = element_blank(),#element_text(angle = 90, vjust = 0.5, hjust = 1),
#              axis.ticks.x = element_blank(),
#              axis.text.y.right = element_text(color = CF_color),
#              axis.title.y = element_text(size = sz,color = AR_color,face="bold"),
#              axis.line.y = element_line(color="black"),
              
              # For transparent background
#              panel.background = element_rect(fill = "transparent"),
#              panel.grid = element_blank(),
#              panel.spacing = unit(1.5, "lines"),
#              panel.border = element_rect(colour = "transparent", fill = "transparent"),
#              plot.background = element_rect(fill = "transparent", color = NA),
#              legend.position = "top",
#              legend.key = element_rect(colour = "transparent", fill = "transparent"),
#              legend.background = element_rect(fill='transparent'),
#              legend.box.background = element_rect(fill='transparent', colour = "transparent"),
#        ) 
      
#      Cap_Fac <- ggplot(data1,
#                    aes(x=fct_reorder(Name,AveRev),
#                        y=Cap_Fac))+
#        geom_point(aes(color=sit),
                 #width=0.75, 
#                 alpha=1,# color="black" #position = position_dodge(width = .85),width = .6
                   #fill=AR_color
#        )+
        #geom_smooth(data=data1,method='lm') +
#        scale_color_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
#                           labels = c("Active"="AESO Queue",
#                                      "Hypo"="Hypothetical Site"))+
#        scale_fill_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
#                          labels = c("Active"="AESO Queue",
#                                     "Hypo"="Hypothetical Site"))+
#        scale_y_continuous(expand=c(0,0),
#                           limits = c(0.27,0.55),
#                           breaks = seq(0.3,0.5, by = 0.1),
#        ) +
#        labs(x="",y="Capacity \nFactor (%)") +
#        theme(axis.text = element_text(size = sz),
              #axis.text.x = element_blank(),#element_text(angle = 90, vjust = 0.5, hjust = 1),
#              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#              axis.text.y.right = element_text(color = CF_color),
#              axis.title.y = element_text(size = sz,color = AR_color,face="bold"),
#              axis.line = element_line(color = "black"),
              
              # For transparent background
#              panel.background = element_rect(fill = "transparent"),
#              panel.grid = element_blank(),
#              panel.spacing = unit(1.5, "lines"),
#              panel.border = element_rect(colour = "transparent", fill = "transparent"),
#              plot.background = element_rect(fill = "transparent", color = NA),
#              legend.position = "none",# "top",
              #legend.key = element_rect(colour = "transparent", fill = "transparent"),
              #legend.background = element_rect(fill='transparent'),
              #legend.box.background = element_rect(fill='transparent', colour = "transparent"),
#        ) 
      
#      plot_grid(iod + theme(plot.margin = unit(c(0,0.25,-0.25,0),"cm")), 
                #Cap_Fac,
#                Rev + theme(plot.margin = unit(c(0,0.25,-0.25,0),"cm")),
#                Cap_Fac + theme(plot.margin = unit(c(0,0.25,0,0),"cm")),
#                ncol = 1, align="v", axis = "l",rel_heights = c(0.75,1.5,1.5))
      
      ggplot(data1,
             aes(x=fct_reorder(Name,AveRev)))+
        geom_col(aes(y=AveRev,fill=sit),
                 width=0.75, alpha=0.8, color="black" 
        )+
        geom_point(data=data2,aes(x=fct_reorder(Name,AveRev),
                                  y=value/0.013, fill=sit,shape=metric),size=4) +
        #geom_point(aes(y=Cap_Fac/0.013, color=sit),shape=16,size=4) +
        
        geom_hline(yintercept=0) +
        scale_shape_manual("",values=c("CF"=21,"IOD"=23),
                           labels = c("CF"="Average Annual \nCapacity Factor",
                                      "IOD"="Capacity Factor \nIndex of Deviation")) +
       # scale_color_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
      #                     labels = c("Active"="AESO Queue",
      #                                "Hypo"="Hypothetical Site"))+
        scale_fill_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
                          labels = c("Active"="AESO Queue",
                                     "Hypo"="Hypothetical Site"))+
        #    facet_grid(~Year) +
        scale_y_continuous(expand=c(0,0),
                           limits = c(-20,45),
                           breaks = seq(-15,45, by = 10),
                           sec.axis = sec_axis(trans=~.*(0.013), 
                                               name ="",
                                               breaks = seq(0,.60,.05))
        ) +
        labs(x="",y="Average Annual Energy Revenue ($/MWh)") +
        guides(shape=guide_legend(nrow=2,byrow=TRUE),
               fill=guide_legend(nrow=2,byrow=TRUE)) +
        theme(axis.text = element_text(size = sz),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              axis.text.y.right = element_text(color = CF_color),
              axis.title.y = element_text(size = sz,color = AR_color,face="bold"),
              axis.title.y.right = element_text(size = sz,color = CF_color,
                                                face="bold",
                                                margin=unit(c(0,0,1,0.3), "cm")
              ),
              
              # For transparent background
              panel.background = element_rect(fill = "transparent"),
              panel.grid = element_blank(),
              panel.spacing = unit(1.5, "lines"),
              panel.border = element_rect(colour = "black", fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              legend.position = "top",
              legend.key = element_rect(colour = "transparent", fill = "transparent"),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent', colour = "transparent"),
        ) 
  }
  
  Res_Value <- function(fuel,case) {
    ResVal <- Value %>%
      filter(Run_ID==case,
             Fuel_Type==fuel,
             #grepl('PotWind',Input_New_Res_ID),
             #LT_Iteration == 0 | 
               LT_Iteration == max(LT_Iteration)
             ) %>%
      subset(.,select=-c(Run_ID,Fuel_Type))
  }
  
  Sim_corr <- function(year,case) {
    
    Pric_sim <- ZH %>%
      filter(date <= as.POSIXct(paste0(year,"/12/31"),"%Y/%m/%d",tz = "MST"),
             date >= as.POSIXct(paste0(year,"/01/01"),"%Y/%m/%d",tz = "MST"),) %>%
      subset(.,select=c(date,Price))
    
    Res_CF <- ResourceHr %>%
      filter(date <= as.POSIXct(paste0(year,"/12/31"),"%Y/%m/%d",tz = "MST"),
             date >= as.POSIXct(paste0(year,"/01/01"),"%Y/%m/%d",tz = "MST"),
             Run_ID == case,
             Primary_Fuel == fuel,
             !is.na(Capacity_Factor),) %>%
      subset(.,select=c(date,Name,Output_MWH,Capacity))
    
    #data <- merge(Res_CF,Pric_sim, by="date")
    
    sim_data <- merge(Res_CF,Pric_sim, by="date") %>%
      mutate(#Status = case_when(grepl("Potential",Name)~"Hypothetical",
              #                  grepl("NewWind",Name)~"Queue",
              #                  TRUE~"Active"),
        Name = gsub("\\s*\\([^\\)]+\\)", "",Name),
        #Name = gsub("^.*?_","",Name),
        Name = gsub(".*Wind_[0-9]* ","",Name),
        Name = gsub(".*Wind_P[0-9]* ","",Name),
      ) %>%
      #group_by(Name) %>%
      #filter(n() >= 2232) %>%
      #ungroup() %>%
      group_by(Name,date) %>%
      summarise(Output_MWH = sum(Output_MWH),
                Capacity = sum(Capacity),
                count = n(),
                Price = median(Price)) #%>%
      ungroup() %>%
      group_by(date) %>%
      mutate(other_CF = (sum(Output_MWH)-Output_MWH)/(sum(Capacity)-Capacity),
             deviance_CF = abs((Output_MWH/Capacity)-other_CF),
             Revenue = Price*Output_MWH,
             #Status = median(Status)
      ) %>%
      ungroup() %>%
      group_by(Name) %>%
      summarize(sd_ID = sqrt(mean(deviance_CF)),
                Capacity = median(Capacity),
                Revenue = sum(Revenue),
                Dispatched = sum(Output_MWH),
                Capture_Price = Revenue/Dispatched) %>%
      mutate(Status=case_when(grepl("Buffalo Atlee",Name) ~ "Queue",
                                 grepl("Buffalo Plains",Name) ~ "Queue",
                                 grepl("Buffalo Trail North",Name) ~ "Queue",
                                 grepl("Buffalo Trail South",Name) ~ "Queue",
                                 grepl("Bull Trail",Name) ~ "Queue",
                                 grepl("Castle Meridian",Name) ~ "Queue",
                                 grepl("EDPR Sharp Hills",Name) ~ "Queue",
                                 grepl("Enerfin Winnifred",Name) ~ "Queue",
                                 grepl("Forty Mile Maleb",Name) ~ "Queue",
                                 grepl("Forty Mile",Name) ~ "Queue",
                                 grepl("Invenergy Schuler",Name) ~ "Queue",
                                 grepl("Lanfine North",Name) ~ "Queue",
                                 grepl("Lone Pine",Name) ~ "Queue",
                                 grepl("Northern Lights",Name) ~ "Queue",
                                 grepl("Old Elm",Name) ~ "Queue",
                                 grepl("Oyen",Name) ~ "Queue",
                                 grepl("Paintearth",Name) ~ "Queue",
                                 grepl("Renewable Energy Service",Name) ~ "Queue",
                                 grepl("Riplinger",Name) ~ "Queue",
                                 grepl("Stirling",Name) ~ "Queue",
                                 
                                 grepl("Fort Saskatchewan",Name) ~ "Hypothetical",
                                 grepl("Clear Prairie",Name) ~ "Hypothetical",
                                 grepl("Lesser Slave Lake",Name) ~ "Hypothetical",
                                 grepl("John D'Or",Name) ~ "Hypothetical",
                                 grepl("Anzac",Name) ~ "Hypothetical",
                                 grepl("Grande Cache",Name) ~ "Hypothetical",
                                 grepl("Pigeon Lake",Name) ~ "Hypothetical",
                                 grepl("Kehewin",Name) ~ "Hypothetical",
                                 grepl("Chain Lakes",Name) ~ "Hypothetical",
                                 grepl("Falher",Name) ~ "Hypothetical",
                                 grepl("Bison Lake",Name) ~ "Hypothetical",
                                 grepl("Hinton",Name) ~ "Hypothetical",
                              TRUE~"Active")) #%>%
      #filter(Status == "Hypothetical")
    
    equ <- summary(lm(Capture_Price ~ sd_ID, data=sim_data))
    
    sz = 15
    
    ch <- ggplot(sim_data, aes(x = sd_ID, y = Capture_Price, 
                              #                           colour = Status, size = Capacity
    )) +
      geom_smooth(data=filter(sim_data,Status == "Active"),
                  method='lm', #aes(colour=Status),
                  show.legend=FALSE,fullrange=TRUE) +
              #stat_regline_equation(data=filter(sim_data,Status == "Active"),
              #                      label.x=0.45,label.y=60,show.legend=FALSE) +
              #stat_cor(data=filter(sim_data,Status == "Active"),
              #         aes(label=..rr.label..), label.x=0.4,label.y=80,show.legend=FALSE) + 
      geom_point(aes(size=Capacity,shape=Status,color=Status
      )) +
      scale_color_manual(values = c("Hypothetical"="forestgreen", 
                                    "Queue"="gray",
                                    "Active"="black")) +
      scale_shape_manual(values=c(16,17,18), 
                         name = "",
                         #labels=c("Active wind farm","Hypothetical site")
                         )+
      geom_text_repel(label=sim_data$Name, size = 4,# hjust = 0, vjust = 0, 
               nudge_x = 0.001, nudge_y = 0.3
               ) +
      labs(x = paste0("Wind farm capacity factor index of deviation from fleet capacity factor in ",year),
           y = "Average Plant Revenue ($/MWh)") + 
      guides(shape = guide_legend(override.aes = list(size = 5),),
             color = guide_legend(override.aes = list(size = 5))
      ) +
      theme(text = element_text(size = sz),
            axis.line = element_line(color="black", size = 0.5),
            panel.background = element_rect(fill = "transparent"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent", 
                                                 color = "transparent"),
            legend.key = element_rect(fill="transparent"),
            rect = element_rect(fill="transparent")
      )
    
    return(list(equ,ch))
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
  
  Month4 <- function(month,case) {
    ggdraw(add_sub(ggarrange(Month14(Yr4Sp[[1]],month,case),
                             Month14(Yr4Sp[[2]],month,case),
                             Month14(Yr4Sp[[3]],month,case),
                             Month14(Yr4Sp[[4]],month,case),
                             #labels = c(Yr4Sp[[1]],Yr4Sp[[2]],Yr4Sp[[3]],Yr4Sp[[4]]),
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
                                         axis.text.x = element_blank(),
                                         ), 
                    Eval_diffcap(input,case)+theme(legend.position ="none"), 
                    ncol = 1, align="v", axis = "l",rel_heights = c(2.5,0.6,1.5))
    
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
  
  BuildUnits2 <- function(case, Fuel1, Fuel2) {
    p1 <- plot_grid(Units2(case,Fuel1,Fuel2)+theme(axis.title.x = element_blank(),
                                            axis.text.x = element_blank(),
                                            text = element_text(size= 15)),
                    Slack2(case,Fuel1,Fuel2)+theme(text = element_text(size= 15)),
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
  setwd("D:/Documents/GitHub/AuroraEval")
  ggsave(path = "images", filename = paste(name,".png", sep = ""), bg = "transparent")
}

mapsave <- function(name) {
  setwd("D:/Documents/GitHub/AuroraEval")
  ggsave(path = "images", filename = paste(name,".png", sep = ""), 
         bg = "transparent",
         dpi = 320,
         height = 25, width = 15, units = "cm")
}
