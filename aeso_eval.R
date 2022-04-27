################################################################################
################################################################################
# Function to plot actual AESO Output data
################################################################################
################################################################################

# Identify Specific Plant traits
################################################################################
plnt_tr <- function(Asset_ID) {
  plnt <- sub_samp %>%
    filter(ID == Asset_ID) %>%
    subset(., select = c(time, Price, gen, Capacity, Plant_Type, AESO_Name, CO2, Heat.Rate, Revenue, Cap_Fac))
}

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