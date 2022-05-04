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

cap_pf <- function(plant){#, year){
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
#  start <- as.Date(paste(year,"-01-01",sep=""))
#  end <- as.Date(paste(year,"-12-31",sep=""))
  
  data <- merit_filt %>%
    filter(asset_id == plant, 
#           date >= start, 
#           date <= end
           ) %>%
    select(date, he, AESO_Name, Plant_Type, flexible, price, dispatched_mw)
  
  cap <- sub_samp %>%
    filter(ID == plant, 
#           date >= start, 
#           date <= end
           ) %>%
    select(date, he, Capacity, Cap_Fac)
  
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(C_Fac = (dispatched_mw/Capacity)) %>%
    mutate(time = as.POSIXct(paste(date, he), format="%Y-%m-%d %H")) %>%
    filter(dispatched_mw != 0)

  must_run <- data1 %>%
    filter(flexible == "N")
  
  must_run <- Mode(must_run$C_Fac)
  
  data2 <- data1 %>%
    filter(flexible != "N") %>%
    mutate(Cap_F = (must_run + C_Fac))
    
  breaks= c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  tags <- c("0-10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","90-100%")
  data2$bins <- cut(data2$Cap_F, 
                    breaks= breaks,
                    labels=tags)
  data2 <- na.omit(data2)
  
  data3 <- data2 %>%
    group_by(bins) %>%
    summarise(avPrice = mean(price))
  
  name <- data[1,3]
  type <- data[1,4]
  
  mx <- plyr::round_any(max(data3$avPrice), 50, f = ceiling)
  mn <- min(data3$avPrice)
  
  data3 <- data3 %>%
    mutate(bid_fact = round(avPrice/mn-1,3))
  
#  ggplot(data1) +
#    geom_line(aes(x = time, y = C_Fac, colour=flexible)) +
#    geom_line(aes(x = time, y = Cap_Fac), alpha = 0.3) +
#    geom_line(data = data2, aes(x = time, y = Cap_F), alpha = 0.9) +
#    theme_bw() +
#    theme(panel.background = element_rect(fill = "transparent"),
#          panel.grid = element_blank(),
#          plot.background = element_rect(fill = "transparent", color = NA),
#          text = element_text(size= 15),
#          plot.title = element_text(hjust = 0.5),
#          axis.text.x = element_text(angle = 45, hjust=1)
#    ) +
#    scale_y_continuous(expand=c(0,0))#,
#                       limits = c(0,750))# +
#    ggtitle(paste(name, type, sep = ": "))+
#    labs(x = "% of Capacity Dispatched",
#         y = "Average Pool Price \n($/MWh)")
  
  ggplot(data3, aes(x = bins, y = avPrice, group=bins)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = 
                    paste("$",round(avPrice, digits = 0),"\n",bid_fact,sep="")), 
              vjust = -0.3, size = 4)+#, angle = 90) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,mx)) +
#    ggtitle(paste(name, type, sep = ": "))+
    labs(x = "% of Capacity Dispatched",
         y = "Average Offer Price \n($/MWh)",
         title = paste(name, type, sep = ": "),
         subtitle = paste("Bids at $0 at ",round(must_run*100, digits=2),"% CF",sep=""))
}

hrc <- function(plant) {
  data <- sub_samp %>%
    filter(ID == plant) %>%
    select(time, gen, ID, AESO_Name, Plant_Type, GHG_ID, CO2, Heat.Rate, Cap_Fac)
  
  ggplot() +
    geom_line(data = data, 
              aes(x = Cap_Fac, y = Heat.Rate)) +#, colour = Year, linetype = sit), size = 1) +
#    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.title = element_blank()
    ) +
    labs(y = "Heat Rate", 
         x = "Capacity Factor", 
#         title = "AESO Data vs Simulation",
#         subtitle = DB
    ) +
    scale_color_manual(values = c("firebrick","goldenrod1", "forestgreen", "cornflowerblue","gray60")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

cap_offer <- function(plant){#, year){
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  data <- merit_filt %>%
    filter(asset_id == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, AESO_Name, Plant_Type,
           #co2_est, size, on_peak, flexible, hourly_dispatch, hourly_renewables, year, available_mw,
           price, dispatched_mw)
  
  name <- data[1,3]
  type <- data[1,4]
  
  data <- data %>%
    select(date, he, price, dispatched_mw)
  
  cap <- sub_samp %>%
    filter(ID == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(C_Fac = (dispatched_mw/Capacity)) %>%
    mutate(time = as.POSIXct(paste(date, he), format="%Y-%m-%d %H")) %>%
    select(-date,-he)
  
  if (sum(data1$price) == 0) {
    data3 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
  } else {

  data2 <- data1 %>%
    filter(price == 0) %>%
    group_by(time) %>%
    mutate(tier = 1, Cap_F = sum(C_Fac))
  
  
  data1 <- data1 %>%
    filter(price != 0) %>%
    group_by(time) %>%
    arrange(price) %>%
    mutate(tier = 2:(n()+1), Cap_F = sum(C_Fac))
  
  data3 <- rbind(data1,data2)
  }
  
  data1 <- na.omit(data3)

  data2 <- data1 %>%
    group_by(tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(C_Fac),
              )
  
  
#  mx <- plyr::round_any(max(data2$avPrice)+2, 100, f = ceiling)
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
#               min(data2$avPrice[data2$avPrice!=min(data2$avPrice)]))
  
  ifelse(mn==0,data2$bid_fact<-0,
  data2 <- data2%>%
    mutate(bid_fact = round(avPrice/mn-1,2)) 
  )
  
  data2 <- data2 %>% 
    mutate(One = avCap_Fac + ifelse(is.na(lag(avCap_Fac))==TRUE,0,lag(avCap_Fac))) %>%
    mutate(Two = avCap_Fac + ifelse(is.na(lag(One))==TRUE,0,lag(One))) %>%
    mutate(Three = avCap_Fac + ifelse(is.na(lag(Two))==TRUE,0,lag(Two))) %>%
    mutate(Four = avCap_Fac + ifelse(is.na(lag(Three))==TRUE,0,lag(Three))) %>%
    mutate(Five = avCap_Fac + ifelse(is.na(lag(Four))==TRUE,0,lag(Four))) %>%
    mutate(Six = avCap_Fac + ifelse(is.na(lag(Five))==TRUE,0,lag(Five))) %>%
    mutate(Seven = avCap_Fac + ifelse(is.na(lag(Six))==TRUE,0,lag(Six)))
  
  data2 <- data2[, c(5,6,7,8,9,10,11,1,2,3,4)]
  
#  mxtier <- ifelse(max(data2$tier)==1,2,max(data2$tier))
  mxtier <- max(data2$tier)
#  names(data2)[mxtier-1] <- "xaxis"
  names(data2)[ifelse(mxtier==1,1,mxtier-1)] <- "xaxis"
  data2$xaxis <- as.character(paste(round(data2$xaxis*100,3),"%",sep=""))
#  data2[ifelse(max(data2$tier)==1,1,mxtier),(mxtier-1)] <- 
#    paste(data2[ifelse(max(data2$tier)==1,1,mxtier),
#                mxtier-1],
#          "-100%",sep="")

  
  if(data2[ifelse(mxtier==1,1,mxtier-1),10]==0) {
    data2[ifelse(mxtier==1,1,mxtier-1),ifelse(mxtier==1,1,mxtier-1)] <- 
      paste(data2[ifelse(mxtier==1,1,mxtier-1), ifelse(mxtier==1,1,mxtier-1)],
            "~100%",sep="")
  }
  
  data2[mxtier,ifelse(mxtier==1,1,mxtier-1)] <- 
    paste(data2[mxtier,
                ifelse(mxtier==1,1,mxtier-1)],
          "-100%",sep="")
  
  ggplot(data2, 
         aes(x = reorder(xaxis,tier), y = avPrice)) +
#         aes(x = reorder(xaxis, +avPrice), y = avPrice)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = 
                    paste("$",round(avPrice, digits = 0),
                          "\nBF: ",bid_fact,sep="")), 
              vjust = -0.3, size = 3.5)+#, angle = 90) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1300)) +
    labs(x = "% of Capacity Dispatched",
         y = "Average Offer Price \n($/MWh)",
         title = paste(name, type, sep = ": "),
         )
}

cap_offermn <- function(plant){#, year){

  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(asset_id == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, month, he, AESO_Name, Plant_Type,
           price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  name <- data[1,4]
  type <- data[1,5]
  
  # Retain only necessary columns
  data <- data %>%
    select(date, month, he, price, dispatched_mw)
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(ID == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(C_Fac = (dispatched_mw/Capacity)) %>%
    mutate(time = as.POSIXct(paste(date, he), format="%Y-%m-%d %H")) %>%
    select(-date,-he)
  
  # Create new column to define the pricing tier. $0/MWh is always tier 1, and 
  # the tiers are then sorted from low to high price.
  if (sum(data1$price) == 0) {
    data3 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
  } else {
    
    data2 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
    
    
    data1 <- data1 %>%
      filter(price != 0) %>%
      group_by(time) %>%
      arrange(price) %>%
      mutate(tier = 2:(n()+1), Cap_F = sum(C_Fac))
    
    data3 <- rbind(data1,data2)
  }
  
  # Omit NA data
  data1 <- na.omit(data3)
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data2 <- data1 %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(C_Fac),
    )
  
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  ifelse(mn==0,data2$bid_fact<-0,
         data2 <- data2%>%
           mutate(bid_fact = round(avPrice/mn-1,2)) 
  )
  
  # Long and cumbersome code to calculate the building capacity factors for each 
  # month.
  data2 <- data2 %>% 
    mutate(One = avCap_Fac + ifelse(is.na(lag(avCap_Fac))==TRUE,0,lag(avCap_Fac))) %>%
    mutate(Two = avCap_Fac + ifelse(is.na(lag(One))==TRUE,0,lag(One))) %>%
    mutate(Three = avCap_Fac + ifelse(is.na(lag(Two))==TRUE,0,lag(Two))) %>%
    mutate(Four = avCap_Fac + ifelse(is.na(lag(Three))==TRUE,0,lag(Three))) %>%
    mutate(Five = avCap_Fac + ifelse(is.na(lag(Four))==TRUE,0,lag(Four))) %>%
    mutate(Six = avCap_Fac + ifelse(is.na(lag(Five))==TRUE,0,lag(Five))) %>%
    mutate(Seven = avCap_Fac + ifelse(is.na(lag(Six))==TRUE,0,lag(Six)))
  
  # Rearrange the columns to put the previously added columns at the front
  data2 <- data2[, c(6,7,8,9,10,11,12,1,2,3,4,5)]
  
  # Identify the largest tier
  mxtier <- max(data2$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  # Rename the column with the correct capacity factors to the 'xaxis' and change
  # column format to character for readability
  names(data2)[mx2tier] <- "xaxis"
  data2$xaxis <- as.character(paste(round(data2$xaxis*100,3),"%",sep=""))
  
  # Remove extra columns
  data2 <- data2[,c(mx2tier,8,9,10,11,12)]
  
  # Relabel the second top tier if it was never dispatched ie. it was another
  # maximum price
  data4 <- data2 %>%
    group_by(month) %>%
    filter(tier == max(tier[tier!=max(tier)])) %>%
    mutate(xaxis = ifelse(avCap_Fac == 0, paste(xaxis,"~100%",sep=""),xaxis))
  
  # Relabel the top tier to be a range to 100%
  data3 <- data2 %>%
    group_by(month) %>%
    filter(tier == max(tier)) %>%
    mutate(xaxis = paste(xaxis,"-100%",sep=""))
  
  data2 <- data2 %>%
    group_by(month) %>%
    filter(tier != max(tier),tier != max(tier[tier!=max(tier)]))
  
  # Recombine the data
  data1 <- rbind(data2,data3,data4)
  
  # Month list for plot labels
  mons <- list(
    "1" = "January",
    "2" = "February",
    "3" = "March",
    "4" = "April",
    "5" = "May",
    "6" = "June",
    "7" = "July",
    "8" = "August",
    "9" = "September",
    "10"= "October",
    "11"= "November",
    "12"= "December"
  )
  
  var_label <- function(variable,value){
    return(mons[value])
  }
  
  # Plot the data
  ggplot(data1, 
         aes(x = reorder(xaxis,tier), y = avPrice)) +
    geom_bar(stat="identity") +
    facet_wrap(~month, scales="free_x", labeller=var_label) +
    geom_text(aes(label = 
                    paste("$",round(avPrice, digits = 0),
#                          "\nBF: ",
                          "\n",bid_fact,sep="")), 
              vjust = -0.3, size = 3.5)+#, angle = 90) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1, size = 8)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1300)) +
    labs(x = "% of Capacity Dispatched",
         y = "Average Offer Price \n($/MWh)",
         title = paste(name, type, sep = ": "),
    )
}

cdata <- function(plant,parameter){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(asset_id == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, month, he, AESO_Name, Plant_Type,
           price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  name <- data[1,4]
#  type <- data[1,5]
  
  # Retain only necessary columns
  data <- data %>%
    select(date, month, he, price, dispatched_mw)
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(ID == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(C_Fac = (dispatched_mw/Capacity)) %>%
    mutate(time = as.POSIXct(paste(date, he), format="%Y-%m-%d %H")) %>%
    select(-date,-he)
  
  # Create new column to define the pricing tier. $0/MWh is always tier 1, and 
  # the tiers are then sorted from low to high price.
  if (sum(data1$price) == 0) {
    data3 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
  } else {
    
    data2 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
    
    
    data1 <- data1 %>%
      filter(price != 0) %>%
      group_by(time) %>%
      arrange(price) %>%
      mutate(tier = 2:(n()+1), Cap_F = sum(C_Fac))
    
    data3 <- rbind(data1,data2)
  }
  
  # Omit NA data
  data1 <- na.omit(data3)
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data2 <- data1 %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(C_Fac),
    )
  
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  ifelse(mn==0,data2$bid_fact<-0,
         data2 <- data2%>%
           mutate(bid_fact = round(avPrice/mn-1,2)) 
  )
  
  # Long and cumbersome code to calculate the building capacity factors for each 
  # month.
  data2 <- data2 %>% 
    mutate(One = avCap_Fac + ifelse(is.na(lag(avCap_Fac))==TRUE,0,lag(avCap_Fac))) %>%
    mutate(Two = avCap_Fac + ifelse(is.na(lag(One))==TRUE,0,lag(One))) %>%
    mutate(Three = avCap_Fac + ifelse(is.na(lag(Two))==TRUE,0,lag(Two))) %>%
    mutate(Four = avCap_Fac + ifelse(is.na(lag(Three))==TRUE,0,lag(Three))) %>%
    mutate(Five = avCap_Fac + ifelse(is.na(lag(Four))==TRUE,0,lag(Four))) %>%
    mutate(Six = avCap_Fac + ifelse(is.na(lag(Five))==TRUE,0,lag(Five))) %>%
    mutate(Seven = avCap_Fac + ifelse(is.na(lag(Six))==TRUE,0,lag(Six)))
  
  # Rearrange the columns to put the previously added columns at the front
  data2 <- data2[, c(6,7,8,9,10,11,12,1,2,3,4,5)]
  
  # Identify the largest tier
  mxtier <- max(data2$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  # Rename the column with the correct capacity factors to the 'xaxis' and change
  # column format to character for readability
  names(data2)[mx2tier] <- "xaxis"
  data2$xaxis <- round(data2$xaxis*100,3)
  
  # Remove extra columns
  data2 <- data2[,c(mx2tier,8,9,10,11,12)]
  
  # Relabel the second top tier if it was never dispatched ie. it was another
  # maximum price
#  data4 <- data2 %>%
#    group_by(month) %>%
#    filter(tier == max(tier[tier!=max(tier)])) %>%
#    mutate(xaxis = ifelse(avCap_Fac == 0, paste(xaxis,"~100%",sep=""),xaxis))
  
  # Relabel the top tier to be a range to 100%
#  data3 <- data2 %>%
#    group_by(month) %>%
#    filter(tier == max(tier)) %>%
#    mutate(xaxis = paste(xaxis,"-100%",sep=""))
#  
#  data2 <- data2 %>%
#    group_by(month) %>%
#    filter(tier != max(tier),tier != max(tier[tier!=max(tier)]))
  
  # Recombine the data
#  data1 <- rbind(data2,data3,data4)
  
#  data <- data2 %>%
#    filter(tier == tr) %>%
#    arrange(month)
  
  colkeep <- ifelse(parameter == "xaxis", 1,6)
  
#  data1 <- t(data[,c(1,6)])
  data1 <- data2[,c(3,2,colkeep)]
  data3 <- reshape2::dcast(data1,tier~month)

   
#  colnames(data1) <- data$month
  
  setwd("F:/My Drive/transfer")
  write.csv(data3, "Incremental_Bid_Factors.csv")
}

cap_box <- function(plant){#, year){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(asset_id == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, month, he, AESO_Name, Plant_Type,
           price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  name <- data[1,4]
  type <- data[1,5]
  
  # Retain only necessary columns
  data <- data %>%
    select(date, month, he, price, dispatched_mw)
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(ID == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(C_Fac = (dispatched_mw/Capacity)) %>%
    mutate(time = as.POSIXct(paste(date, he), format="%Y-%m-%d %H")) %>%
    select(-date,-he)
  
  # Create new column to define the pricing tier. $0/MWh is always tier 1, and 
  # the tiers are then sorted from low to high price.
  if (sum(data1$price) == 0) {
    data3 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
  } else {
    
    data2 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
    
    
    data1 <- data1 %>%
      filter(price != 0) %>%
      group_by(time) %>%
      arrange(price) %>%
      mutate(tier = 2:(n()+1), Cap_F = sum(C_Fac))
    
    data3 <- rbind(data1,data2)
  }
  
  # Omit NA data
  data1 <- na.omit(data3)
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data2 <- data1 %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(C_Fac),
    )
  
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  ifelse(mn==0,data2$bid_fact<-0,
         data2 <- data2%>%
           mutate(bid_fact = round(avPrice/mn-1,2)) 
  )
  
  # Long and cumbersome code to calculate the building capacity factors for each 
  # month.
  data2 <- data2 %>% 
    mutate(One = avCap_Fac + ifelse(is.na(lag(avCap_Fac))==TRUE,0,lag(avCap_Fac))) %>%
    mutate(Two = avCap_Fac + ifelse(is.na(lag(One))==TRUE,0,lag(One))) %>%
    mutate(Three = avCap_Fac + ifelse(is.na(lag(Two))==TRUE,0,lag(Two))) %>%
    mutate(Four = avCap_Fac + ifelse(is.na(lag(Three))==TRUE,0,lag(Three))) %>%
    mutate(Five = avCap_Fac + ifelse(is.na(lag(Four))==TRUE,0,lag(Four))) %>%
    mutate(Six = avCap_Fac + ifelse(is.na(lag(Five))==TRUE,0,lag(Five))) %>%
    mutate(Seven = avCap_Fac + ifelse(is.na(lag(Six))==TRUE,0,lag(Six)))
  
  # Rearrange the columns to put the previously added columns at the front
  data2 <- data2[, c(6,7,8,9,10,11,12,1,2,3,4,5)]
  
  # Identify the largest tier
  mxtier <- max(data2$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  # Rename the column with the correct capacity factors to the 'xaxis' and change
  # column format to character for readability
  names(data2)[mx2tier] <- "xaxis"
  data2$xaxis <- as.character(paste(round(data2$xaxis*100,3),"%",sep=""))
  
  # Remove extra columns
  data2 <- data2[,c(mx2tier,8,9,10,11,12)]
  
  # Relabel the second top tier if it was never dispatched ie. it was another
  # maximum price
  data4 <- data2 %>%
    group_by(month) %>%
    filter(tier == max(tier[tier!=max(tier)])) %>%
    mutate(xaxis = ifelse(avCap_Fac == 0, paste(xaxis,"~100%",sep=""),xaxis))
  
  # Relabel the top tier to be a range to 100%
  data3 <- data2 %>%
    group_by(month) %>%
    filter(tier == max(tier)) %>%
    mutate(xaxis = paste(xaxis,"-100%",sep=""))
  
  data2 <- data2 %>%
    group_by(month) %>%
    filter(tier != max(tier),tier != max(tier[tier!=max(tier)]))
  
  # Recombine the data
  data1 <- rbind(data2,data3,data4)
  
  # Month list for plot labels
  mons <- list(
    "1" = "January",
    "2" = "February",
    "3" = "March",
    "4" = "April",
    "5" = "May",
    "6" = "June",
    "7" = "July",
    "8" = "August",
    "9" = "September",
    "10"= "October",
    "11"= "November",
    "12"= "December"
  )
  
  var_label <- function(variable,value){
    return(mons[value])
  }
  
  # Plot the data
  ggplot(data1, 
         aes(x = reorder(xaxis,tier), y = avPrice)) +
    geom_bar(stat="identity") +
    facet_wrap(~month, scales="free_x", labeller=var_label) +
    geom_text(aes(label = 
                    paste("$",round(avPrice, digits = 0),
                          #                          "\nBF: ",
                          "\n",bid_fact,sep="")), 
              vjust = -0.3, size = 3.5)+#, angle = 90) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1, size = 8)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1300)) +
    labs(x = "% of Capacity Dispatched",
         y = "Average Offer Price \n($/MWh)",
         title = paste(name, type, sep = ": "),
    )
}

Cap3 <- function(plant1,plant2,plant3) {
#  MXa <- plyr::round_any(
#    max(layer_scales(cap_offer(plant1))$y$range$range,
#        layer_scales(cap_offer(plant2))$y$range$range,
#        layer_scales(cap_offer(plant3))$y$range$range)+200,
#    100, f = ceiling)
#  MX <- 1300
  
  ggarrange(cap_offer(plant1), # + 
#              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
#                                 breaks = pretty_breaks(4)), 
            cap_offer(plant2), # + 
#              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
#                                 breaks = pretty_breaks(4)), 
            cap_offer(plant3), # + 
#              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
#                                 breaks = pretty_breaks(4)), 
            ncol = 2, nrow = 2)
}

Cap4 <- function(plant1,plant2,plant3,plant4) {
#  MXa <- plyr::round_any(
#    max(layer_scales(cap_offer(plant1))$y$range$range,
#        layer_scales(cap_offer(plant2))$y$range$range,
#        layer_scales(cap_offer(plant3))$y$range$range,
#        layer_scales(cap_offer(plant4))$y$range$range)+200,
#    100, f = ceiling)
  #
#  MX <- 1300
  
  ggarrange(cap_offer(plant1), #+ 
#              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
#                                 breaks = pretty_breaks(4)), 
            cap_offer(plant2), #+ 
#              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
#                                 breaks = pretty_breaks(4)),
            cap_offer(plant3), #+ 
#              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
#                                 breaks = pretty_breaks(4)),
            cap_offer(plant4), #+ 
#              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
#                                 breaks = pretty_breaks(4)),
            ncol = 2, nrow = 2)
}
