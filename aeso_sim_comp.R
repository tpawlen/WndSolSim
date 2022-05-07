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
  plot_grid(wkPrice(year,month,day) + 
              theme(axis.title.x=element_blank(),axis.text.x=element_blank()),
            Week_act(year,month,day)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5))
}

################################################################################
# Plot comparison between actual and simulated data
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
  
  ggarrange(arrangeGrob(plot_grid(week_price(year,month,day,case) + 
                                    theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = pretty_breaks(4)),
                                  Week1(year,month,day,case)+
                                    theme(legend.position ="none") + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = pretty_breaks(4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5))+
                          ggtitle(paste("Simulated Data for ",year," (",DB,")", sep = ""))+
                          theme(legend.position ="none",
                                plot.title = element_text(hjust = 0.5)),
                        
                        plot_grid(wkPrice(year,month,day) + 
                                    theme(axis.title=element_blank(),
                                          axis.text.x=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = pretty_breaks(4)),
                                  Week_act(year,month,day)+
                                    theme(legend.position ="none",
                                          axis.title.y=element_blank())+#,
                                    #     axis.text.y=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = pretty_breaks(4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5)) +
                          ggtitle(paste("AESO Data for ",year,sep=""))+
                          theme(legend.position ="none",
                                plot.title = element_text(hjust = 0.5)),
                        ncol=2, widths = c(1.05,1)),
            
            legend,
            
            #            arrangeGrob(g_legend(Week1(year,month,day,case)),
            #                       g_legend(Week_act(year,month,day)),
            #                      nrow=2),
            ncol=2, widths =c(5,1))#= unit.c(unit(1, "npc")-legend$width, legend$width))
}

################################################################################
# Plot difference between simulated and actual pool price
################################################################################

comp_dur <- function(year1, year2, case) {
  
  totSim <- ZoneH %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    group_by(Condition, Report_Year) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    select(Condition, Report_Year, Price, perc) %>%
    rename(Year = Report_Year) %>%
    ungroup() %>%
    mutate(sit = "Simulated")
  
  #  totSim$Report_Year <- as.factor(totSim$Report_Year)
  
  Actual <- na.omit(demand)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Hour <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  
  totAct <- Actual %>%
    filter(Year >= year1, 
           Year <= year2,) %>%
    mutate(Condition = if_else(between(Hour, 08, 23), 
                               "On-Peak WECC", "Off-Peak WECC")) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    select(Condition, Year, Price, perc) %>%
    ungroup() %>%
    mutate(sit = "Actual")
  
  total <- rbind(totSim, totAct)
  sz <- 15
  
  #  totAct$Year <- as.factor(totAct$Year)
  
  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Price, colour = Year, linetype = sit), size = 1) +
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
    labs(y = "Pool Price$/MWh", 
         x = "Percentage of Time", 
         title = "AESO Data vs Simulation",
         subtitle = DB) +
    scale_color_manual(values = c("goldenrod1", "forestgreen", "cornflowerblue","gray60")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

year_comp <- function(year,case) {
  # Filters for the desired case study
  
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
  # Filters for the desired case study
  
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
  # Filters for the desired case study
  
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