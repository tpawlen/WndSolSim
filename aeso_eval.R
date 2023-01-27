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

zero_bid <- function(plant_type) {
  CogenA <- sub_samp %>%
    filter(Plant_Type == plant_type, 
           #         time >= as.Date("2020-01-1")
    ) %>%
    mutate(hour = he) %>%
    subset(., select = c(time, date, hour, ID, gen, Capacity))
  
  CogenB <- merit_filt %>%
    filter(Plant_Type == plant_type,
           #         date >= as.Date("2020-01-1")
    ) %>%
    mutate(#time = paste0(date," ", hour-1, ":00:00"),
      ID = asset_id) %>%
    subset(., select = c(date,hour, AESO_Name, ID, available_mw, dispatched_mw, 
                         size, flexible, price))
  
  Cogen <- merge(CogenA, CogenB, by = c("date","hour","ID"))
  
  BidZero<- Cogen %>%
    filter(price == 0) %>%
    group_by(ID, AESO_Name) %>%
    summarise(#dispatched = mean(dispatched_mw), 
              available = mean(available_mw),
              Capacity = mean(Capacity)) %>%
    mutate(#dis_Percent = dispatched/Capacity*100,
           Percent = available/Capacity*100)
  
  if (plant_type == "COGEN" | plant_type == "SCGT") {
    BidZero <- BidZero %>%
      mutate(Heat_Rate = round((5357.1*(Percent/100)^2-11150*(Percent/100)+15493),digits=0),
             Percent = round(Percent,0),
             Heat_Rate5 = (5357.1*((Percent+5)/100)^2-11150*((Percent+5)/100)+15493),
             Heat_Rate100 = (5357.1-11150+15493))
  }
  
  if (plant_type == "COAL") {
    BidZero <- BidZero %>%
      mutate(Heat_Rate = round((1517.9*(Percent/100)^2-3233.9*(Percent/100)+11136),digits=0),
             Percent = round(Percent,0),
             Heat_Rate5 = (1517.9*((Percent+5)/100)^2-3233.9*((Percent+5)/100)+11136),
             Heat_Rate100 = (1517.9-3233.9+11136))
  } 
  
  if (plant_type == "NGCC") {
    BidZero <- BidZero %>%
      mutate(Heat_Rate = round((5803.6*(Percent/100)^2-11034*(Percent/100)+12800),digits=0),
             Percent = round(Percent,0),
             Heat_Rate5 = (5803.6*((Percent+5)/100)^2-11034*((Percent+5)/100)+12800),
             Heat_Rate100 = (5803.6-11034+12800))
  }
  
  if (plant_type == "NGCONV") {
    BidZero <- BidZero %>%
      mutate(Heat_Rate = round((892.86*(Percent/100)^2-2610.7*(Percent/100)+11521),digits=0),
             Percent = round(Percent,0),
             Heat_Rate5 = (892.86*((Percent+5)/100)^2-2610.7*((Percent+5)/100)+11521),
             Heat_Rate100 = (892.86-2610.7+11521))
  }
  
#  Max <- Cogen %>% 
#    filter(price >=900) %>%
#    group_by(ID, AESO_Name) %>%
#    summarise(max_run = mean(available_mw), Capacity = mean(Capacity), price = mean(price)) %>%
#    mutate(Percent = (1-max_run/Capacity)*100)
  
  
  setwd("G:/My Drive/transfer")
  write.csv(BidZero, paste0(plant_type,"_Bid_Zero.csv"))
}

hockey_stick <- function(plant_type) {
  # Code to determine the bidding behavior of different technologies by 
  # identifying the capacity factors for various offers, and separating them into
  # bins, then taking the median value. This is the plot for the box and whisker plots.
  # The bid factor is identified as this median value divided by the overall 
  # average minus one, and is shown in the plots as a label.
  
  # Gather the data
  dataA <- sub_samp %>%
    filter(Plant_Type == plant_type, 
    ) %>%
    mutate(hour = he) %>%
    subset(., select = c(time, date, hour, ID, Capacity))
  
  dataB <- merit_filt %>%
    filter(Plant_Type == plant_type,
    ) %>%
    mutate(ID = asset_id) %>%
    subset(., select = c(date,hour, ID, available_mw, size, block_number,price))
  
  # Combine the data
  data <- merge(dataA, dataB, by = c("date","hour","ID")) 
  
  # Combine individual plant data and calculate the capacity factors for the 
  # different offers.
  data <- data %>%
    group_by(time,price) %>%
    summarise(available = sum(size)) %>%
    ungroup() %>%
    group_by(time) %>%
    arrange(price) %>%
    mutate(csum = cumsum(available),
           Capacity = max(csum),
           Cap_Fac = csum/Capacity)
  
  # Remove duplicate rows
  data <- data[!duplicated(data[c(2,6)]),]
  
  
  # Identify the bins. 
#  tags <- c("[0-10%)","[10-20%)","[20-30%)","[30-40%)","[40-50%)","[50-60%)",
#            "[60-70%)","[70-80%)","[80-90%)","[90-100%]")
  
  tags1 <- c("[0-05%)","[05-10%)","[10-15%)","[15-20%)","[20-25%)","[25-30%)",
             "[30-35%)","[35-40%)","[40-45%)","[45-50%)","[50-55%)","[55-60%)",
            "[60-65%)","[65-70%)","[70-75%)","[75-80%)","[80-85%)","[85-90%)",
            "[90-95%)","[95-100%]")
  
  # Separate the data into the bins
#  data3 <- data1 %>%
#    mutate(tag = case_when(
#      Cap_Fac < 0.1 ~ tags[1],
#      Cap_Fac >= 0.1 & Cap_Fac < 0.2 ~ tags[2],
#      Cap_Fac >= 0.2 & Cap_Fac < 0.3 ~ tags[3],
#      Cap_Fac >= 0.3 & Cap_Fac < 0.4 ~ tags[4],
#      Cap_Fac >= 0.4 & Cap_Fac < 0.5 ~ tags[5],
#      Cap_Fac >= 0.5 & Cap_Fac < 0.6 ~ tags[6],
#      Cap_Fac >= 0.6 & Cap_Fac < 0.7 ~ tags[7],
#      Cap_Fac >= 0.7 & Cap_Fac < 0.8 ~ tags[8],
#      Cap_Fac >= 0.8 & Cap_Fac < 0.9 ~ tags[9],
#      Cap_Fac >= 0.9 & Cap_Fac <= 1 ~ tags[10]
#    ))
  
  data <- data %>%
    mutate(tag = case_when(
      Cap_Fac < 0.05 ~ tags1[1],
      Cap_Fac >= 0.05 & Cap_Fac < 0.1 ~ tags1[2],
      Cap_Fac >= 0.1 & Cap_Fac < 0.15 ~ tags1[3],
      Cap_Fac >= 0.15 & Cap_Fac < 0.2 ~ tags1[4],
      Cap_Fac >= 0.2 & Cap_Fac < 0.25 ~ tags1[5],
      Cap_Fac >= 0.25 & Cap_Fac < 0.3 ~ tags1[6],
      Cap_Fac >= 0.3 & Cap_Fac < 0.35 ~ tags1[7],
      Cap_Fac >= 0.35 & Cap_Fac < 0.4 ~ tags1[8],
      Cap_Fac >= 0.4 & Cap_Fac < 0.45 ~ tags1[9],
      Cap_Fac >= 0.45 & Cap_Fac < 0.5 ~ tags1[10],
      Cap_Fac >= 0.5 & Cap_Fac < 0.55 ~ tags1[11],
      Cap_Fac >= 0.55 & Cap_Fac < 0.6 ~ tags1[12],
      Cap_Fac >= 0.6 & Cap_Fac < 0.65 ~ tags1[13],
      Cap_Fac >= 0.65 & Cap_Fac < 0.7 ~ tags1[14],
      Cap_Fac >= 0.7 & Cap_Fac < 0.75 ~ tags1[15],
      Cap_Fac >= 0.75 & Cap_Fac < 0.8 ~ tags1[16],
      Cap_Fac >= 0.8 & Cap_Fac < 0.85 ~ tags1[17],
      Cap_Fac >= 0.85 & Cap_Fac < 0.9 ~ tags1[18],
      Cap_Fac >= 0.9 & Cap_Fac < 0.95 ~ tags1[19],
      Cap_Fac >= 0.95 & Cap_Fac <= 1 ~ tags1[20]
    ))
  
  tot_ave <- median(data$price)
#  tot_ave <- median(data4$price)
  
  # Summarize the median prices and bid factors.
  means <- data %>%
    group_by(tag) %>%
    summarize(ave = mean(price),
              med = median(price),
              bid = med/tot_ave-1) %>%
    mutate_if(is.numeric,round,2)
  
  # Remove duplicate rows
  means <- means[!duplicated(means[c(1,2,3,4)]),]
  
  # Plot the data
  ggplot(data = data, mapping = aes(x=tag,y=price)) +
    geom_jitter(color = "gray", alpha=0.2) + 
    geom_boxplot(fill="gray",color="black",alpha=0.3) +
#    geom_text(data = means, aes(label = dollar(round(med, digits=0)),hjust=1.2, 
#                                y = med+90)) +
    geom_label(data = means, aes(label = bid, vjust="inward", y = ave),size=5) +
    labs(x='Capacity Factor') + 
    theme_bw() +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 15)
    )  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1000),
                       n.breaks = 8) +
    labs(x = "Capacity Factor",
         y = "Offer Price ($/MWh)",
         title = paste(plant_type, "Bidding Behaviour", sep = " "),
         )
  
  # Unused code to plot simple scatter plot of offers.
#  test2 <- test1 %>%
#    group_by(Cap_Fac) %>%
#    summarize(q95_price=quantile(price, probs=c(0.95)),
#              q5_price=quantile(price, probs=c(0.05)),
#              mean_price=ave(price))
  
#  ggplot(test2) + 
#    geom_line(aes(Cap_Fac,mean_price)) +
#    geom_ribbon(aes(Cap_Fac,ymax=q95_price,ymin=q5_price),alpha=0.5) +
#    theme_bw() +
#    theme(panel.background = element_rect(fill = "transparent"),
#          panel.grid = element_blank(),
#          plot.background = element_rect(fill = "transparent", color = NA),
#          text = element_text(size= 15),
#          plot.title = element_text(hjust = 0.5),
#          plot.subtitle = element_text(hjust = 0.5),
#          axis.text.x = element_text(angle = 45, hjust=1)
#    ) +
#    scale_y_continuous(expand=c(0,0),
#                       limits = c(0,1000)) +
#    labs(x = "Capacity Factor",
#         y = "Offer Price ($/MWh)",
#         title = paste(plant_type, "Bidding Behaviour", sep = " "),
#    )

#  test <- Cogen %>%
#    group_by(time, ID) %>%
#    arrange(block_number) %>%
#    mutate(csum = cumsum(Cap_Fac)) %>%
#    group_by(block_number,price,csum) %>%
#    summarise(block_number = median(block_number),
#              price = median(price),
#              csum = median(csum))
  
  #fit polynomial regression models up to degree 5
#  fit1 <- lm(price~Cap_Fac, data=test1)
#  fit2 <- lm(price~poly(Cap_Fac,2,raw=TRUE), data=test1)
#  fit3 <- lm(price~poly(Cap_Fac,3,raw=TRUE), data=test1)
#  fit4 <- lm(price~poly(Cap_Fac,4,raw=TRUE), data=test1)
#  fit5 <- lm(price~poly(Cap_Fac,5,raw=TRUE), data=test1)
#  fitlog <- lm(log(price)~Cap_Fac, data=test1)
#  fitexp <- lm(price~exp(Cap_Fac), data=test1)
#  log.model <- lm(log(price) ~ Cap_Fac, data=test1)
  
  #calculated adjusted R-squared of each model
#  summary(fit1)$adj.r.squared
#  summary(fit2)$adj.r.squared
#  summary(fit3)$adj.r.squared
#  summary(fit4)$adj.r.squared
#  summary(fit5)$adj.r.squared
  
#  plot(test1$Cap_Fac, test1$price, pch=19, xlab='Capacity Factor', ylab='Price ($/MWh)')
  
#  x_axis <- seq(0,1, length=15)
  
  #add curve of each model to plot
#  lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
#  lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
#  lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
#  lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
#  lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
  
#  Cap_Fac <- c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,
#               0.75,0.8,0.85,0.9,0.95,1)
#  price <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,15,20,25,1000) #COGEN
#  price <- c(0,0,0,0,0,0,0,0,0,5,5,10,15,15,15,20,20,25,30,1000) #COAL
#  price <-c(0,30,30,30,30,30,30,30,30,30,30,35,40,45,50,55,65,75,1000,1000) #SCGT
#  price <-c(0,0,0,0,0,0,0,5,5,5,10,10,10,15,20,25,30,35,40,1000) #NGCC
#  Aurora <- data.frame(Cap_Fac, price)
  
#  gg<-ggplot(test1, aes(x = Cap_Fac, y=price)) + 
#    geom_point() +
##    geom_point(data=Aurora, color="red") +
##    stat_smooth(method = 'nls', formula = 'y~a^(b*x+c)',
##                method.args = list(start=c(a=20, b=10, c=-9)), se=FALSE) +
##    geom_line(mapping=aes(y=my))+
##    geom_smooth(method = 'lm', aes(color="Exp Model"), formula=(y ~ exp(x)), 
##                se = FALSE, linetype = 1) +
#    theme_bw() +
#    theme(panel.background = element_rect(fill = "transparent"),
#          panel.grid = element_blank(),
#          plot.background = element_rect(fill = "transparent", color = NA),
#          text = element_text(size= 15),
#          plot.title = element_text(hjust = 0.5),
#          plot.subtitle = element_text(hjust = 0.5),
#          axis.text.x = element_text(angle = 45, hjust=1)
#    ) +
#    scale_y_continuous(expand=c(0,0),
#                       limits = c(0,1000),
#                       n.breaks = 8) +
    #    ggtitle(paste(name, type, sep = ": "))+
#    labs(x = "Capacity Factor",
#         y = "Offer Price ($/MWh)",
#         title = paste(plant_type, "Bidding Behaviour", sep = " "),
##         subtitle = paste("Bids at $0 at ",round(must_run*100, digits=2),"% CF",sep="")
#)# +
#    lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
  
#    setwd("G:/My Drive/transfer")
#    write.csv(test1, paste0(plant_type, "_Bid_Behaviour.csv"))

}

hockey_stick10 <- function(plant_type) {
  # Code to determine the bidding behavior of different technologies by 
  # identifying the capacity factors for various offers, and separating them into
  # bins, then taking the median value. This is the plot for the box and whisker plots.
  # The bid factor is identified as this median value divided by the overall 
  # average minus one, and is shown in the plots as a label.
  
  # Gather the data
  dataA <- sub_samp %>%
    filter(Plant_Type == plant_type, 
    ) %>%
    mutate(hour = he) %>%
    subset(., select = c(time, date, hour, ID, Capacity))
  
  dataB <- merit_filt %>%
    filter(Plant_Type == plant_type,
    ) %>%
    mutate(ID = asset_id) %>%
    subset(., select = c(date,hour, ID, available_mw, size, block_number,price))
  
  # Combine the data
  data <- merge(dataA, dataB, by = c("date","hour","ID")) 
  
  # Combine individual plant data and calculate the capacity factors for the 
  # different offers.
  data1 <- data %>%
    group_by(time,price) %>%
    summarise(available = sum(size)) %>%
    ungroup() %>%
    group_by(time) %>%
    arrange(price) %>%
    mutate(csum = cumsum(available),
           Capacity = max(csum),
           Cap_Fac = csum/Capacity)
  
  # Remove duplicate rows
  data1 <- data1[!duplicated(data1[c(2,6)]),]
  
  
  # Identify the bins. 
  tags <- c("[0-10%)","[10-20%)","[20-30%)","[30-40%)","[40-50%)","[50-60%)",
            "[60-70%)","[70-80%)","[80-90%)","[90-100%]")
  
  #  tags1 <- c("[0-05%)","[05-10%)","[10-15%)","[15-20%)","[20-25%)","[25-30%)",
  #             "[30-35%)","[35-40%)","[40-45%)","[45-50%)","[50-55%)","[55-60%)",
  #            "[60-65%)","[65-70%)","[70-75%)","[75-80%)","[80-85%)","[85-90%)",
  #            "[90-95%)","[95-100%]")
  
  # Separate the data into the bins
  data3 <- data1 %>%
    mutate(tag = case_when(
      Cap_Fac < 0.1 ~ tags[1],
      Cap_Fac >= 0.1 & Cap_Fac < 0.2 ~ tags[2],
      Cap_Fac >= 0.2 & Cap_Fac < 0.3 ~ tags[3],
      Cap_Fac >= 0.3 & Cap_Fac < 0.4 ~ tags[4],
      Cap_Fac >= 0.4 & Cap_Fac < 0.5 ~ tags[5],
      Cap_Fac >= 0.5 & Cap_Fac < 0.6 ~ tags[6],
      Cap_Fac >= 0.6 & Cap_Fac < 0.7 ~ tags[7],
      Cap_Fac >= 0.7 & Cap_Fac < 0.8 ~ tags[8],
      Cap_Fac >= 0.8 & Cap_Fac < 0.9 ~ tags[9],
      Cap_Fac >= 0.9 & Cap_Fac <= 1 ~ tags[10]
    ))
  
  #  data4 <- data1 %>%
  #    mutate(tag = case_when(
  #      Cap_Fac < 0.05 ~ tags[1],
  #      Cap_Fac >= 0.05 & Cap_Fac < 0.1 ~ tags1[2],
  #      Cap_Fac >= 0.1 & Cap_Fac < 0.15 ~ tags1[3],
  #      Cap_Fac >= 0.15 & Cap_Fac < 0.2 ~ tags1[4],
  #      Cap_Fac >= 0.2 & Cap_Fac < 0.25 ~ tags1[4],
  #      Cap_Fac >= 0.25 & Cap_Fac < 0.3 ~ tags1[5],
  #      Cap_Fac >= 0.3 & Cap_Fac < 0.35 ~ tags1[6],
  #      Cap_Fac >= 0.35 & Cap_Fac < 0.4 ~ tags1[7],
  #      Cap_Fac >= 0.4 & Cap_Fac < 0.45 ~ tags1[8],
  #      Cap_Fac >= 0.45 & Cap_Fac < 0.5 ~ tags1[9],
  #      Cap_Fac >= 0.5 & Cap_Fac < 0.55 ~ tags1[10],
  #      Cap_Fac >= 0.55 & Cap_Fac < 0.6 ~ tags1[11],
  #      Cap_Fac >= 0.6 & Cap_Fac < 0.65 ~ tags1[12],
  #      Cap_Fac >= 0.65 & Cap_Fac < 0.7 ~ tags1[13],
  #      Cap_Fac >= 0.7 & Cap_Fac < 0.75 ~ tags1[14],
  #      Cap_Fac >= 0.75 & Cap_Fac < 0.8 ~ tags1[15],
  #      Cap_Fac >= 0.8 & Cap_Fac < 0.85 ~ tags1[16],
  #      Cap_Fac >= 0.85 & Cap_Fac < 0.9 ~ tags1[17],
  #      Cap_Fac >= 0.9 & Cap_Fac < 0.95 ~ tags1[18],
  #      Cap_Fac >= 0.95 & Cap_Fac <= 1 ~ tags1[19]
  #    ))
  
  tot_ave <- median(data3$price)
  #  tot_ave <- median(data4$price)
  
  # Summarize the median prices and bid factors.
  means <- data3 %>%
    group_by(tag) %>%
    summarize(ave = mean(price),
              med = median(price),
              bid = med/tot_ave-1) %>%
    mutate_if(is.numeric,round,2)
  
  # Plot the data
  ggplot(data = data3, mapping = aes(x=tag,y=price)) +
    geom_jitter(color = "gray", alpha=0.2) + 
    geom_boxplot(fill="gray",color="black",alpha=0.3) +
    #    geom_text(data = means, aes(label = dollar(round(med, digits=0)),hjust=1.2, 
    #                                y = med+90)) +
    geom_label(data = means, aes(label = bid, vjust="inward", y = ave),size=5) +
    labs(x='Capacity Factor') + 
    theme_bw() +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 15)
    )  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1000),
                       n.breaks = 8) +
    labs(x = "Capacity Factor",
         y = "Offer Price ($/MWh)",
         title = paste(plant_type, "Bidding Behaviour", sep = " "),
    )
  
  # Unused code to plot simple scatter plot of offers.
  #  test2 <- test1 %>%
  #    group_by(Cap_Fac) %>%
  #    summarize(q95_price=quantile(price, probs=c(0.95)),
  #              q5_price=quantile(price, probs=c(0.05)),
  #              mean_price=ave(price))
  
  #  ggplot(test2) + 
  #    geom_line(aes(Cap_Fac,mean_price)) +
  #    geom_ribbon(aes(Cap_Fac,ymax=q95_price,ymin=q5_price),alpha=0.5) +
  #    theme_bw() +
  #    theme(panel.background = element_rect(fill = "transparent"),
  #          panel.grid = element_blank(),
  #          plot.background = element_rect(fill = "transparent", color = NA),
  #          text = element_text(size= 15),
  #          plot.title = element_text(hjust = 0.5),
  #          plot.subtitle = element_text(hjust = 0.5),
  #          axis.text.x = element_text(angle = 45, hjust=1)
  #    ) +
  #    scale_y_continuous(expand=c(0,0),
  #                       limits = c(0,1000)) +
  #    labs(x = "Capacity Factor",
  #         y = "Offer Price ($/MWh)",
  #         title = paste(plant_type, "Bidding Behaviour", sep = " "),
  #    )
  
  #  test <- Cogen %>%
  #    group_by(time, ID) %>%
  #    arrange(block_number) %>%
  #    mutate(csum = cumsum(Cap_Fac)) %>%
  #    group_by(block_number,price,csum) %>%
  #    summarise(block_number = median(block_number),
  #              price = median(price),
  #              csum = median(csum))
  
  #fit polynomial regression models up to degree 5
  #  fit1 <- lm(price~Cap_Fac, data=test1)
  #  fit2 <- lm(price~poly(Cap_Fac,2,raw=TRUE), data=test1)
  #  fit3 <- lm(price~poly(Cap_Fac,3,raw=TRUE), data=test1)
  #  fit4 <- lm(price~poly(Cap_Fac,4,raw=TRUE), data=test1)
  #  fit5 <- lm(price~poly(Cap_Fac,5,raw=TRUE), data=test1)
  #  fitlog <- lm(log(price)~Cap_Fac, data=test1)
  #  fitexp <- lm(price~exp(Cap_Fac), data=test1)
  #  log.model <- lm(log(price) ~ Cap_Fac, data=test1)
  
  #calculated adjusted R-squared of each model
  #  summary(fit1)$adj.r.squared
  #  summary(fit2)$adj.r.squared
  #  summary(fit3)$adj.r.squared
  #  summary(fit4)$adj.r.squared
  #  summary(fit5)$adj.r.squared
  
  #  plot(test1$Cap_Fac, test1$price, pch=19, xlab='Capacity Factor', ylab='Price ($/MWh)')
  
  #  x_axis <- seq(0,1, length=15)
  
  #add curve of each model to plot
  #  lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
  #  lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
  #  lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
  #  lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
  #  lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
  
  #  Cap_Fac <- c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,
  #               0.75,0.8,0.85,0.9,0.95,1)
  #  price <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,15,20,25,1000) #COGEN
  #  price <- c(0,0,0,0,0,0,0,0,0,5,5,10,15,15,15,20,20,25,30,1000) #COAL
  #  price <-c(0,30,30,30,30,30,30,30,30,30,30,35,40,45,50,55,65,75,1000,1000) #SCGT
  #  price <-c(0,0,0,0,0,0,0,5,5,5,10,10,10,15,20,25,30,35,40,1000) #NGCC
  #  Aurora <- data.frame(Cap_Fac, price)
  
  #  gg<-ggplot(test1, aes(x = Cap_Fac, y=price)) + 
  #    geom_point() +
  ##    geom_point(data=Aurora, color="red") +
  ##    stat_smooth(method = 'nls', formula = 'y~a^(b*x+c)',
  ##                method.args = list(start=c(a=20, b=10, c=-9)), se=FALSE) +
  ##    geom_line(mapping=aes(y=my))+
  ##    geom_smooth(method = 'lm', aes(color="Exp Model"), formula=(y ~ exp(x)), 
  ##                se = FALSE, linetype = 1) +
  #    theme_bw() +
  #    theme(panel.background = element_rect(fill = "transparent"),
  #          panel.grid = element_blank(),
  #          plot.background = element_rect(fill = "transparent", color = NA),
  #          text = element_text(size= 15),
  #          plot.title = element_text(hjust = 0.5),
  #          plot.subtitle = element_text(hjust = 0.5),
  #          axis.text.x = element_text(angle = 45, hjust=1)
  #    ) +
  #    scale_y_continuous(expand=c(0,0),
  #                       limits = c(0,1000),
  #                       n.breaks = 8) +
  #    ggtitle(paste(name, type, sep = ": "))+
  #    labs(x = "Capacity Factor",
  #         y = "Offer Price ($/MWh)",
  #         title = paste(plant_type, "Bidding Behaviour", sep = " "),
  ##         subtitle = paste("Bids at $0 at ",round(must_run*100, digits=2),"% CF",sep="")
  #)# +
  #    lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
  
  #    setwd("G:/My Drive/transfer")
  #    write.csv(test1, paste0(plant_type, "_Bid_Behaviour.csv"))
  
}

hockey_stick_season <- function(plant_type) {
  # Code to determine the bidding behavior of different technologies by 
  # identifying the capacity factors for various offers, and separating them into
  # bins, then taking the median value. This is the plot for the box and whisker plots.
  # The bid factor is identified as this median value divided by the overall 
  # average minus one, and is shown in the plots as a label.
  
  # Gather the data
  dataA <- sub_samp %>%
    filter(Plant_Type == plant_type, 
    ) %>%
    mutate(hour = he) %>%
    subset(., select = c(time, date, hour, ID, Capacity))
  
  dataB <- merit_filt %>%
    filter(Plant_Type == plant_type,
    ) %>%
    mutate(ID = asset_id) %>%
    subset(., select = c(date, month, day, hour, ID, available_mw, size, 
                         block_number,price))
  
  # Combine the data
  data <- merge(dataA, dataB, by = c("date","hour","ID")) %>%
    mutate(season = case_when(
      (month <= 2 | (month == 3 & day < 21)) ~ "Winter",
      ((month ==3 & day >=21) | (month >= 4 & month <= 5) | 
         (month == 6 & day < 21)) ~ "Spring",
      ((month ==6 & day >=21) | (month >= 7 & month <= 8) | 
         (month == 9 & day < 21)) ~ "Summer",
      ((month ==9 & day >=21) | (month >= 10 & month <= 11) | 
         (month == 12 & day < 21)) ~ "Fall",
      TRUE ~ "Winter"
    ))
  
  # Combine individual plant data and calculate the capacity factors for the 
  # different offers.
  data1 <- data %>%
    group_by(season,time,price) %>%
    summarise(available = sum(size)) %>%
    ungroup() %>%
    group_by(time) %>%
    arrange(price) %>%
    mutate(csum = cumsum(available),
           Capacity = max(csum),
           Cap_Fac = csum/Capacity)
  
  # Remove duplicate rows
  data1 <- data1[!duplicated(data1[c(3,7)]),]
  
  tags1 <- c("[0-05%)","[05-10%)","[10-15%)","[15-20%)","[20-25%)","[25-30%)",
             "[30-35%)","[35-40%)","[40-45%)","[45-50%)","[50-55%)","[55-60%)",
             "[60-65%)","[65-70%)","[70-75%)","[75-80%)","[80-85%)","[85-90%)",
             "[90-95%)","[95-100%]")
  
  data3 <- data1 %>%
    mutate(tag = case_when(
      Cap_Fac < 0.05 ~ tags1[1],
      Cap_Fac >= 0.05 & Cap_Fac < 0.1 ~ tags1[2],
      Cap_Fac >= 0.1 & Cap_Fac < 0.15 ~ tags1[3],
      Cap_Fac >= 0.15 & Cap_Fac < 0.2 ~ tags1[4],
      Cap_Fac >= 0.2 & Cap_Fac < 0.25 ~ tags1[5],
      Cap_Fac >= 0.25 & Cap_Fac < 0.3 ~ tags1[6],
      Cap_Fac >= 0.3 & Cap_Fac < 0.35 ~ tags1[7],
      Cap_Fac >= 0.35 & Cap_Fac < 0.4 ~ tags1[8],
      Cap_Fac >= 0.4 & Cap_Fac < 0.45 ~ tags1[9],
      Cap_Fac >= 0.45 & Cap_Fac < 0.5 ~ tags1[10],
      Cap_Fac >= 0.5 & Cap_Fac < 0.55 ~ tags1[11],
      Cap_Fac >= 0.55 & Cap_Fac < 0.6 ~ tags1[12],
      Cap_Fac >= 0.6 & Cap_Fac < 0.65 ~ tags1[13],
      Cap_Fac >= 0.65 & Cap_Fac < 0.7 ~ tags1[14],
      Cap_Fac >= 0.7 & Cap_Fac < 0.75 ~ tags1[15],
      Cap_Fac >= 0.75 & Cap_Fac < 0.8 ~ tags1[16],
      Cap_Fac >= 0.8 & Cap_Fac < 0.85 ~ tags1[17],
      Cap_Fac >= 0.85 & Cap_Fac < 0.9 ~ tags1[18],
      Cap_Fac >= 0.9 & Cap_Fac < 0.95 ~ tags1[19],
      Cap_Fac >= 0.95 & Cap_Fac <= 1 ~ tags1[20]
    ))
  
  tot <- data3 %>%
    group_by(season) %>%
    summarise(tot = median(price))
  
  #fall <- as.numeric(tot[1,2])
  #spr <- as.numeric(tot[2,2])
  #summer <- as.numeric(tot[3,2])
  #wint <- as.numeric(tot[4,2])
  
  data3 <- merge(data3,tot, by = "season")
  
  # Summarize the median prices and bid factors.
  means <- data3 %>%
    group_by(tag,season) %>%
    summarize(ave = mean(price),
              med = median(price),
              bid = med/tot-1
              ) %>%
    mutate_if(is.numeric,round,2)
  
  # Remove duplicate rows
  means <- means[!duplicated(means[c(1,2,3,4,5)]),]
  
  # Plot the data
  ggplot(data = data3, mapping = aes(x=tag,y=price)) +
    #geom_jitter(color = "gray", alpha=0.2) + 
    geom_boxplot(fill="gray",color="black",alpha=0.3) +
    #    geom_text(data = means, aes(label = dollar(round(med, digits=0)),hjust=1.2, 
    #                                y = med+90)) +
    geom_label(data = means, aes(label = bid, vjust="inward", y = ave),size=4) +
    facet_wrap(~season, ncol=2) +
    labs(x='Capacity Factor') + 
    theme_bw() +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 9)
    )  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1000),
                       n.breaks = 8) +
    labs(x = "Capacity Factor",
         y = "Offer Price ($/MWh)",
         title = paste(plant_type, "Bidding Behaviour", sep = " "),
    )
}

hockey_stick_seasEXTRA <- function(plant_type) {
  # Code to determine the bidding behavior of different technologies by 
  # identifying the capacity factors for various offers, and separating them into
  # bins, then taking the median value. This is the plot for the box and whisker plots.
  # The bid factor is identified as this median value divided by the overall 
  # average minus one, and is shown in the plots as a label.
  
  # Gather the data
  dataA <- sub_samp %>%
    filter(Plant_Type == plant_type, 
    ) %>%
    mutate(hour = he) %>%
    subset(., select = c(time, date, hour, ID, Capacity))
  
  dataB <- merit_filt %>%
    filter(Plant_Type == plant_type,
    ) %>%
    mutate(ID = asset_id) %>%
    subset(., select = c(date, month, day, hour, ID, available_mw, size, 
                         block_number,price))
  
  # Combine the data
  data <- merge(dataA, dataB, by = c("date","hour","ID")) %>%
    mutate(season = case_when(
      (month <= 2 | (month == 3 & day < 21)) ~ "Winter",
      ((month ==3 & day >=21) | (month >= 4 & month <= 5) | 
         (month == 6 & day < 21)) ~ "Spring",
      ((month ==6 & day >=21) | (month >= 7 & month <= 8) | 
         (month == 9 & day < 21)) ~ "Summer",
      ((month ==9 & day >=21) | (month >= 10 & month <= 11) | 
         (month == 12 & day < 21)) ~ "Fall",
      TRUE ~ "Winter"
    ))
  
  # Combine individual plant data and calculate the capacity factors for the 
  # different offers.
  data1 <- data %>%
    group_by(season,time,price) %>%
    summarise(available = sum(size)) %>%
    ungroup() %>%
    group_by(time) %>%
    arrange(price) %>%
    mutate(csum = cumsum(available),
           Capacity = max(csum),
           Cap_Fac = csum/Capacity)
  
  # Remove duplicate rows
  data1 <- data1[!duplicated(data1[c(3,7)]),]
  
  tags1 <- c("[0-05%)","[05-10%)","[10-15%)","[15-20%)","[20-25%)","[25-30%)",
             "[30-35%)","[35-40%)","[40-45%)","[45-50%)","[50-55%)","[55-60%)",
             "[60-65%)","[65-70%)","[70-75%)","[75-80%)","[80-85%)","[85-90%)",
             "[90-95%)","[95-97%)","[97-100%]")
  
  data3 <- data1 %>%
    mutate(tag = case_when(
      Cap_Fac < 0.05 ~ tags1[1],
      Cap_Fac >= 0.05 & Cap_Fac < 0.1 ~ tags1[2],
      Cap_Fac >= 0.1 & Cap_Fac < 0.15 ~ tags1[3],
      Cap_Fac >= 0.15 & Cap_Fac < 0.2 ~ tags1[4],
      Cap_Fac >= 0.2 & Cap_Fac < 0.25 ~ tags1[5],
      Cap_Fac >= 0.25 & Cap_Fac < 0.3 ~ tags1[6],
      Cap_Fac >= 0.3 & Cap_Fac < 0.35 ~ tags1[7],
      Cap_Fac >= 0.35 & Cap_Fac < 0.4 ~ tags1[8],
      Cap_Fac >= 0.4 & Cap_Fac < 0.45 ~ tags1[9],
      Cap_Fac >= 0.45 & Cap_Fac < 0.5 ~ tags1[10],
      Cap_Fac >= 0.5 & Cap_Fac < 0.55 ~ tags1[11],
      Cap_Fac >= 0.55 & Cap_Fac < 0.6 ~ tags1[12],
      Cap_Fac >= 0.6 & Cap_Fac < 0.65 ~ tags1[13],
      Cap_Fac >= 0.65 & Cap_Fac < 0.7 ~ tags1[14],
      Cap_Fac >= 0.7 & Cap_Fac < 0.75 ~ tags1[15],
      Cap_Fac >= 0.75 & Cap_Fac < 0.8 ~ tags1[16],
      Cap_Fac >= 0.8 & Cap_Fac < 0.85 ~ tags1[17],
      Cap_Fac >= 0.85 & Cap_Fac < 0.9 ~ tags1[18],
      Cap_Fac >= 0.9 & Cap_Fac < 0.95 ~ tags1[19],
      Cap_Fac >= 0.95 & Cap_Fac < 0.97 ~ tags1[20],
      Cap_Fac >= 0.97 & Cap_Fac <= 1 ~ tags1[21]
    ))
  
  tot <- data3 %>%
    group_by(season) %>%
    summarise(tot = median(price))
  
  #fall <- as.numeric(tot[1,2])
  #spr <- as.numeric(tot[2,2])
  #summer <- as.numeric(tot[3,2])
  #wint <- as.numeric(tot[4,2])
  
  data3 <- merge(data3,tot, by = "season")
  
  # Summarize the median prices and bid factors.
  means <- data3 %>%
    group_by(tag,season) %>%
    summarize(ave = mean(price),
              med = median(price),
              bid = med/tot-1
    ) %>%
    mutate_if(is.numeric,round,2)
  
  # Remove duplicate rows
  means <- means[!duplicated(means[c(1,2,3,4,5)]),]
  
  # Plot the data
  ggplot(data = data3, mapping = aes(x=tag,y=price)) +
    #geom_jitter(color = "gray", alpha=0.2) + 
    geom_boxplot(fill="gray",color="black",alpha=0.3) +
    #    geom_text(data = means, aes(label = dollar(round(med, digits=0)),hjust=1.2, 
    #                                y = med+90)) +
    geom_label(data = means, aes(label = bid, vjust="inward", y = ave),size=4) +
    facet_wrap(~season, ncol=2) +
    labs(x='Capacity Factor') + 
    theme_bw() +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 9)
    )  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1000),
                       n.breaks = 8) +
    labs(x = "Capacity Factor",
         y = "Offer Price ($/MWh)",
         title = paste(plant_type, "Bidding Behaviour", sep = " "),
    )
}

hockey_stick_onoff <- function(plant_type) {
  # Code to determine the bidding behavior of different technologies by 
  # identifying the capacity factors for various offers, and separating them into
  # bins, then taking the median value. This is the plot for the box and whisker plots.
  # The bid factor is identified as this median value divided by the overall 
  # average minus one, and is shown in the plots as a label.
  
  # Gather the data
  dataA <- sub_samp %>%
    filter(Plant_Type == plant_type, 
    ) %>%
    mutate(hour = he) %>%
    subset(., select = c(time, date, hour, ID, Capacity))
  
  dataB <- merit_filt %>%
    filter(Plant_Type == plant_type,
    ) %>%
    mutate(ID = asset_id,
           Condition = case_when(
             (hour<=06 | hour>=23) ~ "Off-Peak WECC",
             TRUE ~ "On-Peak WECC")) %>%
    subset(., select = c(date,hour, ID, Condition, available_mw, size, block_number,price))
  
  # Combine the data
  data <- merge(dataA, dataB, by = c("date","hour","ID")) 
  
  # Combine individual plant data and calculate the capacity factors for the 
  # different offers.
  data1 <- data %>%
    group_by(time,price,Condition) %>%
    summarise(available = sum(size)) %>%
    ungroup() %>%
    group_by(time) %>%
    arrange(price) %>%
    mutate(csum = cumsum(available),
           Capacity = max(csum),
           Cap_Fac = csum/Capacity)
  
  # Remove duplicate rows
  data1 <- data1[!duplicated(data1[c(2,7)]),]
  
  tags1 <- c("[0-05%)","[05-10%)","[10-15%)","[15-20%)","[20-25%)","[25-30%)",
             "[30-35%)","[35-40%)","[40-45%)","[45-50%)","[50-55%)","[55-60%)",
             "[60-65%)","[65-70%)","[70-75%)","[75-80%)","[80-85%)","[85-90%)",
             "[90-95%)","[95-100%]")
  
  data3 <- data1 %>%
    mutate(tag = case_when(
      Cap_Fac < 0.05 ~ tags1[1],
      Cap_Fac >= 0.05 & Cap_Fac < 0.1 ~ tags1[2],
      Cap_Fac >= 0.1 & Cap_Fac < 0.15 ~ tags1[3],
      Cap_Fac >= 0.15 & Cap_Fac < 0.2 ~ tags1[4],
      Cap_Fac >= 0.2 & Cap_Fac < 0.25 ~ tags1[5],
      Cap_Fac >= 0.25 & Cap_Fac < 0.3 ~ tags1[6],
      Cap_Fac >= 0.3 & Cap_Fac < 0.35 ~ tags1[7],
      Cap_Fac >= 0.35 & Cap_Fac < 0.4 ~ tags1[8],
      Cap_Fac >= 0.4 & Cap_Fac < 0.45 ~ tags1[9],
      Cap_Fac >= 0.45 & Cap_Fac < 0.5 ~ tags1[10],
      Cap_Fac >= 0.5 & Cap_Fac < 0.55 ~ tags1[11],
      Cap_Fac >= 0.55 & Cap_Fac < 0.6 ~ tags1[12],
      Cap_Fac >= 0.6 & Cap_Fac < 0.65 ~ tags1[13],
      Cap_Fac >= 0.65 & Cap_Fac < 0.7 ~ tags1[14],
      Cap_Fac >= 0.7 & Cap_Fac < 0.75 ~ tags1[15],
      Cap_Fac >= 0.75 & Cap_Fac < 0.8 ~ tags1[16],
      Cap_Fac >= 0.8 & Cap_Fac < 0.85 ~ tags1[17],
      Cap_Fac >= 0.85 & Cap_Fac < 0.9 ~ tags1[18],
      Cap_Fac >= 0.9 & Cap_Fac < 0.95 ~ tags1[19],
      Cap_Fac >= 0.95 & Cap_Fac <= 1 ~ tags1[20]
    ))
  
  tot <- data3 %>%
    group_by(Condition) %>%
    summarise(tot = median(price))
  
  data3 <- merge(data3,tot, by = "Condition")
  
  # Summarize the median prices and bid factors.
  means <- data3 %>%
    group_by(tag,Condition) %>%
    summarize(ave = mean(price),
              med = median(price),
              bid = med/tot-1) %>%
    mutate_if(is.numeric,round,2)
  
  # Remove duplicate rows
  means <- means[!duplicated(means[c(1,2,3,4,5)]),]
  
  # Plot the data
  ggplot(
    data = data3, mapping = aes(x=tag,y=price)
    ) +
    #geom_jitter(color = "gray", alpha=0.2) + 
    geom_boxplot(fill="gray",color="black",alpha=0.3) +
    geom_label(data = means, aes(label = bid, vjust="inward", y = ave),size=3) +
    facet_wrap(~Condition) +
    labs(x='Capacity Factor') + 
    theme_bw() +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 9)
    )  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1000),
                       n.breaks = 8) +
    labs(x = "Capacity Factor",
         y = "Offer Price ($/MWh)",
         title = paste(plant_type, "Bidding Behaviour", sep = " "),
    )
}

Week_act <- function(year,month,day) {
  
  colours = c("darkslateblue", "black", "grey", "darkslategrey", "coral4", "goldenrod4", 
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
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCONV", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COGEN", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SCGT", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCC", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "HYDRO", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "OTHER", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "WIND", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SOLAR", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "STORAGE", after = Inf)
  }
  
  WK$Plant_Type <- factor(WK$Plant_Type, levels=c("IMPORT", "COAL", "NGCONV", "COGEN", 
                                                  "SCGT", "NGCC", "HYDRO", 
                                                  "OTHER", "WIND", "SOLAR", "STORAGE"))
  
  levels(WK$Plant_Type) <- c("Import","Coal", "NGConv", "Cogen", "SCGT", "NGCC", "Hydro", 
                             "Other", "Wind", "Solar", "Storage")
  
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

Week_aline <- function(year,month,day) {
  
  colours = c("darkslateblue", "black", "grey", "darkslategrey", "coral4", "goldenrod4", 
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
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCONV", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COGEN", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SCGT", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCC", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "HYDRO", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "OTHER", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "WIND", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SOLAR", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "STORAGE", after = Inf)
  }
  
  WK$Plant_Type <- factor(WK$Plant_Type, levels=c("IMPORT", "COAL", "NGCONV", "COGEN", 
                                                  "SCGT", "NGCC", "HYDRO", 
                                                  "OTHER", "WIND", "SOLAR", "STORAGE"))
  
  levels(WK$Plant_Type) <- c("Import","Coal", "NGConv", "Cogen", "SCGT", "NGCC", "Hydro", 
                             "Other", "Wind", "Solar", "Storage")
  
#  dmd <- demand %>%
#    filter(time >= wk_st & time <= wk_end)
  
  # Plot the data    
  ##############################################################################
  ggplot() +
    geom_line(data = WK, aes(x = time, y = total_gen, color = Plant_Type), 
#              alpha=0.6, 
              size=2) +
    
    # Add hourly load line
#    geom_line(data = dmd, 
#              aes(x = time, y = Demand), size=2, colour = "black") +
    
    scale_x_datetime(expand=c(0,0)) +
    
    # Set the theme for the plot
    ############################################################################
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
    scale_y_continuous(expand=c(0,0)) +
    labs(x = "Date", y = "Output (MWh)", fill = "AESO Data: \nResource") +
    scale_color_manual(values = colours)
}

capVScap <- function(plant_type) {
  # The purpose of this code is to show the relationship between the capacity 
  # factor and the capture price (or wind discount)
  
  data <- sub_samp %>%
    filter(Plant_Type == plant_type) %>%
    subset(., select = c(time, ID, AESO_Name, Capacity, gen, Revenue, Cap_Fac)) %>%
    mutate(capture=Revenue/gen,
           Cap_Fac=case_when(gen>Capacity~1,
                             TRUE~Cap_Fac))
#    group_by(ID, AESO_Name) %>%
#    summarise(total_gen=sum(gen,na.rm=T),
#              total_rev=sum(Revenue,na.rm=T),
#              mean_CapFac=mean(Cap_Fac),
#              capture=total_rev/total_gen) %>%
#    ungroup()
  
  tags <- c("[0-05%)","[05-10%)","[10-15%)","[15-20%)","[20-25%)","[25-30%)",
             "[30-35%)","[35-40%)","[40-45%)","[45-50%)","[50-55%)","[55-60%)",
             "[60-65%)","[65-70%)","[70-75%)","[75-80%)","[80-85%)","[85-90%)",
             "[90-95%)","[95-100%]")
  
  data <- data %>%
    mutate(tag = case_when(
      Cap_Fac < 0.05 ~ tags[1],
      Cap_Fac >= 0.05 & Cap_Fac < 0.1 ~ tags[2],
      Cap_Fac >= 0.1 & Cap_Fac < 0.15 ~ tags[3],
      Cap_Fac >= 0.15 & Cap_Fac < 0.2 ~ tags[4],
      Cap_Fac >= 0.2 & Cap_Fac < 0.25 ~ tags[5],
      Cap_Fac >= 0.25 & Cap_Fac < 0.3 ~ tags[6],
      Cap_Fac >= 0.3 & Cap_Fac < 0.35 ~ tags[7],
      Cap_Fac >= 0.35 & Cap_Fac < 0.4 ~ tags[8],
      Cap_Fac >= 0.4 & Cap_Fac < 0.45 ~ tags[9],
      Cap_Fac >= 0.45 & Cap_Fac < 0.5 ~ tags[10],
      Cap_Fac >= 0.5 & Cap_Fac < 0.55 ~ tags[11],
      Cap_Fac >= 0.55 & Cap_Fac < 0.6 ~ tags[12],
      Cap_Fac >= 0.6 & Cap_Fac < 0.65 ~ tags[13],
      Cap_Fac >= 0.65 & Cap_Fac < 0.7 ~ tags[14],
      Cap_Fac >= 0.7 & Cap_Fac < 0.75 ~ tags[15],
      Cap_Fac >= 0.75 & Cap_Fac < 0.8 ~ tags[16],
      Cap_Fac >= 0.8 & Cap_Fac < 0.85 ~ tags[17],
      Cap_Fac >= 0.85 & Cap_Fac < 0.9 ~ tags[18],
      Cap_Fac >= 0.9 & Cap_Fac < 0.95 ~ tags[19],
      Cap_Fac >= 0.95 & Cap_Fac <= 1 ~ tags[20]
    ))
  
  data <- na.omit(data)
  
  # Summarize the median prices and bid factors.
  means <- data %>%
    group_by(tag) %>%
    summarize(ave = mean(capture),
              ) %>%
    mutate_if(is.numeric,round,2)
  
  # Remove duplicate rows
  #means <- means[!duplicated(means[c(1,2)]),]
  #data1 <- data[!duplicated(data[c(8,9)]),]
  
  ggplot(means, aes(x=tag, y=ave))+
#    geom_jitter(color = "gray", alpha=0.2) + 
#    geom_boxplot(fill="gray",color="black",alpha=0.3) +
    #    geom_text(data = means, aes(label = dollar(round(med, digits=0)),hjust=1.2, 
    #                                y = med+90)) +
    geom_point(#data = means, aes(label = ave, vjust="inward", y = ave),
               size=3) +
    labs(x = "Hourly Capacity Factor averaged by Bin",
         y = "Capture Price ($/MWh)",
         #         title = paste(plant_type, "Bidding Behaviour", sep = " "),
    ) + 
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
    )  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,110),
                       n.breaks = 10)
    
  
  setwd("D:/Documents/GitHub/AuroraEval")
  ggsave(path = "images", filename = "CF_vs_captureprice.png", bg = "transparent")
  setwd("D:/Documents/Education/Masters Degree/Datasets/Market")
}

AvCP <- function(year1,year2) {
  alberta_Price <- sub_samp %>%
    filter(Plant_Type == "WIND",
           #gen != 0,
           time >= as.POSIXct(paste0(year1,"/01/01"),"%Y/%m/%d",tz = "MST"),
           time <= as.POSIXct(paste0(year2,"/12/01"),"%Y/%m/%d",tz = "MST")) %>%
    subset(., select = -c(he,date,Latitude,Longitude,Demand,AIL,NRG_Stream,
                          Plant_Fuel,Plant_Type,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name)) %>%
    na.omit() %>%
    group_by(time) %>%
    mutate(fleet_gen = sum(gen),
           fleet_cap = sum(Capacity),
           fleet_CF = fleet_gen/fleet_cap,
           delta_CF = Cap_Fac-fleet_CF,
           stand_Price = Price*delta_CF,
    ) %>%
    ungroup() %>%
    group_by(ID) %>%
    summarize(Revenue = sum(Revenue),
              Dispatched = sum(gen),
              Capture_Price = Revenue/Dispatched,
              Capacity = median(Capacity))
  
  ggplot(alberta_Price,aes(x=fct_reorder(ID,Capture_Price),Capture_Price)) +
    geom_col(aes(fill=Capacity)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_viridis() +
    labs(x = "Wind Farms",
         y = "Average Capture Price ($/MWh)") +
    theme(text = element_text(size = sz),
          axis.text.x = element_text(angle=90),
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
}

correlation <- function(year1,year2) {
  # Plots the plant average revenue as a function of the index of deviation
  # of the generation per installed megawatt from the rest of the fleet of that 
  # resource type
  
  # Set a minimum number of contributions into the market to be considered
  limit <- 2232
  
  # Filter data for plant_type and date considered, calculate index of deviation,
  # and fill in missing data.
  alberta_samp <- sub_samp %>%
    filter(Plant_Type == "WIND",
           time >= as.POSIXct(paste0(year1,"/01/01"),"%Y/%m/%d",tz = "MST"),
           time <= as.POSIXct(paste0(year2,"/12/31"),"%Y/%m/%d",tz = "MST")) %>%
    subset(., select = -c(he,date,#Latitude,Longitude,
                          Demand,AIL,NRG_Stream,
                          Plant_Fuel,Plant_Type,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name,Revenue,Price)) %>%
    na.omit() %>%
    group_by(ID) %>%
    filter(n() >= limit) %>%
    ungroup() %>%
    group_by(time) %>%
    mutate(fleet_CF = sum(gen)/sum(Capacity),
           other_CF = (sum(gen)-gen)/(sum(Capacity)-Capacity),
           deviance_CF = abs(Cap_Fac-other_CF),
           ) %>%
    ungroup() %>%
    group_by(ID) %>%
    summarize(IOD = sqrt(mean(deviance_CF)),
              Capacity = median(Capacity),
              Latitude = median(as.numeric(Latitude)),
              Longitude = median(as.numeric(Longitude)),
              ) %>%
    mutate(Installation=case_when(grepl("CRR2",ID)~"post2019",
                                  grepl("CYP",ID)~"post2019",
                                  grepl("FMG1",ID)~"post2019",
                                  grepl("HHW1",ID)~"post2019",
                                  grepl("HLD1",ID)~"post2019",
                                  grepl("JNR",ID)~"post2019",
                                  grepl("RIV1",ID)~"post2019",
                                  grepl("RTL1",ID)~"post2019",
                                  grepl("WHE1",ID)~"post2019",
                                  grepl("WHT",ID)~"post2019",
                                  grepl("WRW1",ID)~"post2019",),
           Installation=case_when(is.na(Installation)~"pre2019",
                                  TRUE~"post2019"))
  
  # Filter data for plant_type and date considered, Calculate the plant average
  # revenue per megawatt
  alberta_Price <- sub_samp %>%
    filter(Plant_Type == "WIND",
           time >= as.POSIXct(paste0(year1,"/01/01"),"%Y/%m/%d",tz = "MST"),
           time <= as.POSIXct(paste0(year2,"/12/31"),"%Y/%m/%d",tz = "MST")) %>%
    subset(., select = -c(he,date,#Latitude,Longitude,
                          Demand,AIL,NRG_Stream,
                          Plant_Fuel,Plant_Type,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name)) %>%
    na.omit() %>%
    group_by(ID) %>%
    filter(n() >= limit) %>%
    summarize(Revenue = sum(Revenue),
              Rev = mean(Revenue),
              Dispatched = sum(gen),
              Capture_Price = Revenue/Dispatched,
              Capacity = median(Capacity)) %>%
    subset(.,select=c("ID","Capture_Price","Rev"))
  
  # Combine the datasets
  ab_data <- merge(alberta_samp,alberta_Price, by="ID")
  
  # Run linear regression on the combined dataset between the plant average 
  # revenue and the index of deviation
  equ <- summary(lm(Capture_Price ~ IOD, data=ab_data))
  
  # Set size of text for the charts
  sz = 15
  
  # Plot the data with a scatter plot and the linear regression
  ch <- ggplot(ab_data, aes(x = IOD, y = Capture_Price, 
                           )) +
    geom_smooth(method='lm', 
                show.legend=FALSE,fullrange=TRUE) +
    #stat_regline_equation(label.x=0.4,label.y=90,show.legend=FALSE) +
    #stat_cor(aes(label=..rr.label..), label.x=0.4,label.y=88,show.legend=FALSE) + 
    geom_point(aes(size=Capacity,#color=Installation
                   )) +
    #geom_text(label=ab_data$ID, size = sz-12,
    #          nudge_x = 0.001, nudge_y = 0.5) +
    labs(x = paste0("Generation per installed megawatt index of deviation from fleet in ",year1),
         y = "Plant Average Revenue ($/MWh)") + 
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
  
  setwd("D:/Documents/GitHub/AuroraEval")
  
  # Prepare data for plotting the map with the points
  ################################################################################
  # Load in the data
  # Wind Speed data from Canada Wind Atlas 
  # http://www.windatlas.ca/nav-en.php?no=46&field=EU&height=80&season=ANU
  ################################################################################
  wind_profileAA <- readRDS("WindAtlas_Data00_0.05")
  colnames(wind_profileAA) <- c('Latitude', 'Longitude', 'Wind')
  
  {can_level1 = getData("GADM", country = "CA", level = 1)
    
    WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    canada_level1_ellipsoid = spTransform(can_level1, WGS84)
    
    alberta_ellipsoid1 = 
      canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]
    }
  
  ################################################################################
  ################################################################################
  # Map of Alberta with active sites 
  ################################################################################
  ################################################################################
  
  corr_map <- ggplot() + 
    geom_raster(data = wind_profileAA, 
              aes(x = Longitude, y = Latitude, fill = Wind)) +
    geom_polygon(data = alberta_ellipsoid1, 
                 aes(x = long, y = lat, group = group), 
                 fill = "transparent", colour = "black") +
    geom_point(data = ab_data,
               aes(x= Longitude, y = Latitude, size = IOD, color=Installation), 
               shape = 16) +
    labs(size = "Generation per \ninstalled megawatt \nIndex of Deviation") +
    scale_color_manual(values = c("darkmagenta", "black"), 
                       labels = c("Built since 2019","Built before 2019")) +
    scale_fill_gradientn(colors = matlab.like(100),
                         limits=c(3.5,10), na.value="white",oob=squish, 
                         name = "Mean wind speed \nat 80m height \n(m/s)") +
    scale_size(range = c(0.5,8)) +
    guides(color = guide_legend(override.aes = list(size = 5))) +  
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 15),
          legend.box.background = element_blank(),
          legend.key=element_rect(fill = "transparent"),
          #legend.text = element_blank(),
          #legend.title = element_blank()
    )
  
  return(list(equ,ch,corr_map))
}

correlation_hypoth <- function(year1,year2) {
  # Plots the Capture price as a function of the output correlation to the rest 
  # of the fleet of that resource type
  
  # Filter data for plant_type, calculate total output for fleet for each period
  
  limit <- 2232
  
  alberta_samp <- sub_samp %>%
    filter(Plant_Type == "WIND",
           #gen != 0,
           time >= as.POSIXct(paste0(year1,"/01/01"),"%Y/%m/%d",tz = "MST"),
           time <= as.POSIXct(paste0(year2,"/12/31"),"%Y/%m/%d",tz = "MST")) %>%
    subset(., select = -c(he,date,#Latitude,Longitude,
                          Demand,AIL,NRG_Stream,
                          Plant_Fuel,Plant_Type,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name,Revenue,Price)) %>%
    na.omit() %>%
    group_by(ID) %>%
    filter(n() >= limit) %>%
    ungroup() %>%
    group_by(time) %>%
    mutate(fleet_CF = sum(gen)/sum(Capacity),
           other_CF = (sum(gen)-gen)/(sum(Capacity)-Capacity),
           deviance_CF = abs(Cap_Fac-other_CF),
    ) %>%
    ungroup() %>%
    group_by(ID) %>%
    #filter(n() >= 2232) %>%
    summarize(IOD = sqrt(mean(deviance_CF)),
              Capacity = median(Capacity),
              Latitude = median(as.numeric(Latitude)),
              Longitude = median(as.numeric(Longitude)),
    ) %>%
    mutate(Installation = "Active")
  
#  alberta_samp <- rbind(alberta_samp,hypothetical)
  
  alberta_Price <- sub_samp %>%
    filter(Plant_Type == "WIND",
           time >= as.POSIXct(paste0(year1,"/01/01"),"%Y/%m/%d",tz = "MST"),
           time <= as.POSIXct(paste0(year2,"/12/31"),"%Y/%m/%d",tz = "MST")) %>%
    subset(., select = -c(he,date,Latitude,Longitude,Demand,AIL,NRG_Stream,
                          Plant_Fuel,Plant_Type,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name)) %>%
    na.omit() %>%
    group_by(ID) %>%
    filter(n() >= limit) %>%
    summarize(Revenue = sum(Revenue),
              Dispatched = sum(gen),
              Capture_Price = Revenue/Dispatched,
              Capacity = median(Capacity)) %>%
    subset(.,select=c("ID","Capture_Price"))
  
  ab_data <- merge(alberta_samp,alberta_Price, by="ID",all=T)
  
  # Calculate the linear regression equation
  linreg <- lm(Capture_Price ~ IOD, 
               data=filter(ab_data,Installation == "Active"))
  equ <- summary(linreg)
  
  # Change working directory
  setwd("D:/Documents/GitHub/AuroraEval/WindProfile")
  
  hypothetical <- readRDS("SitesProfiles.RData") %>%
    mutate(time = as.POSIXct(as.character(paste0(year,"/",month,"/",day," ",hour,":00:00")),
                             "%Y/%m/%d %H:%M:%S",
                             tz = "MST"),
           gen = Capacity * Cap_Fac) %>%
    group_by(time) %>%
    mutate(fleet_CF = sum(gen)/sum(Capacity),
           other_CF = (sum(gen)-gen)/(sum(Capacity)-Capacity),
           deviance_CF = abs(Cap_Fac-other_CF),
    ) %>%
    ungroup() %>%
    group_by(ID,Installation) %>%
    summarize(IOD = sqrt(mean(deviance_CF)),
              Capacity = 100,
              CF = mean(Cap_Fac),
              Latitude = median(as.numeric(Latitude)),
              Longitude = median(as.numeric(Longitude)),
    ) %>%
    mutate(Capture_Price = linreg$coefficients[2] * IOD + linreg$coefficients[1])
  
  ab_data <- rbind(ab_data,hypothetical%>%subset(.,select=-CF))
  
  sz = 15

  ch <- ggplot(ab_data, aes(x = IOD, y = Capture_Price, 
                      #                           colour = Installation, size = Capacity
  )) +
    geom_smooth(data=filter(ab_data,Installation == "Active"),
                method='lm', #aes(colour=Installation),
                show.legend=FALSE,fullrange=TRUE) +
#        stat_regline_equation(data=filter(ab_data,Installation == "Active"),
#                              label.x=0.4,label.y=82,show.legend=FALSE) +
#        stat_cor(data=filter(ab_data,Installation == "Active"),
#                 aes(label=..rr.label..), label.x=0.4,label.y=80,show.legend=FALSE) + 
    geom_point(aes(size=Capacity,shape=Installation
    )) +
    scale_shape_manual(values=c(16,9), 
                       name = "",
                       labels=c("Active wind farm","Hypothetical site"))+
#   geom_text(label=ab_data$ID, size = sz-12,# hjust = 0, vjust = 0, 
#             nudge_x = 0.001, nudge_y = 0.7
#             ) +
   labs(x = paste0("Generation per installed megawatt index of deviation from fleet in ",year1),
       y = "Average Plant Revenue ($/MWh)") + 
   guides(shape = guide_legend(override.aes = list(size = 5)),
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
  
  hypothetical <- hypothetical %>%
    mutate(AE = CF*8760,
           years = 8*(21250-2847*CF)/(876*CF*Capture_Price-2975)
           )
  
  col <- ggplot(hypothetical, aes(x = fct_reorder(ID,years),
                                  y = years)) +
    geom_col()+#aes(fill=fct_reorder(sort,desc(sort)))) +
    labs(x = "Hypothetical site",
         y = "Estimated Payback Period (yrs)") + 
    theme(text = element_text(size = sz),
          axis.text.x = element_text(angle=90),
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
  
  setwd("D:/Documents/GitHub/AuroraEval")
  ################################################################################
  # Load in the data
  # Wind Speed data from Canada Wind Atlas 
  # http://www.windatlas.ca/nav-en.php?no=46&field=EU&height=80&season=ANU
  ################################################################################
  wind_profile <- readRDS("WindAtlas_Data00_0.05")
  colnames(wind_profile) <- c('Latitude', 'Longitude', 'Wind')
  
  {can_level1 = getData("GADM", country = "CA", level = 1)
    
    WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    canada_level1_ellipsoid = spTransform(can_level1, WGS84)
    
    alberta_ellipsoid1 = 
      canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]
  }
  
  ################################################################################
  ################################################################################
  # Map of Alberta with active sites 
  ################################################################################
  ################################################################################
  
  legTitle <- 15
  legText <- 12
  
  corr_map <- ggplot() + 
    geom_raster(data = wind_profile, 
              aes(x = Longitude, y = Latitude, fill = Wind)) +
    geom_polygon(data = alberta_ellipsoid1, 
                 aes(x = long, y = lat, group = group), 
                 fill = "transparent", colour = "black") +
    geom_point(data = ab_data,
               aes(x= Longitude, y = Latitude, size = IOD, color=Installation), 
               shape = 16) +
    labs(size = "Generation per \ninstalled MW \nIndex of Deviation") +
    scale_color_manual(values = c("black","red4"), 
                       labels = c("Active","Hypothetical")) +
    scale_fill_gradientn(colors = matlab.like(100),
                         limits=c(2.5,10.5), na.value="white",oob=squish, 
                         name = "Mean annual\nwind speed \nat 80m height \n(m/s)") +
    scale_size(range = c(0.1,8)) +
    guides(color = guide_legend(override.aes = list(size = 5), order = 1),
           size = guide_legend(order = 2),
           #fill = guide_legend(keyheight = unit(0.5,'cm'),
           #                    reverse = TRUE)
    ) +
    #guides(color = guide_legend(override.aes = list(size = 5))) +  
    coord_fixed(ratio=5/3) +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = legText),
          legend.title = element_text(size = legTitle),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key=element_rect(fill = "transparent"),
          #legend.text = element_blank(),
          #legend.title = element_blank()
    )
  
  equ <- summary(lm(Capture_Price ~ IOD, 
                    data=filter(ab_data,Installation == "Active")))
  
  return(list(equ,ch,col,corr_map))
}

HypoIRR <- function(year1) {
  # Calculates the IRR for the hypothetical sites
  
  # open library with irr() function
  library(jrvFinance)
  #library(tidyquant)
  
  limit <- 2232
  
  alberta_samp <- sub_samp %>%
    filter(Plant_Type == "WIND",
           time >= as.POSIXct(paste0(year1,"/01/01"),"%Y/%m/%d",tz = "MST"),
           time <= as.POSIXct(paste0(year1,"/12/31"),"%Y/%m/%d",tz = "MST")) %>%
    subset(., select = -c(he,date,
                          Demand,AIL,NRG_Stream,
                          Plant_Fuel,Plant_Type,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name,Revenue,Price)) %>%
    na.omit() %>%
    group_by(ID) %>%
    filter(n() >= limit) %>%
    ungroup() %>%
    group_by(time) %>%
    mutate(fleet_CF = sum(gen)/sum(Capacity),
           other_CF = (sum(gen)-gen)/(sum(Capacity)-Capacity),
           deviance_CF = abs(Cap_Fac-other_CF),
    ) %>%
    ungroup() %>%
    group_by(ID) %>%
    summarize(IOD = sqrt(mean(deviance_CF)),
              Capacity = median(Capacity),
              Latitude = median(as.numeric(Latitude)),
              Longitude = median(as.numeric(Longitude)),
    ) %>%
    mutate(Installation = "Active")
  
  alberta_Price <- sub_samp %>%
    filter(Plant_Type == "WIND",
           time >= as.POSIXct(paste0(year1,"/01/01"),"%Y/%m/%d",tz = "MST"),
           time <= as.POSIXct(paste0(year1,"/12/31"),"%Y/%m/%d",tz = "MST")) %>%
    subset(., select = -c(he,date,Latitude,Longitude,Demand,AIL,NRG_Stream,
                          Plant_Fuel,Plant_Type,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name)) %>%
    na.omit() %>%
    group_by(ID) %>%
    filter(n() >= limit) %>%
    summarize(Revenue = sum(Revenue),
              Dispatched = sum(gen),
              Capture_Price = Revenue/Dispatched,
              Capacity = median(Capacity)) %>%
    subset(.,select=c("ID","Capture_Price"))
  
  ab_data <- merge(alberta_samp,alberta_Price, by="ID",all=T)
  
  # Calculate the linear regression equation
  linreg <- lm(Capture_Price ~ IOD, 
               data=filter(ab_data,Installation == "Active"))
  equ <- summary(linreg)
  
  # Change working directory
  setwd("D:/Documents/GitHub/AuroraEval/WindProfile")
  
  hypothetical_IRR <- readRDS("SitesProfiles.RData") %>%
    mutate(time = as.POSIXct(as.character(paste0(year,"/",month,"/",day," ",hour,":00:00")),
                             "%Y/%m/%d %H:%M:%S",
                             tz = "MST"),
           gen = Capacity * Cap_Fac) %>%
    group_by(time) %>%
    mutate(fleet_CF = sum(gen)/sum(Capacity),
           other_CF = (sum(gen)-gen)/(sum(Capacity)-Capacity),
           deviance_CF = abs(Cap_Fac-other_CF),
    ) %>%
    ungroup() %>%
    group_by(ID,Installation) %>%
    summarize(IOD = sqrt(mean(deviance_CF)),
              #Capacity = 100,
              CF = mean(Cap_Fac),
              #Latitude = median(as.numeric(Latitude)),
              #Longitude = median(as.numeric(Longitude)),
    ) %>%
    mutate(Capture_Price = linreg$coefficients[2] * IOD + linreg$coefficients[1]) %>%
    filter(Installation == "Potential") %>%
    mutate(AE = CF*8760,
           IC = 1700000,
           IN = 227760*CF,
           AR = Capture_Price,
           AOM = 29750,
           Year = 2020,
           Ct = IN + AR*AE - AOM,
           #          years = (1700000-(50*0.52*AE))/(AE*Capture_Price-(1700000*0.0175)),
           #years = 8*(21250-2847*CF)/(876*CF*Capture_Price-2975)
    ) %>%
    subset(.,select=c(ID,Year,Ct,IC)) %>%
    group_by(ID,Ct) %>%
    complete(Year = full_seq(2020:2035,1)) %>%
    mutate(IC = case_when(is.na(IC)~0,
                          TRUE~IC),
           Cf = Ct-IC) %>%
    ungroup() %>%
    group_by(ID) %>%
    summarize(IRR = irr(Cf))
  
  View(hypothetical_IRR)
    
}

sd_difference <- function() {
  # Plots the Capture price as a function of the output correlation to the rest 
  # of the fleet of that resource type, with the hypothetical sites added
  
  corm <- "pearson" # Define correlation method ("pearson", "kendall", "spearman")
  
  setwd("D:/Documents/GitHub/AuroraEval/WindProfile")
  
  data <- readRDS("SitesProfiles.RData") %>%
    mutate(time = as.POSIXct(as.character(paste0(year,"/",month,"/",day," ",hour,":00:00")),
                             "%Y/%m/%d %H:%M:%S",
                             tz = "MST"),
           gen = Capacity * Cap_Fac) %>%
    group_by(time) %>%
    mutate(fleet_CF = sum(gen)/sum(Capacity),
           deviance_CF = (Cap_Fac-fleet_CF)^2,
           ) %>%
    ungroup() %>%
    group_by(ID,Installation) %>%
    filter(n() >= 2232) %>%
    summarize(sd_wind = sqrt(mean(deviance_CF))) 
  
  alberta_samp <- sub_samp %>%
    filter(Plant_Type == "WIND",
           time >= as.POSIXct("2021/01/01","%Y/%m/%d",tz = "MST"),
           time <= as.POSIXct("2021/12/31","%Y/%m/%d",tz = "MST")) %>%
    subset(., select = -c(he,date,Latitude,Longitude,Demand,AIL,NRG_Stream,
                          Plant_Fuel,Plant_Type,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name,Revenue,Price)) %>%
    na.omit() %>%
    group_by(time) %>%
    mutate(fleet_CF = sum(gen)/sum(Capacity),
           deviance_CF = (Cap_Fac-fleet_CF)^2,
           ) %>%
    ungroup() %>%
    group_by(ID) %>%
    filter(n() >= 2232) %>%
    summarize(sd_CF = sqrt(mean(deviance_CF))
              )
  
  ab_data <- merge(data,alberta_samp, by="ID",#all=T
                   ) %>%
    mutate(diff = sd_wind-sd_CF,
           Installation=case_when(grepl("AKE1",ID)~2003,
                                  grepl("ARD1",ID)~2010,
                                  grepl("BSR1",ID)~2014,
                                  grepl("BTR1",ID)~2009,
                                  grepl("BUL",ID)~2015,
                                  grepl("CR1",ID)~2001,
                                  grepl("CRE3",ID)~2001,
                                  grepl("CRR1",ID)~2012,
                                  grepl("CRR2",ID)~2019,
                                  grepl("CYP",ID)~2022,
                                  grepl("FMG1",ID)~2022,
                                  grepl("GDP1",ID)~2022,
                                  grepl("GRZ1",ID)~2022,
                                  grepl("GWW1",ID)~2006,
                                  grepl("HAL1",ID)~2012,
                                  grepl("HHW1",ID)~2022,
                                  grepl("HLD1",ID)~2022,
                                  grepl("IEW",ID)~2010,
                                  grepl("JNR",ID)~2022,
                                  grepl("KHW1",ID)~2007,
                                  grepl("LAN1",ID)~2022,
                                  grepl("NEP1",ID)~2010,
                                  grepl("OWF1",ID)~2014,
                                  grepl("RIV1",ID)~2019,
                                  grepl("RTL1",ID)~2021,
                                  grepl("SCR2",ID)~2004,
                                  grepl("SCR3",ID)~2006,
                                  grepl("SCR4",ID)~2011,
                                  grepl("TAB1",ID)~2007,
                                  grepl("WHE1",ID)~2022,
                                  grepl("WHT1",ID)~2019,
                                  grepl("WHT2",ID)~2021,
                                  grepl("WRW1",ID)~2021,),
           sort = case_when(Installation < 2021~"old",
                            Installation >=2021~"new"))
  
  sz = 15
  
  ggplot(ab_data, aes(x = ID, y = diff)) +
    geom_col()+#aes(fill=fct_reorder(sort,desc(sort)))) +
#    scale_fill_manual(name="Installation Date", 
#                        labels=c("Before 2019","2019 or After"),
#                        values=c("royalblue3","forestgreen")
                        #values=c("black","grey45")
#    ) +
    #    scale_shape_manual(values=c(16,9), 
    #                       name = "Installation Date",
    #                       labels=c("2019 or After","Before 2019"))+
    #    geom_text(label=alberta_samp$ID, size = sz-12, hjust = 0, vjust = 0, 
    #              nudge_x = 0.001, nudge_y = 0.3) +
    labs(x = "Wind farm site",
         y = "Difference in standard deviation") + 
    theme(text = element_text(size = sz),
          axis.text.x = element_text(angle=90),
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
}

correlation_Aurora <- function() {
  # Plots the Capture price as a function of the output correlation to the rest 
  # of the fleet of that resource type, with the hypothetical sites added
  
  corm <- "pearson" # Define correlation method ("pearson", "kendall", "spearman")
  
  data <- ResourceHr %>%
    filter(Run_ID == BC,
           Primary_Fuel == "Wind")
  
  setwd("D:/Documents/GitHub/AuroraEval/WindProfile")
  
  AuroraProf <- readRDS("AuroraProfiles.RData") 
  
  alberta_samp <- sub_samp %>%
    filter(Plant_Type == plant_type) %>%
    subset(., select = -c(Demand,AIL,NRG_Stream,Plant_Fuel,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name)) %>%
    na.omit() %>%
    group_by(time) %>%
    mutate(fleet_gen = sum(gen),
           fleet_cap = sum(Capacity),
           fleet_CF = fleet_gen/fleet_cap) %>%
    ungroup() %>%
    group_by(ID) %>%
    summarize(#Capacity = median(Capacity),
              #Latitude = median(as.numeric(Latitude)),
              #Longitude = median(as.numeric(Longitude)),
              correlation = cor(Cap_Fac,fleet_CF, method=corm),
              Dispatched = sum(gen),
              Revenue = sum(Revenue),
              Capture_Price = Revenue/Dispatched,
              
    ) %>%
    ungroup() %>%
    mutate(Installation=case_when(grepl("CRR2",ID)~"post2019",
                                  grepl("CYP",ID)~"post2019",
                                  #grepl("CYP2",ID)~"post2019",
                                  grepl("FMG1",ID)~"post2019",
                                  grepl("HHW1",ID)~"post2019",
                                  grepl("HLD1",ID)~"post2019",
                                  grepl("JNR",ID)~"post2019",
                                  grepl("RIV1",ID)~"post2019",
                                  grepl("RTL1",ID)~"post2019",
                                  grepl("WHE1",ID)~"post2019",
                                  grepl("WHT",ID)~"post2019",
                                  grepl("WRW1",ID)~"post2019",),
           Installation=case_when(is.na(Installation)~"pre2019",
                                  TRUE~"post2019"))
  
  Aurora_corr <- AuroraProf %>%
    group_by(time) %>%
    mutate(fleet_gen = sum(gen),
           fleet_cap = sum(Capacity),
           fleet_CF = fleet_gen/fleet_cap) %>%
    ungroup() %>%
    na.omit() %>%
    group_by(ID) %>%
    summarize(Capacity = median(Capacity),
              Latitude = median(as.numeric(Latitude)),
              Longitude = median(as.numeric(Longitude)),
              correlation = cor(Cap_Fac,fleet_CF, method=corm),
              #Dispatched = sum(gen),
              #Revenue = sum(Revenue),
              #Capture_Price = Revenue/Dispatched,
              #Cap_Fac = mean(Cap_Fac),
              #fleet_cap = mean(fleet_cap),
              #fleet_gen = mean(fleet_gen),
              #fleet_CF = mean(fleet_CF)
              
    ) %>%
    ungroup() %>%
    mutate(Built=case_when(grepl("BUL",ID)~"post2015",
                           grepl("CRR2",ID)~"post2015",
                           grepl("CYP",ID)~"post2015",
                           #grepl("CYP2",ID)~"post2015",
                           grepl("FMG1",ID)~"post2015",
                           grepl("GRZ1",ID)~"post2015",
                           grepl("HHW1",ID)~"post2015",
                           grepl("HLD1",ID)~"post2015",
                           grepl("JNR",ID)~"post2015",
                           grepl("RIV1",ID)~"post2015",
                           grepl("RTL1",ID)~"post2015",
                           grepl("WHE1",ID)~"post2015",
                           grepl("WHT",ID)~"post2015",
                           grepl("WRW1",ID)~"post2015",
                           grepl("Anzac",ID)~"Potential",
                           grepl("BisonLake",ID)~"Potential",
                           grepl("ChainLakes",ID)~"Potential",
                           grepl("ClearPrairie",ID)~"Potential",
                           grepl("Falher",ID)~"Potential",
                           grepl("FortSaskatchewan",ID)~"Potential",
                           grepl("GrandeCache",ID)~"Potential",
                           grepl("Hinton",ID)~"Potential",
                           grepl("JohnDOr",ID)~"Potential",
                           grepl("Kehewin",ID)~"Potential",
                           grepl("LesserSlave",ID)~"Potential",
                           grepl("PigeonLake",ID)~"Potential",
                           grepl("SwanHills",ID)~"Potential",
                           TRUE~"pre2015"
    ),
    ) %>%
    arrange(match(Built, c("Potential","pre2015", "post2015")), 
            desc(Built))
  
  Aurora_corr$Built <- factor(Aurora_corr$Built, 
                              levels = c("pre2015","post2015","Potential"))
  
  sz = 15
  
  ggplot(Aurora_corr, aes(x = correlation, y = Capture_Price, 
                           colour = Installation, size = Capacity)) +
    geom_smooth(method='lm', aes(colour=Installation),
                show.legend=FALSE,fullrange=TRUE) +
    stat_regline_equation(label.x=0.6,show.legend=FALSE) +
    stat_cor(aes(label=..rr.label..), label.x=0.68,show.legend=FALSE) + 
    geom_point() +
    scale_colour_manual(name="Installation Date", 
                        labels=c("Before 2019","2019 or After"),
                        values=c("royalblue3","forestgreen")
                        #values=c("black","grey45")
    ) +
    #    scale_shape_manual(values=c(16,9), 
    #                       name = "Installation Date",
    #                       labels=c("2019 or After","Before 2019"))+
    #    geom_text(label=alberta_samp$ID, size = sz-12, hjust = 0, vjust = 0, 
    #              nudge_x = 0.001, nudge_y = 0.3) +
    labs(x = "Correlation between single site and aggregate capacity factors",
         y = "Average Capture Price ($/MWh)") + #, 
    #       title = "Alberta Wind and Solar Farms", 
    #       subtitle = "Capture Price vs Correlation") +
    guides(colour = guide_legend(override.aes = list(size = 5))#,
           #colour = guide_legend(override.aes = list(size =2))
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
    ) #+
  #    scale_x_reverse() 
}

correlation_map <- function(plant_type) {
  # Plots the Capture price as a function of the output correlation to the rest 
  # of the fleet of that resource type
  
  date_s <- "2020-01-01" # Define date for the start of the study period
  date_e <- "2021-02-01" # Define date for the end of the study period
  corm <- "pearson" # Define correlation method ("pearson", "kendall", "spearman")
  
  # Filter data for plant_type, calculate total output for fleet for each period
  alberta_samp <- sub_samp %>%
    filter(Plant_Type == plant_type) %>%
    subset(., select = -c(Demand,AIL,NRG_Stream,Plant_Fuel,GHG_ID,CO2,Heat.Rate,
                          co2_est,AESO_Name)) %>%
    na.omit() %>%
    group_by(time) %>%
    mutate(fleet_gen = sum(gen),
           fleet_cap = sum(Capacity),
           fleet_CF = fleet_gen/fleet_cap) %>%
    ungroup() %>%
    group_by(ID) %>%
    summarize(Capacity = median(Capacity),
              Latitude = median(as.numeric(Latitude)),
              Longitude = median(as.numeric(Longitude)),
              correlation = cor(Cap_Fac,fleet_CF, method=corm),
              Dispatched = sum(gen),
              Revenue = sum(Revenue),
              Capture_Price = Revenue/Dispatched,
              
    ) %>%
    ungroup() %>%
    mutate(Installation=case_when(grepl("CRR2",ID)~"post2019",
                                  grepl("CYP",ID)~"post2019",
                                  #grepl("CYP2",ID)~"post2019",
                                  grepl("FMG1",ID)~"post2019",
                                  grepl("HHW1",ID)~"post2019",
                                  grepl("HLD1",ID)~"post2019",
                                  grepl("JNR",ID)~"post2019",
                                  grepl("RIV1",ID)~"post2019",
                                  grepl("RTL1",ID)~"post2019",
                                  grepl("WHE1",ID)~"post2019",
                                  grepl("WHT",ID)~"post2019",
                                  grepl("WRW1",ID)~"post2019",),
           Installation=case_when(is.na(Installation)~"pre2019",
                                  TRUE~"post2019"))
  
  ################################################################################
  #Level 1 shows provinces, while level 2 shows individual counties
  #When getData is removed, use geodata package instead
  ################################################################################
  {can_level1 = getData("GADM", country = "CA", level = 1)
  
  WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  canada_level1_ellipsoid = spTransform(can_level1, WGS84)
  
  alberta_ellipsoid1 = 
    canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]
  }
  
  ################################################################################
  ################################################################################
  # Map of Alberta with active sites 
  ################################################################################
  ################################################################################
  
  ggplot() + 
    geom_tile(data = wind_profile, 
              aes(x = Longitude, y = Latitude, fill = Wind)) +
    geom_polygon(data = alberta_ellipsoid1, 
                 aes(x = long, y = lat, group = group), 
                 fill = "transparent", colour = "black") +
    geom_point(data = alberta_samp,
               aes(x= Longitude, y = Latitude, size = Capture_Price, color=Installation), 
               shape = 16) +
    labs(size = "Capture Price ($/MWh)") +
    scale_color_manual(values = c("darkmagenta", "black"), 
                       labels = c("Built since 2019","Built before 2019")) +
      scale_fill_gradientn(colors = matlab.like2(100),
                           limits=c(3.5,10), na.value="white",oob=squish, 
                           name = "Mean wind speed \nat 80m height \n(m/s)") +
    #scale_fill_gradient(low="white", high="white", limits=c(3.5,25), na.value="red",
    #                      oob=squish, 
    #                      name = "Mean wind speed \nat 80m height \n(m/s)"
    #)+
    #  scale_fill_gradientn(colors = c("navy","turquoise1","green",
    #                                  "yellow","orangered","red4"),
    #                       values=c(3.5,5,6.5,7.5,8.5,10),oob=squish, 
    #                       name = "Mean wind speed \nat 80m height \n(m/s)") +
  #guides(shape = guide_legend(override.aes = list(size = 5))) +  
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        #legend.background = element_blank(),
        #legend.box.background = element_blank(),
        #legend.text = element_blank(),
        #legend.title = element_blank()
  ) 
  
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

cogen_bidding <- function(Type) {
  # A function to calculate the interquartile range of the capacity factor for a 
  # specific plant along with the heat rates at those capacities.
  
  # Filter Dr. Leach's student data for specific plant
  dataA <- merit_filt %>%
    filter(Plant_Type == Type,
           price != 0) %>%
    group_by(date,he) %>%
    summarise(available = sum(available_mw))
    #subset(., select = c(date, he, block_number, size, price, available_mw, 
    #                     dispatched_mw, flexible))
  
  # Filter NRG Stream data for specific plant, and add columns for month and day
  dataB <- sub_samp %>%
    filter(Plant_Type == Type) %>%
    subset(., select = c(date, he, gen, Capacity, Cap_Fac, Heat.Rate)) %>%
    group_by(date,he) %>%
    summarise(#Cap_Fac = weighted.mean(Cap_Fac,gen),
              #gen = sum(gen),
              Capacity = sum(Capacity)) %>%
    mutate(month = month(as.POSIXlt(date, format="%Y-%m-%d")), 
           day = day(as.POSIXlt(date, format="%Y-%m-%d"))) #%>%
#    mutate(Cap_Fac = gen/Capacity)
  
  data <- merge(dataA, dataB, by = c("date", "he"))
  
  # Cumbersome code to specify the season  
  {
  data1 <-data %>%
    filter(month == 1 | month == 2) %>%
    mutate(Season = "Winter")
  
  data2 <- data %>%
    filter(month == 3 & day < 21) %>%
    mutate(Season = "Winter")
  
  data3 <- data %>%
    filter(month == 3 & day >= 21) %>%
    mutate(Season = "Spring")
  
  data4 <- data %>%
    filter(month == 4 | month == 5) %>%
    mutate(Season = "Spring")
  
  data5 <- data %>%
    filter(month == 6 & day < 21) %>%
    mutate(Season = "Spring")
  
  data6 <- data %>%
    filter(month == 6 & day >= 21) %>%
    mutate(Season = "Summer")
  
  data7 <- data %>%
    filter(month == 7 | month == 8) %>%
    mutate(Season = "Summer")
  
  data8 <- data %>%
    filter(month == 9 & day < 21) %>%
    mutate(Season = "Summer")
  
  data9 <- data %>%
    filter(month == 9 & day >= 21) %>%
    mutate(Season = "Fall")
  
  data10 <- data %>%
    filter(month == 10 | month == 11) %>%
    mutate(Season = "Fall")
  
  data11 <- data %>%
    filter(month == 12 & day < 21) %>%
    mutate(Season = "Fall")
  
  data12 <- data %>%
    filter(month == 12 & day >= 21) %>%
    mutate(Season = "Winter")
  }
  
  # Group by season, calculate the quartile ranges and heat rates at those ranges
  data <- rbind(data1,data2,data3,data4,data5,data6,
                data7,data8,data9,data10,data11,data12)
  
  plant <- data %>%
    group_by(Season)%>% 
#    group_by(date, he) %>%
    summarise(Available = sum(available), 
              Capacity = sum(Capacity)) %>%
#    summarise(#Generation = quantile(gen,probs=seq(0.25,0.75, 1/4)),
#      Cap_Fac = quantile(Cap_Fac,probs=seq(0.25,0.75, 1/4)),
#    ) %>%
    mutate(Cap_Fac = 1-Available/Capacity,
           Heat_Rate = (5357.1*(Cap_Fac)^2-11150*(Cap_Fac)+15493),
           Cap_Fac = Cap_Fac*100)
  
  
  
  # Display the data
  View(plant)
  
}

cogen_range <- function(plant_ID) {
  # A function to calculate the interquartile range of the capacity factor for a 
  # specific plant along with the heat rates at those capacities.
  
  # Filter Dr. Leach's student data for specific plant
  dataA <- merit_filt %>%
    filter(plant_ID == asset_id) %>%
    subset(., select = c(date, he, block_number, size, price, available_mw, dispatched_mw, flexible)) %>%
    group_by(date,he) #%>%
#    summarise(available = sum(available_mw),
#              size = sum(size),
#              dispatched = sum(dispatched_mw))
  
  # Filter NRG Stream data for specific plant, and add columns for month and day
  dataB <- sub_samp %>%
    filter(plant_ID == ID) %>%
    subset(., select = c(date, he, gen, Capacity, Cap_Fac, Heat.Rate)) %>%
    mutate(month = month(as.POSIXlt(date, format="%Y-%m-%d")), 
           day = day(as.POSIXlt(date, format="%Y-%m-%d")))
  
  data <- merge(dataA, dataB, by = c("date", "he"))
  
  # Cumbersome code to specify the season  
  data1 <-data %>%
    filter(month == 1 | month == 2) %>%
    mutate(Season = "Winter")
  
  data2 <- data %>%
    filter(month == 3 & day < 21) %>%
    mutate(Season = "Winter")
  
  data3 <- data %>%
    filter(month == 3 & day >= 21) %>%
    mutate(Season = "Spring")
  
  data4 <- data %>%
    filter(month == 4 | month == 5) %>%
    mutate(Season = "Spring")
  
  data5 <- data %>%
    filter(month == 6 & day < 21) %>%
    mutate(Season = "Spring")
  
  data6 <- data %>%
    filter(month == 6 & day >= 21) %>%
    mutate(Season = "Summer")
  
  data7 <- data %>%
    filter(month == 7 | month == 8) %>%
    mutate(Season = "Summer")
  
  data8 <- data %>%
    filter(month == 9 & day < 21) %>%
    mutate(Season = "Summer")
  
  data9 <- data %>%
    filter(month == 9 & day >= 21) %>%
    mutate(Season = "Fall")
  
  data10 <- data %>%
    filter(month == 10 | month == 11) %>%
    mutate(Season = "Fall")
  
  data11 <- data %>%
    filter(month == 12 & day < 21) %>%
    mutate(Season = "Fall")
  
  data12 <- data %>%
    filter(month == 12 & day >= 21) %>%
    mutate(Season = "Winter")
  
  # Group by season, calculate the quartile ranges and heat rates at those ranges
  data <- rbind(data1,data2,data3,data4,data5,data6,
                 data7,data8,data9,data10,data11,data12) %>%
    mutate(BTF = (gen-dispatched_mw)/Capacity*100)
  
  plant <- data %>%
    group_by(Season) %>%
    summarise(#Generation = quantile(gen,probs=seq(0.25,0.75, 1/4)),
      BTF = mean(BTF),
      Cap_Fac = quantile(Cap_Fac,probs=seq(0.25,0.75, 1/4)),
      #heat.rate = median(Heat.Rate)
      ) %>%
    mutate(Heat_Rate = (5357.1*(Cap_Fac)^2-11150*(Cap_Fac)+15493),
           Cap_Fac = Cap_Fac*100)
  
  
  
  # Display the data
  View(plant)
  
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
  
  colkeep <- ifelse(parameter == "xaxis", 1,6)
  
  data1 <- data2[,c(3,2,colkeep)]
  data3 <- reshape2::dcast(data1,tier~month)

  setwd("F:/My Drive/transfer")
  write.csv(data3, "Incremental_Bid_Factors.csv")
}

cap_type <- function(plant_type){#, year){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date,he,month,price) %>%
    summarise(dispatched_mw = sum(dispatched_mw)) %>%
    select(date, month, he, price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  type <- plant_type
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date, he) %>%
    summarise(Capacity = sum(Capacity)) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
#  input <- ifelse(peak == "on", "On-Peak WECC", "Off-Peak WECC")
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(Capacity_Factor = ifelse(dispatched_mw/Capacity > 1, 1, 
                                    dispatched_mw/Capacity),
    )
  
    data2 <- data1 %>%
      filter(price == 0) %>%
      group_by(date,he) %>%
      mutate(tier = 1, Cap_F = sum(Capacity_Factor))
    
    data1 <- data1 %>%
      filter(price != 0) %>%
      group_by(date,he) %>%
      arrange(price) %>%
      mutate(tier = 2:(n()+1), Cap_F = sum(Capacity_Factor))
    
    data3 <- rbind(data1,data2)
  
  # Omit NA data
  data <- na.omit(data3)
  
  data <- data %>%
    mutate(
      Condition = if_else(between(he, 08, 23), 
                          "On-Peak", "Off-Peak"),
      bidding_fact = round(price/mn-1,2)
    )
  
  data1 <- data %>%
    filter(Condition == "On-Peak")
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data1 %>%
#    mutate(fact = round(price/mn-1,2)) %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
#              sdP = sd(price),
              avCap_Fac = mean(Capacity_Factor),
#              sdC = sd(Capacity_Factor),
#              avbid_fact = mean(bidding_fact),
#              sdB = sd(bid_fact)
    ) %>%
    mutate(bid_fact = round(avPrice/mn-1,2))
  
  data2 <- data3 %>%
    group_by(month,avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
              bid_fact = max(bid_fact),
              tier = min(tier)) %>%
    arrange(month,tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row

  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(month) %>%
    mutate(overall_cap = cumsum(avCap_Fac))
    
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
#  par <- ifelse(parameter == "cap", "CapFac", "BidFac")
  
#  colkeep1 <- ifelse(parameter == "cap", 6, 4)
  
  data1 <- data3[,c(1,5,6)]
  data1 <- reshape2::dcast(data1,tier~month)
  data2 <- data2[,c(1,5,4)]
  data2 <- reshape2::dcast(data2,tier~month)
  
  setwd("F:/My Drive/transfer")
  write.csv(data1, paste(type,"CapFac","OnPeak","Incremental_Bid_Factors.csv",sep="_"))
  write.csv(data2, paste(type,"BidFac","OnPeak","Incremental_Bid_Factors.csv",sep="_"))
  
  data1 <- data %>%
    filter(Condition == "Off-Peak")
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data1 %>%
    #    mutate(fact = round(price/mn-1,2)) %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(Capacity_Factor),
    ) %>%
    mutate(bid_fact = round(avPrice/mn-1,2))
  
  data2 <- data3 %>%
    group_by(month,avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
              bid_fact = max(bid_fact),
              tier = min(tier)) %>%
    arrange(month,tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(month) %>%
    mutate(overall_cap = cumsum(avCap_Fac))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  data1 <- data3[,c(1,5,6)]
  data1 <- reshape2::dcast(data1,tier~month)
  data2 <- data2[,c(1,5,4)]
  data2 <- reshape2::dcast(data2,tier~month)
  
  write.csv(data1, paste(type,"CapFac","OffPeak","Incremental_Bid_Factors.csv",sep="_"))
  write.csv(data2, paste(type,"BidFac","OffPeak","Incremental_Bid_Factors.csv",sep="_"))
}

table_type <- function(plant_type){#, year){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date,he,month,price) %>%
    summarise(dispatched_mw = sum(dispatched_mw)) %>%
    select(date, he, price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  type <- plant_type
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date, he) %>%
    summarise(Capacity = sum(Capacity)) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(Capacity_Factor = ifelse(dispatched_mw/Capacity > 1, 1, 
                                    dispatched_mw/Capacity),
    )
  
  data2 <- data1 %>%
    filter(price == 0) %>%
    group_by(date,he) %>%
    mutate(tier = 1)#, Cap_F = sum(Capacity_Factor))
  
  data1 <- data1 %>%
    filter(price != 0) %>%
    group_by(date,he) %>%
    arrange(price) %>%
    mutate(tier = 2:(n()+1))#, Cap_F = sum(Capacity_Factor))
  
  data3 <- rbind(data1,data2)
  
  # Omit NA data
  data <- na.omit(data3)
  
  data <- data %>%
    mutate(
      Condition = if_else(between(he, 08, 23), 
                          "On-Peak", "Off-Peak")
    )
  
#  data1 <- data %>%
#    filter(Condition == "On-Peak")
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data %>%
    group_by(tier, Condition) %>%#,month) %>%
    summarise(avPrice = mean(price), 
              #              sdP = sd(price),
              avCap_Fac = mean(Capacity_Factor),
              #              sdC = sd(Capacity_Factor),
    )
  
  # Lump duplicate avCap_Fac together
  data2 <- data3 %>%
    group_by(avCap_Fac, Condition) %>%
    summarise(avPrice = max(avPrice),
              tier = min(tier)) %>%
    arrange(tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(Condition) %>% #month) %>%
    mutate(overall_cap = round(cumsum(avCap_Fac),2),
           bid_fact = round(avPrice/mn-1,2)) %>%
    ungroup() %>%
    group_by(overall_cap, Condition) %>%
    summarise(tier = min(tier),
              bid_fact = mean(bid_fact))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  #  par <- ifelse(parameter == "cap", "CapFac", "BidFac")
  
  #  colkeep1 <- ifelse(parameter == "cap", 6, 4)
  
#  data1 <- data3[,c(2,3,4,5,6)]
#  data1 <- reshape2::dcast(data1,tier~month)
#  data2 <- data2[,c(1,5,4)]
#  data2 <- reshape2::dcast(data2,tier~month)
  
  setwd("F:/My Drive/transfer")
  write.csv(data3, paste(type,"Incremental_Bid_Factors.csv",sep="_"))
#  write.csv(data2, paste(type,"BidFac","OnPeak","Incremental_Bid_Factors.csv",sep="_"))
}

table_data <- function(plant_type){#, year){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date,he,month,price) %>%
    summarise(dispatched_mw = sum(dispatched_mw)) %>%
    select(date, he, price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  type <- plant_type
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date, he) %>%
    summarise(Capacity = sum(Capacity)) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(Capacity_Factor = ifelse(dispatched_mw/Capacity > 1, 1, 
                                    dispatched_mw/Capacity),
    )
  
  data2 <- data1 %>%
    filter(price == 0) %>%
    group_by(date,he) %>%
    mutate(tier = 1)#, Cap_F = sum(Capacity_Factor))
  
  data1 <- data1 %>%
    filter(price != 0) %>%
    group_by(date,he) %>%
    arrange(price) %>%
    mutate(tier = 2:(n()+1))#, Cap_F = sum(Capacity_Factor))
  
  data3 <- rbind(data1,data2)
  
  # Omit NA data
  data <- na.omit(data3)
  
  #data <- data %>%
  #  mutate(
  #    Condition = if_else(between(he, 08, 23), 
  #                        "On-Peak", "Off-Peak")
  #  )
  
  #  data1 <- data %>%
  #    filter(Condition == "On-Peak")
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data %>%
    group_by(tier) %>%#,month) %>%
    summarise(avPrice = mean(price), 
              #              sdP = sd(price),
              avCap_Fac = mean(Capacity_Factor),
              #              sdC = sd(Capacity_Factor),
    )
  
  # Lump duplicate avCap_Fac together
  data2 <- data3 %>%
    group_by(avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
              tier = min(tier)) %>%
    arrange(tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
#    group_by(Condition) %>% #month) %>%
    mutate(overall_cap = round(cumsum(avCap_Fac),2),
           bid_fact = round(avPrice/mn-1,2)) %>%
    ungroup() %>%
    group_by(overall_cap) %>%
    summarise(tier = min(tier),
              bid_fact = mean(bid_fact))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  #  par <- ifelse(parameter == "cap", "CapFac", "BidFac")
  
  #  colkeep1 <- ifelse(parameter == "cap", 6, 4)
  
  #  data1 <- data3[,c(2,3,4,5,6)]
  #  data1 <- reshape2::dcast(data1,tier~month)
  #  data2 <- data2[,c(1,5,4)]
  #  data2 <- reshape2::dcast(data2,tier~month)
  
  setwd("F:/My Drive/transfer")
  write.csv(data3, paste(type,"_Bid_Factors.csv",sep="_"))
  #  write.csv(data2, paste(type,"BidFac","OnPeak","Incremental_Bid_Factors.csv",sep="_"))
}

graph_type <- function(plant_type){
  # Plots each month capture prices, bidding factors at the various capacities
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date,he,month,price) %>%
    summarise(dispatched_mw = sum(dispatched_mw)) %>%
    select(date, month, he, price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  type <- plant_type
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date, he) %>%
    summarise(Capacity = sum(Capacity)) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  #  input <- ifelse(peak == "on", "On-Peak WECC", "Off-Peak WECC")
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(Capacity_Factor = ifelse(dispatched_mw/Capacity > 1, 1, 
                                    dispatched_mw/Capacity),
    )
  
  data2 <- data1 %>%
    filter(price == 0) %>%
    group_by(date,he) %>%
    mutate(tier = 1, Cap_F = sum(Capacity_Factor))
  
  data1 <- data1 %>%
    filter(price != 0) %>%
    group_by(date,he) %>%
    arrange(price) %>%
    mutate(tier = 2:(n()+1), Cap_F = sum(Capacity_Factor))
  
  data3 <- rbind(data1,data2)
  
  # Omit NA data
  data <- na.omit(data3)
  
  data <- data %>%
    mutate(
      Condition = if_else(between(he, 08, 23), 
                          "On-Peak", "Off-Peak"),
 #     bidding_fact = round(price/mn-1,2)
    )

  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data %>%
    #    mutate(fact = round(price/mn-1,2)) %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              #              sdP = sd(price),
              avCap_Fac = mean(Capacity_Factor),
              #              sdC = sd(Capacity_Factor),
              #              avbid_fact = mean(bidding_fact),
              #              sdB = sd(bid_fact)
    ) #%>%
#    mutate(bid_fact = round(avPrice/mn-1,2))
  
  data2 <- data3 %>%
    group_by(month,avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
#              bid_fact = max(bid_fact),
              tier = min(tier)) %>%
    arrange(month,tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(month) %>%
    mutate(overall_cap = cumsum(avCap_Fac))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data1 %>%
    #    mutate(fact = round(price/mn-1,2)) %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(Capacity_Factor),
    ) %>%
    mutate(bid_fact = round(avPrice/mn-1,2))
  
  data2 <- data3 %>%
    group_by(month,avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
              bid_fact = max(bid_fact),
              tier = min(tier)) %>%
    arrange(month,tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(month) %>%
    mutate(overall_cap = cumsum(avCap_Fac))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)

  # Rename the column with the correct capacity factors to the 'xaxis' and change
  # column format to character for readability
#  names(data2)[mx2tier] <- "xaxis"
  data3$overall_cap <- as.character(paste(round(data3$overall_cap*100,3),"%",sep=""))
  
  # Remove extra columns
#  data2 <- data2[,c(mx2tier,8,9,10,11,12)]
  
  # Relabel the second top tier if it was never dispatched ie. it was another
  # maximum price
  data4 <- data3 %>%
    group_by(month) %>%
    filter(tier == max(tier[tier!=max(tier)])) %>%
    mutate(overall_cap = ifelse(avCap_Fac == 0, paste(overall_cap,"~100%",sep=""),overall_cap))
  
  # Relabel the top tier to be a range to 100%
  data2 <- data3 %>%
    group_by(month) %>%
    filter(tier == max(tier)) %>%
    mutate(overall_cap = paste(overall_cap,"-100%",sep=""))
  
  data3 <- data3 %>%
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
         aes(x = reorder(overall_cap,tier), y = avPrice)) +
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
         title = type
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

yearly_dmd <- function(year) {
  data <- demand 
  
  data$Hour <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  data$Year <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  data$Date <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%Y-%m-%d")
  
  data <- data %>%
    filter(Year == year) %>%
    select(Date, Hour, Demand)
  
  setwd("D:/Documents/GitHub/AuroraEval")
  
  #SAVE FILE (To save the processed data) This is to be entered into Aurora.
  write.csv(data, file=paste("Alberta",year,".csv",sep=""))
}

monthly_dmd_ave <- function() {
  data <- demand 
  
#  data$Hour <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
#  data$Month <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%m")
  data$Year <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
#  data$Date <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%Y-%m-%d")
  
  data <- na.omit(data)
  
  data1 <- data %>%
    group_by(Year) %>%
    summarise(Load = mean(Demand)) %>%
    mutate(case = "Average")
  
  data2 <- data %>%
    group_by(Year) %>%
    summarise(Load = max(Demand)) %>%
    mutate(case = "Peak")
  
  data <- rbind(data1,data2)
  data$Year <- as.numeric(data$Year)
#  data$Year <- ISOdate(data$Year, 1, 1)
  
  ggplot(data = data, 
         aes(x=Year, y=Load, group = case)) +
#    geom_line(aes(linetype=case, color=case)) +
    geom_point(aes(shape = case, color=case)) +
#    geom_smooth(method="lm", fullrange=TRUE) +
    geom_smooth(method=lm, fullrange = TRUE, se=FALSE, 
                aes(color=case)) +
    stat_regline_equation(
#      label.y = 12000, 
      aes(label = ..eq.label..)
      ) +
    stat_regline_equation(
      label.x = 2010,
      aes(label = ..rr.label..)
      ) +
    geom_vline(xintercept = 2040,
               linetype = "solid", color = "dodgerblue", size = 0.5) +
    geom_hline(yintercept = 14860, linetype = "solid", color = "forestgreen", 
               size = 0.5) +
    geom_hline(yintercept = 12625, linetype = "solid", color = "forestgreen", 
               size = 0.5) +
#    geom_text(x = 2005, y = 11500, label = lm_eqn(data), parse = TRUE) +
    scale_color_manual(values=c("black","grey40")) +
#    scale_linetype_manual(values=c("solid","longdash")) +
    scale_x_continuous(breaks = seq(2004,2040,2),
                     limits = c(2004,2040)
                     ) +
    theme_bw() +
    theme(axis.text.x = element_text(vjust = 1, hjust = 0.5),
          panel.background = element_rect(fill = "transparent"),
#          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          legend.title = element_blank(),
    ) +
    scale_y_continuous(limits=c(7000,15000),
                       breaks=seq(7000,15000,500)) +
    labs(y = "Load (MWh)",
         )
}

load_duration <- function(year1, year2) {
  # Plots the load duration vs percentile for AESO
  # Like AESO Market Report 2021 Figures 7 and 8
  
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
                               "On-Peak", "Off-Peak")) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(AIL)(AIL)) %>%
    subset(., select = c(Condition, Year, AIL, perc)) %>%
    ungroup() %>%
    mutate(Demand = AIL) %>%
    dplyr::select(Condition, Year, Demand, perc)

  # Set font size for plot
  sz <- 15
  loadcolors <- c("2017"="gray60", "2018"="forestgreen", 
                  "2019"="cornflowerblue",
                  "2020"="goldenrod1","2021"="firebrick")
  
  ggplot() +
    geom_line(data = totAct, 
              aes(x = perc, y = Demand, colour = Year), size = 1) +
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
    #     title = "AESO Historical Data"
    ) +
    scale_color_manual(values = AESO_colours1) +
    scale_x_continuous(expand=c(0,0), 
                       #limits = c(0,1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

power_curve <- function() {
  # a function to plot a typical wind turbine power curve
  
  # Define cut-in, rated, and cut-out
  cutin <- 3.5
  rated <- 13
  cutout <- 25
  
  P_rated <- 1.225/2*3.14*300*rated^3/1000
  
  data <- data.frame(u = seq(0,27, by=0.01),
                     P = 0)
                       
  data <- data %>%
    mutate(P = case_when(u >= rated & u < cutout ~ (1.225/2*3.14*300*rated^3/1000),
                         u < rated & u >= cutin ~ (1.225/2*3.14*300*u^3/1000),
                         u < cutin | u >= cutout ~ 0))
  
  sz <- 10
  
  ggplot(data,aes(u,P)) +
    geom_hline(yintercept = P_rated, linetype="dashed", color="gray",size=1) +
    geom_vline(xintercept = cutin, linetype="dashed", color="gray",size=1) +
    geom_vline(xintercept = rated, linetype="dashed", color="gray",size=1) +
    geom_vline(xintercept = cutout, linetype="dashed", color="gray",size=1) +
    geom_line(size = 1) +
    geom_text(data = data.frame(x = cutin, y = -30, 
                                 label = "italic('v')[italic('cut-in')]"),
               aes(x=x,y=y,label=label),parse=TRUE,
               x = cutin, y = -(sz*10),
              color = "black",size=sz,family="serif",fontface="italic") +
    geom_text(data = data.frame(x = rated, y = -30, 
                                 label = "italic('v')[italic('rated')]"),
               aes(x=x,y=y,label=label), parse=TRUE,
              x = rated, y = -(sz*10),
              color = "black",size=sz,family="serif",fontface="italic") +
    geom_text(data = data.frame(x = cutout, y = -30, 
                                 label = "italic('v')[italic('cut-out')]"),
               aes(x=x,y=y,label=label), parse=TRUE,
              x = cutout, y = -(sz*10),
              color = "black",size=sz,family="serif",fontface="italic") +
    geom_text(data = data.frame(x = -1, y = P_rated, 
                                 label = "italic('P')[italic('rated')]"),
               aes(x=x,y=y,label=label), parse=TRUE,
               x = -1.5, y = P_rated,
               color = "black",size=sz,family="serif",fontface="italic") +
    scale_x_continuous(expand=c(0,0),
                       breaks = c(cutin,rated,cutout)) +
    scale_y_continuous(expand=c(0,0),
                       breaks = P_rated,
                       limits = c(0,P_rated+100)) +
    labs(y = "Power Output \n(kW)", x = "Wind Speed (m/s)") +
    coord_cartesian(xlim = c(0, 27), # This focuses the x-axis on the range of interest
                    clip = 'off') + 
    theme(axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_blank(),
          axis.title.x = element_text(vjust=-5,size=sz+15),
          axis.title.y = element_text(vjust=3,size=sz+15),
          panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.margin = unit(c(1,1,2.5,2),"lines"),
          text = element_text(size= 15)
    )
}