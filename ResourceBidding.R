#plant <- "GN2"

#SDH1a <- sub_samp %>%
#  filter(ID == plant, 
#         time >= as.Date("2020-01-1")) %>%
#  subset(., select = c(time, ID, Capacity, Plant_Type))

#SDH1 <- merit_filt %>%
#  filter(asset_id == plant, 
#         date >= as.Date("2020-01-1")) %>%
#  mutate(time = paste0(date," ", he-1, ":00:00")) %>%
#  subset(., select = c(time, available_mw, price))# %>%
#  group_by(time)
#  summarise(total = sum(available_mw))

#  SDH1 <- merge(SDH1a, SDH1, by = "time")
  
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
  subset(., select = c(date,hour, AESO_Name, ID, available_mw, dispatched_mw, size, flexible, price))

Cogen <- merge(CogenA, CogenB, by = c("date","hour","ID"))

BidZero<- Cogen %>%
  filter(price == 0) %>%
  group_by(ID, AESO_Name) %>%
  summarise(must_run = mean(dispatched_mw), Capacity = mean(Capacity)) %>%
  mutate(Percent = must_run/Capacity*100)

if (plant_type == "COGEN" | plant_type == "SCGT") {
  BidZero <- BidZero %>%
    mutate(Heat_Rate = (5357.1*(Percent/100)^2-11150*(Percent/100)+15493),
           Heat_Rate1 = (5357.1*((Percent+1)/100)^2-11150*((Percent+1)/100)+15493),
           Heat_Rate100 = (5357.1-11150+15493))
}

if (plant_type == "COAL") {
  BidZero <- BidZero %>%
    mutate(Heat_Rate = (1517.9*(Percent/100)^2-3233.9*(Percent/100)+11136),
           Heat_Rate1 = (1517.9*((Percent+1)/100)^2-3233.9*((Percent+1)/100)+11136),
           Heat_Rate100 = (1517.9-3233.9+11136))
} 

if (plant_type == "NGCC") {
  BidZero <- BidZero %>%
    mutate(Heat_Rate = (5803.6*(Percent/100)^2-11034*(Percent/100)+12800),
           Heat_Rate1 = (5803.6*((Percent+1)/100)^2-11034*((Percent+1)/100)+12800),
           Heat_Rate100 = (5803.6-11034+12800))
}

if (plant_type == "NGCONV") {
  BidZero <- BidZero %>%
    mutate(Heat_Rate = (892.86*(Percent/100)^2-2610.7*(Percent/100)+11521),
           Heat_Rate1 = (892.86*((Percent+1)/100)^2-2610.7*((Percent+1)/100)+11521),
           Heat_Rate100 = (892.86-2610.7+11521))
}

Max <- Cogen %>% 
  filter(price >=900) %>%
  group_by(ID, AESO_Name) %>%
  summarise(max_run = mean(available_mw), Capacity = mean(Capacity), price = mean(price)) %>%
  mutate(Percent = (1-max_run/Capacity)*100)
  

setwd("E:/My Drive/transfer")
write.csv(BidZero, paste0(plant_type,"_Bid_Zero.csv"))
}
