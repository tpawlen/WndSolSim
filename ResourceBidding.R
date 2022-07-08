plant <- "GN2"

SDH1a <- sub_samp %>%
  filter(ID == plant, 
         time >= as.Date("2020-01-1")) %>%
  subset(., select = c(time, ID, Capacity, Plant_Type))

SDH1 <- merit_filt %>%
  filter(asset_id == plant, 
         date >= as.Date("2020-01-1")) %>%
  mutate(time = paste0(date," ", he-1, ":00:00")) %>%
  subset(., select = c(time, available_mw, price))# %>%
  group_by(time)
  summarise(total = sum(available_mw))

  SDH1 <- merge(SDH1a, SDH1, by = "time")
  
  
CogenA <- sub_samp %>%
  filter(Plant_Type == "COGEN", 
         time >= as.Date("2020-01-1")) %>%
  subset(., select = c(time, ID, gen, Capacity))

CogenB <- merit_filt %>%
  filter(Plant_Type == "COGEN",
         date >= as.Date("2020-01-1")) %>%
  mutate(time = paste0(date," ", hour-1, ":00:00"),
         ID = asset_id) %>%
  subset(., select = c(time, ID, available_mw, dispatched_mw, size, flexible, price))

Cogen <- merge(CogenA, CogenB, by = c("time","ID"))
