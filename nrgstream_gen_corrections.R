# Set location where AESO data is stored
################################################################################
setwd("D:/Documents/Education/Masters Degree/Datasets/Market")

load("nrgstream_gen.RData") 
nrgstream_gen <- nrgstream_gen %>% rename(time=Time)

nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 

corrected <- nrgstream_gen %>%
  filter(is.na(Latitude),
#         Plant_Type != "TRADE" & Plant_Type != "EXPORT" & Plant_Type != "IMPORT" & 
#         Plant_Type != "CR" & Plant_Type != "TOTAL" & Plant_Type !="PRICE" &
#         Plant_Type != "WIND_FCAST" & Plant_Type != "COGEN" & Plant_Type != "SCGT" &
#         Plant_Type != "STORAGE", Plant_Type != "OTHER"
  ) #%>%
#  group_by(ID,AESO_Name,Plant_Type) %>%
#  summarize(cap = median(Capacity),
#            Latitude = median(Latitude),
#            Longitude = median(Longitude))

corrected <- corrected %>%
  mutate(Latitude=case_when(grepl("BRD1",ID) ~ 49.842735,
                            grepl("BUR1",ID) ~ 49.814877,
                            grepl("CLR",ID) ~ 50.032911,
                            grepl("CLY",ID) ~ 49.840967,
                            grepl("CHP1",ID) ~ 50.22189,
                            grepl("COL1",ID) ~ 49.833218,
                            grepl("CRD",ID) ~ 49.807,
                            grepl("CRR2",ID) ~ 49.55891,
                            grepl("FMG1",ID) ~ 49.66334,
                            grepl("KKP",ID) ~ 53.469986,
                            grepl("MON1",ID) ~ 49.833144,
                            grepl("NMK1",ID) ~ 51.026118,
                            grepl("RIV1",ID) ~ 49.53245,
                            grepl("STR",ID) ~ 51.033273,
                            grepl("TVS1",ID) ~ 50.27324,
                            grepl("VCN1",ID) ~ 50.0975,
                            grepl("VXH1",ID) ~ 50.095223,
                            grepl("WEF1",ID) ~ 49.65405,
                            grepl("WHT",ID) ~ 49.64029),
         Longitude=case_when(grepl("BRD1",ID) ~ -111.537891,
                             grepl("BUR1",ID) ~ -111.543323,
                             grepl("CHP1",ID) ~ -110.437106,
                             grepl("CLR",ID) ~ -113.484369,
                             grepl("CLY",ID) ~ -110.356864,
                             grepl("COL1",ID) ~ -112.97448,
                             grepl("CRD",ID) ~ -112.578,
                             grepl("CRR2",ID) ~ -113.983,
                             grepl("FMG1",ID) ~ -111.122,
                             grepl("KKP",ID) ~ -113.61337,
                             grepl("MON1",ID) ~ -112.974231,
                             grepl("NMK1",ID) ~ -113.163017,
                             grepl("RIV1",ID) ~ -113.977,
                             grepl("STR",ID) ~ -113.371296,
                             grepl("TVS1",ID) ~ -112.73059,
                             grepl("VCN1",ID) ~ -112.84841,
                             grepl("VXH1",ID) ~ -112.149936,
                             grepl("WEF1",ID) ~ -111.515812,
                             grepl("WHT",ID) ~ -111.291))

nocorrection <- nrgstream_gen %>%
  filter(!is.na(Latitude))

nrgstream_gen <- rbind(corrected,nocorrection)

saveRDS(nrgstream_gen, file = "nrgstream_gen.RData")
