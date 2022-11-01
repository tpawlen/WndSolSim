# Script to merge wind profiles
setwd("D:/Documents/GitHub/AuroraEval/WindProfile")
getwd()

{
  Anzac <- read.csv(file = 'la56345lo248727_Anzac.csv') %>%
  subset(., select = c(year,month,day,hour,Outage)) %>%
  na.omit() %>%
  mutate(Latitude = 56.345,
         Longitude = -(360-248.727),
         Cap_Fac = 1-Outage/100,
         ID = "Anzac")
  
  BisonLake <- read.csv(file = 'la57387lo244203_BisonLake.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 57.387,
           Longitude = -(360-244.203),
           Cap_Fac = 1-Outage/100,
           ID = "BisonLake")
  
  ChainLakes <- read.csv(file = 'la50257lo245828_ChainLakes.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 50.257,
           Longitude = -(360-245.828),
           Cap_Fac = 1-Outage/100,
           ID = "ChainLakes")
  
  ClearPrairie <- read.csv(file = 'la56724lo240504_ClearPrairie.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 56.724,
           Longitude = -(360-240.504),
           Cap_Fac = 1-Outage/100,
           ID = "ClearPrairie")
  
  Falher <- read.csv(file = 'la55729lo242805_Falher.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 55.729,
           Longitude = -(360-242.805),
           Cap_Fac = 1-Outage/100,
           ID = "Falher")
  
  FortSaskatchewan <- read.csv(file = 'la53680lo246840_FortSaskatchewan.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 53.67672,
           Longitude = -(360-246.830),
           Cap_Fac = 1-Outage/100,
           ID = "FortSaskatchewan")
  
  GrandeCache <- read.csv(file = 'la54435lo240672_GrandeCache.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 54.435,
           Longitude = -(360-240.672),
           Cap_Fac = 1-Outage/100,
           ID = "GrandeCache")
  
  Hinton <- read.csv(file = 'la53342lo242524_Hinton.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 53.342,
           Longitude = -(360-242.524),
           Cap_Fac = 1-Outage/100,
           ID = "Hinton")
  
  JohnDOr <- read.csv(file = 'la58794lo245024_JohnDOr.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 58.794,
           Longitude = -(360-245.024),
           Cap_Fac = 1-Outage/100,
           ID = "JohnDOr")
  
  Kehewin <- read.csv(file = 'la54059lo249196_Kehewin.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 54.059,
           Longitude = -(360-249.196),
           Cap_Fac = 1-Outage/100,
           ID = "Kehewin")
  
  LesserSlave <- read.csv(file = 'la55435lo244922_LesserSlave.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 55.435,
           Longitude = -(360-244.922),
           Cap_Fac = 1-Outage/100,
           ID = "LesserSlave")
  
  PigeonLake <- read.csv(file = 'la53082lo245821_PigeonLake.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 53.082,
           Longitude = -(360-245.821),
           Cap_Fac = 1-Outage/100,
           ID = "PigeonLake")
  
  SwanHills <- read.csv(file = 'la54761lo244344_SwanHills.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 54.761,
           Longitude = -(360-244.344),
           Cap_Fac = 1-Outage/100,
           ID = "SwanHills")
}

Pot_sites <- rbind(Anzac,BisonLake,ChainLakes,ClearPrairie,Falher,FortSaskatchewan,
                   GrandeCache,Hinton,JohnDOr,Kehewin,LesserSlave,PigeonLake
                   #SwanHills
                   ) %>%
  mutate(Capacity = 0,
         Installation = "Potential")

{
  AKE1 <- read.csv(file = 'McBride(AKE1)_la49551lo246524.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.54506,
           Longitude = -113.4770,
           Capacity = 73.3,
           Cap_Fac = 1-Outage/100,
           ID = "AKE1")
  
  ARD1 <- read.csv(file = 'Ardenville(ARD1)_la49547lo246574.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.55412,
           Longitude = -113.4320,
           Capacity = 68.2,
           Cap_Fac = 1-Outage/100,
           ID = "ARD1")
  
  BSR1 <- read.csv(file = 'Blackspring_BlueTrail(BSR1_BTR1)_la49658lo246543.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.65405,
           Longitude = -113.4674,
           Capacity = 300,
           Cap_Fac = 1-Outage/100,
           ID = "BSR1")
  
  BTR1 <- BSR1 %>%
    mutate(Capacity = 66,
           ID = "BTR1")
  
  BUL1 <- read.csv(file = 'BullCreek(BUL1_2)_la52506lo249926.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 52.50956,
           Longitude = -110.0588,
           Capacity = 13.65,
           Cap_Fac = 1-Outage/100,
           ID = "BUL1")
  
  BUL2 <- BUL1 %>%
    mutate(Capacity = 16,
           ID = "BUL2")
  
  CR1 <- read.csv(file = 'CastleRiver(CR1)_la49503lo245969.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.50690,
           Longitude = -114.0419,
           Capacity = 39,
           Cap_Fac = 1-Outage/100,
           ID = "CR1")
  
  CRE3 <- read.csv(file = 'CowleyRidge(CRE3)_la49562lo245899.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.56257,
           Longitude = -114.1077,
           Capacity = 20,
           Cap_Fac = 1-Outage/100,
           ID = "CRE3")
  
  CRR1 <- read.csv(file = 'CastleRock(CRR1)_la50584lo246977.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 50.58452,
           Longitude = -113.0134,
           Capacity = 77,
           Cap_Fac = 1-Outage/100,
           ID = "CRR1")
  
  CRR2 <- read.csv(file = 'CastleRockRidge(CRR2)_la49554lo246004.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.55891,
           Longitude = -113.9830,
           Capacity = 29,
           Cap_Fac = 1-Outage/100,
           ID = "CRR2")
  
  CYP1 <- read.csv(file = 'Cypress(CYP1_2)_la49833lo249637.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.84097,
           Longitude = -110.357,
           Capacity = 196,
           Cap_Fac = 1-Outage/100,
           ID = "CYP1")
  
  CYP2 <- CYP1 %>%
    mutate(Capacity = 46,
           ID = "CYP2")
  
  FMG1 <- read.csv(file = 'FMGranlea(FMG1)_la49660lo248875.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.66334,
           Longitude = -111.1220,
           Capacity = 200,
           Cap_Fac = 1-Outage/100,
           ID = "FMG1")
  
  GRZ1 <- read.csv(file = 'WildRunGrizzlyBear(GRZ1)_la53213lo248903.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 53.21519,
           Longitude = -111.095,
           Capacity = 152,
           Cap_Fac = 1-Outage/100,
           ID = "GRZ1")
  
  GWW1 <- read.csv(file = 'Soderglen(GWW1)_la50479lo246926.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 50.47987,
           Longitude = -113.0579,
           Capacity = 71,
           Cap_Fac = 1-Outage/100,
           ID = "GWW1")
  
  HAL1 <- read.csv(file = 'Halkirk(HAL1)_la52283lo247926.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 52.27438,
           Capacity = 150,
           Longitude = -112.0622,
           Cap_Fac = 1-Outage/100,
           ID = "HAL1")
  
  HHW1 <- read.csv(file = 'HandHills(HHW1)_la51596lo247676.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 51.59,
           Longitude = -112.32,
           Capacity = 150.8,
           Cap_Fac = 1-Outage/100,
           ID = "HHW1")
  
  IEW1 <- read.csv(file = 'Summerview1(IEW1)_la49592lo246231.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.59720,
           Longitude = -113.7676,
           Capacity = 66,
           Cap_Fac = 1-Outage/100,
           ID = "IEW1")
  
  IEW2 <- read.csv(file = 'Summerview2(IEW2)_la49630lo246211.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.62531,
           Longitude = -113.7934,
           Capacity = 66,
           Cap_Fac = 1-Outage/100,
           ID = "IEW2")
  
  JNR1 <- read.csv(file = 'Jenner(JNR1)_la50781lo248961.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 50.77712,
           Longitude = -111.046,
           Capacity = 109.2,
           Cap_Fac = 1-Outage/100,
           ID = "JNR1")
  
  JNR2 <- read.csv(file = 'Jenner2(JNR2)_la50823lo248918.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 50.82,
           Longitude = -111.07,
           Capacity = 71.4,
           Cap_Fac = 1-Outage/100,
           ID = "JNR2")
  
  KHW1 <- read.csv(file = 'KettlesHill(KHW1)_la49505lo246188.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.50982,
           Longitude = -113.8178,
           Capacity = 63,
           Cap_Fac = 1-Outage/100,
           ID = "KHW1")
  
  NEP1 <- read.csv(file = 'GhostPine(NEP1)_la51899lo246649.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 51.89888,
           Longitude = -113.3650,
           Capacity = 82,
           Cap_Fac = 1-Outage/100,
           ID = "NEP1")
  
  OWF1 <- read.csv(file = 'Oldman2(OWF1)_la49630lo246211.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.62531,
           Longitude = -113.7934,
           Capacity = 46,
           Cap_Fac = 1-Outage/100,
           ID = "OWF1")
  
  RIV1 <- read.csv(file = 'Riverview(RIV1)_la49535lo246028.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.53245,
           Longitude = -113.9770,
           Capacity = 105,
           Cap_Fac = 1-Outage/100,
           ID = "RIV1")
  
  RTL1 <- read.csv(file = 'Rattlesnake(RTL1)_la50573lo248121.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 50.57726,
           Longitude = -111.8662,
           Capacity = 130,
           Cap_Fac = 1-Outage/100,
           ID = "RTL1")
  
  SCR2 <- read.csv(file = 'Magrath(SCR2)_la49381lo247032.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.38747,
           Longitude = -112.9547,
           Capacity = 30.2,
           Cap_Fac = 1-Outage/100,
           ID = "SCR2")
  
  SCR3 <- read.csv(file = 'ChinChute(SCR3)_la49687lo247672.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.68482,
           Longitude = -112.3244,
           Capacity = 30,
           Cap_Fac = 1-Outage/100,
           ID = "SCR3")
  
  SCR4 <- read.csv(file = 'WinteringHills(SCR4)_la50332lo247176.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 50.33874,
           Longitude = -112.8286,
           Capacity = 88,
           Cap_Fac = 1-Outage/100,
           ID = "SCR4")
  
  TAB1 <- read.csv(file = 'Taber(TAB1)_la49706lo248059.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.71351,
           Longitude = -111.9328,
           Capacity = 81,
           Cap_Fac = 1-Outage/100,
           ID = "TAB1")
  
  WHE1 <- read.csv(file = 'Wheatland(WHE1)_la51242lo247571.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 51.2375,
           Longitude = -112.426,
           Capacity = 120,
           Cap_Fac = 1-Outage/100,
           ID = "WHE1")
  
  WHT1 <- read.csv(file = 'Whitla(WHT1_2)_la49641lo248703.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.64029,
           Longitude = -111.2910,
           Capacity = 202,
           Cap_Fac = 1-Outage/100,
           ID = "WHT1")
  
  WHT2 <- WHT1 %>%
    mutate(Capacity = 151,
           ID = "WHT2")
  
  WRW1 <- read.csv(file = 'Windrise(WRW1)_la50573lo248121.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 50.57726,
           Longitude = -111.8662,
           Capacity = 207,
           Cap_Fac = 1-Outage/100,
           ID = "WRW1")
  }

Act_sites <- rbind(AKE1,ARD1,BSR1,BTR1,BUL1,BUL2,CR1,CRE3,CRR1,CRR2,CYP1,CYP2,
                   FMG1,GRZ1,GWW1,HAL1,HHW1,IEW1,IEW2,JNR1,JNR2,KHW1,NEP1,OWF1,
                   RIV1,RTL1,SCR2,SCR3,SCR4,TAB1,WHE1,WHT1,WHT2,WRW1) %>%
  mutate(Installation = "Active")

sites <- rbind(Pot_sites,Act_sites)

saveRDS(sites, file = "SitesProfiles.RData")
