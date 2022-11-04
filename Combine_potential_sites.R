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

################################################################################
# Using the data in Aurora
################################################################################

{
  derate <- read_excel('Renewable_Derate_Hourly.xlsx')
  
  AKE1 <- derate %>%
    subset(., select = c(Date,Hour,McBrideLake)) %>%
    na.omit() %>%
    mutate(Latitude = 49.54506,
           Longitude = -113.4770,
           Capacity = 73.3,
           Outage = as.numeric(McBrideLake),
           Cap_Fac = 1-Outage/100,
           ID = "AKE1") %>%
    subset(., select = -c(McBrideLake))
  
  ARD1 <- derate %>%
    subset(., select = c(Date,Hour,Ardenville)) %>%
    na.omit() %>%
    mutate(Latitude = 49.55412,
           Longitude = -113.4320,
           Capacity = 68.2,
           Outage = as.numeric(Ardenville),
           Cap_Fac = 1-Outage/100,
           ID = "ARD1") %>%
    subset(., select = -c(Ardenville))
  
  BSR1 <- derate %>%
    subset(., select = c(Date,Hour,BlackspringRidge)) %>%
    na.omit() %>%
    mutate(Latitude = 49.65405,
           Longitude = -113.4674,
           Capacity = 300,
           Outage = as.numeric(BlackspringRidge),
           Cap_Fac = 1-Outage/100,
           ID = "BSR1") %>%
    subset(., select = -c(BlackspringRidge))
  
  BTR1 <- derate %>%
    subset(., select = c(Date,Hour,BlueTrail)) %>%
    na.omit() %>%
    mutate(Latitude = 49.65405,
           Longitude = -113.4674,
           Capacity = 300,
           Outage = as.numeric(BlueTrail),
           Cap_Fac = 1-Outage/100,
           ID = "BTR1") %>%
    subset(., select = -c(BlueTrail))
  
  BUL1 <- derate %>%
    subset(., select = c(Date,Hour,Bullcreek1)) %>%
    na.omit() %>%
    mutate(Latitude = 52.50956,
           Longitude = -110.0588,
           Capacity = 13.65,
           Outage = as.numeric(Bullcreek1),
           Cap_Fac = 1-Outage/100,
           ID = "BUL1") %>%
    subset(., select = -c(Bullcreek1))
  
  BUL2 <- derate %>%
    subset(., select = c(Date,Hour,Bullcreek2)) %>%
    na.omit() %>%
    mutate(Latitude = 52.50956,
           Longitude = -110.0588,
           Capacity = 13.65,
           Outage = as.numeric(Bullcreek2),
           Cap_Fac = 1-Outage/100,
           ID = "BUL1") %>%
    subset(., select = -c(Bullcreek2))
  
  CR1 <- derate %>%
    subset(., select = c(Date,Hour,CastleRiver)) %>%
    na.omit() %>%
    mutate(Latitude = 49.50690,
           Longitude = -114.0419,
           Capacity = 39,
           Outage = as.numeric(CastleRiver),
           Cap_Fac = 1-Outage/100,
           ID = "CR1") %>%
    subset(., select = -c(CastleRiver))
  
  CRE3 <- derate %>%
    subset(., select = c(Date,Hour,CowleyRidge)) %>%
    na.omit() %>%
    mutate(Latitude = 49.56257,
           Longitude = -114.1077,
           Capacity = 20,
           Outage = as.numeric(CowleyRidge),
           Cap_Fac = 1-Outage/100,
           ID = "CRE3") %>%
    subset(., select = -c(CowleyRidge))
  
  CRR1 <- derate %>%
    subset(., select = c(Date,Hour,CastleRock)) %>%
    na.omit() %>%
    mutate(Latitude = 50.58452,
           Longitude = -113.0134,
           Capacity = 77,
           Outage = as.numeric(CastleRock),
           Cap_Fac = 1-Outage/100,
           ID = "CRR1") %>%
    subset(., select = -c(CastleRock))
  
  CRR2 <- derate %>%
    subset(., select = c(Date,Hour,CastleRockRidge2)) %>%
    na.omit() %>%
    mutate(Latitude = 49.55891,
           Longitude = -113.9830,
           Capacity = 29,
           Outage = as.numeric(CastleRockRidge2),
           Cap_Fac = 1-Outage/100,
           ID = "CRR2") %>%
    subset(., select = -c(CastleRockRidge2))
  
  CYP1 <- derate %>%
    subset(., select = c(Date,Hour,Cypress)) %>%
    na.omit() %>%
    mutate(Latitude = 49.84097,
           Longitude = -110.357,
           Capacity = 196,
           Outage = as.numeric(Cypress),
           Cap_Fac = 1-Outage/100,
           ID = "CYP1") %>%
    subset(., select = -c(Cypress))
  
  CYP2 <- CYP1 %>%
    mutate(Capacity = 46,
           ID = "CYP2")
  
  FMG1 <- derate %>%
    subset(., select = c(Date,Hour,FortyMileGranlea)) %>%
    na.omit() %>%
    mutate(Latitude = 49.66334,
           Longitude = -111.1220,
           Capacity = 200,
           Outage = as.numeric(FortyMileGranlea),
           Cap_Fac = 1-Outage/100,
           ID = "FMG1") %>%
    subset(., select = -c(FortyMileGranlea))
  
  GRZ1 <- derate %>%
    subset(., select = c(Date,Hour,GrizzlyBear)) %>%
    na.omit() %>%
    mutate(Latitude = 53.21519,
           Longitude = -111.095,
           Capacity = 152,
           Outage = as.numeric(GrizzlyBear),
           Cap_Fac = 1-Outage/100,
           ID = "GRZ1") %>%
    subset(., select = -c(GrizzlyBear))
  
  GWW1 <- derate %>%
    subset(., select = c(Date,Hour,Soderglen)) %>%
    na.omit() %>%
    mutate(Latitude = 50.47987,
           Longitude = -113.0579,
           Capacity = 71,
           Outage = as.numeric(Soderglen),
           Cap_Fac = 1-Outage/100,
           ID = "GWW1") %>%
    subset(., select = -c(Soderglen))
  
  HAL1 <- derate %>%
    subset(., select = c(Date,Hour,Halkirk)) %>%
    na.omit() %>%
    mutate(Latitude = 52.27438,
           Capacity = 150,
           Longitude = -112.0622,
           Outage = as.numeric(Halkirk),
           Cap_Fac = 1-Outage/100,
           ID = "HAL1") %>%
    subset(., select = -c(Halkirk))
  
  HHW1 <- derate %>%
    subset(., select = c(Date,Hour,HandHills)) %>%
    na.omit() %>%
    mutate(Latitude = 51.59,
           Longitude = -112.32,
           Capacity = 150.8,
           Outage = as.numeric(HandHills),
           Cap_Fac = 1-Outage/100,
           ID = "HHW1") %>%
    subset(., select = -c(HandHills))
  
  IEW1 <- derate %>%
    subset(., select = c(Date,Hour,Summerview1)) %>%
    na.omit() %>%
    mutate(Latitude = 49.59720,
           Longitude = -113.7676,
           Capacity = 66,
           Outage = as.numeric(Summerview1),
           Cap_Fac = 1-Outage/100,
           ID = "IEW1") %>%
    subset(., select = -c(Summerview1))
  
  IEW2 <- derate %>%
    subset(., select = c(Date,Hour,Summerview2)) %>%
    na.omit() %>%
    mutate(Latitude = 49.62531,
           Longitude = -113.7934,
           Capacity = 66,
           Outage = as.numeric(Summerview2),
           Cap_Fac = 1-Outage/100,
           ID = "IEW2") %>%
    subset(., select = -c(Summerview2))
  
  JNR1 <- derate %>%
    subset(., select = c(Date,Hour,Jenner1)) %>%
    na.omit() %>%
    mutate(Latitude = 50.77712,
           Longitude = -111.046,
           Capacity = 109.2,
           Outage = as.numeric(Jenner1),
           Cap_Fac = 1-Outage/100,
           ID = "JNR1") %>%
    subset(., select = -c(Jenner1))
  
  JNR2 <- derate %>%
    subset(., select = c(Date,Hour,Jenner2)) %>%
    na.omit() %>%
    mutate(Latitude = 50.82,
           Longitude = -111.07,
           Capacity = 71.4,
           Outage = as.numeric(Jenner2),
           Cap_Fac = 1-Outage/100,
           ID = "JNR2") %>%
    subset(., select = -c(Jenner2))
  
  JNR3 <- derate %>%
    subset(., select = c(Date,Hour,Jenner3)) %>%
    na.omit() %>%
    mutate(Latitude = 50.82,
           Longitude = -111.07,
           Capacity = 71.4,
           Outage = as.numeric(Jenner3),
           Cap_Fac = 1-Outage/100,
           ID = "JNR2") %>%
    subset(., select = -c(Jenner3))
  
  KHW1 <- derate %>%
    subset(., select = c(Date,Hour,KettlesHill)) %>%
    na.omit() %>%
    mutate(Latitude = 49.50982,
           Longitude = -113.8178,
           Capacity = 63,
           Outage = as.numeric(KettlesHill),
           Cap_Fac = 1-Outage/100,
           ID = "KHW1") %>%
    subset(., select = -c(KettlesHill))
  
  NEP1 <- derate %>%
    subset(., select = c(Date,Hour,GhostPine)) %>%
    na.omit() %>%
    mutate(Latitude = 51.89888,
           Longitude = -113.3650,
           Capacity = 82,
           Outage = as.numeric(GhostPine),
           Cap_Fac = 1-Outage/100,
           ID = "NEP1") %>%
    subset(., select = -c(GhostPine))
  
  OWF1 <- derate %>%
    subset(., select = c(Date,Hour,Oldman2)) %>%
    na.omit() %>%
    mutate(Latitude = 49.62531,
           Longitude = -113.7934,
           Capacity = 46,
           Outage = as.numeric(Oldman2),
           Cap_Fac = 1-Outage/100,
           ID = "OWF1") %>%
    subset(., select = -c(Oldman2))
  
  RIV1 <- derate %>%
    subset(., select = c(Date,Hour,Riverview)) %>%
    na.omit() %>%
    mutate(Latitude = 49.53245,
           Longitude = -113.9770,
           Capacity = 105,
           Outage = as.numeric(Riverview),
           Cap_Fac = 1-Outage/100,
           ID = "RIV1") %>%
    subset(., select = -c(Riverview))
  
  RTL1 <- derate %>%
    subset(., select = c(Date,Hour,RattlesnakeRidge)) %>%
    na.omit() %>%
    mutate(Latitude = 50.57726,
           Longitude = -111.8662,
           Capacity = 130,
           Outage = as.numeric(RattlesnakeRidge),
           Cap_Fac = 1-Outage/100,
           ID = "RTL1") %>%
    subset(., select = -c(RattlesnakeRidge))
  
  SCR2 <- derate %>%
    subset(., select = c(Date,Hour,Magrath)) %>%
    na.omit() %>%
    mutate(Latitude = 49.38747,
           Longitude = -112.9547,
           Capacity = 30.2,
           Outage = as.numeric(Magrath),
           Cap_Fac = 1-Outage/100,
           ID = "SCR2") %>%
    subset(., select = -c(Magrath))
  
  SCR3 <- derate %>%
    subset(., select = c(Date,Hour,ChinChute)) %>%
    na.omit() %>%
    mutate(Latitude = 49.68482,
           Longitude = -112.3244,
           Capacity = 30,
           Outage = as.numeric(ChinChute),
           Cap_Fac = 1-Outage/100,
           ID = "SCR3") %>%
    subset(., select = -c(ChinChute))
  
  SCR4 <- derate %>%
    subset(., select = c(Date,Hour,WinteringHills)) %>%
    na.omit() %>%
    mutate(Latitude = 50.33874,
           Longitude = -112.8286,
           Capacity = 88,
           Outage = as.numeric(WinteringHills),
           Cap_Fac = 1-Outage/100,
           ID = "SCR4") %>%
    subset(., select = -c(WinteringHills))
  
  TAB1 <- derate %>%
    subset(., select = c(Date,Hour,Taber)) %>%
    na.omit() %>%
    mutate(Latitude = 49.71351,
           Longitude = -111.9328,
           Capacity = 81,
           Outage = as.numeric(Taber),
           Cap_Fac = 1-Outage/100,
           ID = "TAB1") %>%
    subset(., select = -c(Taber))
  
  WHE1 <- derate %>%
    subset(., select = c(Date,Hour,Wheatland)) %>%
    na.omit() %>%
    mutate(Latitude = 51.2375,
           Longitude = -112.426,
           Capacity = 120,
           Outage = as.numeric(Wheatland),
           Cap_Fac = 1-Outage/100,
           ID = "WHE1") %>%
    subset(., select = -c(Wheatland))
  
  WHT1 <- derate %>%
    subset(., select = c(Date,Hour,Whitla)) %>%
    na.omit() %>%
    mutate(Latitude = 49.64029,
           Longitude = -111.2910,
           Capacity = 202,
           Outage = as.numeric(Whitla),
           Cap_Fac = 1-Outage/100,
           ID = "WHT1") %>%
    subset(., select = -c(Whitla))
  
  WHT2 <- WHT1 %>%
    mutate(Capacity = 151,
           ID = "WHT2")
  
  WRW1 <- derate %>%
    subset(., select = c(Date,Hour,WindRise)) %>%
    na.omit() %>%
    mutate(Latitude = 50.57726,
           Longitude = -111.8662,
           Capacity = 207,
           Outage = as.numeric(WindRise),
           Cap_Fac = 1-Outage/100,
           ID = "WRW1") %>%
    subset(., select = -c(WindRise))
}

Aurora_sites <- rbind(AKE1,ARD1,BSR1,BTR1,BUL1,BUL2,CR1,CRE3,CRR1,CRR2,CYP1,CYP2,
                   FMG1,GRZ1,GWW1,HAL1,HHW1,IEW1,IEW2,JNR1,JNR2,JNR3,KHW1,NEP1,OWF1,
                   RIV1,RTL1,SCR2,SCR3,SCR4,TAB1,WHE1,WHT1,WHT2,WRW1) %>%
  mutate(Installation = "Aurora",
         he = as.numeric(Hour)-1,
         time = as.POSIXct(paste0(Date," ",he,":00:00"),
                           format = "%m/%d/%Y %H:%M:%S"),
         gen = Cap_Fac*Capacity,
         ) %>%
  subset(.,select=c(Latitude,Longitude,Cap_Fac,ID,time,gen,Capacity,
                    Installation))

Pot_sites1 <- Pot_sites %>%
  mutate(time = ymd_h(paste0(year,"-",month,"-",day," ",hour)),
         gen = Cap_Fac*Capacity,
         Installation = "Potential") %>%
  subset(., select=-c(year,month,day,hour,Outage))

sites <- rbind(Pot_sites1,Aurora_sites)

saveRDS(sites, file = "AuroraProfiles.RData")
