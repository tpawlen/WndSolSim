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

Pot_sites <- rbind(Anzac,BisonLake,ChainLakes,ClearPrairie,Falher,GrandeCache,
                   Hinton,JohnDOr,Kehewin,LesserSlave,PigeonLake,SwanHills) %>%
  mutate(Installation = "Potential")

{
  AKE1 <- read.csv(file = 'McBride(AKE1)_la49551lo246524.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.54506,
           Longitude = -113.4770,
           Cap_Fac = 1-Outage/100,
           ID = "AKE1")
  
  ARD1 <- read.csv(file = 'Ardenville(ARD1)_la49547lo246574.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.55412,
           Longitude = -113.4320,
           Cap_Fac = 1-Outage/100,
           ID = "ARD1")
  
  BSR1 <- read.csv(file = 'Blackspring_BlueTrail(BSR1_BTR1)_la49658lo246543.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.65405,
           Longitude = -113.4674,
           Cap_Fac = 1-Outage/100,
           ID = "BSR1")
  
  BTR1 <- BSR1 %>%
    mutate(ID = "BTR1")
  
  BUL1 <- read.csv(file = 'BullCreek(BUL1_2)_la52506lo249926.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 52.50956,
           Longitude = -110.0588,
           Cap_Fac = 1-Outage/100,
           ID = "BUL1")
  
  BUL2 <- BUL1 %>%
    mutate(ID = "BUL2")
  
  CR1 <- read.csv(file = 'CastleRiver(CR1)_la49503lo245969.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.50690,
           Longitude = -114.0419,
           Cap_Fac = 1-Outage/100,
           ID = "CR1")
  
  CRE3 <- read.csv(file = 'CowleyRidge(CRE3)_la49562lo245899.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.56257,
           Longitude = -114.1077,
           Cap_Fac = 1-Outage/100,
           ID = "CRE3")
  
  CRR1 <- read.csv(file = 'CastleRock(CRR1)_la50584lo246977.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 50.58452,
           Longitude = -113.0134,
           Cap_Fac = 1-Outage/100,
           ID = "CRR1")
  
  CRR2 <- read.csv(file = 'CastleRockRidge(CRR2)_la49554lo246004.csv') %>%
    subset(., select = c(year,month,day,hour,Outage)) %>%
    na.omit() %>%
    mutate(Latitude = 49.55891,
           Longitude = -113.9830,
           Cap_Fac = 1-Outage/100,
           ID = "CRR2")
  
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

saveRDS(Pot_sites, file = "PotentialSites.RData")
