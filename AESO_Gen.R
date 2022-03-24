library(lubridate)
library(dplyr)

setwd("D:/Documents/GitHub/AuroraEval")

header <- read.csv('AESO_GenTable.csv', nrows = 2, header = FALSE)
header[3,] <- paste(header[1,],header[2,], sep = " ")

gen_table <- read.csv(file = 'AESO_GenTable.csv', skip = 2, header = FALSE)
colnames(gen_table) <- header[3,]

gen_table <- gen_table %>%
  rename(date = 1)

gen_table$date <- as.POSIXct(gen_table$date, format="%d/%m/%Y %I:%M:%S %p")

date1 <- as.POSIXct("01/01/2020 12:00:00 AM", format="%d/%m/%Y %I:%M:%S %p")

act_gen <- gen_table %>%
  filter(date >= date1)
