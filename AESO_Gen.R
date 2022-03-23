
setwd("D:/Documents/GitHub/AuroraEval")

header <- read.csv('AESO_GenTable.csv', nrows = 2, header = FALSE)
header[3,] <- paste(header[1,],header[2,], sep = " ")

gen_table <- read.csv(file = 'AESO_GenTable.csv', skip = 2, header = FALSE)
colnames(gen_table) <- header[3,]
row.names(gen_table) <- gen_table[,1]

gen_table <- gen_table[-1]
