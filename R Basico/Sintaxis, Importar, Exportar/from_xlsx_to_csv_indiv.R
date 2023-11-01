rm(list=ls())

library(readxl)
library(dplyr)

#inpath <- 'C:/Users/Diego Torres/OneDrive - Ticket Fast SAS/Macros 2022/'
inpath <- 'C:/Users/Diego/OneDrive - Ticket Fast SAS/Macros 2022/'
outpath <- 'C:/Users/Diego Torres/OneDrive - Ticket Fast SAS/Macros 2022/csv/'

#files <- dir(inpath)

#for(i in files){
data <- read_excel(paste(inpath,'2022.01.xlsx', sep = ''), na = '-', sheet = 'Data')
  
write.table(data, file = paste(outpath, file_name, '.csv', sep = ''), 
              fileEncoding = 'UTF-8',
              row.names = F,
              quote = F, #omite las comillas de cada elemento del output
              sep = '|',
              dec = '.',
              na = '-')
#}
  
