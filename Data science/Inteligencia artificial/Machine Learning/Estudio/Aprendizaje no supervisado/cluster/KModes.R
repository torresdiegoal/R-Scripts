
library(tidyr)
library(data.table)
library('stringr') # Para str_trim()
library(dplyr)
library(lubridate) # para manejo de fechas
library(reshape2) # para pivot tablas
library(plyr)
#install.packages("klaR")
library(klaR)

data(GermanCredit)
GermanCredit_x <- GermanCredit[, -c(2,5,7,8,11,13,16,18)]
  KModes_ej <- kmodes(GermanCredit_x, 
                    modes = 4,
                    iter.max = 10,
                    weighted = FALSE)
GermanCredit_x %>% str()
# Cual es la moda de cada campo por cluster ?
a <- KModes_ej$modes
rownames(a) <- paste('Segmento_',1:nrow(a))






### a 5-dimensional toy-example:
## generate data set with two groups of data:
set.seed(1)
x <- rbind(matrix(rbinom(250, 2, 0.25), ncol = 5),
           matrix(rbinom(250, 2, 0.75), ncol = 5))
colnames(x) <- c("a", "b", "c", "d", "e")
## run algorithm on x:
KModes_ej <- kmodes(x, 2)

plot(jitter(x), col = KModes_ej$cluster)
points(KModes_ej$modes, col = 1:5, pch = 8)
# inpath  <- "C:/Users/Diego Torres/OneDrive - Universidad de los Andes/R scripts/Machine learning/Apendizaje no supervisado/cluster/"
# outpath <- "C:/Users/Diego Torres/OneDrive - Universidad de los Andes/R scripts/Machine learning/Apendizaje no supervisado/cluster/"

# movies <- read.csv(paste(inpath,"movie_metadata.csv", sep = ""),
#                                 sep = ',')
# 
# pelis <- as.data.frame( movies$movie_title %>%  table()) %>%  arrange(desc(Freq))
# movies$movie_title <- movies$movie_title %>% str_trim()
# 
# a <- movies %>%  
#   #c() %>% 
#   filter( movie_title %in%  c('Ben-Hur'))
# 
# t <- movies$movie_title[1]
# str_trim(t)
# s <- "    this is an example string    "
# trim(s)
# 
# f <- factor(c(s, s, " A", " B ", "  C ", "D "))
# levels(f)
# 
# trim(f)
# levels(trim(f))
# 
# trim(f, recode.factor=FALSE)
# levels(trim(f, recode.factor=FALSE))
# 
# l <- list(s=rep(s, times=6), f=f, i=1:6)
# trim(l)
# 
# df <- as.data.frame(l)
# trim(df)

