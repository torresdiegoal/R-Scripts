# fuente: https://www.youtube.com/watch?v=PoTqmxa-Tnk&t=301s

# install.packages('tidyverse')
library(rvest) # paquete para web scrapping
library(tidyverse)

# Aqui generamos una lista de listas a partir de lo que trae la funcion en html
paldea_html <- rvest::read_html('https://vandal.elespanol.com/guias/guia-pokemon-escarlata-y-purpura-trucos-consejos-y-secretos/pokedex-de-paldea')


# Filtramos la informacion de html por los titulos o headlines y pasamos a texto
titulos <- paldea_html %>% 
  html_nodes('.mw-headline') %>% 
  html_text()
titulos

# Con 'p', obtenemos los parrafos
parrafos <- paldea_html %>% 
  html_nodes('p') %>% 
  html_text()
parrafos

# Con html_table obtenemos una lista de las tablas dentro del documento
paldea <- paldea_html %>% 
  html_table()

# Cada tabla dentro de la lista anterior son los pokemones dentro de Paldea que vienen de cada una de las 9 generaciones, por eso 9 elementos
# Los juntamos en una sola tabla
pokedex_paldea <- rbind(paldea[[1]],paldea[[2]],paldea[[3]],paldea[[4]],paldea[[5]],paldea[[6]],paldea[[7]],paldea[[8]],paldea[[9]])

# ahora separamos el campo tipo en dos de acuerdo a si el tipo del pokemon 
pokedex_paldea <- pokedex_paldea %>% 
  mutate(Tipo = gsub("/", "y",Tipo),
         campo_separado = strsplit(Tipo, " y "),
         Tipo1 = sapply(campo_separado, function(x) x[1]),
         Tipo2 = sapply(campo_separado, function(x) x[2])) 


tipos <- na.omit(c(pokedex_paldea$Tipo1,pokedex_paldea$Tipo2))
tipos %>% table() %>% 
  data.frame() %>% 
  arrange(desc(Freq))
