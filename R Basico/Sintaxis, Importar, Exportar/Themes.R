#install.packages("rsthemes")

# Tomado de > https://www.garrickadenbuie.com/project/rsthemes/

# funciona
install.packages(
  "rsthemes",
  repos = c(gadenbuie = 'https://gadenbuie.r-universe.dev', getOption("repos"))
)
#devtools::install_github("gadenbuie/rsthemes")


library(devtools)

# installa ciertos temas y Cascadian code que es lo que queriamos

rsthemes::install_rsthemes()
rsthemes::list_rsthemes()
