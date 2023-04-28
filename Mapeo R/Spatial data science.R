###############################
### SPATIAL DATA SCIENCE ####
##############################


#install.packages("sf")

library(dplyr)
library(sf)
ind_sf <- st_read("D:/Disco D/R/Mapas/shapes/viz_india-master/india_states_2014/india_states.shp")


# mutate type variable to define union territories
uts <- c("Delhi", "Andaman & Nicobar Islands", "Puducherry", 
         "Lakshadweep", "Dadra & Nagar Haveli", "Daman & Diu",
         "Chandigarh")

ind_sf <- ind_sf %>% 
  select(name, abbr) %>% 
  mutate( #ifelse reclasifica de acuerdo a una condición dada en el 1er parámetro, yes or no)
    type = ifelse(name %in% uts, "Union Territory", "State") #%in% es lógico, pregunta si algo está dentro de otra cosa
  ) %>% 
  rename(abb = abbr, state_ut = name)
# This doesn't explicitly select the geometry column, but the geometry in sf objects is sticky. 
# It remains in the object unless explicitly dropped with ind_sf %>% st_set_geometry(NULL).


#################################
### PREPARING ATRIBUTTE DATA ####
################################

attributes_df <- readRDS("D:/Disco D/R/Mapas/shapes/viz_india-master/attributes.rds")

ind_sf <- ind_sf %>%
  left_join(attributes_df, by = "state_ut") %>%
  mutate(
    per_capita_gdp_inr = nominal_gdp_inr / pop_2011,
    per_capita_gdp_usd = nominal_gdp_usd / pop_2011
  )

#####################
# CALCULING AREAS #
####################

library(units)
# install.packages("lwgeom")
library(lwgeom)
# mutate area
ind_sf <- ind_sf %>%
  mutate(my_area = st_area(.))


###################
# CONVERT FORMAT #
##################

# convert units   ## ud_units es un vector de todas las posibles unidades existentes
units(ind_sf$my_area) <- with(ud_units, km^2)

# mutate gdp density
ind_sf <- ind_sf %>%
  mutate(gdp_density_usd_km2 = nominal_gdp_usd / my_area)


######################
# simplify geometry #
####################

# install.packages("rmapshaper")
library(rmapshaper)

ind_sf <- ind_sf %>%
  mutate(
    my_area = as.vector(my_area),
    gdp_density_usd_km2 = as.vector(gdp_density_usd_km2)
  )

original_geometry <- st_geometry(ind_sf)
# st_geometry() When passing it an sf object, it will return just the geometry. This allows us to create a quick plot

simp_sf <- ms_simplify(ind_sf, keep = 0.01, keep_shapes = TRUE)
simple_geometry <- st_geometry(simp_sf)

par(mfrow = c(1,2))
plot(original_geometry, main = "Original Geometry")
plot(simple_geometry, main = "Simplified Geometry")



# Medimos y comparamos tamaño de archivos 
# install.packages("pryr")
library(pryr)
object_size(original_geometry)

object_size(simple_geometry)



# Finally, let's save the simplified spatial dataframe for the next lesson.
saveRDS(simp_sf, "D:/Disco D/R/Mapas/shapes/viz_india-master/simp_sf.rds")


#convert the sf object to a SpatialPolygonsDataFrame, an S4 class defined by the sp package
my_spdf <- as(my_sf, "Spatial")
class(my_spdf)


#convert it back to an sf object
ind_sf <- st_as_sf(my_spdf)
class(ind_sf)