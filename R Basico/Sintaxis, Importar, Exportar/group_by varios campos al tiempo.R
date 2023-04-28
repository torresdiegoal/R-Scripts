
campos_factores <- colnames(SISMAR2)[-3] # Menos TONELADAS
#  str(SISMAR2)
# Volvemos factores a todos
SISMAR2[campos_factores] <- lapply(SISMAR2[campos_factores],
                                   FUN = function(y){as.factor(y)})

SISMAR_Segmentacion <- SISMAR2 %>%
  group_by(across(where(is.factor))) %>%
  dplyr::summarise(across(everything(), sum))
SISMAR_Segmentacion <- as.data.frame(SISMAR_Segmentacion)
str(SISMAR_grp)
SISMAR_grp2 <- SISMAR2 %>%
  group_by(across(where(is.factor))) %>%
  dplyr::summarise(across(where(is.numeric), sum))


#Muestreo aleatorio sobre un dataframe
SISMAR_Segment_samp <-SISMAR_Segmentacion %>%
  distinct(SISMAR_Segmentacion) %>%
  sample_n(2, replace = FALSE) %>%
  inner_join(SISMAR_Segmentacion, .)