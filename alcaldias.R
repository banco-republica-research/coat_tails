library(plyr)
library(dplyr)
library(haven)
library(stringr)
library(plotly)
library(ggplot2)
library(tidyr)
library(rgeos)
library(rgdal)
library(kml)

###########################################################################################################
############################################### ARRANGE DATA ##############################################
###########################################################################################################

setwd("~/Google Drive/DATOS_ELECTORALES/CEDE/Archivos_DTA/")
alcaldias <- read_dta("Alcaldias.dta") %>%
  arrange(muni_code, year, desc(posici_muni)) %>%
  # filter(pvotos_muni_can != 100) %>%
  dplyr::group_by(muni_code, year) %>% #Calculate difference
  dplyr::mutate(diff =  ave(pvotos_muni, factor(muni_code), factor(year), FUN = function(x) c(0, diff(x)))) %>%
  dplyr::mutate(diff = ifelse(posici_muni == 1 & pvotos_muni_can == 100 & diff == 0, 100, diff))

#Collapse data.frame

alcaldias <- alcaldias %>%
  dplyr::group_by(muni_code, year) %>%
  dplyr::summarize(difference = sum(diff))

alcaldias_wide <- alcaldias %>%
  spread(year, difference, fill = 100, sep = "") 

alcaldias_long <- alcaldias_wide %>%
  gather(year, diff, year1988:year2015)
  
#Density by year
p <- ggplot(alcaldias_long, aes(diff, colour = factor(year))) + geom_density()
ggplotly(p)

###########################################################################################################
################################### LONGITUDINAL CLUSTER ANALYSIS #########################################
###########################################################################################################

cld <- cld(alcaldias_wide, timeInData = c(6:11), idAll = alcaldias_wide$muni_code)
kml(cld, nbRedraw = 2, toPlot = "both")

#Get clusters
alcaldias_wide$cluster <- getClusters(cld, 6)

#Merge with geographical data
setwd("~/Dropbox/Geografia/")
municipios <- readOGR(".", "Municipios")
municipios@data <- merge(municipios@data, alcaldias_wide, by.x = "ID_ESPACIA", by.y = "muni_code")






