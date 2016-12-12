library(foreign)
library(plyr)
library(dplyr)
library(haven)
library(stringr)
library(plotly)
library(ggplot2)
library(tidyr)
library(rgeos)
library(rgdal)
library(raster)
library(kml)
library(broom)
library(ggplot2)

###########################################################################################################
############################################### ARRANGE DATA ##############################################
###########################################################################################################

# Get mayor's election data (only from electoral years since 1997). 
setwd("~/Dropbox/BANREP/Elecciones/Data/CEDE/Microdatos/")
list_files <- list.files() %>%
  .[. %in% c("1997", "2000", "2003", "2007", "2011", "2015")] %>%
  str_c("~/Dropbox/BANREP/Elecciones/Data/CEDE/Microdatos/", ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){
    x[str_detect(x, "Alcal")]
  }) %>%
  str_c("~/Dropbox/BANREP/Elecciones/Data/CEDE/Microdatos", c("1997", "2000", "2003", "2007", "2011", "2015"),
        ., sep = "/")

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados")

alcaldes <- lapply(list_files, read_dta)

alcaldes_merge <- alcaldes %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
    mutate(non_candidate = ifelse(primer_apellido %in% non_candidate_votes, 1, 0)) %>%
    group_by(codmpio, ano) %>%
    mutate(prop_votes_total = votos / sum(votos)) %>%
    filter(non_candidate == 0) %>%
    mutate(prop_votes_candidates = votos / sum(votos)) %>%
    mutate(rank = dense_rank(desc(votos))) %>%
    filter(rank <= 2)
  })






###########################################################################################################
############################################### ARRANGE DATA ##############################################
###########################################################################################################

setwd("~/Google Drive/DATOS_ELECTORALES/CEDE/Archivos_DTA/")
alcaldias <- read_dta("Alcaldias.dta") %>%
  arrange(muni_code, year, desc(posici_muni)) %>%
  filter(year >= 1997) %>%
  dplyr::group_by(muni_code, year) %>% #Calculate difference
  dplyr::mutate(diff =  ave(pvotos_muni, factor(muni_code), factor(year), FUN = function(x) c(0, diff(x)))) 
  # %>%
  # dplyr::mutate(diff = ifelse(posici_muni == 1 & pvotos_muni_can == 100 & diff == 0, 100, diff))

#Collapse data.frame

alcaldias <- alcaldias %>%
  dplyr::group_by(muni_code, year) %>%
  dplyr::summarize(difference = sum(diff))

alcaldias_wide <- alcaldias %>%
  spread(year, difference, fill = 100, sep = "") 

alcaldias_long <- alcaldias_wide %>%
  gather(year, diff, year1997:year2015)
  
#Density by year
p <- ggplot(alcaldias_long, aes(diff, colour = factor(year))) + geom_density()
ggplotly(p)

###########################################################################################################
################################### LONGITUDINAL CLUSTER ANALYSIS #########################################
###########################################################################################################

cld <- cld(alcaldias_wide, timeInData = c(6:11), idAll = alcaldias_wide$muni_code)
kml(cld, toPlot = "both")

#Get clusters
alcaldias_wide$cluster <- getClusters(cld, 6)

#Merge with geographical data
setwd("~/Dropbox/Geografia/")
municipios <- readOGR("mpio", "mpio") %>%
  spTransform(CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  

municipios_clust <- municipios %>%
  tidy(region = "MPIOS_1") %>%
  mutate(muni_code = as.numeric(id)) %>%
  merge(., municipios@data, by.x = "id", by.y = "MPIOS_1", all = T) %>%
  merge(., alcaldias_wide, by.x = "id", by.y = "muni_code", all = T)


# Plot using ggplot
setwd("~/Desktop/")
g <- ggplot(data = municipios_clust, aes(x = long, y = lat)) + geom_polygon(aes(group = NOMBRE_MPI, fill = cluster))
g <- g + coord_equal()
# g <- g + theme(legend.position = c(.15, .25))
ggplotly(g)
# ggsave(str_c("americas_regions.pdf"), width=45, height=55, units="cm")





