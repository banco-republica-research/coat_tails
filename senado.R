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
######################################### ARRANGE DATA I : VOTES ##########################################
###########################################################################################################

# Get mayor's election data (only from electoral years since 1997). 
setwd("~/Dropbox/BANREP/Elecciones/Data/CEDE/Microdatos/")
list_files <- list.files() %>%
  .[. %in% c("1998", "2002", "2006", "2010", "2014")] %>%
  str_c("~/Dropbox/BANREP/Elecciones/Data/CEDE/Microdatos/", ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){
    x[str_detect(x, "Sena")]
  }) %>%
  str_c("~/Dropbox/BANREP/Elecciones/Data/CEDE/Microdatos", c("1998", "2002", "2006", "2010", "2014"),
        ., sep = "/")


#Open dta files into a list and split national from municipal results
senadores <- lapply(list_files, read_dta)
senadores_ganadores <- lapply(senadores, filter, codmpio == 99 & curules >= 1)

#Get voting rank by municipalities in a list of years
senadores_cluster <- senadores %>%
  lapply(., function(x){
    filter(x, codmpio != 99) %>%
    mutate(non_candidate = ifelse(codlista %in% c(996, 997, 998, 999), 1, 0)) %>%
    group_by(codmpio, ano) %>%
    mutate(prop_votes_total = votos / sum(votos)) %>%
    filter(non_candidate == 0) %>%
    mutate(prop_votes_candidates = votos / sum(votos)) %>%
    mutate(rank = dense_rank(desc(votos))) %>%
    arrange(codmpio, rank)
  })

#Identify winners
senadores_cluster <- senadores_cluster %>%
  mapply(function(x, y){
    mutate(x, ganador = ifelse(primer_apellido %in% y$primer_apellido &
                               nombre %in% y$nombre, 1, 0))
  }, x = ., y = senadores_ganadores)






