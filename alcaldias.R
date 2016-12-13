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
  str_c("~/Dropbox/BANREP/Elecciones/Data/CEDE/Microdatos", c("", "1997", "2000", "2003", "2007", "2011", "2015"),
        ., sep = "/")


#Open dta files into a list
alcaldes <- lapply(list_files, read_dta)


#Aggregate totals for each year and clean non-candidate data

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados")

alcaldes_aggregate <- alcaldes %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
    mutate(non_candidate = ifelse(primer_apellido %in% non_candidate_votes, 1, 0)) %>% 
    group_by(codmpio, ano) %>%
    mutate(prop_votes_total = votos / sum(votos)) %>%
    filter(non_candidate == 0) %>%
    mutate(prop_votes_candidates = votos / sum(votos)) %>%
    mutate(rank = dense_rank(desc(votos))) %>%
    filter(rank <= 2) %>%
    filter(prop_votes_candidates < 1) #Eliminate elections with only the winner reported
  })

#Arrange data in a long format
alcaldes_merge <- alcaldes_aggregate %>%
  ldply() %>%
  arrange(codmpio, ano, desc(rank)) %>%
  dplyr::select(c(ano, codmpio, codep, municipio, departamento, primer_apellido, nombre, codpartido, votos, 
                  prop_votes_total, prop_votes_candidates, rank))

###########################################################################################################
############################################### ARRANGE DATA ##############################################
###########################################################################################################

alcaldes_difference <- alcaldes_merge %>%
  arrange(codmpio, ano, desc(rank)) %>%
  group_by(codmpio, ano) %>% #Calculate difference
  mutate(diff =  ave(prop_votes_total, factor(codmpio), factor(ano), FUN = function(x) c(0, diff(x)))) 
  # %>%
  # dplyr::mutate(diff = ifelse(posici_muni == 1 & pvotos_muni_can == 100 & diff == 0, 100, diff))

#Collapse data.frame

alcaldes_difference <- alcaldes_difference %>%
  dplyr::group_by(codmpio, ano) %>%
  dplyr::summarize(difference = sum(diff))

# This process can generate NA's. This results from the fact that for some years and municipalities
# elections were not held or that results were only reported for winner. Thus, the no result is 
# reported as NA in the wide version of the df. 

alcaldes_wide <- alcaldes_difference %>%
  spread(ano, difference, sep = "") 

# To account for the no-election situation, the data.frame can be "cleaned" of NA's removing all the 
# municipalities with at least one NA. This is desireable for the calculation of the trajectories. 
# The other option is interpolation, but for this data the bias can be *huge*. 

alcaldes_wide <- alcaldes_wide[complete.cases(alcaldes_wide), ]

# Longitudinal (again).

alcaldes_long <- alcaldes_wide %>%
  gather(ano, diff, ano1997:ano2015)
  
# Density by year (interactive!)
p <- ggplot(alcaldes_long, aes(diff, colour = factor(ano))) + geom_density()
ggplotly(p)

###########################################################################################################
################################### LONGITUDINAL CLUSTER ANALYSIS #########################################
###########################################################################################################

cld <- cld(alcaldes_wide, timeInData = c(2:7), idAll = alcaldes_wide$codmpio)
kml(cld, toPlot = "both")

#Get clusters
alcaldes_wide$cluster <- getClusters(cld, 6)

#Merge with geographical data
setwd("~/Dropbox/Geografia/")
municipios <- readOGR("mpio", "mpio") %>%
  spTransform(CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  

municipios_clust <- municipios %>%
  tidy(region = "MPIOS_1") %>%
  mutate(muni_code = as.numeric(id)) %>%
  merge(., municipios@data, by.x = "id", by.y = "MPIOS_1", all = T) %>%
  merge(., alcaldes_wide, by.x = "id", by.y = "codmpio", all = T)


# Plot using ggplot
setwd("~/Desktop/")
g <- ggplot(data = municipios_clust, aes(x = long, y = lat)) + geom_polygon(aes(group = id, fill = cluster))
g <- g + coord_equal()
# g <- g + theme(legend.position = c(.15, .25))
ggplotly(g)
# ggsave(str_c("americas_regions.pdf"), width=45, height=55, units="cm")





