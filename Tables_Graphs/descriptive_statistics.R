###########################################################################################################
############################################# COAT-TAILS RD ###############################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust","rddensity")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
dnp_e <- "Data/DNP/Ejecuciones/"
dnp_d <- "Data/DNP/Desempeno/"
invias <- "Data/invias/"
pgn <- "Data/PGN/"
violencia <- "Data/Violencia/"
agro <- "Data/Agro/"
edu <- "Data/Educacion/"
noaa <- "Data/NOAA/"

results <- "Results/Descriptives/"

# COntrols
cede <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))
controls <- cede %>%
  dplyr::select(coddepto, codmpio, municipio, ano, nbi) %>%
  filter(ano == 1993) %>%
  merge(., cede, by.x = c("codmpio"), by.y = c("codmpio"), all = T)

# Load ejecuciones
ejecu_all <- read_dta(paste0(dnp_e,"Ejecuciones_all.dta"))
vias_all <- read_dta(paste0(dnp_e,"Vias_SICEP_all.dta"))
invias_all <- read_dta(paste0(invias,"invias_all.dta"))

# Load outcomes
desempeno <- read_dta(paste0(dnp_d,"desempeno_last.dta"))
pgn <- read_dta(paste0(pgn,"PGN_all.dta"))
hom <- read_dta(paste0(violencia,"homicidios_all.dta"))
agro <- read_dta(paste0(agro,"agro_all.dta"))
cobertura <- read_dta(paste0(edu,"cobertura_all.dta"))
icfes <- read_dta(paste0(edu,"icfes_all.dta"))
teen <- read_dta(paste0(edu,"nac_all.dta"))
nightlights <- read_dta(paste0(noaa,"nightlights_all.dta"))

# Load maire data
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))


# Subsequent elections
alcaldes_t1 <-  readRDS(paste0(res,"alcaldes_t1.rds"))
representantes <- readRDS(paste0(res,"representantes_merge.rds")) 
senado <- readRDS(paste0(res,"senado_merge.rds")) 
president_1 <- readRDS(paste0(res, "presidentes_primera_merge.rds")) 
president_2 <- readRDS(paste0(res, "presidentes_segunda_merge.rds")) 

###########################################################################################################
######################################## ELECTIONS DATA ###################################################
###########################################################################################################

# Dataset: incumbency regression 

alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  arrange(codmpio, ano, codpartido) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) %>% 
  merge(., ejecu_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., desempeno,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., pgn,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., hom,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., agro,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., cobertura,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., icfes,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., teen,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., nightlights,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T)
  
# Descriptive statistics: Characteristics and outcomes

des <- alcaldes_merge_r2 %>% dplyr::select(pobl_tot.x, altura,discapital, disbogota, nbi.x ,)
  
setwd(results)
stargazer(des, summary.stat = c("mean", "sd"), type = "latex", out= "descriptives.tex")

# Close elections 
des_2 <- des %>% filter(margin_prop_2>=-0.2 & margin_prop_2<=0.2)
stargazer(des_2, summary.stat = c("mean", "sd"), type = "latex", out= "descriptives_02.tex")

des_1 <- des %>% filter(margin_prop_2>=-0.1 & margin_prop_2<=0.1)
stargazer(des_1, summary.stat = c("mean", "sd"), type = "latex", out= "descriptives_01.tex")

