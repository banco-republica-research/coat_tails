###########################################################################################################
############################################# COAT-TAILS RD ###############################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","stargazer")
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
teen <- read_dta(paste0(edu,"fert_all.dta"))
mort <- read_dta(paste0(edu,"tasa_mort_all.dta"))
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
########################################  VARIABLES #######################################################
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
  merge(., mort,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., nightlights,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T)
  
# Descriptive statistics: Characteristics and outcomes

des <- alcaldes_merge_r2 %>% dplyr::select(pobl_tot.x, altura,discapital, disbogota, nbi.x, D2000_pc, D1000_pc, D3000_pc, f_SGPp_pc,f_regalias_pc, f_trans_nac_pc, tasa_m, cob_pri, cob_sec, matematicas_s,lenguaje_s,fert_19_10_p,hom_tasa, desemp_fisc,desemp_int, alcalde, alcalde_guilty, top, top_guilty, light_pix,light_dm,ba_tot_vr_pc, ba_peq_vr_pc)

setwd(results)
stargazer(des, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2, out= "descriptives.tex")

###########################################################################################################
######################################### ELECTIONS #######################################################
###########################################################################################################





des_ele <- alcaldes_merge_r2 %>% filter(cand==1) %>% filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>% 
  filter(rank==1) %>% dplyr::select(prop_votes_cand, margin_prop_2) 
stargazer(des_ele, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

des_ele2 <- alcaldes_merge_r2 %>% filter(!is.finite(prop_votes_cand)==F) %>% filter(prop_votes_cand <= 1) %>% 
  group_by(ano, codmpio) %>% summarize(vs2 = sum(prop_votes_cand)) %>% filter(vs2 <= 1) %>% 
  dplyr::select(vs2) 

summary(des_ele2)
stargazer(des_ele2, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

