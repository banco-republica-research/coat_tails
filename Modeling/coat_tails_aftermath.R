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
dnp <- "Data/DNP/Desempeno/"
pgn <- "Data/PGN/"
violencia <- "Data/Violencia/"
agro <- "Data/Agro/"
edu <- "Data/Educacion/"
noaa <- "Data/NOAA/"

results <- "Results/RD/"

###########################################################################################################
######################################## ELECTIONS DATA ###################################################
###########################################################################################################

# Load maire data
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

# Load party codes and municipal covariates
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

cede <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))
controls <- cede %>%
  dplyr::select(coddepto, codmpio, municipio, ano, nbi) %>%
  filter(ano == 1993) %>%
  merge(., cede, by.x = c("codmpio"), by.y = c("codmpio"), all = T)

# Load presidential 
win_apellido <- c("PASTRANA", "URIBE", "SANTOS")
win_nom <- c("ANDRES", "ALVARO", "JUAN MANUEL")

president <- readRDS(paste0(res, "presidentes_segunda_merge.rds")) %>%
  mutate(coalition = ifelse(primer_apellido %in% win_apellido & nombre %in% win_nom , 1, 0)) %>% 
  plyr::rename(., c("ano" = "ano_p")) %>%
  mutate(ano_pl = ano_p-4) 

# Load outcomes
desempeno <- read_dta(paste0(dnp,"desempeno_last.dta"))
pgn <- read_dta(paste0(pgn,"PGN_all.dta"))
hom <- read_dta(paste0(violencia,"homicidios_all.dta"))
agro <- read_dta(paste0(agro,"agro_all.dta"))
cobertura <- read_dta(paste0(edu,"cobertura_all.dta"))
icfes <- read_dta(paste0(edu,"icfes_all.dta"))
teen <- read_dta(paste0(edu,"fert_all.dta"))
mort <- read_dta(paste0(edu,"tasa_mort_all.dta"))
nightlights <- read_dta(paste0(noaa,"nightlights_all.dta"))
  
###########################################################################################################
################################### desempeño: LAST year ##################################################
######################################## PGN: all #########################################################
################################ Coalition wrt CURRENT president ##########################################
###########################################################################################################

# Note: maires elected in 1997 begin in 01/01/1998 which is a presidential election year. Not enough overlapping with current
# Not enough observations for 2015 maires either 

# FINAL round CURRENT coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new)  %>%
  unique(.)

table(coalitions_long$ano,coalitions_long$year)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn==2) %>%
  dplyr::select(-c(codep,n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., president,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano_pl", "codmpio", "coalition"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, pobl_tot, nbi.x, discapital, disbogota, altura, coddepto.x, ano, year, codpartido_t, win_t, 
                votos_t, votos_t1, starts_with("prop"), margin_prop_2) %>% 
  filter(is.na(prop_votes_total_t1)==0 & is.na(prop_votes_c2)==0) %>% 
  merge(., desempeno,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., pgn,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., hom,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., agro,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., cobertura,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., icfes,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., teen,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., mort,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., nightlights,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# RD and OLS regressions 

# Select period: Drop 2011 given the coalition change during Santos I
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/After/RD_", o, ".pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         title = " ",
         x.label = "Victory Margin",
         y.label = "Vote share (subsequent Election)",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95 
  )
  dev.off()
  mean <- l %>% filter(prop_votes_c2 <= 0.5 + r$bws[1] &
                         prop_votes_c2 >= 0.5 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}


# outcomes

out <- c("cob_pri", "cob_sec", "matematicas_s","lenguaje_s","fert_19_10_p", "tasa_m", "hom_tasa")
r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "aftermath_publicgoods.rds"))
r

out <- c("log_ba_tot_vr", "log_ba_peq_vr","log_light_pix","log_light_dm")
r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "aftermath_growths.rds"))
r

out <- c("desemp_fisc","desemp_int", "alcalde", "alcalde_guilty", "top", "top_guilty")
r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "aftermath_institutions.rds"))
r
