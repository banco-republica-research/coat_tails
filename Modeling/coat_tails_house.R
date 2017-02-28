###########################################################################################################
############################################ COAT TAILS HOUSE: RD #########################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"


###########################################################################################################
############################################# LOAD DATA ###################################################
###########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  arrange(codmpio, ano, codpartido) %>%
  filter(codpartido != 98 & codpartido != 99) %>% 
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>% 
  filter(nn==2) %>% 
  dplyr::select(-c(n,nn)) 

# Load party codes and municipal covariates
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))
controls <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))

# Load house candidates (collapsed by party) for t+1
representantes <- readRDS(paste0(res, "representantes_merge.rds")) %>%
  dplyr::rename(., ano_t = ano)

representantes_coalition <- readRDS(paste0(res,"representantes_coalition_merge.rds"))

###########################################################################################################
########################### RD: REVERSE COAT-TAIL EFFECT - ONE PARTY APPROACH #############################
###########################################################################################################

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(codpartido == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(representantes,  by.x = c("ano", "codmpio","codpartido"), by.y = c("year_lag_presidencial", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, ano, ano_t, codpartido, win_t, rank_t,
                rank_t1, starts_with("prop"), name_party) %>%
  arrange(codmpio, ano)

dim(alcaldes_rd)
hist(alcaldes_rd$prop_votes_c2)

# RD and OLS regressions on restricted sample
l <- alcaldes_rd %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 
# %>% filter(prop_votes_c2 <= 0.5 + sd(prop_votes_c2) * 1.96 & prop_votes_c2 >= 0.5 - sd(prop_votes_c2) * 1.96)

dim(l)
hist(l$prop_votes_c2)

a <- rdrobust(y = l$prop_votes_total_t1,
              x = l$prop_votes_c2,
              covs = cbind(as.factor(l$ano), l$pobl_tot, as.factor(l$coddepto)),
              c = 0.5,
              all = T,
              vce = "hc1"
              ) 
a


###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - N PARTIES APPROACH ##################################
###########################################################################################################

# list of parties by total number of wins
parties <- alcaldes_merge %>% filter(rank == 1) %>% filter(codpartido!= 98 & codpartido!= 99) %>% 
  group_by(codpartido) %>% summarize(win = n()) %>% 
  merge(party_code,  by.x = c("codpartido"), by.y = c("party_code"), all.x = T) %>% 
  dplyr::select(codpartido, name_party, win) %>% 
  arrange(desc(win)) 

# list of N big parties (by total number of wins)
big_parties <- parties[1:50,]$codpartido
# big_parties <- parties$codpartido

# Function: Create RD dataset by party (Restrict to big parties and difference to bdw < 0.15)
RD_data <- function(x){
  alcaldes_rd <- alcaldes_merge_r2 %>%
    filter(codpartido == x) %>%
    group_by(ano, codmpio) %>%
    mutate(party_2 = n()) %>%
    filter(party_2 == 1) %>% 
    mutate(win_t = ifelse(rank== 1, 1, 0)) %>% 
    merge(representantes,  by.x = c("ano", "codmpio","codpartido"), by.y = c("year_lag_presidencial", "codmpio", "codpartido"), 
          suffixes = c("_t", "_t1"), all.x = T) %>%
    dplyr::select(codmpio, ano, ano_t, codpartido, win_t, rank_t,
                  rank_t1, starts_with("prop")) %>%
    arrange(codmpio, ano)
}

# Foreach all parties create RD dataset and then append 
alcaldes_rd_a <- lapply(big_parties, RD_data) 
alcaldes_rd_n <- alcaldes_rd_a %>% ldply() %>% arrange(codpartido, codmpio, ano)

# RD and OLS regressions on restricted sample
l <- alcaldes_rd_n %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 
# %>% filter(prop_votes_c2 <= 0.5 + sd(prop_votes_c2) * 1.96 & prop_votes_c2 >= 0.5 - sd(prop_votes_c2) * 1.96)

dim(l)
hist(l$prop_votes_c2)
table(l$codpartido,l$ano)

# RD and OLS regressions on restricted sample
a <- rdrobust(y = alcaldes_rd_n$prop_votes_total_t1,
              x = alcaldes_rd_n$prop_votes_c2,
              covs = cbind(as.factor(l$ano),l$pobl_tot, as.factor(l$coddepto),as.factor(alcaldes_rd_n$codpartido)),
              c = 0.5,
              all = T,
              vce = "hc1")
a


###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
###########################################################################################################


# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions, by.x = c("codpartido","ano") , by.y = c("party_code", "year_lag_presidencial"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition) == F & coalition != 98 & coalition != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(codep,n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 


###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################### PRESIDENT SECOND ROUND ##########################################
###########################################################################################################

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., representantes_coalition,  by.x = c("year", "codmpio", "coalition"), by.y = c("ano", "codmpio", "coalition"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  # merge(., ejecu_mean,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  # merge(., vias_mean,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  # merge(., invias_mean,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  # merge(., ejecu_dep,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  # mutate(eje_dep1 = ifelse(eje_dep >= 0.8, 1,0)) %>%
  # dplyr::select(codmpio, pobl_tot, coddepto, ano, year, codpartido_t, win_t, 
                # votos_t, votos_t1, starts_with("prop"), starts_with("eje_"), starts_with("vias"),starts_with("f_"), starts_with("log")) %>% 
  filter(is.na(prop_votes_total_t1) == F & is.na(prop_votes_c2) == F, prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

a <- rdrobust(y = alcaldes_rd$prop_votes_total_t1,
              x = alcaldes_rd$prop_votes_c2,
              c = 0.5,
              all = T,
              vce = "hc1")



