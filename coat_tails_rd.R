###########################################################################################################
############################################# COAT-TAILS RD ###############################################
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
alcaldes_merge_r2 <- alcaldes_merge %>% filter(rank <= 2)
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

# Load presidential for t+1
president <- readRDS(paste0(res, "presidentes_merge.rds")) %>%
  filter(rank <= 2) %>%
  mutate(coalition = ifelse(rank == 1 , 1, 0)) 



###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
###########################################################################################################

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition == 1) %>%
  filter(ano != 2015) %>%
  #  filter(prop_votes_c2 >= 0.35 & prop_votes_c2 <= 0.65) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(president,  by.x = c("year", "codmpio","coalition"), by.y = c("ano", "codmpio", "coalition"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, ano, year, codpartido_t, win_t, rank_t,
                rank_t1 ,parties_t,parties_ef_t, starts_with("prop")) %>%
  arrange(codmpio, ano)

dim(alcaldes_rd)
hist(alcaldes_rd$prop_votes_c2_t)

# RD and OLS regressions on restricted sample
# alcaldes_rd <- subset(alcaldes_rd, ano == 2007)

a <- rdrobust(y = alcaldes_rd$prop_votes_c2_t1,
              x = alcaldes_rd$prop_votes_c2_t,
              covs = cbind(as.factor(alcaldes_rd$ano), as.factor(alcaldes_rd$codmpio)),
              c = 0.5,
              all = T, 
              vce = "hc1")
a

alcaldes_rd_b <- alcaldes_rd %>% filter(prop_votes_c2_t >= (0.5 - a$bws[1,1]) & prop_votes_c2_t <= (0.5 + a$bws[1,1]))
dim(alcaldes_rd_b)
hist(alcaldes_rd_b$prop_votes_c2_t)

b <- lm(formula = prop_votes_total_t1 ~ win_t*prop_votes_c2_t + factor(ano), data = alcaldes_rd_b)
summary(b)


