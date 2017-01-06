###########################################################################################################
################################ ########### COAT TAILS HOUSE: RD #########################################
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

# Load house candidates (collapsed by party) for t+1
representantes <- readRDS(paste0(res, "representantes_merge.rds")) %>%
  dplyr::rename(., ano_t = ano)

###########################################################################################################
########################### RD: REVERSE COAT-TAIL EFFECT - ONE PARTY APPROACH #############################
###########################################################################################################

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(codpartido == 1) %>%
  filter(ano != 2015) %>%
  #  filter(prop_votes_c2 >= 0.35 & prop_votes_c2 <= 0.65) %>%
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
# alcaldes_rd <- subset(alcaldes_rd, ano == 2007)

a <- rdrobust(y = alcaldes_rd$prop_votes_total_t,
              x = alcaldes_rd$prop_votes_cand_t1,
              # covs = cbind(as.factor(alcaldes_rd$ano), alcaldes_rd$parties_t),
              c = 0.5,
              all = T,
              vce = "hc1")

alcaldes_rd_b <- alcaldes_rd %>% filter(prop_votes_c2 >= (0.5 - a$bws[1,1]) & prop_votes_c2 <= (0.5 + a$bws[1,1]))
dim(alcaldes_rd_b)
hist(alcaldes_rd_b$prop_votes_c2)

b <- lm(formula = prop_votes_total_t1 ~ prop_votes_c2*win_t + factor(ano), data = alcaldes_rd_b)

a 
summary(b)



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
big_parties <- parties[1:20,]$codpartido

# Function: Create RD dataset by party (Restrict to big parties and difference to bdw < 0.15)
RD_data <- function(x){
  alcaldes_rd <- alcaldes_merge_r2 %>%
    filter(codpartido == x) %>%
    filter(ano != 2015) %>%
    #  filter(prop_votes_c2 >= 0.35 & prop_votes_c2 <= 0.65) %>%
    group_by(ano, codmpio) %>%
    mutate(party_2 = n()) %>%
    filter(party_2 == 1) %>% 
    mutate(win_t = ifelse(rank== 1, 1, 0)) %>% 
    merge(senado_merge,  by.x = c("ano", "codmpio","codpartido"), by.y = c("year_lag_presidencial", "codmpio", "codpartido"), 
          suffixes = c("_t", "_t1"), all.x = T) %>%
    dplyr::select(codmpio, ano, ano_t, codpartido, win_t, rank_t,
                  rank_t1, starts_with("prop")) %>%
    arrange(codmpio, ano)
}

# Foreach all parties create RD dataset and then append 
alcaldes_rd_a <- lapply(big_parties, RD_data) 
alcaldes_rd_n <- alcaldes_rd_a %>% ldply() %>% arrange(codpartido, codmpio, ano)
dim(alcaldes_rd_n)
hist(alcaldes_rd_n$prop_votes_c2)

table(alcaldes_rd_n$codpartido,alcaldes_rd_n$ano)


# RD and OLS regressions on restricted sample
a <- rdrobust(y = alcaldes_rd_n$prop_votes_cand_t1,
              x = alcaldes_rd_n$prop_votes_c2,
              covs = cbind(as.factor(alcaldes_rd_n$ano),as.factor(alcaldes_rd_n$codpartido),as.factor(alcaldes_rd_n$codmpio), alcaldes_rd_n$parties_t),
              c = 0.5,
              all = T,
              vce = "hc1")

alcaldes_rd_nb <- alcaldes_rd_n %>% filter(prop_votes_c2 >= (0.5 - a$bws[1,1]) & prop_votes_c2 <= (0.5 + a$bws[1,1]))
dim(alcaldes_rd_nb)
hist(alcaldes_rd_nb$prop_votes_c2)
b <- lm(formula = prop_votes_total_t1 ~ prop_votes_c2 + win_t + factor(ano)+ factor(codpartido) + factor(codmpio), data = alcaldes_rd_nb)

a 
summary(b)

# RD and OLS regressions by year (restricted sample)

years <- names(table(alcaldes_rd_n$ano))
alcaldes_rd_y <- lapply(years, function(x){
  alcaldes_rd_n %>% filter(ano == x)
}) 

a <-  lapply(alcaldes_rd_y, function(x){
  rdrobust(y = x$prop_votes_total_t1,
           x = x$prop_votes_c2,
           covs = cbind(as.factor(x$codpartido), x$parties_t),
           c = 0.5,
           all = T)
})

a
years




