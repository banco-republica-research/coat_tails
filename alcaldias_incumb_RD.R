
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"

###########################################################################################################
############################################# LOAD DATA ###################################################
###########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
alcaldes_merge_r2 <- alcaldes_merge %>% filter(rank <= 2)

# Create lagged year and collapse by party (or group of parties) for t+1 outcome  
alcaldes_merge_collapse <- alcaldes_merge %>%
  filter(ano != 1997) %>%
  filter(cand == 1) %>%
  mutate(ano_lag = as.factor(ano)) %>%
  mutate(ano_lag = fct_recode(ano_lag,
                              "1997" = "2000",
                              "2000" = "2003",
                              "2003" = "2007",
                              "2007" = "2011",
                              "2011" = "2015")) %>%
  mutate(ano_lag = as.character(ano_lag)) %>%
  rename(ano_t1 = ano) %>% 
  group_by(codmpio, ano_lag, ano_t1, codpartido, parties, parties_ef) %>%
  summarize(prop_votes_cand = sum(prop_votes_cand),
            prop_votes_total = sum(prop_votes_total),
            rank = max(rank))

# Test duplicates
# table(duplicated(alcaldes_merge_collapse[,c("codmpio", "ano_lag", "codpartido")]))


###########################################################################################################
############################### RD: IMCUMBENCY EFFECT - ONE PARTY APPROACH ################################
###########################################################################################################

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(codpartido == 198) %>%
  filter(ano != 2015) %>%
  filter(prop_votes_c2 >= 0.35 & prop_votes_c2 <= 0.65) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank==1,1,0)) %>% 
  merge(alcaldes_merge_collapse,  by.x = c("ano", "codmpio","codpartido"), by.y = c("ano_lag", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, ano, ano_t1, codpartido, win_t, rank_t,
                rank_t1 ,parties_t,parties_ef_t,parties_t1,parties_ef_t1, starts_with("prop")) %>%
  arrange(codmpio, ano) %>%
  mutate(reelection = ifelse(rank_t == 1 & rank_t1 == 1, 1, 0))

dim(alcaldes_rd)
hist(alcaldes_rd$prop_votes_c2)

# RD and OLS regressions on restricted sample
# alcaldes_rd <- subset(alcaldes_rd, ano == 2007)

a <- rdrobust(y = alcaldes_rd$prop_votes_total_t1,
         x = alcaldes_rd$prop_votes_c2,
         covs = cbind(as.factor(alcaldes_rd$ano), alcaldes_rd$parties_t),
         c = 0.5,
         all = T)

alcaldes_rd_b <- alcaldes_rd %>% filter(prop_votes_c2 >= (0.5 - a$bws[1,1]) & prop_votes_c2 <= (0.5 + a$bws[1,1]))
dim(alcaldes_rd_b)
hist(alcaldes_rd_b$prop_votes_c2)

b <- lm(formula = prop_votes_total_t1 ~ prop_votes_c2 + win_t + factor(ano), data = alcaldes_rd_b)

a 
summary(b)



###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - N PARTIES APPROACH ##################################
###########################################################################################################

# list of N largest parties
parties <- names(sort(table(alcaldes_merge_r2$codpartido),decreasing=TRUE)[1:20]) 

# Function: Create RD dataset by party: Restrict difference to <= 0.2
RD_data <- function(x){
alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(codpartido == x) %>%
  filter(ano != 2015) %>%
  filter(prop_votes_c2 >= 0.35 & prop_votes_c2 <= 0.65) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank==1,1,0)) %>% 
  merge(alcaldes_merge_collapse,  by.x = c("ano", "codmpio","codpartido"), by.y = c("ano_lag", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, ano, ano_t1, codpartido, win_t, rank_t,
                rank_t1 ,parties_t,parties_ef_t,parties_t1,parties_ef_t1, starts_with("prop")) %>%
  arrange(codmpio, ano) %>%
  mutate(reelection = ifelse(rank_t == 1 & rank_t1 == 1, 1, 0))
  }

# Foreach all parties create RD dataset and then append 
alcaldes_rd_a <- lapply(parties, RD_data) 
alcaldes_rd_n <- alcaldes_rd_a %>% ldply() %>% arrange(codpartido, codmpio, ano)
dim(alcaldes_rd_n)
hist(alcaldes_rd_n$prop_votes_c2)

# RD and OLS regressions on restricted sample
a <- rdrobust(y = alcaldes_rd_n$prop_votes_total_t1,
              x = alcaldes_rd_n$prop_votes_c2,
              covs = cbind(as.factor(alcaldes_rd_n$ano),as.factor(alcaldes_rd_n$codpartido),as.factor(alcaldes_rd_n$codmpio), alcaldes_rd_n$parties_t),
              c = 0.5,
              all = T)

alcaldes_rd_nb <- alcaldes_rd_n %>% filter(prop_votes_c2 >= (0.5 - a$bws[1,1]) & prop_votes_c2 <= (0.5 + a$bws[1,1]))
dim(alcaldes_rd_nb)
hist(alcaldes_rd_nb$prop_votes_c2)
b <- lm(formula = prop_votes_total_t1 ~ prop_votes_c2 + win_t + factor(ano)+ factor(codpartido) + factor(codmpio), data = alcaldes_rd_nb)

a 
summary(b)






