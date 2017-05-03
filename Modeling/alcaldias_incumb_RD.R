###########################################################################################################
############################################# INCUMBENCY RD ###############################################
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
results <- "Results/RD"

###########################################################################################################
############################################# LOAD DATA ###################################################
###########################################################################################################

# load all datasets
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))
controls <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))

# Elections at t
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  arrange(codmpio, ano, codpartido) %>%
  filter(codpartido != 98 & codpartido != 99) %>% 
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>% 
  filter(nn==2) %>% 
  dplyr::select(-c(n,nn)) %>% 
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T)


# Elections at t+1 Create lagged year and collapse by party (or group of parties) for t+1 outcome  
alcaldes_merge_t1 <- alcaldes_merge %>%
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
  summarize(votos = sum(votos), 
            prop_votes_cand = sum(prop_votes_cand),
            prop_votes_total = sum(prop_votes_total),
            rank = max(rank))



# Test duplicates
# table(duplicated(alcaldes_merge_collapse[,c("codmpio", "ano_lag", "codpartido")]))


###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - ALL PARTIES #########################################
###########################################################################################################

alcaldes_rd_all <- alcaldes_merge_r2 %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  group_by(ano, codmpio, codpartido) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  merge(alcaldes_merge_t1,  by.x = c("ano", "codmpio","codpartido"), by.y = c("ano_lag", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x=T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_b_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  dplyr::select(codmpio,pobl_tot, coddepto,  ano, codpartido, win_t, rank_t, votos_t, prop_votes_c2,
                run_t1, rank_t1 , votos_t1, prop_votes_cand_t1, prop_votes_total_t1,prop_votes_total_b_t1) %>%
  arrange(codmpio, ano) %>%
  mutate(win_t1 = ifelse(is.na(rank_t1) == 1 | rank_t1 != 1, 0, 1)) 

l <- alcaldes_rd_all 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(as.factor(l$codpartido), as.factor(l$ano), l$pobl_tot, as.factor(l$coddepto)),
                c = 0.5,
                all = T)
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 15, kernel="triangular", p=3, ci=95
  )
  mean <- l %>% filter(prop_votes_c2 <= 0.5 + r$bws[1] &
                         prop_votes_c2 >= 0.5 - r$bws[1])
  mean <- mean(l[,out], na.rm = T)
  return(list(rd = r, mean = mean)) 
}

out <- c("prop_votes_total_t1")
other <- c("win_t1","prop_votes_total_b_t1","run_t1")
r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/incumbency_party.rds"))


###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
############################################## FINAL COALITION ############################################
###########################################################################################################
coalitions_long <- readRDS(paste0(res,"coalitions_new.rds"))

alcaldes_rd_c <- alcaldes_merge_r2 %>%
  mutate(win_t = ifelse(rank==1,1,0)) %>% 
  group_by(ano, codmpio, codpartido) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>%
  merge(., coalitions_long, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
  # merge(., coalitions, by.x = c("codpartido","ano") , by.y = c("party_code", "year_lag_presidencial"), all.x = T) %>%
  # filter(coalition!=98 & coalition!=99) %>%
  merge(alcaldes_merge_t1,  by.x = c("ano", "codmpio","codpartido"), by.y = c("ano_lag", "codmpio", "codpartido"),
        suffixes = c("_t", "_t1"), all.x=T) %>%
  mutate(run_t1 = ifelse(is.na(prop_votes_total_t1), 0, 1)) %>%
  mutate(prop_votes_total_b_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  group_by(ano, codmpio, codep, municipio, cand, prop_votes_c2, prop_votos_total)
  # dplyr::select(codmpio, pobl_tot, coddepto, ano, codpartido, coalition, win_t, rank_t, votos_t, prop_votes_c2,
                # run_t1, rank_t1 , votos_t1, prop_votes_cand_t1, prop_votes_total_t1,prop_votes_total_b_t1) %>%
  arrange(codmpio, ano) %>%
  mutate(win_t1 = ifelse(is.na(rank_t1) == 1 | rank_t1 != 1, 0, 1)) 

table(alcaldes_rd_c$coalition_new)
l <- alcaldes_rd_c 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# RD: Vote share conditional on running
l_f <- function(c){
  lc <- l %>% filter(coalition_new == c)
  lc2 <- lc %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)
  r <- rdrobust(y = lc$win_t1,
            x = lc$prop_votes_c2,
            # covs = cbind(as.factor(lc$codpartido), as.factor(lc$ano), lc$pobl_tot, as.factor(lc$coddepto)),
            c = 0.5,
            all = T)
  rdplot(y=lc2$win_t1, x=lc2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 15, kernel="triangular", p=3, ci=95)
  return(r)
  }

coal <- c(0,1)
lapply(coal,l_f) 














###########################################################################################################
############################### RD: IMCUMBENCY EFFECT - ONE PARTY APPROACH ################################
###########################################################################################################

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd_1 <- alcaldes_merge_r2 %>%
  filter(codpartido == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank==1,1,0)) %>% 
  merge(alcaldes_merge_t1,  by.x = c("ano", "codmpio","codpartido"), by.y = c("ano_lag", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x=T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_b_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  dplyr::select(codmpio, pobl_tot, coddepto, ano, codpartido, win_t, rank_t, votos_t, prop_votes_c2,
                run_t1, rank_t1 , votos_t1, prop_votes_cand_t1, prop_votes_total_t1,prop_votes_total_b_t1) %>%
  arrange(codmpio, ano) %>%
  mutate(win_t1 = ifelse(is.na(rank_t1) == 1 | rank_t1 != 1, 0, 1)) 

l <- alcaldes_rd_1 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(as.factor(l$ano), l$pobl_tot),
                c = 0.5,
                all = T)
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 15, kernel="triangular", p=3, ci=95, 
  )
  return(r)  
}

out <- c("prop_votes_total_t1","win_t1","prop_votes_total_b_t1","run_t1")
lapply(out, l_f) 


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
# big_parties <- parties$codpartido

# Function: Create RD dataset by party 
RD_data <- function(x){
  alcaldes_rd <- alcaldes_merge_r2 %>%
    filter(codpartido == x) %>%
    group_by(ano, codmpio) %>%
    mutate(party_2 = n()) %>%
    filter(party_2 == 1) %>% 
    mutate(win_t = ifelse(rank==1,1,0)) %>% 
    merge(alcaldes_merge_t1,  by.x = c("ano", "codmpio","codpartido"), by.y = c("ano_lag", "codmpio", "codpartido"), 
          suffixes = c("_t", "_t1"), all.x = T) %>%
    mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
    mutate(prop_votes_total_b_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
    dplyr::select(codmpio, pobl_tot, coddepto, ano, codpartido, win_t, rank_t, votos_t, prop_votes_c2,
                  run_t1, rank_t1 , votos_t1, prop_votes_cand_t1, prop_votes_total_t1,prop_votes_total_b_t1) %>%
    arrange(codmpio, ano) %>%
    mutate(win_t1 = ifelse(is.na(rank_t1) == 1 | rank_t1 != 1, 0, 1)) 
}

alcaldes_rd_a <- lapply(big_parties, RD_data) 
alcaldes_rd_n <- alcaldes_rd_a %>% ldply() %>% arrange(codpartido, codmpio, ano)

l <- alcaldes_rd_n 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(as.factor(l$ano), l$pobl_tot, as.factor(l$coddepto)),
                c = 0.5,
                all = T)
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 15, kernel="triangular", p=3, ci=95, 
  )
  return(r)
}

out <- c("prop_votes_total_t1","win_t1","prop_votes_total_b_t1","run_t1")
lapply(out, l_f) 




# RD and OLS regressions by year (restricted sample)

years <- names(table(l$ano))
l_y <- lapply(years, function(x){
  l %>% filter(ano == x)
}) 

lapply(l_y, function(a){
  rdrobust(y = a$prop_votes_total_t1,
           x = a$prop_votes_c2,
           covs = cbind(a$pobl_tot),
           c = 0.5,
           all = T,
           vce = "hc1")
})


