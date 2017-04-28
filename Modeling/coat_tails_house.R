###########################################################################################################
############################################ COAT TAILS HOUSE: RD #########################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","broom","gtools","cluster", "rdrobust")
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


###########################################################################################################
########################### RD: REVERSE COAT-TAIL EFFECT - ONE PARTY APPROACH #############################
###########################################################################################################

# representantes <- readRDS(paste0(res, "representantes_merge.rds")) %>% dplyr::rename(., ano_t = ano)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t
# 
# alcaldes_rd <- alcaldes_merge_r2 %>%
#   filter(codpartido == 1) %>%
#   group_by(ano, codmpio) %>%
#   mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
#   filter(party_2 == 1) %>% 
#   mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
#   merge(representantes,  by.x = c("ano", "codmpio","codpartido"), by.y = c("year_lag_presidencial", "codmpio", "codpartido"), 
#         suffixes = c("_t", "_t1"), all.x = T) %>%
#   dplyr::select(codmpio, ano, ano_t, codpartido, win_t, rank_t,
#                 rank_t1, starts_with("prop"), name_party) %>%
#   arrange(codmpio, ano)
# 
# dim(alcaldes_rd)
# hist(alcaldes_rd$prop_votes_c2)

# RD and OLS regressions on restricted sample
# l <- alcaldes_rd %>%
#   merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 
# # %>% filter(prop_votes_c2 <= 0.5 + sd(prop_votes_c2) * 1.96 & prop_votes_c2 >= 0.5 - sd(prop_votes_c2) * 1.96)
# 
# dim(l)
# hist(l$prop_votes_c2)
# 
# a <- rdrobust(y = l$prop_votes_total_t1,
#               x = l$prop_votes_c2,
#               covs = cbind(as.factor(l$ano), l$pobl_tot, as.factor(l$coddepto)),
#               c = 0.5,
#               all = T,
#               vce = "hc1"
#               ) 
# a


###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - N PARTIES APPROACH ##################################
###########################################################################################################

# # list of parties by total number of wins
# parties <- alcaldes_merge %>% filter(rank == 1) %>% filter(codpartido!= 98 & codpartido!= 99) %>% 
#   group_by(codpartido) %>% summarize(win = n()) %>% 
#   merge(party_code,  by.x = c("codpartido"), by.y = c("party_code"), all.x = T) %>% 
#   dplyr::select(codpartido, name_party, win) %>% 
#   arrange(desc(win)) 
# 
# # list of N big parties (by total number of wins)
# big_parties <- parties[1:50,]$codpartido
# # big_parties <- parties$codpartido
# 
# # Function: Create RD dataset by party (Restrict to big parties and difference to bdw < 0.15)
# RD_data <- function(x){
#   alcaldes_rd <- alcaldes_merge_r2 %>%
#     filter(codpartido == x) %>%
#     group_by(ano, codmpio) %>%
#     mutate(party_2 = n()) %>%
#     filter(party_2 == 1) %>% 
#     mutate(win_t = ifelse(rank== 1, 1, 0)) %>% 
#     merge(representantes,  by.x = c("ano", "codmpio","codpartido"), by.y = c("year_lag_presidencial", "codmpio", "codpartido"), 
#           suffixes = c("_t", "_t1"), all.x = T) %>%
#     dplyr::select(codmpio, ano, ano_t, codpartido, win_t, rank_t,
#                   rank_t1, starts_with("prop")) %>%
#     arrange(codmpio, ano)
# }
# 
# # Foreach all parties create RD dataset and then append 
# alcaldes_rd_a <- lapply(big_parties, RD_data) 
# alcaldes_rd_n <- alcaldes_rd_a %>% ldply() %>% arrange(codpartido, codmpio, ano)
# 
# # RD and OLS regressions on restricted sample
# l <- alcaldes_rd_n %>%
#   merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 
# # %>% filter(prop_votes_c2 <= 0.5 + sd(prop_votes_c2) * 1.96 & prop_votes_c2 >= 0.5 - sd(prop_votes_c2) * 1.96)
# 
# dim(l)
# hist(l$prop_votes_c2)
# table(l$codpartido,l$ano)
# 
# # RD and OLS regressions on restricted sample
# a <- rdrobust(y = alcaldes_rd_n$prop_votes_total_t1,
#               x = alcaldes_rd_n$prop_votes_c2,
#               covs = cbind(as.factor(l$ano),l$pobl_tot, as.factor(l$coddepto),as.factor(alcaldes_rd_n$codpartido)),
#               c = 0.5,
#               all = T,
#               vce = "hc1")
# a

###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
########################################  COALITION FIRST ROUND ###########################################
###########################################################################################################

# coalition FIRST round
coalitions_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 
representantes_coalition <- readRDS(paste0(res,"representantes_coalition_primera_merge.rds"))

# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 


# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., representantes_coalition,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  filter(is.na(prop_votes_total_t1) == F & is.na(prop_votes_c2) == F, prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# All 
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot),
                c = 0.5,
                all = T,
                vce = "nn")
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95, 
  )
  return(r)
}

lapply(out, l_f) 



###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
########################################  COALITION FINAL ROUND ###########################################
###########################################################################################################

# coalition FINAL roundS
coalitions_long <- readRDS(paste0(res,"coalitions_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 
representantes_coalition <- readRDS(paste0(res,"representantes_coalition_merge.rds"))

# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 


# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., representantes_coalition,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  filter(is.na(prop_votes_total_t1) == F & is.na(prop_votes_c2) == F, prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# All 
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot),
                c = 0.5,
                all = T,
                vce = "nn")
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95, 
  )
  return(r)
}

lapply(out, l_f) 


############################
# RD and OLS regressions by year 

years <- names(table(l$ano))
l_y <- lapply(years, function(x){
  alcaldes_rd %>% filter(ano == x)
}) 

lapply(l_y, function(a){
  rdrobust(y = a$prop_votes_total_t1,
           x = a$prop_votes_c2,
           covs = cbind(a$pobl_tot),
           c = 0.5,
           all = T,
           vce = "nn")
})


###########################################################################################################
######################################## COAT TAILS HOUSE BY PARTY: RD ####################################
###########################################################################################################

#House representatives by party (previous house data bases were arranged by coalition of first or second round)
representantes <- readRDS(paste0(res,"representantes_merge.rds")) 

# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>%
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 


# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Merge elections by their party code (codpartido)

alcaldes_rd <- alcaldes_merge_r2 %>%
  # filter(coalition_new == 1) %>%
  # group_by(ano, codmpio) %>%
  # mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  # filter(party_2 == 1) %>%
  # mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., representantes,  by.x = c("year", "codmpio", "codpartido"), by.y = c("ano", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1")) %>%
  filter(is.na(prop_votes_total_t1) == F & is.na(prop_votes_c2) == F, prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# All 
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot),
                c = 0.5,
                all = T,
                vce = "nn")
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95, 
  )
  return(r)
}

lapply(out, l_f) 

