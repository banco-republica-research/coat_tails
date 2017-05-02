###########################################################################################################
############################################# COAT-TAILS RD ###############################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
results <- "Results/RD"

###########################################################################################################
######################################## ELECTIONS DATA ###################################################
###########################################################################################################

# Load maire and coalition data
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
coalitions_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 

# Load party codes and municipal covariates
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))
controls <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))

# top2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99)  %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn==2) %>%
  dplyr::select(-c(codep,n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 

dim(alcaldes_merge_r2)

# Load presidential for t+1
win_apellido <- c("PASTRANA", "URIBE", "SANTOS")
win_nom <- c("ANDRES", "ALVARO", "JUAN MANUEL")

president <- readRDS(paste0(res, "presidentes_segunda_merge.rds")) %>%
  mutate(coalition = ifelse(primer_apellido %in% win_apellido & nombre %in% win_nom , 1, 0))


###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################### PRESIDENT FIRST ROUND ###########################################
###########################################################################################################

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., president,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, pobl_tot, coddepto, ano, year, codpartido_t, win_t, 
                votos_t, votos_t1, starts_with("prop")) %>% 
  filter(is.na(prop_votes_total_t1)==0 & is.na(prop_votes_c2)==0) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# Second rounds only
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
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  mean <- l %>% filter(prop_votes_c2 <= 0.5 + r$bws[1] &
                         prop_votes_c2 >= 0.5 - r$bws[1])
  mean <- mean(l[,out], na.rm = T)
  return(list(rd = r, mean = mean))
}
r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/coat_tails_president1_coalition.rds"))


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
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
############################################### BY PARTY ##################################################
###########################################################################################################

# Use the same data base but merge with between party codes (codpartido) instead of coalition
alcaldes_rd <- alcaldes_merge_r2 %>%
  # filter(coalition_new == 1) %>%
  # group_by(ano, codmpio) %>%
  # mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  # filter(party_2 == 1) %>% 
  # mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., president,  by.x = c("year", "codmpio", "codpartido"), by.y = c("ano", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, pobl_tot, coddepto, ano, year, 
                votos_t, votos_t1, starts_with("prop")) %>% 
  filter(is.na(prop_votes_total_t1)==0 & is.na(prop_votes_c2)==0) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# Second rounds only
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
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  mean <- l %>% filter(prop_votes_c2 <= 0.5 + r$bws[1] &
                         prop_votes_c2 >= 0.5 - r$bws[1])
  mean <- mean(l[,out], na.rm = T)
  return(list(rd = r, mean = mean))
}
r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/coat_tails_president1_party.rds"))





###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
############################################ miscellaneous ################################################
###########################################################################################################

#From elections.R
president1 <- readRDS(paste0(res, "presidentes_primera_merge.rds"))
candidatos1 <- readRDS(paste0(res, "candidates_primera_vuelta.rds"))

# Create a coalition new variable which identify mayors with party-presidential candidates in the first 
# To be done manually

# alcaldes_rd_1 <- alcaldes_merge_r2 %>%
#   merge(., candidatos1, by.x = c("codpartido", "year"), by.y = c("codpartido", "ano"),  all.x = T, suffixes = c("", "_p")) %>% 
#   mutate(coalition_party = ifelse(!is.na(primer_apellido_p), codpartido,
#                            ifelse(coalition == 1 & is.na(primer_apellido_p), 1111,
#                            ifelse(coalition == 0, 0, 0))
#   )) %>%
#   dplyr::select(., -ends_with("_p"), votos_totales)
# 
# coal_dic <- alcaldes_rd_1 %>% group_by(year, codpartido, coalition_party) %>% 
#   summarize(sum = sum(votos)) %>% dplyr::select(-sum)
# 
# # President aggregate results to merge by coallition_party
# president1 <- president1 %>% filter(ano >= 1998 & codpartido != "NaN") %>% dplyr::select(.,codmpio, codpartido, ano, prop_votes_total) %>%
#   merge(.,coal_dic, by.x = c("codpartido", "ano"), by.y = c("codpartido", "year"), all.x = T) %>%
#   group_by(coalition_party) %>%
#   summarize(prop_votes_total_coal = sum(prop_votos_total))


table(alcaldes_rd_1$coalition_party, alcaldes_rd_1$coalition)


# Repeat modeling as incumbency effect RD: per party in coalition
# list of parties by total number of wins
# parties <- alcaldes_rd_1 %>% filter(rank == 1) %>% filter(codpartido!= 98 & codpartido!= 99) %>%
#   group_by(codpartido) %>% summarize(win = n()) %>%
#   merge(party_code, by.x = c("codpartido"), by.y = c("party_code"), all.x = T) %>%
#   dplyr::select(codpartido, name_party, win) %>%
#   arrange(desc(win))
# 
# # list of N big parties (by total number of wins)
# big_parties <- parties[1:20,]$codpartido

# Function: Create RD dataset by party 
RD_data <- function(x){
  alcaldes_rd <- alcaldes_rd_1 %>%
    filter(coalition_party == x) %>%
    group_by(ano, codmpio) %>%
    mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
    filter(party_2 == 1) %>% 
    mutate(win_t = ifelse(rank==1,1,0)) %>% 
    merge(president, by.x = c("year", "codmpio","codpartido"), by.y = c("ano", "codmpio", "codpartido"), 
          suffixes = c("_t", "_t1"), all.x = T) %>%
    # dplyr::select(codmpio, ano, ano_t1, codpartido, win_t, rank_t,
    # rank_t1 ,parties_t,parties_ef_t,parties_t1,parties_ef_t1, votos_t, votos_t1, starts_with("prop")) %>%
    arrange(codmpio, ano)
}

# Foreach all parties create RD dataset and then append 
alcaldes_rd_a <- lapply(unique(alcaldes_rd_1$coalition_party), RD_data) 
alcaldes_rd_n <- alcaldes_rd_a %>% ldply() %>% arrange(codpartido, codmpio, ano)
