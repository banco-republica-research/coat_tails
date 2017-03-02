###########################################################################################################
############################################# COAT-TAILS RD ###############################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
dnp <- "Data/DNP/Ejecuciones/"
invias <- "Data/invias/"

###########################################################################################################
######################################## ELECTIONS DATA ###################################################
###########################################################################################################

# Load maire and coalition data
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
coalitions <- readRDS(paste0(res,"coalitions.rds"))
# coalitions_con <- readRDS(paste0(res,"coalitions_con.rds"))

# Load party codes and municipal covariates
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))
controls <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))

# top2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions, by.x = c("codpartido","ano") , by.y = c("party_code", "year_lag_presidencial"), all.x = T) %>%
  #   merge(., coalitions_con, by = c("codmpio", "codpartido","ano"), all.x = T) 
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition) == 0 & coalition != 98 & coalition != 99) %>%
  mutate(ano = as.character(ano)) %>%
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

# Load ejecuciones
ejecu_after <- read_dta(paste0(dnp,"Ejecuciones_after.dta"))
vias_after <- read_dta(paste0(dnp,"Vias_SICEP_after.dta"))
ejecu_before <- read_dta(paste0(dnp,"Ejecuciones_before.dta"))
vias_before <- read_dta(paste0(dnp,"Vias_SICEP_before.dta"))
invias_before <- read_dta(paste0(invias,"invias_before.dta"))


###########################################################################################################
######################################### INVESTMENT: PREMIO ##############################################
################################## Coalition wrt next president  ##########################################
###########################################################################################################

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., president,  by.x = c("year", "codmpio", "coalition"), by.y = c("ano", "codmpio", "coalition"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, pobl_tot, coddepto, ano, year, codpartido_t, win_t, 
                votos_t, votos_t1, starts_with("prop")) %>% 
  filter(is.na(prop_votes_total_t1)==0 & is.na(prop_votes_c2)==0) %>% 
  merge(., ejecu_after,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_after,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# Do coallition maires get more funds after presidential election?   

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot, as.factor(l$ano)),
                c = 0.5,
                all = T,
                vce = "hc1")
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95, 
  )
  return(r)
}

# linear
lm_f <- function(o){
  r <- summary(lm(l[,o] ~ prop_votes_c2 + pobl_tot + as.factor(ano), l))
  return(r)
}


# All periods? or drop 2011 (cambio en  ley regalias)
l <- alcaldes_rd 
# %>% filter(ano != 2011)
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# outcomes
# out <- c("log_A","log_A1000","log_A2000","log_A3010")
 out <- c("log_D","log_D1000", "log_D2000", "log_D3000")
# out <- c("log_B","log_B1000","log_B1010","log_B1020","log_B1030")
# out <- c("log_E","log_E1000","log_E2000")
# out <- c("log_vias","log_f_nac","log_f_regalias", "log_f_trans_nac")


lapply(out, l_f) 
# lapply(out, lm_f) 


###########################################################################################################
###################################### INVESTMENT: Incentivo ##############################################
################################## Coalition wrt before president #########################################
###########################################################################################################

############################
# Reelections only (>2003)

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., president,  by.x = c("year", "codmpio", "coalition"), by.y = c("ano", "codmpio", "coalition"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, pobl_tot, coddepto, ano, year, codpartido_t, win_t, 
                votos_t, votos_t1, starts_with("prop")) %>% 
  filter(is.na(prop_votes_total_t1)==0 & is.na(prop_votes_c2)==0) %>% 
  merge(., ejecu_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# Do coallition maires get more funds after presidential election?   

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot, as.factor(l$ano)),
                c = 0.5,
                all = T,
                vce = "hc1")
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95, 
  )
  return(r)
}

# linear
lm_f <- function(o){
  r <- summary(lm(l[,o] ~ prop_votes_c2 + pobl_tot + as.factor(ano), l))
  return(r)
}

# Reelection only
l <- alcaldes_rd  %>% filter(ano >= 2003) 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
# out <- c("log_A","log_A1000","log_A2000","log_A3010")
 out <- c("log_D","log_D1000", "log_D2000", "log_D3000")
# out <- c("log_B","log_B1000","log_B1010","log_B1020","log_B1030")
# out <- c("log_E","log_E1000","log_E2000")
# out <- c("log_inv","log_vias","log_f_nac","log_f_regalias", "log_f_trans_nac")


lapply(out, l_f) 
# lapply(out, lm_f) 




############################
# Do coallition maires get more funds after presidential election?   

# second round reelection only (Santos)
l <- alcaldes_rd  %>% filter(ano >= 2007) %>%
  mutate(dep = ifelse(tot_dep >= summary(.$tot_dep)[[3]], 1,0)) 
dep <- c(0,1)
table(l$dep)

l_y <- lapply(dep, function(x){
  l %>% filter(dep == x)
}) 

lapply(l_y, function(a){
  rdrobust(y = a$prop_votes_total_t1,
           x = a$prop_votes_c2,
           covs = cbind(a$pobl_tot),
           c = 0.5,
           all = T,
           vce = "hc1")
})
