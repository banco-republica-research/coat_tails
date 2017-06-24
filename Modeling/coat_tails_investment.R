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
dnp <- "Data/DNP/Ejecuciones/"
invias <- "Data/invias/"
results <- "Results/RD"

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

# Load ejecuciones
ejecu_all <- read_dta(paste0(dnp,"Ejecuciones_all.dta"))
ejecu_after <- read_dta(paste0(dnp,"Ejecuciones_after.dta"))
ejecu_before <- read_dta(paste0(dnp,"Ejecuciones_before.dta"))
vias_all <- read_dta(paste0(dnp,"Vias_SICEP_all.dta"))
vias_after <- read_dta(paste0(dnp,"Vias_SICEP_after.dta"))
vias_before <- read_dta(paste0(dnp,"Vias_SICEP_before.dta"))
invias_all <- read_dta(paste0(invias,"invias_all.dta"))
invias_after <- read_dta(paste0(invias,"invias_after.dta"))
invias_before <- read_dta(paste0(invias,"invias_before.dta"))

# out_inv <- c("log_D_pc","log_D1000_pc", "log_D2000_pc", "log_D3000_pc")
out_inv <- c("log_D","log_D1000", "log_D2000", "log_D3000")

# out_road <- c("log_vias_pc", "log_f_SGPp_pc","log_f_regalias_pc", "log_f_trans_nac_pc")
out_road <- c("log_vias","log_f_SGPp","log_f_regalias", "log_f_trans_nac")

###########################################################################################################
##################################### INVESTMENT: TOTAL term ##############################################
############################### Coalition wrt CURRENT president  ##########################################
########################################## Could be 0 #####################################################
###########################################################################################################

# Note: maires elected in 1997 begin in 01/01/1998 which is a presidential election year. Not enough overlapping with current
# Not enough observations for 2015 maires either 

# FINAL round CURRENT coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new) %>%
  unique(.)


# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(cand==1) %>%
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
  merge(., ejecu_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# Select period: 
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Investment", "/RD_", type, "_",o, "_total_current.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "log(per capita investment)",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}


r <- lapply(out_inv, l_f, type = "investment") 
saveRDS(r, str_c(results, "/investment_total_current.rds"))
r

r <- lapply(out_road, l_f,  type = "roads") 
saveRDS(r, str_c(results, "/roads_total_current.rds"))
r


############################################# Sin 2011 ####################################################

# Select periods
l <- alcaldes_rd %>% filter(ano!=2011)
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out_inv, l_f, type = "investment") 
saveRDS(r, str_c(results, "/investment_total_s2011_current.rds"))
r

r <- lapply(out_road, l_f,  type = "roads") 
saveRDS(r, str_c(results, "/roads_total_s2011_current.rds"))
r



###########################################################################################################
######################################## INVESTMENT: TOTAL ################################################
################################## Coalition wrt NEXT president  ##########################################
########################################### could be + ####################################################
###########################################################################################################

# FINAL round NEXT coalition
coalitions_long <- readRDS(paste0(res,"coalitions_new.rds"))  %>% 
  dplyr::select(codpartido,ano, codmpio, coalition_new) %>%
  unique(.)

# top2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
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

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., ejecu_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# Do coallition maires get more funds after presidential election?   

# Select periods
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Investment", "/RD_", type, "_",o, "_total_next", ".pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "log(per capita investment)",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}


r <- lapply(out_inv, l_f, type = "investment") 
saveRDS(r, str_c(results, "/investment_total_next.rds"))
r

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_total_next.rds"))
r


###########################################################################################################
################################### INVESTMENT: TOTAL #####################################################
################################ CURRENT and INCOMING president ###########################################
######################################### Should be + #####################################################
###########################################################################################################

# Note: maires elected in 1997 begin in 01/01/1998 which is a presidential election year. Not enough overlapping with current
# Not enough observations for 2015 maires either 

# FINAL round CURRENT coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current_final.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new) %>% 
  unique(.)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(cand==1) %>%
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
  dplyr::select(codmpio, pobl_tot, nbi.x, discapital, disbogota, altura, coddepto.x, ano, codpartido, win_t, 
                votos, starts_with("prop"), margin_prop_2) %>% 
  merge(., ejecu_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# RD and OLS regressions 

# Select period: Drop 2011 given the coalition change during Santos I
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Investment", "/RD_", type, "_",o, "_total_curnext.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "log(per capita investment)",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}


# outcomes

r <- lapply(out_inv, l_f, type = "investment")
saveRDS(r, str_c(results, "/investment_total_curnext.rds"))
r

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_total_curnext.rds"))
r


############################################# Sin 2011 ####################################################

# Select periods
l <- alcaldes_rd %>% filter(ano!=2011)
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out_inv, l_f, type = "investment")
saveRDS(r, str_c(results, "/investment_total_s2011_curnext.rds"))
r

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_total_s2011_curnext.rds"))
r

###########################################################################################################
################################### INVESTMENT: BEFORE  ###################################################
################################ CURRENT and INCOMING president ###########################################
######################################### Should be + #####################################################
###########################################################################################################

# Note: maires elected in 1997 begin in 01/01/1998 which is a presidential election year. Not enough overlapping with current
# Not enough observations for 2015 maires either 

# FINAL round CURRENT coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current_final.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new) %>% 
  unique(.)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(cand==1) %>%
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
  dplyr::select(codmpio, pobl_tot, nbi.x, discapital, disbogota, altura, coddepto.x, ano, codpartido, win_t, 
                votos, starts_with("prop"), margin_prop_2) %>% 
  merge(., ejecu_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# RD and OLS regressions 

# Select period: Drop 2011 given the coalition change during Santos I
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Investment", "/RD_", type, "_",o, "_before_curnext.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "log(per capita investment)",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}


# outcomes

r <- lapply(out_inv, l_f, type = "investment")
saveRDS(r, str_c(results, "/investment_before_curnext.rds"))
r

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_before_curnext.rds"))
r


############################################# Sin 2011 ####################################################

# Select periods
l <- alcaldes_rd %>% filter(ano!=2011)
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out_inv, l_f, type = "investment")
saveRDS(r, str_c(results, "/investment_before_s2011_curnext.rds"))
r

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_before_s2011_curnext.rds"))
r



###########################################################################################################
################################### INVESTMENT: BEFORE  ###################################################
################################ Coalition wrt CURRENT president ##########################################
######################################### Should be + #####################################################
###########################################################################################################

# Note: maires elected in 1997 begin in 01/01/1998 which is a presidential election year. Not enough overlapping with current
# Not enough observations for 2015 maires either 

# FINAL round CURRENT coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new)  %>% 
  unique(.)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(cand==1) %>%
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
  dplyr::select(codmpio, pobl_tot, nbi.x, discapital, disbogota, altura, coddepto.x, ano, codpartido, win_t, 
                votos, starts_with("prop"), margin_prop_2) %>% 
  merge(., ejecu_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# RD and OLS regressions 

# Select period: Drop 2011 given the coalition change during Santos I
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Investment", "/RD_", type, "_",o, "_before_current.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "log(per capita investment)",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}


# outcomes

r <- lapply(out_inv, l_f, type = "investment")
saveRDS(r, str_c(results, "/investment_before_current.rds"))
r
r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_before_current.rds"))
r




###########################################################################################################
################################### INVESTMENT: AFTER #####################################################
################################ Coalition wrt CURRENT president ##########################################
######################################### Should be 0 #####################################################
###########################################################################################################

# Note: maires elected in 1997 begin in 01/01/1998 which is a presidential election year. Not enough overlapping with current
# Not enough observations for 2015 maires either 

# FINAL round CURRENT coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new) %>% 
  unique(.)


# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(cand==1) %>%
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
  merge(., ejecu_after,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_after,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_after,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# RD and OLS regressions 

# Select period 
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Investment", "/RD_", type, "_",o, "_after_current", ".pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "log(per capita investment)",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out_inv, l_f, type = "investment") 
saveRDS(r, str_c(results, "/investment_after_current.rds"))
r 

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_after_current.rds"))
r



###########################################################################################################
######################################## INVESTMENT: BEFORE ###############################################
################################## Coalition wrt NEXT president  ##########################################
########################################### should be 0 ###################################################
###########################################################################################################

# FINAL round NEXT coalition
coalitions_long <- readRDS(paste0(res,"coalitions_new.rds"))  %>% 
  dplyr::select(codpartido,ano, codmpio, coalition_new) %>%
  unique(.)

# top2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
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

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., ejecu_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_before,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# Do coallition maires get more funds after presidential election?   

# Select periods
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Investment", "/RD_", type, "_",o, "_before_next", ".pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "log(per capita investment)",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}


r <- lapply(out_inv, l_f, type = "investment") 
saveRDS(r, str_c(results, "/investment_before_next.rds"))
r

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_before_next.rds"))
r




############################################# Sin 2011 ####################################################

# Select periods
l <- alcaldes_rd %>% filter(ano!=2011)
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out_inv, l_f, type = "investment") 
saveRDS(r, str_c(results, "/investment_before_s2011_next.rds"))
r 

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_before_s2011_next.rds"))
r

###########################################################################################################
##################################### INVESTMENT: AFTER (PREMIO) ##########################################
################################## Coalition wrt NEXT president  ##########################################
########################################### Could be + ####################################################
###########################################################################################################

# FINAL round NEXT coalition
coalitions_long <- readRDS(paste0(res,"coalitions_new.rds"))  %>% 
  dplyr::select(codpartido,ano, codmpio, coalition_new) %>%
  unique(.)

# top2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
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

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., ejecu_after,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_after,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_after,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)


############################
# Do coallition maires get more funds after presidential election?   

# Select periods
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Investment", "/RD_", type, "_",o, "_after_next.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "log(per capita investment)",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out_inv, l_f, type = "investment") 
saveRDS(r, str_c(results, "/investment_after_next.rds"))
r 

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_after_next.rds"))
r

############################################# Sin 2011 ####################################################

# Select periods
l <- alcaldes_rd %>% filter(ano!=2011)
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o, type){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out_inv, l_f, type = "investment") 
saveRDS(r, str_c(results, "/investment_after_s2011_next.rds"))
r 

r <- lapply(out_road, l_f, type = "roads") 
saveRDS(r, str_c(results, "/roads_after_s2011_next.rds"))
r

