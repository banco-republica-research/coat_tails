###########################################################################################################
############################################# COAT-TAILS RD ###############################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","broom","gtools","cluster", "rdrobust", "rddensity")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
results <- "Results/RD/"
doc <- "Document/Figures/"

###########################################################################################################
######################################## ELECTIONS DATA ###################################################
###########################################################################################################

# Load maire and coalition data
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

# Load party codes and municipal covariates
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

cede <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))
controls <- cede %>%
  dplyr::select(coddepto, codmpio, municipio, ano, nbi) %>%
  filter(ano == 1993) %>%
  merge(., cede, by.x = c("codmpio"), by.y = c("codmpio"), all = T)

# Load presidential for t+1
win_apellido <- c("PASTRANA", "URIBE", "SANTOS")
win_nom <- c("ANDRES", "ALVARO", "JUAN MANUEL")

president <- readRDS(paste0(res, "presidentes_primera_merge.rds")) %>%
  mutate(coalition = ifelse(primer_apellido %in% win_apellido & nombre %in% win_nom , 1, 0))


###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
############################################### BY PARTY ##################################################
###########################################################################################################

# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  arrange(codmpio, ano, codpartido) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) %>% 
  mutate(year = fct_recode(ano,
                           "1998" = "1997",
                           "2002" = "2000",
                           "2006" = "2003",
                           "2010" = "2007",
                           "2014" = "2011")) %>%  mutate(year = as.character(year)) 

table(alcaldes_merge_r2$ano, alcaldes_merge_r2$year)

# Use the same data base but merge with between party codes (codpartido) instead of coalition
# All parties!!

alcaldes_rd <- alcaldes_merge_r2 %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  group_by(ano, codmpio, codpartido) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  merge(., president,  by.x = c("year", "codmpio", "codpartido"), by.y = c("ano", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>%
  # dplyr::select(codmpio, pobl_tot, coddepto, ano, year, 
  # votos_t, votos_t1, starts_with("prop")) %>% 
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# Second rounds only
l <- alcaldes_rd
l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

party_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
party_des

# outcomes
out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/First_round", "/RD_presfirst_party.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "Vote share (subsequent Election)",
         binselect="es", nbins= 10, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/coat_tails_presfirst_party.rds"))
r


###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################### PRESIDENT FIRST ROUND ###########################################
###########################################################################################################

# coalition FIRST roundS
coalitions_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio,coalition_new) %>%
  unique(.)
table(coalitions_long$ano,coalitions_long$year)

# top2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99)  %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn==2) %>%
  dplyr::select(-c(codep,n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

dim(alcaldes_merge_r2)

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
  # dplyr::select(codmpio, pobl_tot, coddepto.x, ano, year, codpartido_t, win_t, 
  # votos_t, votos_t1, starts_with("prop")) %>% 
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# Second rounds only
l <- alcaldes_rd
l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

first_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
first_des

# outcomes
out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/First_round", "/RD_presfirst_first.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         y.lim = c(0.3, 0.7),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "Vote share (subsequent Election)",
         binselect="es", nbins= 10, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_presfirst_1_coalition.rds"))
r



###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################## CURRENT and FIRST  ###############################################
###########################################################################################################

# Load maire and coalition data
coalitions_long <- readRDS(paste0(res,"coalitions_current_primera.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio, coalition_new, year_first) %>% 
  unique(.)
table(coalitions_long$ano,coalitions_long$year_first)

# top2 and drop municipality if at least one of the top2 is 98 or 99
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>%
  merge(., coalitions_long, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99)  %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn==2) %>%
  dplyr::select(-c(codep,n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

table(alcaldes_merge_r2$ano, alcaldes_merge_r2$year_first)
table(president$ano)

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>%
  merge(., president,  by.x = c("year_first", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  # dplyr::select(codmpio, pobl_tot, coddepto.x, ano, year, codpartido_t, win_t,
  # votos_t, votos_t1, starts_with("prop")) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions

# Second rounds only
l <- alcaldes_rd
l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

current_final_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
current_final_des

# outcomes
out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/First_round", "/RD_presfirst_current.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "Vote share (subsequent Election)",
         binselect="es", nbins= 10, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)

  dens <- rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0)
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_presfirst_current_first_coalition.rds"))
r



###########################################################################################################
####################################### RD: TOTAL OBSERVATIONS ############################################
###########################################################################################################


des <- rbind(first_des, current_final_des, party_des)
des

