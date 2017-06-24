###########################################################################################################
############################################# INCUMBENCY RD ###############################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust","rddensity", "knitr")
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
############################################# LOAD DATA ###################################################
###########################################################################################################

# load all datasets
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))
alcaldes_t1 <-  readRDS(paste0(res,"alcaldes_t1.rds"))
alcaldes_t1_coalition <-  readRDS(paste0(res,"alcaldes_t1_coalition.rds"))
alcaldes_t1_coalition_current <- readRDS(paste0(res,"alcaldes_t1_coalition_current.rds"))


cede <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))
controls <- cede %>%
  dplyr::select(coddepto, codmpio, municipio, ano, nbi) %>%
  filter(ano == 1993) %>%
  merge(., cede, by.x = c("codmpio"), by.y = c("codmpio"), all = T)

# READ ALCALDES_T1_COALITION WHICH IS A LIST 
names <- c("primera", "segunda", "final")

lapply(seq_along(alcaldes_t1_coalition), 
       function(x) {
         assign(names[x], alcaldes_t1_coalition[[x]], envir=.GlobalEnv)
       }
  )

# READ ALCALDES_T1_COALITION CURRENT WHICH IS A LIST 
names <- c("current", "current_primera", "current_segunda", "current_final")

lapply(seq_along(alcaldes_t1_coalition_current), 
       function(x) {
         assign(names[x], alcaldes_t1_coalition_current[[x]], envir=.GlobalEnv)
       }
)

###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - ALL PARTIES #########################################
###########################################################################################################

# Elections at t
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
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

alcaldes_rd_all <- alcaldes_merge_r2 %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  group_by(ano, codmpio, codpartido) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  merge(alcaldes_t1,  by.x = c("ano", "codmpio","codpartido"), by.y = c("ano_lag", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x=T) %>%
  filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  # dplyr::select(codmpio,pobl_tot, coddepto,  ano, codpartido, win_t, rank_t, votos_t, prop_votes_c2,
                # run_t1, rank_t1 , votos_t1, prop_votes_cand_t1, prop_votes_total_t1,prop_votes_total_b_t1) %>%
  arrange(codmpio, ano)
  # %>% mutate(win_t1 = ifelse(is.na(rank_t1) == 1 | rank_t1 != 1, 0, 1)) 

l <- alcaldes_rd_all 
l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

party_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
party_des


# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency/RD_incumbency_party.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "Vote share (subsequent Election)",
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


out <- c("prop_votes_total_t1")
# other <- c("win_t1","prop_votes_total_b_t1","run_t1")
r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/incumbency_party.rds"))
r


###########################################################################################################
######################### RD: IMCUMBENCY EFFECT - TRADITIONAL PARTIES #####################################
###########################################################################################################

# Function
l_f <- function(o){
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

#################
# Traditional

l <- alcaldes_rd_all %>% filter(codpartido == 1 | codpartido == 2) 
l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

party2_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
party2_des

out <- c("prop_votes_total_t1")
# other <- c("win_t1","prop_votes_total_b_t1","run_t1")
r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/incumbency_party2.rds"))
r

#################
# Non Traditional

l <- alcaldes_rd_all %>% filter(codpartido != 1 & codpartido != 2) 
l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

party2n_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
party2n_des

out <- c("prop_votes_total_t1")
# other <- c("win_t1","prop_votes_total_b_t1","run_t1")
r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/incumbency_party2n.rds"))
r



###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
############################################## FIRST COALITION ############################################
###########################################################################################################

coalitions_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio, coalition_new) %>%
  unique(.)

# Elections at t
# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 


alcaldes_rd_c <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., primera,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_lag", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  #filter(is.na(prop_votes_total_t1) == F) %>%
  filter(is.na(prop_votes_c2) == F | prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

table(alcaldes_rd_c$coalition_new)

l <- alcaldes_rd_c
l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

first_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t))) 
first_des


out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency", "/RD_incumbency_first.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "Vote share (subsequent Election)",
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

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/incumbency_1_coalition.rds"))
r 

###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
############################################# SECOND COALITION ############################################
###########################################################################################################

coalitions_long <- readRDS(paste0(res,"coalitions_segunda_new.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio, coalition_new) %>%
  unique(.)

# Elections at t
# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 


alcaldes_rd_c <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., segunda,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_lag", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  #filter(is.na(prop_votes_total_t1) == F) %>%
  filter(is.na(prop_votes_c2) == F | prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

table(alcaldes_rd_c$coalition_new)

l <- alcaldes_rd_c
l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

second_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t))) 
second_des


out <- c("prop_votes_total_t1")
# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency", "/RD_incumbency_second.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "Vote share (subsequent Election)",
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

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/incumbency_2_coalition.rds"))
r 

###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
############################################## FINAL COALITION ############################################
###########################################################################################################

coalitions_long <- readRDS(paste0(res,"coalitions_new.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio, coalition_new) %>%
  unique(.)

# Elections at t
# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 


alcaldes_rd_c <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., final,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_lag", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  #filter(is.na(prop_votes_total_t1) == F) %>%
  filter(is.na(prop_votes_c2) == F | prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

table(alcaldes_rd_c$coalition_new)

l <- alcaldes_rd_c
l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

final_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
final_des


out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency", "/RD_incumbency_final.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.0, 0.4),
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
saveRDS(r, str_c(results, "/incumbency_final_coalition.rds"))
r

# Graph doc
pdf(str_c(doc, "/RD_incumbency_final.pdf"), height=6, width=12)
rdplot(y=l2$prop_votes_total_t1, x=l2$margin_prop_2, c = 0,
       y.lim = c(0.1, 0.4),
       # x.lim = c(0.45, 0.55),
       title = " ",
       x.label = "Victory Margin",
       y.label = "Vote share (subsequent Election)",
       binselect="es", nbins= 10, kernel="triangular", p=3, ci=95
)
dev.off()




###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
############################################ CURRENT COALITION ############################################
###########################################################################################################

# Only for observations: Regression is not interpretable. 

coalitions_long <- readRDS(paste0(res,"coalitions_current.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio,coalition_new) %>% 
  unique(.)

table(coalitions_long$ano, coalitions_long$year)


# Elections at t
# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>%  
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

# Election t+1 
table(alcaldes_merge_r2$ano, alcaldes_merge_r2$year)
table(final$ano_lag, final$ano_t1)

alcaldes_rd_c <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., current,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_lag", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  arrange(codmpio, ano)

table(alcaldes_rd_c$coalition_new)

l <- alcaldes_rd_c

l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

current_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
current_des



###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
#################################### CURRENT and FIRST COALITION ##########################################
###########################################################################################################

coalitions_long <- readRDS(paste0(res,"coalitions_current_primera.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new) %>%
  unique(.)


# Elections at t
# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>%  
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

# Election t+1 
table(alcaldes_merge_r2$ano, alcaldes_merge_r2$year_current)
table(final$ano_lag, final$ano_t1)

alcaldes_rd_c <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., current_primera,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_lag", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  #filter(is.na(prop_votes_total_t1) == F) %>%
  filter(is.na(prop_votes_c2) == F | prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

table(alcaldes_rd_c$coalition_new)

l <- alcaldes_rd_c

l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

current_primera_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
current_primera_des

out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency", "/RD_incumbency_current_primera.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "Vote share (subsequent Election)",
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


r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/incumbency_current_primera_coalition.rds"))
r


###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
################################### CURRENT and FINAL COALITION ###########################################
###########################################################################################################

coalitions_long <- readRDS(paste0(res,"coalitions_current_final.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new, year_current, year_final) %>%
  unique(.)

# Elections at t
# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>%  
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

# Election t+1 
table(alcaldes_merge_r2$ano, alcaldes_merge_r2$year_current)
table(final$ano_lag, final$ano_t1)

alcaldes_rd_c <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., current_final,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_lag", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  #filter(is.na(prop_votes_total_t1) == F) %>%
  filter(is.na(prop_votes_c2) == F | prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

table(alcaldes_rd_c$coalition_new)

l <- alcaldes_rd_c

l2 <- l %>% filter(margin_prop_2 <= 0.2 & margin_prop_2 >= -0.2)
l1 <- l %>% filter(margin_prop_2 <= 0.1 & margin_prop_2 >= -0.1)

current_final_des <- cbind(t(table(l$win_t)),t(table(l2$win_t)),t(table(l1$win_t)))
current_final_des

out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency", "/RD_incumbency_current_final.pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$margin_prop_2, c = 0,
         # y.lim = c(0.2, 0.8),
         # x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Victory Margin",
         y.label = "Vote share (subsequent Election)",
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


r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/incumbency_current_final_coalition.rds"))
r







###########################################################################################################
####################################### RD: TOTAL OBSERVATIONS ############################################
###########################################################################################################


des <- rbind(first_des,final_des, current_des, current_primera_des, current_final_des, party_des)
des
kable(des, format = "latex")







