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
alcaldes_t1 <-  readRDS(paste0(res,"alcaldes_t1.rds"))
alcaldes_t1_coalition <-  readRDS(paste0(res,"alcaldes_t1_coalition.rds"))

cede <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))
controls <- cede %>%
  dplyr::select(coddepto, codmpio, municipio, ano, nbi) %>%
  filter(ano == 1993) %>%
  merge(., cede, by.x = c("codmpio"), by.y = c("codmpio"), all = T)


##############################################################################
# AS ALCALDES_T1_COALITION IS A LIST, IS BETTER TO SPLIT IT AND ARRANGE THE  #
# RESULTS BY INDIVIDUAL OBJECTS. THIS IN NOT A GOOD WAY TO CODE IT, BUT CAN  #
# RESULT IN READBILITY FOR US                                                #
##############################################################################

names <- c("primera", "segunda", "final")

lapply(seq_along(alcaldes_t1_coalition), 
       function(x) {
         assign(names[x], alcaldes_t1_coalition[[x]], envir=.GlobalEnv)
       }
)



# Elections at t
# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  arrange(codmpio, ano, codpartido) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - ALL PARTIES #########################################
###########################################################################################################

alcaldes_rd_all <- alcaldes_merge_r2 %>%
  # mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  group_by(ano, codmpio, codpartido) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  merge(alcaldes_t1,  by.x = c("ano", "codmpio","codpartido"), by.y = c("ano_lag", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x=T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_b_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  # dplyr::select(codmpio,pobl_tot, coddepto,  ano, codpartido, win_t, rank_t, votos_t, prop_votes_c2,
                # run_t1, rank_t1 , votos_t1, prop_votes_cand_t1, prop_votes_total_t1,prop_votes_total_b_t1) %>%
  arrange(codmpio, ano)
  # mutate(win_t1 = ifelse(is.na(rank_t1) == 1 | rank_t1 != 1, 0, 1)) 

l <- alcaldes_rd_all 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0.5,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency", "/RD_", o, "party", ".pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5,
         # y.lim = c(1, 7),
         title = " ",
         x.label = "Vote margin at t",
         y.label = "Vote share at t + 1",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95 
  )
  dev.off()
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
############################################## FIRST COALITION ############################################
###########################################################################################################

coalitions_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 

# Elections at t
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
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 


alcaldes_rd_c <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  # mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., primera,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_t1", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  filter(is.na(prop_votes_total_t1) == F & is.na(prop_votes_c2) == F, prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

table(alcaldes_rd_c$coalition_new)

l <- alcaldes_rd_c
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)
out <- c("prop_votes_total_t1")
# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0.5,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency", "/RD_", o, "1_coalition", ".pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5,
         y.lim = c(0.2, 0.8),
         x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Vote margin at t",
         y.label = "Vote share at t + 1",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(prop_votes_c2 <= 0.5 + r$bws[1] &
                         prop_votes_c2 >= 0.5 - r$bws[1])
  mean <- mean(l[,out], na.rm = T)
  return(list(rd = r, mean = mean))
}

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/incumbency1_coalition.rds"))

###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
############################################# SECOND COALITION ############################################
###########################################################################################################

coalitions_long <- readRDS(paste0(res,"coalitions_segunda_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 

# Elections at t
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
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 


alcaldes_rd_c <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  # mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., segunda,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_t1", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  filter(is.na(prop_votes_total_t1) == F & is.na(prop_votes_c2) == F, prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

table(alcaldes_rd_c$coalition_new)

l <- alcaldes_rd_c
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)
out <- c("prop_votes_total_t1")
# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0.5,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency", "/RD_", o, "2_coalition", ".pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5,
         y.lim = c(0.2, 0.8),
         x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Vote margin at t",
         y.label = "Vote share at t + 1",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(prop_votes_c2 <= 0.5 + r$bws[1] &
                         prop_votes_c2 >= 0.5 - r$bws[1])
  mean <- mean(l[,out], na.rm = T)
  return(list(rd = r, mean = mean))
}

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/incumbency2_coalition.rds"))

###########################################################################################################
############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
############################################## FINAL COALITION ############################################
###########################################################################################################

coalitions_long <- readRDS(paste0(res,"coalitions_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 

# Elections at t
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
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 


alcaldes_rd_c <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  # mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., final,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_t1", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  filter(is.na(prop_votes_total_t1) == F & is.na(prop_votes_c2) == F, prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

table(alcaldes_rd_c$coalition_new)

l <- alcaldes_rd_c
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)
out <- c("prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0.5,
                all = T,
                vce = "nn")
  pdf(str_c(results, "/Graphs/Incumbency", "/RD_", o, "final", ".pdf"), height=6, width=12)
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5,
         y.lim = c(0.2, 0.8),
         x.lim = c(0.45, 0.55),
         title = " ",
         x.label = "Vote margin at t",
         y.label = "Vote share at t + 1",
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95
  )
  dev.off()
  mean <- l %>% filter(prop_votes_c2 <= 0.5 + r$bws[1] &
                         prop_votes_c2 >= 0.5 - r$bws[1])
  mean <- mean(l[,out], na.rm = T)
  return(list(rd = r, mean = mean))
}

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/incumbency_final_coalition.rds"))


# # RD: Vote share conditional on running
# l_f <- function(c){
#   lc <- l %>% filter(coalition_new == c)
#   # lc2 <- lc %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)
#   r <- rdrobust(y = lc$prop_cotes_total.y,
#             x = lc$prop_votes_c2,
#             # covs = cbind(as.factor(lc$codpartido), as.factor(lc$ano), lc$pobl_tot, as.factor(lc$coddepto)),
#             c = 0.5,
#             all = T)
#   rdplot(y=lc2$win_t1, x=lc2$prop_votes_c2, c = 0.5,
#          binselect="es", nbins= 15, kernel="triangular", p=3, ci=95)
#   return(r)
#   }
# 
# coal <- c(0,1)
# lapply(coal,l_f) 


# ###########################################################################################################
# ############################# RD: IMCUMBENCY EFFECT - COALITION PARTIES ###################################
# ############################################# SECOND COALITION ############################################
# ###########################################################################################################
# 
# coalitions_long <- readRDS(paste0(res,"coalitions_segunda_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 
# 
# # Elections at t
# # Top 2 and drop municipality if at least one of the top2 is 98 or 99 
# alcaldes_merge_r2 <- alcaldes_merge %>% 
#   filter(ano != 2015) %>%
#   filter(rank <= 2) %>% 
#   merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
#   arrange(codmpio, ano, codpartido) %>%
#   filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
#   mutate(ano = as.character(ano)) %>%
#   group_by(codmpio, ano) %>%
#   mutate(n = 1, nn = sum(n)) %>%
#   filter(nn == 2) %>%
#   dplyr::select(-c(n,nn)) %>%
#   merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 
# 
# 
# alcaldes_rd_c <- alcaldes_merge_r2 %>%
#   filter(coalition_new == 1) %>%
#   group_by(ano, codmpio) %>%
#   mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
#   filter(party_2 == 1) %>%
#   # mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
#   merge(., segunda,  by.x = c("ano", "codmpio", "coalition_new"), by.y = c("ano_t1", "codmpio", "coalition_new"), 
#         suffixes = c("_t", "_t1"), all.x = T) %>%
#   filter(is.na(prop_votes_total_t1) == F & is.na(prop_votes_c2) == F, prop_votes_c2 != 0.5) %>%
#   arrange(codmpio, ano)
# 
# table(alcaldes_rd_c$coalition_new)
# 
# l <- alcaldes_rd_c
# l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)
# out <- c("prop_votes_total_t1")
# # Regressions for list of outcomes
# l_f <- function(o){
#   r <- rdrobust(y = l[,o],
#                 x = l$prop_votes_c2,
#                 covs = cbind(as.factor(l$codpartido), as.factor(l$ano), l$pobl_tot, as.factor(l$coddepto)),
#                 c = 0.5,
#                 all = T)
#   rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
#          binselect="es", nbins= 15, kernel="triangular", p=3, ci=95
#   )
#   mean <- l %>% filter(prop_votes_c2 <= 0.5 + r$bws[1] &
#                          prop_votes_c2 >= 0.5 - r$bws[1])
#   mean <- mean(l[,out], na.rm = T)
#   return(list(rd = r, mean = mean)) 
# }
# lapply(out, l_f)
# 

# # RD: Vote share conditional on running
# l_f <- function(c){
#   lc <- l %>% filter(coalition_new == c)
#   # lc2 <- lc %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)
#   r <- rdrobust(y = lc$prop_cotes_total.y,
#             x = lc$prop_votes_c2,
#             # covs = cbind(as.factor(lc$codpartido), as.factor(lc$ano), lc$pobl_tot, as.factor(lc$coddepto)),
#             c = 0.5,
#             all = T)
#   rdplot(y=lc2$win_t1, x=lc2$prop_votes_c2, c = 0.5,
#          binselect="es", nbins= 15, kernel="triangular", p=3, ci=95)
#   return(r)
#   }
# 
# coal <- c(0,1)
# lapply(coal,l_f) 

