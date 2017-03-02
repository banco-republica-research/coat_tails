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
ejecu_dep <- read_dta(paste0(dnp,"Ejecuciones_coat_dep.dta"))
vias_dep <- read_dta(paste0(dnp,"Vias_SICEP_dep.dta"))



###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################### PRESIDENT SECOND ROUND ##########################################
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
  merge(., ejecu_dep,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_dep,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano)

   
############################
# RD and OLS regressions 

# Second rounds only
l <- alcaldes_rd %>% filter(ano != 2000 & ano != 2003) 
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
                vce = "hc1")
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 14, kernel="triangular", p=3, ci=95, 
  )
  return(r)
}

lapply(out, l_f) 

# linear
lm_f <- function(o){
  r <- summary(lm(l[,o] ~ prop_votes_c2 + pobl_tot + as.factor(ano) + as.factor(coddepto), l))
  return(r)
}

lapply(out, lm_f) 


############################
# RD and OLS regressions by subgroup: national transfers dependency

l <- alcaldes_rd %>% filter(ano != 2000 & ano != 2003) %>% 
  mutate(dep = ifelse(vias_dep2 >= summary(.$vias_dep2)[[3]], 1,0)) 
dep <- c(0,1)

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

############################
# RD and OLS regressions by year (incluiding Uribe: first round)

l <- alcaldes_rd 

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
           vce = "hc1")
})


############################
# RD and OLS regressions by president (incluiding Uribe: first round)

l_y <- list()
l_y[[1]] <- alcaldes_rd %>% filter(ano == 1997)
l_y[[2]] <- alcaldes_rd %>% filter(ano == 2000 | ano == 2003)
l_y[[3]] <- alcaldes_rd %>% filter(ano == 2007 | ano == 2011)

lapply(l_y, function(a){
  rdrobust(y = a$prop_votes_total_t1,
           x = a$prop_votes_c2,
           covs = cbind(a$pobl_tot),
           c = 0.5,
           all = T,
           vce = "hc1")
})


l2 <- l_y[[1]] %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

rdplot(y=l2$prop_votes_total_t1, x=l2$prop_votes_c2, c = 0.5, 
       binselect="es", nbins= 12, kernel="triangular", p=2, ci=95, 
)


############################
# RD and OLS regressions: Santos II (vias para la prosperidad)

# Second rounds only
l <- alcaldes_rd  %>% filter(ano == 2011) 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# out <- c("log_A1000","log_A1010","log_A1020","log_B","log_B1000","log_B1010","log_B1020","log_B1030","log_E1000","log_E2000","log_D1000", "log_D2000", "log_D3000", "prop_votes_total_t1")
# out <- c("log_E1000","log_E2000","log_D1000", "log_D2000", "log_D3000")
# out <- c("log_vias","log_f_nac2","log_f_nac3","prop_votes_total_t1")
 out <- c("log_vias_ter","prop_votes_total_t1")

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(l$pobl_tot),
                c = 0.5,
                all = T,
                vce = "hc1")
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 15, kernel="triangular", p=3, ci=95, 
  )
  return(r)
}

lapply(out, l_f) 


# linear
lm_f <- function(o){
  r <- summary(lm(l[,o] ~ prop_votes_c2 + pobl_tot, l))
  return(r)
}

lapply(out, lm_f) 




###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
############################################ miscellaneous ################################################
###########################################################################################################

############################
# ggplot RD 
g <- l %>%
  mutate(bin = cut(prop_votes_c2, breaks = seq(0.3, 0.7, 0.001), include.lowest = T)) %>%
  group_by(bin) %>%
  summarize(mean_bin = mean(prop_votes_total_t1), sd_bin = sd(prop_votes_total_t1), n = length(codmpio)) %>%
  .[complete.cases(.),] %>%
  as.data.frame() %>%
  mutate(treatment = ifelse(as.numeric(row.names(.)) >= 172, 1, 0), bins = row.names(.)) %>%
  mutate(bins = mapvalues(.$bins, from = c(1:347), to = seq(0.329, 0.675, 0.001)))

#RD Graph 
p <- ggplot(g, aes(y = mean_bin, x = as.numeric(bins), colour = as.factor(treatment)))
p <- p + geom_point(colour = "black", size = 1)
p <- p + stat_smooth(data = alcaldes_rd, aes(x = prop_votes_c2, y = prop_votes_total_t1, 
                                             colour = as.factor(win_t)), method = "loess", level = 0.9) 
p <- p + scale_x_continuous(limits = c(0.45, 0.55))
# p <- p + coord_cartesian(xlim = c(0.45, 0.55))
p