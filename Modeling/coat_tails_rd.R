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
dnp <-"Data/DNP/Ejecuciones/"


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
ejecu_mean <- read_dta(paste0(dnp,"Ejecuciones_coat_mean.dta"))
ejecu_dep <- read_dta(paste0(dnp,"Ejecuciones_coat_dep.dta"))
# ejecu_before1 <- read_dta(paste0(dnp,"Ejecuciones_coat_before1.dta"))
# ejecu_after1 <- read_dta(paste0(dnp,"Ejecuciones_coat_after1.dta"))


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
  merge(., ejecu_dep,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  mutate(eje_dep1 = ifelse(eje_dep >= 0.8, 1,0)) %>%
  merge(., ejecu_mean,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  dplyr::select(codmpio, pobl_tot, coddepto, ano, year, codpartido_t, win_t, 
                votos_t, votos_t1, starts_with("prop"), starts_with("eje_")) %>% 
  filter(is.na(prop_votes_total_t1)==0 & is.na(prop_votes_c2)==0) %>%
  arrange(codmpio, ano)

# Second rounds only
l <- alcaldes_rd %>% filter(ano != 2000 & ano != 2003) 
dim(l)
# hist(l$prop_votes_c2)

# %>% filter(prop_votes_c2 <= 0.5 + sd(prop_votes_c2) * 1.96 & prop_votes_c2 >= 0.5 - sd(prop_votes_c2) * 1.96)
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


############################
# RD and OLS regressions 


# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$prop_votes_c2,
                covs = cbind(as.factor(l$ano), l$pobl_tot, as.factor(l$coddepto)),
                c = 0.5,
                all = T,
                vce = "hc1")
  rdplot(y=l2[,o], x=l2$prop_votes_c2, c = 0.5, 
         binselect="es", nbins= 15, kernel="triangular", p=3, ci=95, 
  )
  return(r)
}

# out <- c("eje_A1000","eje_A1010","eje_A1020","eje_B","eje_B1000","eje_B1010","eje_B1020","eje_B1030","eje_E1000","eje_E2000","eje_D1000", "eje_D2000", "eje_D3000", "prop_votes_total_t1")
out <- c("prop_votes_total_t1")

lapply(out, l_f) 


############################
# RD and OLS regressions by national transfers dependency

dep <- c(0,1)
l_y <- lapply(dep, function(x){
  l %>% filter(eje_dep1 == x)
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










###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################### PRESIDENT FIRST ROUND ###########################################
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

# RD and OLS regressions on restricted sample
l <- alcaldes_rd_n %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) 
# %>% filter(prop_votes_c2 <= 0.5 + sd(prop_votes_c2) * 1.96 & prop_votes_c2 >= 0.5 - sd(prop_votes_c2) * 1.96)

dim(l)
hist(l$prop_votes_c2)
table(l$codpartido,l$ano)

# RD and OLS regressions on restricted sample
rdrobust(y = l$prop_votes_total_t1,
              x = l$prop_votes_c2,
              covs = cbind(as.factor(l$ano),l$pobl_tot, as.factor(l$coddepto),as.factor(alcaldes_rd_n$codpartido)),
              c = 0.5,
              all = T,
              vce = "hc1")




