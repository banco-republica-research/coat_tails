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

###########################################################################################################
############################################# LOAD DATA ###################################################
###########################################################################################################

# Load maire top2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  arrange(codmpio, ano, codpartido) %>%
  filter(coalition != 98 & coalition != 99) %>% 
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>% 
  filter(nn==2) %>% 
  dplyr::select(-c(n,nn)) 


dim(alcaldes_merge_r2)

# Load party codes and municipal covariates
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))
controls <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))

# Load presidential for t+1
win_apellido <- c("PASTRANA", "URIBE", "SANTOS")
win_nom <- c("ANDRES", "ALVARO", "JUAN MANUEL")

president <- readRDS(paste0(res, "presidentes_segunda_merge.rds")) %>%
  mutate(coalition = ifelse(primer_apellido %in% win_apellido & nombre %in% win_nom , 1, 0))

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
  dplyr::select(codmpio, ano, year, codpartido_t, win_t, 
                votos_t, votos_t1, starts_with("prop")) %>%
  arrange(codmpio, ano)

# RD and OLS regressions on restricted sample
l <- alcaldes_rd %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T)
# %>% filter(prop_votes_c2 <= 0.5 + sd(prop_votes_c2) * 1.96 & prop_votes_c2 >= 0.5 - sd(prop_votes_c2) * 1.96)

dim(l)
hist(l$prop_votes_c2)

a <- rdrobust(y = l$prop_votes_total_t1,
              x = l$prop_votes_c2,
              covs = cbind(as.factor(l$ano), l$pobl_tot, as.factor(l$coddepto)),
              c = 0.5,
              all = T,
              vce = "nn", p=2
              )
a


# Arrange data by bins and eliminate outliers
l <- l %>%
  mutate(bin = cut(prop_votes_c2, breaks = seq(0.3, 0.7, 0.001), include.lowest = T)) %>%
  group_by(bin) %>%
  summarize(mean_bin = mean(prop_votes_total_t1), sd_bin = sd(prop_votes_total_t1), n = length(codmpio)) %>%
  .[complete.cases(.),] %>%
  as.data.frame() %>%
  mutate(treatment = ifelse(as.numeric(row.names(.)) >= 172, 1, 0), bins = row.names(.)) %>%
  mutate(bins = mapvalues(.$bins, from = c(1:347), to = seq(0.329, 0.675, 0.001)))

#RD Graph 
p <- ggplot(l, aes(y = mean_bin, x = as.numeric(bins), colour = as.factor(treatment)))
p <- p + geom_point(colour = "black", size = 1)
p <- p + stat_smooth(data = alcaldes_rd, aes(x = prop_votes_c2, y = prop_votes_total_t1, 
                                             colour = as.factor(win_t)), method = "loess", level = 0.9) 
p <- p + scale_x_continuous(limits = c(0.45, 0.55))
# p <- p + coord_cartesian(xlim = c(0.45, 0.55))
p


# RD and OLS regressions by year (restricted sample)

years <- names(table(alcaldes_rd$ano))
alcaldes_rd_y <- lapply(years, function(x){
  alcaldes_rd %>% filter(ano == x)
}) 

a <-  lapply(alcaldes_rd_y, function(x){
  rdrobust(y = x$prop_votes_total_t1,
           x = x$prop_votes_c2,
           # covs = cbind(x$parties_t),
           c = 0.5,
           all = T,
           vce = "hc1")
})

a
years

###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################### PRESIDENT FIRST ROUND ###########################################
###########################################################################################################

# Load presidential for t+1
president <- readRDS(paste0(res, "presidentes_primera_merge.rds"))

#Aggregate and merge with president electoral data
alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition == 1) %>%
  filter(ano != 2015) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., president,  by.x = c("year", "codmpio"), by.y = c("ano", "codmpio"), 
        suffixes = c("_t", "_t1"), all = T) %>%
  mutate(coalicion_1 = ifelse(codpartido_t == codpartido_t1, 1, 0)) %>% 
  mutate(coalicion_2 = ifelse(coalition == 1, 1, 0)) %>%
  filter(coalicion_1 == 1 & coalicion_2 == 1) %>%
  arrange(codmpio, ano) 


a <- rdrobust(y = alcaldes_rd$prop_votes_total_t1,
              x = alcaldes_rd$prop_votes_c2,
              # covs = cbind(as.factor(alcaldes_rd$ano), alcaldes_rd$pobl_tot),
              c = 0.5,
              all = T,
              vce = "hc1"
)
a


