###########################################################################################################
############################################# COAT-TAILS RD ###############################################
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

###########################################################################################################
############################################# LOAD DATA ###################################################
###########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
alcaldes_merge_r2 <- alcaldes_merge %>% filter(rank <= 2)
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))
controls <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))

# Load presidential for t+1
win_apellido <- c("PASTRANA", "URIBE", "SANTOS")
win_nom <- c("ANDRES", "ALVARO", "JUAN MANUEL")

president <- readRDS(paste0(res, "presidentes_segunda_merge.rds")) %>%
  mutate(coalition = ifelse(primer_apellido %in% win_apellido & nombre %in% win_nom , 1, 0))

###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
###########################################################################################################

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition == 1) %>%
  filter(ano != 2015) %>%
  #  filter(prop_votes_c2 >= 0.35 & prop_votes_c2 <= 0.65) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., president,  by.x = c("year", "codmpio", "coalition"), by.y = c("ano", "codmpio", "coalition"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, ano, year, codpartido_t, win_t, 
                votos_t, votos_t1, starts_with("prop")) %>%
  arrange(codmpio, ano)

dim(alcaldes_rd)
hist(alcaldes_rd$prop_votes_c2)


# Scatter and RD Graphs 

#Drop outliers

l <- alcaldes_rd %>%
  merge(., controls[, c("pobl_tot", "coddepto", "ano", "codmpio")], by = c("codmpio", "ano"), all.x = T) %>%
  filter(prop_votes_c2 <= 0.5 + sd(prop_votes_c2) * 1.645 & prop_votes_c2 >= 0.5 - sd(prop_votes_c2) * 1.645) %>%
  mutate(bin = cut(prop_votes_c2, breaks = seq(0.3, 0.7, 0.001), include.lowest = T)) %>%
  group_by(bin) %>%
  summarize(mean_bin = mean(prop_votes_total_t1), sd_bin = sd(prop_votes_total_t1), n = length(codmpio)) %>%
  .[complete.cases(.),] %>%
  as.data.frame() %>%
  mutate(treatment = ifelse(as.numeric(row.names(.)) >= 172, 1, 0), bins = row.names(.)) %>%
  mutate(bins = mapvalues(.$bins, from = c(1:347), to = seq(0.329, 0.675, 0.001)))

p <- ggplot(l, aes(y = mean_bin, x = as.numeric(bins), colour = as.factor(treatment)))
p <- p + geom_point(colour = "black", size = 1)
p <- p + stat_smooth(data = alcaldes_rd, aes(x = prop_votes_c2, y = prop_votes_total_t1, 
                         colour = as.factor(win_t)), method = "loess") 
p <- p + scale_x_continuous(limits = c(0.44, 0.56))
# p <- p + coord_cartesian(xlim = c(0.4, 0.6))
p




p <- ggplot(l, aes(x = prop_votes_c2, y = prop_votes_total_t1, colour = as.factor(ano)))
p <- p + geom_point(aes(name = codmpio , size = pobl_tot))
p <- p + geom_smooth(method = "lm", se = FALSE) 
p <- p + labs(x = "Proporción alcalde ganador/per", y = "Proporción presidente coalición (t+1)")
p
ggplotly(p)


# RD and OLS regressions on restricted sample
# alcaldes_rd <- subset(alcaldes_rd, ano == 2007)

a <- rdrobust(y = l$prop_votes_total_t1,
              x = l$prop_votes_c2,
              covs = cbind(as.factor(l$ano), l$pobl_tot),
              c = 0.5,
              all = T,
              vce = "hc1"
)
a

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

