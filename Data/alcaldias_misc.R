
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
############################### WINNING PARTIES 1997-2015 #################################################
###########################################################################################################

# Load data
alcaldes_difference <- readRDS(paste0(res,"alcaldes_difference.rds"))
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
alcaldes_merge_old <- readRDS(paste0(res,"alcaldes_merge_old.rds"))
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

# Winning parties 1997-2015
alcaldes_merge_new <- alcaldes_merge %>% filter(rank == 1) %>% 
  dplyr::select(ano, codmpio, municipio, primer_apellido, nombre,codpartido, votos, prop_votes_total) 

# Append old and new 
alcaldes_win <- rbind(alcaldes_merge_old, alcaldes_merge_new) %>% arrange(codmpio, ano)
table(alcaldes_win$codpartido,alcaldes_win$ano)

# Density vote share by year (interactive!)
d <- ggplot(alcaldes_win, aes(prop_votes_total, colour = factor(ano))) + geom_density() + 
  labs(color="Año", y= "Densidad", x = "votes winner") + theme_bw()
ggplotly(d)

# Party win by year
parties_win <- alcaldes_win %>% group_by(ano, codpartido) %>% summarize(win = n()) %>% 
  merge(party_code,  by.x = c("codpartido"), by.y = c("party_code"), all.x = T) %>% 
  arrange(ano, desc(win)) 

# Big parties: >x win in at least one year
big_parties <- parties_win  %>% filter(codpartido!= 98 & codpartido!= 99) %>% filter(win >= 20) %>% 
  group_by(codpartido, name_party) %>% summarize(win = sum(win))%>% 
  arrange(desc(win)) 

# Keep only big parties and collapse others
parties_win_big <- parties_win %>% 
  mutate(party_big = ifelse(codpartido %in% big_parties$codpartido, codpartido, 9999)) %>% 
  mutate(party = ifelse(codpartido %in% big_parties$codpartido, name_party, "Otros")) %>% 
  group_by(ano, party_big, party) %>% summarize(win = sum(win)) %>% 
  group_by(ano) %>% 
  mutate(win_share = win/sum(win)) %>% 
  arrange(ano, party_big) 

# Keep liberal/conservador 
parties_win_lc <- parties_win %>% 
  mutate(party_lc = ifelse(codpartido %in% c(1,2), codpartido, 9999)) %>% 
  mutate(party = ifelse(codpartido %in% c(1,2), name_party, "Otros")) %>% 
  group_by(ano, party_lc, party) %>% summarize(win = sum(win)) %>% 
  group_by(ano) %>% 
  mutate(win_share = win/sum(win)) %>% 
  arrange(ano, party_lc) 

ggplot(parties_win_lc, aes(x = ano, y = win_share, fill = party)) + geom_bar(stat = "identity") + 
  labs(x = "Año") + scale_x_continuous(breaks = round(seq(min(parties_win_big$ano), max(parties_win_big$ano), by = 1),1)) + 
  theme_bw() + scale_fill_manual(values=c("grey","blue","red"), name = "", labels = c("Otros","Conservador","Liberal")) 



###########################################################################################################
################################ COMPETITION AND NO PARTIES ###############################################
###########################################################################################################


# Density by year (interactive!)
d <- ggplot(alcaldes_difference, aes(difference, colour = factor(ano))) + geom_density() + 
  labs(color="Año", y= "Densidad", x = "Competencia") + theme_bw()
ggplotly(d)

p <- ggplot(alcaldes_difference, aes(parties, colour = factor(ano))) + geom_density() + 
  labs(color="Año", y= "Densidad", x = "Partidos") + theme_bw()
ggplotly(p)

p_ef <- ggplot(alcaldes_difference, aes(parties_ef, colour = factor(ano))) + geom_density() + 
  labs(color="Año", y= "Densidad", x = "Partidos efectivos") + theme_bw()
ggplotly(p_ef)

# Number of parties and political competition 

s <- ggplot(alcaldes_difference, aes(parties, difference)) + geom_point(aes(colour = factor(ano), size=votes_tot)) + 
  labs(shape = "", color="Año", y= "Competencia", x = "Partidos") + theme_bw() 

ggplotly(s)

s_ef <- ggplot(alcaldes_difference, aes(parties_ef, difference)) + geom_point(aes(colour = factor(ano), size=votes_tot)) +
  labs(shape = "", color="Año", y= "Competencia", x = "Partidos efectivos") + theme_bw() 

ggplotly(s_ef)



