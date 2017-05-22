
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
final <- "Results/Descriptives/"
doc <- "Document/Figures/"

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
  dplyr::select(ano, codmpio, codep, municipio, primer_apellido, nombre,codpartido, votos, prop_votes_total) 

# Append old and new 
alcaldes_win <- rbind(alcaldes_merge_old, alcaldes_merge_new) %>% arrange(codmpio, ano)
table(alcaldes_win$codpartido,alcaldes_win$ano)

#Filter data.frame for Caribe region majors
cede <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta")) %>%
  filter(ano == 2016) %>% dplyr::select(coddepto, codmpio)

alcaldes_win_caribe <- alcaldes_win %>%
  merge(., cede, by.x = c("codmpio"), by.y = c("codmpio"), all.x = T) %>%
  filter(coddepto %in% c(44, 47, 8, 13, 20, 70, 23))

# Density vote share by year (interactive!)
d <- ggplot(alcaldes_win, aes(prop_votes_total, colour = factor(ano))) + geom_density()
d <- d + geom_density(data = alcaldes_win_caribe, aes(prop_votes_total, colour = factor(ano)), linetype = "dashed")
d <- d + labs(color="Año", y= "Densidad", x = "votes winner") + theme_bw()
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
  labs(y= "Percentage of Mayors", x = "Year") + scale_x_continuous(breaks = c(1988 ,1990, 1992, 1994, 1997, 2000, 2003, 2007, 2011, 2015)) + 
  theme_bw() + scale_fill_manual(values=c("grey","dodgerblue3","red3"), name = "", labels = c("New Parties","Conservative","Liberal")) + theme_bw() +  
  theme(legend.position="bottom", axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

ggsave(path=final,"alcaldia_win_party.pdf", width = 8, height = 5, dpi = 300)
ggsave(path=doc,"alcaldia_win_party.pdf", width = 8, height = 5, dpi = 300)




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



