
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
############################################### ARRANGE DATA ##############################################
###########################################################################################################

##########################
# Get mayor's election data (Winners and loosers since 1997). 

list_files <- list.files(path=data) %>%
  .[. %in% c("1997", "2000", "2003", "2007", "2011", "2015")]%>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Alcal")]}) %>% 
  str_c(data, c("1997", "2000", "2003", "2007", "2011", "2015"),"/", ., sep = "")

# Get party-code list
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
alcaldes <- lapply(list_files, read_dta) 
alcaldes[[5]] <- alcaldes[[5]] %>%
  mutate(nombre = "") %>%
  mutate(partido_1 = as.factor(partido_1)) %>%
  mutate(partido_1 = fct_recode(partido_1,
                              "PARTIDO ALIANZA SOCIAL INDEPENDIENTE 'ASI'" = "PARTIDO ALIANZA SOCIAL INDEPENDIENTE",
                              "PARTIDO DE INTEGRACION NACIONAL PIN" = "PARTIDO DE INTEGRACION NACIONAL"
  )) %>% mutate(partido_1 = as.character(partido_1)) %>%
  stringdist_left_join(party_code, by = c(partido_1 = "name_party"), distance_col = "distance", max_dist = 2) %>%
  rename(codpartido = party_code)

alcaldes[[6]] <- alcaldes[[6]] %>%
  rename(codpartido = codpartido_1)

#Check for not-joined data
not_joined <- alcaldes[[5]] %>%
  filter(is.na(codpartido))

#Aggregate totals for each year and clean non-candidate data

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")

alcaldes_aggregate <- alcaldes %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
    filter(is.na(votos)==0) %>% 
    mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
    mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>% 
    group_by(codmpio, ano) %>%
    mutate(rank = row_number(desc(votos))) %>% 
    mutate(prop_votes_total = votos / sum(votos)) %>%
    mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>%
    mutate(prop_votes_cand = votos / sum(votos_cand)) %>%
    mutate(votos_r2 = ifelse(rank <= 2, votos,0)) %>% 
    mutate(prop_votes_c2 = votos / sum(votos_r2)) %>% 
    mutate(parties = sum(cand)) %>%
    mutate(party_ef = ifelse(prop_votes_cand > 0.1, 1,0)) %>%
    mutate(parties_ef = sum(party_ef)) %>% 
    filter(is.na(prop_votes_total)==0) 
  })

#Arrange data in a long format
alcaldes_merge <- alcaldes_aggregate %>%
  ldply() %>%
  arrange(codmpio, ano, desc(rank)) %>%
  dplyr::select(c(ano, codmpio, codep, municipio, parties, parties_ef, rank, primer_apellido, nombre, codpartido, cand, votos, votos_cand, votos_r2,
  prop_votes_total, prop_votes_cand, prop_votes_c2)) 

# Only rank <= 2 and drop if total votes == 0 
alcaldes_merge_r2 <- alcaldes_merge %>% filter(rank <= 2)
    #  %>%  filter(prop_votes_candidates < 1) #Eliminate elections with only one candidate

# Diagnistics
hist(alcaldes_merge_r2$prop_votes_c2)
alcaldes_merge_r2 %>% filter(is.na(prop_votes_c2)==0) %>%
  group_by(rank) %>% summarize(mean=mean(prop_votes_c2),sd=sd(prop_votes_c2),median=median(prop_votes_c2),min=min(prop_votes_c2),max=max(prop_votes_c2))

# test <- alcaldes_merge %>% filter(rank == 1)


##########################
# Only winners 1988-1994 

list_files_old <- list.files(path=data) %>%
  .[. %in% c("1988", "1990", "1992", "1994")]%>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Alcal")]}) %>% 
  str_c(data, c("1988", "1990", "1992", "1994"),"/", ., sep = "")

alcaldes_old <- lapply(list_files_old, read_dta) 
View(alcaldes_old[[4]])

non_candidate_votes <- c("RESTO DE VOTACION","VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")

alcaldes_aggregate_old <- alcaldes_old %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
      filter(is.na(votos)==0) %>% 
      mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
      mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>% 
      group_by(codmpio, ano) %>%
      mutate(prop_votes_total = votos / sum(votos)) %>%
      mutate(n = n())     %>%
      mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>% 
      mutate(rank = row_number(desc(votos_cand))) %>% 
      filter(rank==1 & no_cand==0)   %>%
      filter(is.na(prop_votes_total)==0) 
    
  })

View(alcaldes_aggregate_old[[4]])

alcaldes_merge_old <- alcaldes_aggregate_old %>%
  ldply() %>%
  arrange(codmpio, ano, desc(rank)) %>%
  dplyr::select(c(ano, codmpio, codep, municipio, primer_apellido, nombre, codpartido, votos, prop_votes_total)) 

hist(alcaldes_merge_old$prop_votes_total)



###########################################################################################################
######################################### ARRANGE DATA II : GAPS ##########################################
###########################################################################################################

# Calculate difference between first and second
alcaldes_difference <- alcaldes_merge_r2 %>%
  arrange(codmpio, ano, desc(rank)) %>%
  group_by(codmpio, ano) %>% #Calculate difference
  mutate(diff =  ave(prop_votes_c2, factor(codmpio), factor(ano), FUN = function(x) c(0, diff(x)))) %>%
  mutate(diff = ifelse(diff==0 & rank==1 & prop_votes_c2 == 1, 1, diff))

#Collapse by codmpio and year, and create categorical variables
alcaldes_difference <- alcaldes_difference %>%
  dplyr::group_by(codmpio, ano) %>%
  dplyr::summarize(votes_tot = sum(votos), parties = mean(parties),parties_ef = mean(parties_ef), difference = sum(diff)) 

alcaldes_difference$dif_q <- quantcut(alcaldes_difference$difference, labels=c(1,2,3,4))
saveRDS(alcaldes_difference,paste0(res,"alcaldes_difference.rds"))

alcaldes_difference %>% 
  group_by(dif_q) %>% summarize(mean=mean(difference),sd=sd(difference),min=min(difference),max=max(difference))


# Wide format: This process can generate NA's. This results from the fact that for some years and municipalities
# elections were not held or not reported. Thus, the no result is reported as NA in the wide version of the df. 

alcaldes_wide <- alcaldes_difference[,c("codmpio","ano","difference")] %>%
  spread(ano, difference, sep = "") 

alcaldes_wide_p <- alcaldes_difference[,c("codmpio","ano","parties")] %>%
  spread(ano, parties, sep = "") 

alcaldes_wide_pe <- alcaldes_difference[,c("codmpio","ano","parties_ef")] %>%
  spread(ano, parties_ef, sep = "") 

alcaldes_wide_q <- alcaldes_difference[,c("codmpio","ano","dif_q")] %>%
  spread(ano, dif_q, sep = "") 

###########################################################################################################
################################################ PLOTS ####################################################
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


###########################################################################################################
######################## LONGITUDINAL CLUSTER ANALYSIS: CONTINUOUS ########################################
###########################################################################################################

# To account for the no-election situation, the data.frame can be "cleaned" of NA's removing all the 
# municipalities with at least one NA. This is desireable for the calculation of the trajectories. 
# The other option is interpolation, but for this data the bias can be *huge*. 

alcaldes_wide_bal <- alcaldes_wide[complete.cases(alcaldes_wide), ]
cld <- cld(alcaldes_wide_bal, timeInData = c(2:7), idAll = alcaldes_wide_bal$codmpio)
kml(cld, toPlot = "both")

#Get clusters
alcaldes_wide_bal$cluster <- getClusters(cld, 6)

#Merge with geographical data
setwd("~/Dropbox/Geografia/")
municipios <- readOGR("mpio", "mpio") %>%
  spTransform(CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  

municipios_clust <- municipios %>%
  tidy(region = "MPIOS_1") %>%
  mutate(muni_code = as.numeric(id)) %>%
  merge(., municipios@data, by.x = "id", by.y = "MPIOS_1", all = T) %>%
  merge(., alcaldes_wide_bal, by.x = "id", by.y = "codmpio", all = T)


# Plot using ggplot
# setwd("~/Desktop/")
g <- ggplot(data = municipios_clust, aes(x = long, y = lat)) + geom_polygon(aes(group = id, fill = cluster))
g <- g + coord_equal()
# g <- g + theme(legend.position = c(.15, .25))
ggplotly(g)
# ggsave(str_c("americas_regions.pdf"), width=45, height=55, units="cm")



###########################################################################################################
######################## LONGITUDINAL CLUSTER ANALYSIS: DISCRETE   ########################################
###########################################################################################################


colnames(alcaldes_wide_q) <- c("codmpio","1997","2000","2003","2007","2011","2015")

# Balanced sample
alcaldes_wide_bal <- alcaldes_wide_q[complete.cases(alcaldes_wide_q), ]

# Suppose NA are equivalente to level 4 (low competition)
alcaldes_wide_na <- alcaldes_wide_q 
alcaldes_wide_na[, 2:7][is.na(alcaldes_wide_na[, 2:7])] <- 4

# Prepare sequences
a <- c(1,2,3,4)
b <- c("1","2","3","4")
seq <- seqdef(alcaldes_wide_bal, 2:7, states = b, labels = a, xtstep = 6)
head(seq)

# plots 
seqdplot(seq)
seqfplot(seq, withlegend = F, border = NA, title = "Sequence frequency plot")
seqdplot(seq, withlegend = F, border = NA, title = "State distribution plot")
seqHtplot(seq, title = "Entropy index")
seqtab(seq)
seq_ient <- seqient(seq, norm = FALSE)
hist(seq_ient, main = NULL, col = "cyan", xlab = "Entropy")

seq_st <- seqST(seq)
summary(seq_st)
hist(seq_st, col = "cyan", main = "Sequence turbulence")

# Distances 
seq_dist <- seqdist(seq, method = "LCS", with.missing = TRUE)
ccost <- seqsubm(seq, method = "CONSTANT", cval = 2)
seq_dist2 <- seqdist(seq, method = "OM", sm = ccost)

# Clustering

clus_seq <- agnes(seq_dist2, diss = TRUE, method = "ward")
# plot(clus_seq, which.plots = 2)

clus <- cutree(clus_seq, k = 4)
clus <- factor(clus)
table(clus)

seqfplot(seq, group = clus, pbarw = T)
seqmtplot(seq, group = clus)

###########################################################################################################
######################## LONGITUDINAL CLUSTER ANALYSIS: DISCRETE PARTIES ##################################
###########################################################################################################

alcaldes_wide_p_clus <- alcaldes_wide_p
alcaldes_wide_p_clus[,2:7][ alcaldes_wide_p_clus[ , 2:7 ] >= 6 ] <- 6

colnames(alcaldes_wide_p) <- c("codmpio","1997","2000","2003","2007","2011","2015")

# Balanced sample
alcaldes_wide_bal <- alcaldes_wide_p_clus[complete.cases(alcaldes_wide_p_clus), ]

# Suppose NA are equivalente to 1 (low competition)
alcaldes_wide_na <- alcaldes_wide_p_clus 
alcaldes_wide_na[, 2:7][is.na(alcaldes_wide_na[, 2:7])] <- 1

# Prepare sequences
a <- c(1,2,3,4,5,6)
b <- c("1","2","3","4","5","6")
seq <- seqdef(alcaldes_wide_bal, 2:7, states = b, labels = a, xtstep = 6)
head(seq)

# plots 
seqdplot(seq)
seqfplot(seq, withlegend = F, border = NA, title = "Sequence frequency plot")
seqdplot(seq, withlegend = F, border = NA, title = "State distribution plot")
seqHtplot(seq, title = "Entropy index")
seqtab(seq)
seq_ient <- seqient(seq, norm = FALSE)
hist(seq_ient, main = NULL, col = "cyan", xlab = "Entropy")

seq_st <- seqST(seq)
summary(seq_st)
hist(seq_st, col = "cyan", main = "Sequence turbulence")

# Distances 
seq_dist <- seqdist(seq, method = "LCS", with.missing = TRUE)
ccost <- seqsubm(seq, method = "CONSTANT", cval = 2)
seq_dist2 <- seqdist(seq, method = "OM", sm = ccost)

# Clustering

clus_seq <- agnes(seq_dist, diss = TRUE, method = "ward")
# plot(clus_seq, which.plots = 2)

clus <- cutree(clus_seq, k = 4)
clus <- factor(clus)
table(clus)

seqfplot(seq, group = clus, pbarw = T)
seqmtplot(seq, group = clus)


###########################################################################################################
############################### RD: IMCUMBENCY EFFECT - NAÏVE PARTY APPROACH ##############################
###########################################################################################################

# Create lagged year and collapse by party (or group of parties) for t+1 outcome  
alcaldes_merge_collapse <- alcaldes_merge %>%
  filter(ano != 1997) %>%
  filter(cand == 1) %>%
  mutate(ano_lag = as.factor(ano)) %>%
  mutate(ano_lag = fct_recode(ano_lag,
                               "1997" = "2000",
                               "2000" = "2003",
                               "2003" = "2007",
                               "2007" = "2011",
                               "2011" = "2015")) %>%
  mutate(ano_lag = as.character(ano_lag)) %>%
  rename(ano_t1 = ano) %>% 
  group_by(codmpio, ano_lag, ano_t1, codpartido, parties, parties_ef) %>%
  summarize(prop_votes_cand = sum(prop_votes_cand),
            prop_votes_total = sum(prop_votes_total),
            rank = max(rank))

# Test duplicates
table(duplicated(alcaldes_merge_collapse[,c("codmpio", "ano_lag", "codpartido")]))

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

sort(table(alcaldes_merge_r2$codpartido),decreasing=TRUE)[1:20]

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(codpartido == 198) %>%
  filter(ano != 2015) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank==1,1,0)) %>% 
  merge(alcaldes_merge_collapse,  by.x = c("ano", "codmpio","codpartido"), by.y = c("ano_lag", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  dplyr::select(codmpio, ano, ano_t1, codpartido, win_t, rank_t,
                rank_t1 ,parties_t,parties_ef_t,parties_t1,parties_ef_t1, starts_with("prop")) %>%
  arrange(codmpio, ano) %>%
  mutate(reelection = ifelse(rank_t == 1 & rank_t1 == 1, 1, 0))

# RD 
alcaldes_rd_fin <- alcaldes_rd
# alcaldes_rd_fin <- subset(alcaldes_rd, ano == 2007)
dim(alcaldes_rd_fin)
hist(alcaldes_rd_fin$prop_votes_c2)

rdrobust(y = alcaldes_rd_fin$prop_votes_total_t1,
              x = alcaldes_rd_fin$prop_votes_c2,
              covs = cbind(as.factor(alcaldes_rd$ano), alcaldes_rd$parties_t),
              c = 0.5,
              all = T
              )

alcaldes_rd_fin_b <- alcaldes_rd_fin %>% filter(prop_votes_c2 >= (0.5 - 0.0851) & prop_votes_c2 <= (0.5 + 0.0851))
dim(alcaldes_rd_fin_b)
hist(alcaldes_rd_fin_b$prop_votes_c2)

summary(lm(formula = prop_votes_total_t1 ~ prop_votes_c2 + win_t, data = alcaldes_rd_fin_b))






  








