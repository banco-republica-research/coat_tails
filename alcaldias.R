
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases trabajo/"

###########################################################################################################
############################################### ARRANGE DATA ##############################################
###########################################################################################################

# Get mayor's election data (only from electoral years since 1997). 

list_files <- list.files(path=data) %>%
  .[. %in% c("1997", "2000", "2003", "2007", "2011", "2015")]%>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Alcal")]}) %>% 
  str_c(data, c("1997", "2000", "2003", "2007", "2011", "2015"),"/", ., sep = "")

#Open dta files into a list
alcaldes <- lapply(list_files, read_dta) 
alcaldes[[5]]$nombre <- ""


#Aggregate totals for each year and clean non-candidate data

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","RETIRADO (A)", "TARJETAS NO MARCADOS")

alcaldes_aggregate <- alcaldes %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
    mutate(non_candidate = ifelse(.$primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
    group_by(codmpio, ano) %>%
    mutate(prop_votes_total = votos / sum(votos)) %>%
    filter(non_candidate == 0)  %>%
    mutate(parties = n()) %>%
    mutate(prop_votes_candidates = votos / sum(votos)) %>%
    mutate(rank = dense_rank(desc(votos))) %>% 
    mutate(party_ef = ifelse(prop_votes_candidates > 0.1, 1,0)) %>%
    mutate(parties_ef = sum(party_ef)) %>%
    filter(rank <= 2) %>%
    mutate(prop_votes_c2 = votos / sum(votos)) 
  #  %>%  filter(prop_votes_candidates < 1) #Eliminate elections with only one candidate
  })

#Arrange data in a long format
alcaldes_merge <- alcaldes_aggregate %>%
  ldply() %>%
  arrange(codmpio, ano, desc(rank)) %>%
  dplyr::select(c(ano, codmpio, codep, municipio, departamento, parties, parties_ef, rank, primer_apellido, nombre, codpartido, votos, 
                  prop_votes_total, prop_votes_candidates, prop_votes_c2)) 


###########################################################################################################
######################################### ARRANGE DATA II : GAPS ##########################################
###########################################################################################################

# Calculate difference
alcaldes_difference <- alcaldes_merge %>%
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

clus <- cutree(clus_seq, k = 2)
clus <- factor(clus)
table(clus)

seqfplot(seq, group = clus, pbarw = T)
seqmtplot(seq, group = clus)


