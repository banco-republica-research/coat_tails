
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList, library,character.only=TRUE)
# lapply(packageList,install.packages)


# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"

###########################################################################################################
############################################# LOAD DATA ###################################################
###########################################################################################################

alcaldes_difference <- readRDS(paste0(res,"alcaldes_difference.rds"))


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
######################## LONGITUDINAL CLUSTER: CONTINUOUS COMPETITION #####################################
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
######################## LONGITUDINAL CLUSTER: DISCRETE COMPETITION #######################################
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
########################### LONGITUDINAL CLUSTER: DISCRETE NO PARTIES #####################################
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