
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

dnp <- "Data/DNP/Ejecuciones/"
final <- "Results/Descriptives/"
doc <- "Document/Figures/"
pres <- "Presentation/Material/"

###########################################################################################################
########################### Local government funds 1993-2014 ##############################################
###########################################################################################################

# Load ejecuciones
ejecu <- read_dta(paste0(dnp,"Ejecuciones_1993_2014.dta"))

# Investment by source: 
inv <- ejecu %>% dplyr::select(codmpio, ano, D1000,D2000,D3000) %>% filter(ano >= 1996) %>%
  group_by(ano) %>% summarise(SGP = sum(D2000, na.rm = TRUE), Royalties = sum(D1000, na.rm = TRUE), Cofinanced =sum(D3000, na.rm = TRUE)) %>% 
  gather(source,value, SGP:Cofinanced)

ggplot(data = inv, aes(x = ano, y = value, fill = source)) + geom_bar(stat = "identity") + 
  labs(y= "Investment (Million pesos)", x = "Year") +
  theme_bw() + scale_fill_manual(values=c("#2b8cbe","#a6bddb","#ece7f2"), name = "", labels = c("Cofinanced Projects","Royalties","SGP")) + theme_bw() +  
  theme(legend.position="bottom", axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

ggsave(path=final,"investment_source.pdf", width = 8, height = 5, dpi = 300)
ggsave(path=doc,"investment_source.pdf", width = 8, height = 5, dpi = 300)
ggsave(path=pres,"investment_source.pdf", width = 8, height = 5, dpi = 300)

