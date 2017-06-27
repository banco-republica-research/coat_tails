
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
inv <- ejecu %>% dplyr::select(codmpio, ano, D1000,D2000,D3000,D4000) %>% filter(ano >= 1996) %>%
  group_by(ano) %>% summarise(SGP = sum(D2000, na.rm = TRUE), Royalties = sum(D1000, na.rm = TRUE), Cofinanced =sum(D3000, na.rm = TRUE), Other =sum(D4000, na.rm = TRUE)) %>% 
  gather(source,value, SGP:Other)

inv2 <- inv %>% filter(source!="Other")

# Sin otros
ggplot(data = inv2, aes(x = ano, y = value, fill = factor(source, levels=c("Cofinanced","Royalties","SGP")))) + geom_bar(stat = "identity") + 
  labs(y= "Investment (Million pesos 2014)", x = "Year") + scale_x_continuous(breaks = c(1996, 1998, 2000, 2002, 2004, 2006, 2008,2010, 2012, 2014)) + 
  theme_bw() + scale_fill_manual(values=c("#2b8cbe","#a6bddb","#ece7f2"), name = "", labels = c("Cofinancing","Royalties","SGP")) + theme_bw() +  
  theme(legend.position="bottom", axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

# Con otros
ggplot(data = inv, aes(x = ano, y = value, fill = factor(source, levels=c("Other","Cofinanced","Royalties","SGP")))) + geom_bar(stat = "identity") + 
  labs(y= "Investment (Million pesos 2014)", x = "Year") + scale_x_continuous(breaks = c(1996, 1998, 2000, 2002, 2004, 2006, 2008,2010, 2012, 2014)) + 
  theme_bw() + scale_fill_manual(values=c("#a1d99b","#2b8cbe","#a6bddb","#ece7f2"), name = "", labels = c("Others","Cofinancing","Royalties","SGP")) + theme_bw() +  
  theme(legend.position="bottom", axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


ggsave(path=final,"investment_source.pdf", width = 8, height = 5, dpi = 300)
ggsave(path=doc,"investment_source.pdf", width = 8, height = 5, dpi = 300)
ggsave(path=pres,"investment_source.pdf", width = 8, height = 5, dpi = 300)


###########################################################################################################
#################################### Roads funding 1993-2014 ##############################################
###########################################################################################################

# Load ejecuciones
vias <- read_dta(paste0(dnp,"Vias_SICEP.dta")) %>% filter(ano>=1996 & ano <= 2014)

# credito en others 
vias_source <- vias %>% dplyr::select(codmpio, ano, f_propios, f_SGPp, f_regalias, f_trans_nac, f_trans_dep, f_credito,f_otras) %>% 
  mutate(otras = f_credito + f_trans_dep + f_otras) %>% 
  dplyr::select(-c(f_trans_dep, f_otras)) %>% 
  group_by(ano) %>% summarise(Local =sum(f_propios, na.rm = TRUE), SGP = sum(f_SGPp, na.rm = TRUE), Royalties = sum(f_regalias, na.rm = TRUE), Cofinanced =sum(f_trans_nac, na.rm = TRUE), Other =sum(otras, na.rm = TRUE)) %>% 
  gather(source,value, Local:Other)

ggplot(data = vias_source, aes(x = ano, y = value, fill = factor(source, levels=c("Other","Cofinanced","Royalties","SGP","Local")))) + geom_bar(stat = "identity") + 
  labs(y= "Investment (Million pesos 2014)", x = "Year") + scale_x_continuous(breaks = c(1996, 1998, 2000, 2002, 2004, 2006, 2008,2010, 2012, 2014)) + 
  theme_bw() + scale_fill_manual(values=c("#ece7f2","#a1d99b","#2b8cbe","#a6bddb","#F1E7DA"), name = "", labels = c("Other","Cofinanced","Royalties","SGP","Local Revenue")) + theme_bw() +  
  theme(legend.position="bottom", axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

ggsave(path=final,"roads_source.pdf", width = 8, height = 5, dpi = 300)
ggsave(path=doc,"roads_source.pdf", width = 8, height = 5, dpi = 300)
ggsave(path=pres,"roads_source.pdf", width = 8, height = 5, dpi = 300)





# Con todos 
vias_source <- vias %>% dplyr::select(codmpio, ano, f_propios, f_SGPp, f_regalias, f_trans_nac, f_trans_dep, f_credito, f_otras) %>% 
  mutate(otras = f_trans_dep + f_otras) %>% 
  dplyr::select(-c(f_trans_dep, f_otras)) %>% 
  group_by(ano) %>% summarise(Local =sum(f_propios, na.rm = TRUE), SGP = sum(f_SGPp, na.rm = TRUE), Royalties = sum(f_regalias, na.rm = TRUE), Cofinanced =sum(f_trans_nac, na.rm = TRUE), Credit =sum(f_credito, na.rm = TRUE), Other =sum(otras, na.rm = TRUE)) %>% 
  gather(source,value, Local:Other)

ggplot(data = vias_source, aes(x = ano, y = value, fill = factor(source, levels=c("Other","Credit","Cofinanced","Royalties","SGP","Local")))) + geom_bar(stat = "identity") + 
  labs(y= "Investment (Million pesos 2014)", x = "Year") + scale_x_continuous(breaks = c(1996, 1998, 2000, 2002, 2004, 2006, 2008,2010, 2012, 2014)) + 
  theme_bw() + scale_fill_manual(values=c("blue","red","#a1d99b","#2b8cbe","#a6bddb","#ece7f2"), name = "", labels = c("Other","Credit","Cofinanced","Royalties","SGP","Local Taxes")) + theme_bw() +  
  theme(legend.position="bottom", axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

# Share of transfers
vias_source_tot <- vias %>% dplyr::select(codmpio, ano, f_propios, f_SGPp, f_regalias, f_trans_nac, f_trans_dep, f_credito, f_otras) %>% 
  mutate(otras = f_trans_dep + f_otras) %>% 
  dplyr::select(-c(f_trans_dep, f_otras)) %>% 
  group_by() %>% summarise(Local =sum(f_propios, na.rm = TRUE), SGP = sum(f_SGPp, na.rm = TRUE), Royalties = sum(f_regalias, na.rm = TRUE), Cofinanced =sum(f_trans_nac, na.rm = TRUE), Credit =sum(f_credito, na.rm = TRUE), Other =sum(otras, na.rm = TRUE)) %>% 
  gather(source,value, Local:Other) %>% 
  mutate(share = value / sum(value)) 

trans <- c("Cofinanced", "Royalties","SGP")
trans_tot <- vias_source_tot %>% filter(source %in% trans) %>%
    mutate(share = sum(share)) %>%
    mutate(share_2 = value / sum(value)) 



# Solo transferencias

trans <- c("Cofinanced", "Royalties","SGP")
vias_source_2 <- vias_source %>% filter(source %in% trans)

  
ggplot(data = vias_source_2, aes(x = ano, y = value, fill = factor(source, levels=c("Cofinanced","Royalties","SGP")))) + geom_bar(stat = "identity") + 
  labs(y= "Investment (Million pesos 2014)", x = "Year") + scale_x_continuous(breaks = c(1996, 1998, 2000, 2002, 2004, 2006, 2008,2010, 2012, 2014)) + 
  theme_bw() + scale_fill_manual(values=c("#2b8cbe","#a6bddb","#ece7f2"), name = "", labels = c("Cofinancing","Royalties","SGP")) + theme_bw() +  
  theme(legend.position="bottom", axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

