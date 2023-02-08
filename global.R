library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(sf)

## --------------Read data retail prices:----------------------
adm_bound<- function(area){
  if(str_detect(area, fixed("Philippines", ignore_case=TRUE)) == TRUE){
    ext="Country"
  }else if(stringr::str_detect(area, "^[^[:lower:]]+$") == FALSE){
    ext<-"Province"
  }else{
    ext <-"Region"
  }
  
  return(ext)
  
}


commod_names<-list.files(path = "./data/prices_psa/2012-based",pattern = "*.csv") %>%
  str_split(., "_")%>%
  lapply(., function(x) x[[2]]) %>%
  unlist()


edit_commodity_df<-function(x){
  x %>%
    mutate(Admin_level = sapply(Region.Province,adm_bound)) %>%
    mutate(Region.Province = str_remove_all(Region.Province, "\\.")) %>%
    pivot_longer(cols= X2012.January:X2021.Annual, names_to = "Date") %>%   #combine all date columns
    filter(str_detect(Date, 'Annual', negate = TRUE)) %>% #remove annual rows %>%
    mutate(Date = str_replace(Date, "X","")) %>%
    mutate(Date = as.Date(paste0("01.", Date), "%d.%Y.%B")) %>%
    arrange(Admin_level,Region.Province) %>% # Rearrange Region.Province
    rename(Price = value) %>%
    mutate(Region.Province = ifelse(Region.Province == "MIMAROPA REGION", "Region IV-B (Mimaropa)",Region.Province)) %>%
    mutate(Region.Province = ifelse(grepl("REGION\\b",Region.Province), str_replace(Region.Province,"REGION","Region"), str_to_title(Region.Province))) %>%
    mutate(Region.Province = ifelse(grepl("AUTONOMOUS", Region.Province), "Bangsamoro Autonomous Region in Muslim Mindanao (ARMM)",Region.Province)) %>%
    mutate(Region.Province = ifelse(grepl("CORDILLERA", Region.Province), "Cordillera Adminsitrative Region (CAR)",Region.Province)) %>%
    mutate(Region.Province = ifelse(grepl("CAPITAL", Region.Province), "National Capital Region (NCR)",Region.Province))
}


df<-list.files(path = "./data/prices_psa/2012-based",pattern = "*.csv",full.names = TRUE) %>%
  lapply(., function(x) read.csv(x,header=TRUE, skip=2,na.strings=c("",".","NA",".."))) %>%
  lapply(., function(x) edit_commodity_df(x))


for(i in seq_along(df)){
  df[[i]]$Group <-commod_names[[i]]
}


# 
# #into one large data frame!
# df%<>%do.call("rbind",.)


## -------------- Merging with shp file admin boundaries--------------------
ph_geom<-st_read("./data/adm/phl_admbnda_adm2_psa_namria_20200529.shp") %>%
  select("ADM1_EN","ADM2_EN","ADM0_EN") %>%
  mutate(ADM0_EN = "Philippines")%>%
  mutate(ADM1_EN = ifelse(grepl("Samar", ADM1_EN), "Western Samar", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(grepl("Dinagat Islands", ADM1_EN), "Dinagat Island", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(grepl("Sarangani", ADM1_EN), "Saranggani", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(grepl("Cotabato", ADM1_EN), "North Cotabato", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(grepl("City of Isabela", ADM1_EN), "Isabela City", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(grepl("Davao Occidental", ADM1_EN), "Davao de Oro", ADM1_EN))%>%
  rmapshaper::ms_simplify(keep = 0.01, keep_shapes = F)

#NO geometry
ph_adm <- ph_geom %>%
  st_drop_geometry() 



regions<-unique(ph_adm$ADM1_EN)
