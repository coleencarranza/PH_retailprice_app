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
    mutate(Price = value) %>%
    mutate(Region.Province = ifelse(Region.Province == "MIMAROPA REGION", "Region IV-B (Mimaropa)",Region.Province))
  
}


df<-list.files(path = "./data/prices_psa/2012-based",pattern = "*.csv",full.names = TRUE) %>%
  lapply(., function(x) read.csv(x,header=TRUE, skip=2,na.strings=c("",".","NA",".."))) %>%
  lapply(., function(x) edit_commodity_df(x))


for(i in seq_along(df)){
  df[[i]]$Group <-commod_names[[i]]
}

#into one large data frame!
df%<>%do.call("rbind",.)


## -------------- Merging with shp file admin boundaries--------------------
#NO geometry
ph_adm1 <- st_read("./data/adm/phl_admbnda_adm2_psa_namria_20200529.shp") %>%
  st_drop_geometry() %>%
  select("ADM1_EN","ADM2_EN","ADM0_EN")
#YES geometry
ph_geom<-st_read("./data/adm/phl_admbnda_adm2_psa_namria_20200529.shp") %>%
  select("ADM1_EN","ADM2_EN","ADM0_EN")


regions<-unique(ph_adm1$ADM1_EN)


df <- df%>% 
  left_join(., ph_adm1, by=c("Region.Province" = "ADM2_EN")) %>%
  mutate(ADM1_EN = ifelse(Region.Province == "PHILIPPINES" & is.na(ADM1_EN),"Philippines", ADM1_EN))%>%
  mutate(ADM1_EN = ifelse(is.na(ADM1_EN),regions[match(tolower(word(Region.Province,2)),tolower(word(regions,2)))], ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(is.na(ADM1_EN) & grepl("Davao", Region.Province), "Region XI", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(is.na(ADM1_EN) & grepl("AUTONOMOUS", Region.Province), "Bangsamoro Autonomous Region in Muslim Mindanao", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(is.na(ADM1_EN) & grepl("Samar", Region.Province), "Region VIII", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(is.na(ADM1_EN) & grepl("Dinagat", Region.Province), "Region XIII", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(is.na(ADM1_EN) & grepl("Isabela City", Region.Province), "Region IX", ADM1_EN)) %>%
  mutate(ADM1_EN = ifelse(is.na(ADM1_EN) & grepl("Saranggani", Region.Province) | grepl("Cotabato", Region.Province), "Region XII", ADM1_EN))




#input to the app

# #Data cleaning
# commod_tpose<-function(df){
#   df %>%
#     tibble::column_to_rownames(var="Region.Province") %>%
#     t %>% #easiest way to transpose in this case!
#     data.frame() %>%
#     tibble::rownames_to_column("Date") %>%
#     mutate(Date = str_replace(Date, "X","")) 
# 
# }
# 
# 
# 
# commod_rearrange <- function(df){
#   reg<-df%>%
#     filter(stringr::str_detect(Region.Province, "^[^[:lower:]]+$"))
#   
#   prov<- df[ !(df$Region.Province  %in% reg$Region.Province), ]
#   
#   ls<-list(reg,prov) %>%
#     setNames(c("Region","Province"))%>% 
#     #transpose
#     lapply(., commod_tpose) 
#     #date fix
#     ls <-lapply(ls, function(x) filter(x, str_detect(Date, 'Annual',negate = TRUE)))%>%
#       lapply(., function(x) {x$Date<-as.Date(paste0("01.",x$Date), "%d.%Y.%B");x}) %>%
#       lapply(., function(x) {x$Date <- format(x$Date,'%m/%Y');x})
# }
# 
# #Read data
# commod <- read.csv("/home/coleen/Documents/Z_libangan/PH_retailprice_app/data/2012-based/2M4ARN08.csv",header=TRUE,
#                 skip=2,na.strings=c("",".","NA","..")) 
# 
# # get name for each group
# group_name <- unique(commod$Commodity)
# 
# commod <- commod %>%
#   mutate(Region.Province = str_remove_all(Region.Province, fixed("."))) %>%
#   group_split(Commodity,keep = FALSE) %>%
#   setNames(group_name)
# 
# 
# commod_grps<-lapply(commod, commod_rearrange)
# 

