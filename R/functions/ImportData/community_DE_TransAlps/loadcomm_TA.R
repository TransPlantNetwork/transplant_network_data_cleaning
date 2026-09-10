###### DE_TransAlps community data functions ####
#newly received data, C. Chisholm 28.05.2021

load_cover_DE_TransAlps <- function(){
  #import data
  comm <- read_delim(file = "./data/DE_TransAlps/DE_TransAlps_commdata/TransPlantNet_DACH_TransAlps_2016-2020.csv", delim=";")
  cover <- comm %>% 
    rename(year = Harvest_title, turfID = Plot_label, originSiteID = Origin_site, destSiteID = Transplant_site, SpeciesName = Species_name) %>%
    separate(year, c("year", "harvest"), ' ') %>% #all controls harvested at peak season, don't need to keep this column
    mutate(year = gsub('\\..*', '', year)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(bm = as.numeric(gsub("\\,", ".", bm))) %>%
    filter(treatment == 'ctrl') %>% #remove water treatments
    group_by(SpeciesName, turfID, year, originSiteID, destSiteID) %>%
    summarize(biomass = sum(bm, na.rm=T)) %>%
    select(originSiteID, destSiteID, year, turfID, SpeciesName, biomass)
  
  return(cover)
}

