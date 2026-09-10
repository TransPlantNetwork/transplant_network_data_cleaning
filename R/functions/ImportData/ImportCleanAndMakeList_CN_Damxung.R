####################
### CN_Damxung  ###
####################

#### Import Community ####

#This lists all the different files from years 2013-2018 and combines them
ImportCommunity_CN_Damxung <- function(){
  files <- list.files("data/CN_Damxung/CN_Damxung_commdata/")
  community_CN_Damxung_raw <- map_df(files, ~ read_excel(paste0("data/CN_Damxung/CN_Damxung_commdata/", .)))
  return(community_CN_Damxung_raw)
} 


#### Cleaning Code ####
# Cleaning Damxung community data
CleanCommunity_CN_Damxung <- function(community_CN_Damxung_raw){
    dat <- community_CN_Damxung_raw %>% 
    select(c(SITE:`cover class`), -PLOT) %>% 
    rename(SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , destPlotID = PLOT.ID , Treatment = TREATMENT , Year = YEAR)%>% 
    mutate(originSiteID = str_replace(Treatment, '(.*)_.*', "\\1"), 
           originSiteID = toupper(originSiteID),
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
      #Convert cover classes to percent values (taking mid-point)
      mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 80 )) %>% 
      mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>% 
      mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA)
    
    dat2 <- dat %>%  
      filter(!is.na(Cover)) %>%
      group_by_at(vars(-SpeciesName, -Cover)) %>%
      summarise(SpeciesName = "Other", Cover = pmax((100 - sum(Cover)), 0)) %>% #All total cover >100, pmax rebases this to zero
      bind_rows(dat) %>% 
      mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
    
    comm <- dat2 %>% filter(!SpeciesName %in% c('Other', 'Litter', 'litter', 'moss', 'rock', 'bareground', 'Bare', 'Moss', 'Rock')) %>% 
      filter(Cover>0) 
    cover <- dat2 %>% filter(SpeciesName %in% c('Other', 'Litter', 'litter', 'moss', 'rock', 'bareground', 'Bare', 'Moss', 'Rock')) %>% 
      mutate(SpeciesName=recode(SpeciesName, 'litter'="Litter", "bareground"='Bareground', "Bare"='Bareground', 'moss'= 'Moss', 'rock'='Rock')) %>%
      select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
      rename(CoverClass=SpeciesName)
    return(list(comm=comm, cover=cover)) 
}

# Clean metadata
CleanMeta_CN_Damxung <- function(community_CN_Damxung){
 dat <- community_CN_Damxung %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'HIGH' = 4800, 'LOW' = 4313)),
           Gradient = "CN_Damxung",
           Country = "China",
           Longitude = as.numeric(recode(destSiteID, 'HIGH' = 91.05491, 'LOW' = 91.0646299)),
           Latitude = as.numeric(recode(destSiteID, 'HIGH' = 30.531410, 'LOW' = 30.4971)),
           YearEstablished = 2013,
           PlotSize_m2 = 0.25) %>%
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country)
  
  return(dat)
}

# Cleaning Damxung species list
CleanTaxa_CN_Damxung <- function(community_CN_Damxung){
  taxa<-unique(community_CN_Damxung$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_CN_Damxung <- function(){
  
  ### IMPORT DATA
  community_CN_Damxung_raw = ImportCommunity_CN_Damxung()
  
  ### CLEAN DATA SETS
  cleaned_CN_Damxung = CleanCommunity_CN_Damxung(community_CN_Damxung_raw)
  community_CN_Damxung = cleaned_CN_Damxung$comm
  cover_CN_Damxung = cleaned_CN_Damxung$cover
  meta_CN_Damxung = CleanMeta_CN_Damxung(community_CN_Damxung) 
  taxa_CN_Damxung = CleanTaxa_CN_Damxung(community_CN_Damxung)
  
  
  # Make list
  CN_Damxung = list(meta = meta_CN_Damxung,
                    community = community_CN_Damxung,
                    cover = cover_CN_Damxung,
                    taxa = taxa_CN_Damxung)
  
  return(CN_Damxung)
}

