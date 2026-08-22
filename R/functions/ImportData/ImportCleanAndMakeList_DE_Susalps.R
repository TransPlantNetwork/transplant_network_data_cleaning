####################
### DE_SUSALPS  ###
####################

source("R/functions/ImportData/community_DE_Susalps/loadcomm_SA.r")

#### Import Community ####
ImportCommunity_DE_Susalps <- function(){
  #load cover data and metadata
  community_DE_Susalps_raw <- load_cover_DE_Susalps()
  return(community_DE_Susalps_raw)
}


#### Cleaning Code ####
# Cleaning NO community data
CleanCommunity_DE_Susalps <- function(community_DE_Susalps_raw){
  dat <- community_DE_Susalps_raw %>% 
    rename(Cover = biomass, Year = year, destPlotID = turfID) %>% 
    #filter(Year >2016 ) %>% # filtering out 2016, only available for a few sites
    # only select control, local control, warm/down transplant, sites low elev to high : BT, FE, GW, EB
    mutate(Treatment = case_when(originSiteID == "BT" & destSiteID == 'BT' ~ "LocalControl",
                                 originSiteID == "EB" & destSiteID == 'BT' ~ "Warm",
                                 originSiteID == "EB" & destSiteID == 'EB' ~ "LocalControl",
                                 originSiteID == "EB" & destSiteID == 'FE' ~ "Warm",
                                 originSiteID == "EB" & destSiteID == 'GW' ~ "Warm",
                                 originSiteID == "FE" & destSiteID == 'BT' ~ "Warm",
                                 originSiteID == "FE" & destSiteID == 'FE' ~ "LocalControl",
                                 originSiteID == "GW" & destSiteID == 'BT' ~ "Warm",
                                 originSiteID == "GW" & destSiteID == 'FE' ~ "Warm",
                                 originSiteID == "GW" & destSiteID == 'GW' ~ "LocalControl")) %>% 
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>% 
    mutate(destPlotID = as.character(destPlotID),
           destBlockID = if (exists('destBlockID', where = .)){ as.character(destBlockID)} else {NA})
  
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>% #not creating other cover as biomass, doesn't exist
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    mutate(Total_Cover = sum(Cover, na.rm=T), Rel_Cover = Cover / Total_Cover) %>%
    ungroup()
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Moss', 'Dead biomass')) %>% 
                            filter(Cover > 0)
  cover <- dat2 %>% filter(SpeciesName %in% c('Moss', 'Dead biomass')) %>%
    mutate(SpeciesName=recode(SpeciesName, 'Bryophyta'= 'Moss', 'Litter %'='Dead biomass')) %>%
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% 
    group_by(UniqueID, SpeciesName) %>% 
    summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)

  return(list(comm=comm, cover=cover))
}


# Clean taxa list (add these to end of above)
CleanTaxa_DE_Susalps <- function(community_DE_Susalps){
  taxa <- unique(community_DE_Susalps$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_DE_Susalps <- function(community_DE_Susalps){
  dat <- community_DE_Susalps %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'BT'=350, 'FE' = 600, 'GW'= 860, 'EB'= 1260)),
           Gradient = 'DE_Susalps',
           Country = 'Germany',
           Longitude = as.numeric(recode(destSiteID, 'BT' = 11.581944,'FE' = 11.066260, 'GW'= 11.031010, 'EB'= 11.157730)),
           Latitude = as.numeric(recode(destSiteID, 'BT' = 49.921111, 'FE' = 47.829320, 'GW'= 47.569750, 'EB'= 47.516340)),
           YearEstablished = 2016,
           PlotSize_m2 = 0.09) %>% #circle
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country)
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_DE_Susalps <- function(){
  
  ### IMPORT DATA
  community_DE_Susalps_raw = ImportCommunity_DE_Susalps()
  
  ### CLEAN DATA SETS
  cleaned_DE_Susalps = CleanCommunity_DE_Susalps(community_DE_Susalps_raw)
  community_DE_Susalps = cleaned_DE_Susalps$comm
  cover_DE_Susalps = cleaned_DE_Susalps$cover
  meta_DE_Susalps = CleanMeta_DE_Susalps(community_DE_Susalps) 
  taxa_DE_Susalps = CleanTaxa_DE_Susalps(community_DE_Susalps)
  
  
  # Make list
  DE_Susalps = list(meta = meta_DE_Susalps,
                        community = community_DE_Susalps,
                        cover = cover_DE_Susalps,
                        taxa = taxa_DE_Susalps)
  
  return(DE_Susalps)
}

