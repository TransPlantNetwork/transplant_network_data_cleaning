#####################
#### CH_CALANDA2  ####
#####################

source("R/functions/ImportData/community_CH_Calanda2/load_comm_cal2.r")

#### Import Community ####
ImportCommunity_CH_Calanda2 <- function(){
  
  #load cover data and metadata
  community_CH_Calanda2_raw <- load_cover_CH_Calanda2()
  
  return(community_CH_Calanda2_raw)
}

#Remove alpine focals (is.focal column)
#### Cleaning Code ####
# Cleaning Calanda community data
CleanCommunity_CH_Calanda2 <- function(community_CH_Calanda2_raw) {
  dat <- community_CH_Calanda2_raw %>% 
    # create transplant categories (Nes## 01/03 is HL, Nes ## 08/09 is LL)
    mutate(originSiteID = case_when(site == "Cal" ~ "Cal",
                                    plot %in% c(1,3) & site == "Nes" ~ 'Cal',
                                    plot %in% c(8,9) & site == "Nes" ~ 'Nes'),
           Treatment = case_when(plot %in% c(1,3) & site == "Nes" ~ "Warm", 
                                 plot %in% c(8,9) & site == "Nes" ~ "LocalControl",
                                  site == "Cal" ~ "LocalControl")) %>%
    select(-plot) %>%
    rename(destSiteID = site, Cover = cover, Year = year, SpeciesName = species, destPlotID = plot_id, destBlockID = block) %>% 
    mutate(Cover=as.numeric(Cover)) %>%
    filter(!is.na(Cover)) %>%
    # only select control, local control, warm/down transplant
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_'))  %>% 
    mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA) %>%
    select(Year, originSiteID, destSiteID, Treatment, destBlockID, destPlotID, UniqueID, SpeciesName, Cover) 
  
  dat2 <- dat %>%  
    group_by(UniqueID) %>%
    mutate(Total_Cover = sum(Cover, na.rm=T), Rel_Cover = Cover / Total_Cover) %>%
    ungroup()
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Moss Group', 'Lychen Group', 'Mushroom Group', 'Cetraria islandica')) %>%
    filter(Cover > 0)  
  cover <- dat2 %>% filter(SpeciesName %in% c('Moss Group', 'Lychen Group', 'Mushroom Group', 'Cetraria islandica')) %>% 
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover))
}


# Clean taxa list (add these to end of above)
CleanTaxa_CH_Calanda2 <- function(community_CH_Calanda2) {
  taxa <- unique(community_CH_Calanda2$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_CH_Calanda2 <- function(community_CH_Calanda2) {
  dat <- community_CH_Calanda2 %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'Cal'=2000, 'Nes'=1400)),
           Gradient = "CH_Calanda2",
           Country = "Switzerland",
           Longitude = as.numeric(recode(destSiteID, 'Cal'=9.48939, 'Nes'=9.49013)),
           Latitude = as.numeric(recode(destSiteID, 'Cal'=46.88778, 'Nes'=46.86923)),
           YearEstablished = 2016,
           PlotSize_m2 = 1) %>%
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country)
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_CH_Calanda2 <- function(){
  
  ### IMPORT DATA
  community_CH_Calanda2_raw = ImportCommunity_CH_Calanda2()
  
  ### CLEAN DATA SETS
  cleaned_CH_Calanda2 = CleanCommunity_CH_Calanda2(community_CH_Calanda2_raw)
  community_CH_Calanda2 = cleaned_CH_Calanda2$comm
  cover_CH_Calanda2 = cleaned_CH_Calanda2$cover
  meta_CH_Calanda2 = CleanMeta_CH_Calanda2(community_CH_Calanda2) 
  taxa_CH_Calanda2 = CleanTaxa_CH_Calanda2(community_CH_Calanda2)
  
  
  # Make list
  CH_Calanda2 = list(meta = meta_CH_Calanda2,
                    community = community_CH_Calanda2,
                    cover = cover_CH_Calanda2,
                    taxa = taxa_CH_Calanda2)
  
  return(CH_Calanda2)
}

