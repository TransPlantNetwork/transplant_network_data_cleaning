####################
###  SE_Abisko  ####
####################



#### Import Community ####
ImportCommunity_SE_Abisko <- function(){

  community_SE_Abisko_raw <- read_excel("data/SE_Abisko/SE_Abisko_commdata/Vegetation data Abisko transplantation experiment (2012 + 2013 + 2014 + 2015)_for Chelsea.xlsx", sheet = "Vegetation control treatments")
  
  return(community_SE_Abisko_raw)
}



#### Cleaning Code ####
# Cleaning Abisko community data
CleanCommunity_SE_Abisko <- function(community_SE_Abisko_raw){
  dat <- community_SE_Abisko_raw %>% 
    select(-c(El, Ori, Yr, `Spot ID`, Tag, `Bare soil`:Mosses)) %>% 
    rename(originSiteID = 'Elevation of origin', destSiteID = 'Transplant elevation' , destBlockID = Block , destPlotID = `Core ID` , Treatment = Treatment) %>% 
    mutate(Treatment = case_when(originSiteID =="High" & destSiteID == "High" ~ "LocalControl" , 
                                 originSiteID =="Mid" & destSiteID == "Mid" ~ "LocalControl" ,
                                 originSiteID =="Low" & destSiteID == "Low" ~ "LocalControl" , 
                                 originSiteID =="High" & destSiteID == "Low" ~ "Warm" ,
                                 originSiteID =="High" & destSiteID == "Mid" ~ "Warm" ,
                                 originSiteID =="Mid" & destSiteID == "Low" ~ "Warm" , 
                                 originSiteID =="Low" & destSiteID == "Mid" ~ "Cold" , 
                                 originSiteID =="Low" & destSiteID == "High" ~ "Cold" ,
                                 originSiteID =="Mid" & destSiteID == "High" ~ "Cold")) %>%
    gather(SpeciesName, 'Cover', -destSiteID, -originSiteID, -destPlotID, -destBlockID, -Treatment, -Year) %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>% 
    mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA)
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other",Cover = pmax((100 - sum(Cover)), 0)) %>% 
    bind_rows(dat) %>% 
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other', 'Sph spe'))  %>% #I removed moss but kept in ferns and clubmosses (large portion of community data and IDed to species)
    filter(Cover > 0) 
  cover <- dat2 %>% filter(SpeciesName %in% c('Other', 'Sph spe')) %>% 
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover)) 
}

# Clean metadata
#Paul wasn't particular about these elevations, check this!
CleanMeta_SE_Abisko <- function(community_SE_Abisko){
  dat <- community_SE_Abisko %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'High' = 1000, 'Mid' = 690, 'Low' = 500)),
           Gradient = 'SE_Abisko',
           Country = 'Sweden',
           Longitude = as.numeric(recode(destSiteID, 'High' = 19.0921899, 'Mid' = 19.17353299, 'Low' = 19.190148)),
           Latitude = as.numeric(recode(destSiteID, 'High' = 68.29267099, 'Mid' = 68.294066999, 'Low' = 68.300843)),
           YearEstablished = 2012,
           PlotSize_m2 = 0.0177) %>%
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country) 
  
  return(dat)
}

# Cleaning species list (full name found in other sheet of excel)
CleanTaxa_SE_Abisko <- function(){
  splist <- read_excel("data/SE_Abisko/SE_Abisko_commdata/Vegetation data Abisko transplantation experiment (2012 + 2013 + 2014 + 2015)_for Chelsea.xlsx", sheet = "Species list 2012")
  taxa <- c(splist$Name...2, splist$Name...4)
  taxa <- taxa[!is.na(taxa)]
  taxa <- taxa[!taxa %in% c('Lichen', 'Litter', 'Moss')] 
  
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_SE_Abisko <- function(){
  
  ### IMPORT DATA
  community_SE_Abisko_raw = ImportCommunity_SE_Abisko()
  
  ### CLEAN DATA SETS
  cleaned_SE_Abisko = CleanCommunity_SE_Abisko(community_SE_Abisko_raw)
  community_SE_Abisko = cleaned_SE_Abisko$comm
  cover_SE_Abisko = cleaned_SE_Abisko$cover
  meta_SE_Abisko = CleanMeta_SE_Abisko(community_SE_Abisko) 
  taxa_SE_Abisko = CleanTaxa_SE_Abisko()
  
  
  # Make list
  SE_Abisko = list(meta = meta_SE_Abisko,
                    community = community_SE_Abisko,
                    cover = cover_SE_Abisko,
                    taxa = taxa_SE_Abisko)
  
  return(SE_Abisko)
}

