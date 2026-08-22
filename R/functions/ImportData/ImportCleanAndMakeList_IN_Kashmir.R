####################
### IN_Kashmir  ###
####################

#### Import Community ####
ImportCommunity_IN_Kashmir <- function(){
  community_IN_Kashmir_14<-read_xlsx(path ="data/IN_Kashmir/IN_Kashmir_commdata/background 2014.xlsx", range = "A1:K158")
  community_IN_Kashmir_15<-read_xlsx(path ="data/IN_Kashmir/IN_Kashmir_commdata/2015.xlsx", range = "A1:J178") %>% mutate(YEAR=2015) #to account for dragging issues in their dataset, added extra years accidentally
  community_IN_Kashmir_raw <- bind_rows(community_IN_Kashmir_14, community_IN_Kashmir_15)
  return(community_IN_Kashmir_raw)
} 


#### Cleaning Code ####
# Cleaning Kashmir community data
CleanCommunity_IN_Kashmir <- function(community_IN_Kashmir_raw){
    dat <- community_IN_Kashmir_raw %>% 
    mutate(destPlot = paste (REGION, SITE, BLOCK, PLOT, sep = ".")) %>% 
    select(c(SITE:`cover class`), destPlot, -PLOT, PLOT.ID) %>% 
      rename(Collector = collector, SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , Treatment = TREATMENT , Year = YEAR)%>%
      mutate(SpeciesName = recode(SpeciesName, "Fragaria spp" = "Fragaria sp." , "Ranunculus spp" = "Ranunculus sp.", "Pinus spp" = "Pinus sp.", "CYANODON dACTYLON" = "Cyanodon dactylon", "Hordeum spp" = "Hordeum sp.", "Rubus spp" = "Rubus sp.", "Cyanodondactylon" = "Cyanodon dactylon")) %>% 

      mutate(originSiteID = str_replace(Treatment, '(.*)_.*', "\\1"), 
           originSiteID = toupper(originSiteID),
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
      mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 70 , `11` = 90)) %>% 
     # Create new destplotID and UniqueID)     
      mutate(destPlotID = paste(originSiteID, destSiteID, destBlockID, sep='_')) %>% 
      mutate(UniqueID = paste(destPlotID, Year, sep='_')) %>% 
      mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA)  %>%
      distinct() %>% #one duplicated row in original dataframe
      group_by(Year, originSiteID, destSiteID, destBlockID, destPlotID, UniqueID, Treatment, SpeciesName) %>%
      summarize(Cover = sum(Cover, na.rm=T)) %>% #had one species which occurred twice in a plot, summing across
      ungroup()

    
 dat2<- dat %>%  
      filter(!is.na(Cover)) %>%
      group_by_at(vars(-SpeciesName, -Cover)) %>%
      summarise(SpeciesName = "Other",Cover = pmax((100 - sum(Cover)), 0)) %>% 
      bind_rows(dat) %>% 
      mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)  
 
    
    comm <- dat2 %>% filter(!SpeciesName %in% c('Other')) %>% 
      filter(Cover > 0) 
    cover <- dat2 %>% filter(SpeciesName %in% c('Other')) %>% 
      select(destPlotID, SpeciesName, Cover, Rel_Cover) %>% group_by(destPlotID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
      rename(CoverClass=SpeciesName)
    return(list(comm=comm, cover=cover)) 
    return(dat)
}

# Clean metadata

CleanMeta_IN_Kashmir <- function(community_IN_Kashmir){
  dat <- community_IN_Kashmir %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'HIGH' = 2684, 'LOW' = 1951)),
           Gradient = 'IN_Kashmir',
           Country = 'India',
           Longitude = as.numeric(recode(destSiteID, 'HIGH' = 74.39961099, 'LOW' = 74.832931)),
           Latitude = as.numeric(recode(destSiteID, 'HIGH' = 34.050736, 'LOW' = 34.13218899)),
           YearEstablished = 2013,
           PlotSize_m2 = 0.25) %>% 
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country) 
  
  return(dat)
}

# Cleaning Kashmir species list
CleanTaxa_IN_Kashmir <- function(community_IN_Kashmir){
  taxa <- unique(community_IN_Kashmir$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_IN_Kashmir <- function(){
  
  ### IMPORT DATA
  community_IN_Kashmir_raw = ImportCommunity_IN_Kashmir()
 
  
  ### CLEAN DATA SETS
  ## IN_Kashmir
  
  cleaned_IN_Kashmir = CleanCommunity_IN_Kashmir(community_IN_Kashmir_raw)
  community_IN_Kashmir = cleaned_IN_Kashmir$comm
  cover_IN_Kashmir = cleaned_IN_Kashmir$cover
  meta_IN_Kashmir = CleanMeta_IN_Kashmir(community_IN_Kashmir) 
  taxa_IN_Kashmir = CleanTaxa_IN_Kashmir(community_IN_Kashmir)

  
  
  # Make list
  IN_Kashmir = list (community = community_IN_Kashmir,
                     meta =  meta_IN_Kashmir,
                     cover = cover_IN_Kashmir,
                    taxa = taxa_IN_Kashmir)
  
  return(IN_Kashmir)
}

