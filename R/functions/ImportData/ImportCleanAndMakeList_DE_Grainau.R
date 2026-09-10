####################
###  DE_Grainau  ###
####################

#### Import Community ####
ImportCommunity_DE_Grainau <- function(){
  community_DE_Grainau_raw<-read_xlsx(path = "data/DE_Grainau/DE_Grainau_commdata/Vegetation 2014-17.xlsx")
  return(community_DE_Grainau_raw)
}


#### Cleaning Code ####
# Cleaning Grainau community data
CleanCommunity_DE_Grainau <- function(community_DE_Grainau_raw){
    dat <- community_DE_Grainau_raw %>% 
    select(c(site:cover.class), -plot) %>% 
      rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , destPlotID = plot.ID , Treatment = treatment , Year = year )%>% 
      mutate(originSiteID = str_replace(Treatment, '(.*)_.*', "\\1"), 
             originSiteID = toupper(originSiteID),
             Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
      mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 65.5 , `11` = 75.5 , `12` = 85.5 , `13` = 95.5 )) %>% 
      mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_'))  %>% 
      mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA) %>%
      distinct() %>% #one duplicated row in original dataframe
      group_by(Year, originSiteID, destSiteID, destBlockID, destPlotID, UniqueID, Treatment, SpeciesName) %>%
      summarize(Cover = sum(Cover, na.rm=T)) %>% #had one species which occured twice in a plot, summing across
      ungroup()
    
    dat2 <- dat %>%  
      filter(!is.na(Cover)) %>%
      group_by_at(vars(-SpeciesName, -Cover)) %>%
      summarise(SpeciesName = "Other",Cover = pmax((100 - sum(Cover)), 0)) %>% #All total cover >100, pmax rebases this to zero
      bind_rows(dat) %>% 
      mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
    
    comm <- dat2 %>% filter(!SpeciesName %in% c('Other')) %>% 
      filter(Cover > 0) 
    cover <- dat2 %>% filter(SpeciesName %in% c('Other')) %>% 
      select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
      rename(CoverClass=SpeciesName)
    return(list(comm=comm, cover=cover)) 

}

# Clean metadata

CleanMeta_DE_Grainau <- function(community_DE_Grainau){
  dat <- community_DE_Grainau %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'HIGH'=1714, 'LOW'=773)),
           Gradient = 'DE_Grainau',
           Country = 'Germany',
           Longitude = as.numeric(recode(destSiteID, 'HIGH'=11.0617667, 'LOW'=11.011217)),
           Latitude = as.numeric(recode(destSiteID, 'HIGH'=47.4414333, 'LOW'=47.4761499)),
           YearEstablished = 2013,
           PlotSize_m2 = 0.25) %>%
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country)
  
  
  return(dat)
}

# Cleaning Grainau species list
CleanTaxa_DE_Grainau <- function(community_DE_Grainau){
  taxa <- unique(community_DE_Grainau$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_DE_Grainau <- function(){
  
  
  ### IMPORT DATA
  community_DE_Grainau_raw = ImportCommunity_DE_Grainau()
  
  ### CLEAN DATA SETS
  cleaned_DE_Grainau = CleanCommunity_DE_Grainau(community_DE_Grainau_raw)
  community_DE_Grainau = cleaned_DE_Grainau$comm
  cover_DE_Grainau = cleaned_DE_Grainau$cover
  meta_DE_Grainau = CleanMeta_DE_Grainau(community_DE_Grainau) 
  taxa_DE_Grainau = CleanTaxa_DE_Grainau(community_DE_Grainau)
  
  
  # Make list
  DE_Grainau = list(meta = meta_DE_Grainau,
                     community = community_DE_Grainau,
                     cover = cover_DE_Grainau,
                     taxa = taxa_DE_Grainau)
  
  
  return(DE_Grainau)
}

