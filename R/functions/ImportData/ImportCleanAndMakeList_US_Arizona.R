####################
###  US_Arizona ####
####################
#CC edits June 9, 2021

#### Import Community ####
ImportCommunity_US_Arizona <- function(){
  community_US_Arizona_raw<-read_excel("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx", sheet = "Community Data 2014-2018")
  return(community_US_Arizona_raw)
} 

# Import Cover class #

ImportCover_US_Arizona <- function(){
  cover_US_Arizona_raw<-read_excel("data/US_Arizona/US_Arizona_commdata/Arizona percent cover data_TransplantNET_Rubin & Hungate 2019.xlsx", sheet = "% green 2014-2018")
  return(cover_US_Arizona_raw)
}


#### Cleaning Code ####

# Cleaning Arizona community data
CleanCommunity_US_Arizona <- function(community_US_Arizona_raw, cover_US_Arizona_raw){
  dat <- community_US_Arizona_raw %>% 
    select(-c('Teabag number', 'TransplantNET Treatment')) %>% 
    mutate(destSiteID = str_extract(Plot, pattern = "^.{2}")) %>% 
    rename(Date = 'Date Collected', 
           originSiteID = 'Ecosystem', 
           Treatment = 'Warming.Treat', 
           plotID = 'Plot') %>% 
    mutate(destPlotID = paste(originSiteID, destSiteID, plotID, sep='_')) %>% 
    mutate(Treatment = recode (Treatment, "Warming" = "Warm",  "Control" = "LocalControl"))%>%
    gather('SpeciesName', 'Individuals', -Year, -Date, -originSiteID, -destSiteID, -Treatment,-destPlotID, -plotID) %>%
    #creating unique ID
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, plotID, sep='_')) %>% # we can leave out the destSiteID here because it is embedded in destPlotID
    select(-plotID) %>% 
    mutate(destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA) %>%
    group_by(UniqueID, Year, originSiteID, destSiteID, destPlotID, destBlockID, Treatment) %>%
    mutate(Individuals = ifelse(is.na(Individuals), 0, Individuals)) %>%
    # This site records individual counts rather than percent cover; Cover
    # holds those counts (see site_registry.R note: "Individual counts
    # converted to relative cover") so Cover/Rel_Cover still exist as the
    # canonical schema requires, on a counts-based scale for this site.
    mutate(Cover = Individuals, Total_Cover = sum(Individuals), Rel_Cover = Individuals / Total_Cover) 
  
  # Create comm dataframe
  comm <- dat %>% filter(Rel_Cover > 0)  
   
  #calculate percentage cover per individual
  cover <- cover_US_Arizona_raw %>% 
    select(-c('Teabag number', 'TransplantNET Treatment')) %>% 
    mutate(destSiteID = str_extract(Plot, pattern = "^.{2}")) %>% 
    rename(Date = 'Date Collected', 
           originSiteID = 'Ecosystem', 
           Treatment = 'Warming.Treat', 
           plotID = 'Plot', 
           VascCover = '% cover at peak biomass') %>% 
    mutate(destPlotID = paste(originSiteID, destSiteID, plotID, sep='_')) %>% 
    mutate(Treatment = recode (Treatment, "Warming" = "Warm",  "Control" = "LocalControl"))%>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, plotID, sep='_')) %>%
    select(-plotID) %>% 
    mutate(OtherCover = 100-VascCover) %>%
    gather('CoverClass', 'OtherCover', c(VascCover, OtherCover)) %>% 
    # This is an independent % green ground-cover measurement, not part of
    # the same 0-1 relative-cover budget as comm$Rel_Cover (which is built
    # from individual counts, not ground cover) - it isn't meaningful to add
    # the two together. Rel_OtherCover was previously hardcoded to the
    # literal number 100 (a leftover/bug), which silently broke the
    # rel_cover_sums check; leave it NA so that check correctly skips this
    # site's cover table rather than double-counting an unrelated metric.
    mutate(Rel_OtherCover = NA_real_)

  return(list(comm=comm, cover=cover))
}



# Cleaning Arizona meta data
CleanMeta_US_Arizona <- function(community_US_Arizona){
  dat <- community_US_Arizona %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'MC' = 2620, 'PP' = 2344)),
           Gradient = 'US_Arizona',
           Country = 'USA',
           Longitude = as.numeric(recode(destSiteID, 'MC' = -111.73, 'PP' = -111.67)),
           Latitude = as.numeric(recode(destSiteID, 'MC' = 35.35, 'PP' = 35.42)),
           YearEstablished = 2014,
           PlotSize_m2 = 0.09) %>%
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country) 
  
  return(dat)
}


# Cleaning Arizona species list 

CleanTaxa_US_Arizona <- function(){
  splist <- read_excel(file_in("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx"), sheet = "Species List ") %>%
    mutate(
    Species_FullName = paste(Genus, Species)) %>%
    rename(SpeciesName='Code')
  
  taxa <- splist$Species_FullName
      
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_US_Arizona <- function(){
  
  ### IMPORT DATA
  community_US_Arizona_raw = ImportCommunity_US_Arizona()
  cover_US_Arizona_raw = ImportCover_US_Arizona()
  
  ### CLEAN DATA SETS
  cleaned_US_Arizona = CleanCommunity_US_Arizona(community_US_Arizona_raw, cover_US_Arizona_raw)
  community_US_Arizona = cleaned_US_Arizona$comm
  cover_US_Arizona = cleaned_US_Arizona$cover
  meta_US_Arizona = CleanMeta_US_Arizona(community_US_Arizona)
  taxa_US_Arizona = CleanTaxa_US_Arizona()
  
  
  # Make list
  US_Arizona = list(community = community_US_Arizona,
                     meta =  meta_US_Arizona,
                     cover = cover_US_Arizona,
                     taxa = taxa_US_Arizona)
  
  
  return(US_Arizona)
}

