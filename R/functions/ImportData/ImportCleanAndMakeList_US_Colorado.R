####################
### US_Colorado  ###
####################

#### Import Community ####
ImportCommunity_US_Colorado <- function(){
  community_US_Colorado_raw<-read.csv(file = "./data/US_Colorado/US_Colorado_commdata/Final_IntensiveCover_2018-2023_RMBLTransplant.csv")
  return(community_US_Colorado_raw)
} 


#### Cleaning Code ####
# Cleaning Colorado community data
CleanCommunity_US_Colorado <- function(community_US_Colorado_raw){
  dat <- community_US_Colorado_raw %>% 
    dplyr::select(year, turfID, species, percentCover)%>%
    rename(SpeciesName = species, Cover = percentCover, Year = year) %>% 
    mutate(Year = as.numeric(Year),
           Cover = as.numeric(Cover)) %>%
    filter(!is.na(Cover))%>%
    mutate(destSiteID = substr(turfID, 1, 2),
           destBlockID = substr(turfID, 3, 3),
           Treatment = substr(turfID, 7, 8),
           originSiteID = substr(turfID, nchar(turfID)-4, nchar(turfID)-3),
           originBlockID = substr(turfID, nchar(turfID)-2, nchar(turfID)-2)) %>% 
      mutate(Treatment = recode(Treatment, "c1" = "Cold", "c2" = "Cold", "w1" = "Warm", "w2" = "Warm", "nu" = "NettedControl", "u_" = "Control", "ws" = "LocalControl")) %>% 
    rename(destPlotID = turfID) %>% 
    filter(!is.na(Treatment))%>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destBlockID, destPlotID, sep='_'), SpeciesName = recode(SpeciesName, 'Rock' = "rock", 'Moss' = "moss")) %>% 
    mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA) %>%
    distinct() #removes Viola nuttallii which was doubled in 2019
    #filter(!destBlockID %in% c("6")) #filter out new turfs added end of 2018 (though this could be useful for single year of data for non-temporal analyses)
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other", Cover = pmax((100 - sum(Cover)), 0)) %>% 
    bind_rows(dat) %>% 
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  #dat2 %>% filter(Total_Cover<100) #There are plots with <100, so we need to create an Other cover class
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other', 'Bare Soil', 'Bare soil', 'bare soil', 'Litter', 'LItter', 'litter', 'rock', 'Rock', 'moss')) %>% 
    filter(Cover > 0)
  cover <- dat2 %>% filter(SpeciesName %in% c('Other', 'Bare Soil', 'Bare soil', 'bare soil', 'Litter', 'LItter', 'litter', 'rock', 'Rock', 'moss')) %>%
    mutate(SpeciesName=recode(SpeciesName, "Bare Soil"='Bareground', "Bare soil"='Bareground', "bare soil"='Bareground', 'LItter'='Litter', 'litter'='Litter', 'moss'= 'Moss', 'rock'='Rock')) %>%
    select(Year, destSiteID, destPlotID, SpeciesName, Cover, Rel_Cover) %>% 
    group_by(Year, destSiteID, destPlotID, SpeciesName) %>% 
    summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  
  return(list(comm=comm, cover=cover))
    
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_US_Colorado <- function(community_US_Colorado){
taxa <- unique(community_US_Colorado$SpeciesName)
return(taxa)
}

# Clean metadata
CleanMeta_US_Colorado <- function(community_US_Colorado){
  dat <- community_US_Colorado %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'um' = 2900, 'pf'= 3200, 'mo' = 3300)),
           Gradient = 'US_Colorado',
           Country = 'USA',
           Longitude =  as.numeric(recode(destSiteID, 'um' = -107.010180, 'pf'= -107.0326199, 'mo' = -107.04908)),
           Latitude =  as.numeric(recode(destSiteID, 'um' = 38.9324799, 'pf'= 38.9324799, 'mo' = 38.9727400)),
           YearEstablished = 2017,
           PlotSize_m2 = 0.25) %>%
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country) 
  
  return(dat)
}

#Clean trait data
CleanTrait_US_Colorado <- function(trait_US_Colorado_raw){
  trait_plot_site <- trait_US_Colorado_raw %>%
    dplyr::select(-destPlotID)%>%
    rename(destPlotID = turfID, SpeciesName = species, Individual_number = individualNumber, Year = year, Treatment = treatmentAbbrev) %>%
    mutate(destSiteID = substr(destPlotID, 1, 2))%>%
    mutate(destSiteID = case_when(is.na(destSiteID) & originSite == "Monument" ~ "mo",
                                  is.na(destSiteID) & originSite == "Pfeiler" ~ "pf",
                                  is.na(destSiteID) & originSite == "Upper Montane"~"um",
                                  .default = destSiteID))%>%
    mutate(Treatment = recode(Treatment, "c1" = "Cold", "c2" = "Cold", "w1" = "Warm", "w2" = "Warm", "nu" = "NettedControl", "u" = "Control", "ws" = "LocalControl")) %>% 
    filter(!(is.na(Treatment)))%>%
    mutate(Year = as.numeric(Year))%>%
    rename(Wet_Mass_g = "wetMassg", 
           Dry_Mass_g = "dryMassg", 
           Leaf_Area_cm2 = "leafAreacm2", 
           Plant_Veg_Height_cm = "vegHeightcm",
           SLA_cm2_g = "SLA",  
           Leaf_Thickness_Ave_mm = "thickAvgmm") %>%
    mutate(Country = "USA",
           Gradient = "US_Colorado") %>%
    dplyr::select(Country, Gradient, destSiteID, destPlotID, Treatment, Year, SpeciesName, Individual_number, Wet_Mass_g, Dry_Mass_g, Leaf_Area_cm2, SLA_cm2_g, LDMC, Leaf_Thickness_Ave_mm, Plant_Veg_Height_cm) %>%
    mutate(Plant_Veg_Height_cm = as.numeric(Plant_Veg_Height_cm)) %>%
    pivot_longer(names_to = "Trait", values_to = "Value", cols = c(Wet_Mass_g, Dry_Mass_g, Leaf_Area_cm2, SLA_cm2_g, LDMC, Leaf_Thickness_Ave_mm, Plant_Veg_Height_cm)) %>%
    mutate(UniqueID = paste(Year, destPlotID, sep='_')) %>% 
    mutate(PlantID = paste(UniqueID, Individual_number, SpeciesName, sep = "_"))%>%
    mutate(Individual_number = as.character(Individual_number), Value = as.numeric(Value)) %>%
    filter(!is.na(Value))%>%
    select(Country, Gradient, destSiteID, Year, destPlotID, UniqueID, Treatment, SpeciesName, Individual_number, PlantID, Trait, Value) 
  return(trait_plot_site)
}

#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_US_Colorado <- function(){
  
  ### IMPORT DATA
  community_US_Colorado_raw = ImportCommunity_US_Colorado()
  trait_US_Colorado_raw = read.csv("./data/US_Colorado/US_Colorado_traitdata/Traits_2017-2021_RMBLTransplant.csv")
  
  ### CLEAN DATA SETS
  cleaned_US_Colorado = CleanCommunity_US_Colorado(community_US_Colorado_raw)
  community_US_Colorado = cleaned_US_Colorado$comm
  cover_US_Colorado = cleaned_US_Colorado$cover
  meta_US_Colorado = CleanMeta_US_Colorado(community_US_Colorado) 
  taxa_US_Colorado = CleanTaxa_US_Colorado(community_US_Colorado)
  trait_US_Colorado = CleanTrait_US_Colorado(trait_US_Colorado_raw)
  
  
  # Make list
  US_Colorado = list(meta = meta_US_Colorado,
                   community = community_US_Colorado,
                   cover = cover_US_Colorado,
                   taxa = taxa_US_Colorado,
                   trait = trait_US_Colorado)
  
  return(US_Colorado)
}

