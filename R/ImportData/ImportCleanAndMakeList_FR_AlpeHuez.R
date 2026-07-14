####################
###  FR_AlpeHuez  ###
####################



#### Import Community ####
ImportCommunity_FR_AlpeHuez <- function(){
 
   #import data
  community_FR_AlpeHuez_raw <- read_excel("data/FR_AlpeHuez/FR_AlpeHuez_commdata/2023-07_MIREN Transplant Experiment_datasheet_140405_AlpeHuezFRANCE.xlsx", sheet = "relevee")
  
  return(community_FR_AlpeHuez_raw)
  }
 
  

#### Cleaning Code ####
# Cleaning AlpeHuez community data
CleanCommunity_FR_AlpeHuez <- function(community_FR_AlpeHuez_raw){
    dat <- community_FR_AlpeHuez_raw %>% 
    select(c(site:cover.class), -plot, -species.name) %>% 
    rename(SpeciesName = `corrected name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , plotID = plot.ID , Treatment = treatment , Date = date)%>% 
     mutate(SpeciesName = sub("^(\\S*\\s+\\S+).*", "\\1", SpeciesName)) %>%     # This selects only the first two words in SpeciesName.
      filter(Treatment %in% c("HIGH_TURF", "LOW_TURF")) %>% 
      mutate(Date = case_when(
        # Numeric dates (Excel serial numbers)
        str_detect(Date, "^\\d+$") ~ as.Date(as.numeric(Date), origin = "1899-12-30"),
        
        # Handling dates with single or double digit months/days using lubridate's flexible parser
        str_detect(Date, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ mdy(Date),  # Handles mm/dd/yyyy without leading zero
        str_detect(Date, "^\\d{1,2}/\\d{1,2}/\\d{2}$") ~ mdy(Date),  # Handles mm/dd/yy for 2-digit years
        str_detect(Date, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(Date),      # ISO date yyyy-mm-dd
        
        TRUE ~ NA_Date_))%>%
      mutate(originSiteID = str_replace(Treatment, '(.*)_.*', "\\1"), 
             originSiteID = toupper(originSiteID),
             Treatment = case_when(Treatment =="LOW_TURF" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "HIGH" ~ "LocalControl"),
           Year = year(as.Date(Date, format='%Y-%m-%d')),
           Cover = recode(Cover, `+` = "0.5"),
           Cover = as.numeric(as.character(Cover))) %>% 
           select(-Date) %>% 
      mutate(UniqueID = paste(Year, originSiteID, destSiteID, plotID, sep='_')) %>% 
      mutate(destPlotID = paste(originSiteID, destSiteID, plotID, sep='_')) %>% 
      mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA) %>%
      distinct() %>% #duplicated row in original dataframe
      group_by(Year, originSiteID, destSiteID, destBlockID, destPlotID, UniqueID, Treatment, SpeciesName) %>%
      summarize(Cover = sum(Cover, na.rm=T)) %>% #had one species which occured twice in a plot, summing across
      ungroup()
    
    dat2 <- dat %>%  
      filter(!is.na(Cover)) %>%
      group_by_at(vars(-SpeciesName, -Cover)) %>%
      summarise(SpeciesName = "Other",Cover = pmax((100 - sum(Cover)), 0)) %>% 
      bind_rows(dat) %>% 
      mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
    
    comm <- dat2 %>% filter(!SpeciesName %in% c('Other', 'Bare ground')) %>% 
      filter(Cover > 0) 
    cover <- dat2 %>% filter(SpeciesName %in% c('Other', 'Bare ground')) %>% 
      select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
      rename(CoverClass=SpeciesName)
    return(list(comm=comm, cover=cover)) 
    
}

# Clean metadata

CleanMeta_FR_AlpeHuez <- function(community_FR_AlpeHuez){
  dat <- community_FR_AlpeHuez %>%
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'HIGH' = 2072, 'LOW' = 1481)),
           Gradient = 'FR_AlpeHuez',
           Country = 'France',
           Longitude = as.numeric(recode(destSiteID, 'HIGH' = 6.0554500, 'LOW' = 6.035933)),
           Latitude = as.numeric(recode(destSiteID, 'HIGH' = 45.0999830, 'LOW' = 45.08820)),
           YearEstablished = 2014,
           PlotSize_m2 = 0.25) %>% 
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country) 
  
  return(dat)
}

# Cleaning species list
CleanTaxa_FR_AlpeHuez <- function(community_FR_AlpeHuez){
  taxa <- unique(community_FR_AlpeHuez$SpeciesName)
  return(taxa)
}

#Clean trait data
CleanTrait_FR_Lautaret <- function(dat){
  dat2 <- dat %>%
    rename(SpeciesName = species, Individual_number = rep, destPlotID = plot, Trait = trait, destSiteID = climate, Treatment = treatment, Value=value) %>% 
    mutate(Country = "France",
           Trait = recode(Trait, 'F_Mass' = 'W_Mass_g',  'D_Mass' = 'Dry_Mass_g', 'L_Area' = 'Leaf_Area_cm2', 'H_Repr' = 'Plant_Rep_Height_cm',  'H_Veg' = 'Plant_Veg_Height_cm'),
           Treatment = recode(Treatment, "CP" = "LocalControl", "TP" = "Warm")) %>%
    dplyr::select(Country, destSiteID, Treatment, SpeciesName, Individual_number, Trait, Value) %>%
    filter(!is.na(Value), !is.na(SpeciesName))
  return(dat2)
}

#Clean trait data
CleanTrait_FR_AlpeHuez <- function(trait_FR_AlpeHuez_raw){
  trait <- bind_rows(trait_FR_AlpeHuez_raw[1:15,], trait_FR_AlpeHuez_raw[20:36,], .id = 'destSiteID') %>%
    mutate(Country = 'France',
           destSiteID = recode(destSiteID, '1'='LOW', '2' = 'HIGH'), 
           SpeciesName = paste(LOW, ...2, sep = ' '),
           Trait = 'Plant_Veg_Height_cm') %>%
    dplyr::select(-LOW, -`...2`) %>%
    gather(key = Individual_number, value = Value, -Country, -SpeciesName, -destSiteID, -Trait) %>%
    mutate(Individual_number = as.character(Individual_number), Value = as.numeric(Value)) %>%
    filter(!is.na(Value))
  return(trait)
}

#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_FR_AlpeHuez <- function(){
  
  ### IMPORT DATA
  community_FR_AlpeHuez_raw = ImportCommunity_FR_AlpeHuez()
  trait_FR_AlpeHuez_raw = read_excel("./data/FR_AlpeHuez/FR_AlpeHuez_traitdata/2019-HUEZ-TRAITS-SP-SELECTION.xlsx", sheet=6)
  
  ### CLEAN DATA SETS
  cleaned_FR_AlpeHuez = CleanCommunity_FR_AlpeHuez(community_FR_AlpeHuez_raw)
  community_FR_AlpeHuez = cleaned_FR_AlpeHuez$comm
  cover_FR_AlpeHuez = cleaned_FR_AlpeHuez$cover
  meta_FR_AlpeHuez = CleanMeta_FR_AlpeHuez(community_FR_AlpeHuez) 
  taxa_FR_AlpeHuez = CleanTaxa_FR_AlpeHuez(community_FR_AlpeHuez)
  trait_FR_AlpeHuez = CleanTrait_FR_AlpeHuez(trait_FR_AlpeHuez_raw)
  
  
  # Make list
  FR_AlpeHuez = list(meta = meta_FR_AlpeHuez,
                        community = community_FR_AlpeHuez,
                        cover = cover_FR_AlpeHuez,
                        taxa = taxa_FR_AlpeHuez,
                        trait = trait_FR_AlpeHuez)
  
  
  return(FR_AlpeHuez)
}

