#####################
#### CH_CALANDA  ####
#####################

source("R/functions/ImportData/community_CH_Calanda/load_comm_cal.r")

#### Import Community ####
ImportCommunity_CH_Calanda <- function(){

  #load cover data and metadata
  community_CH_Calanda_raw <- load_cover_CH_Calanda()
  
  return(community_CH_Calanda_raw)
}

#### Cleaning Code ####
# Cleaning Calanda community data
CleanCommunity_CH_Calanda <- function(community_CH_Calanda_raw) {
  dat <- community_CH_Calanda_raw %>% 
    mutate(Treatment = case_when(Treatment == "veg_away" & Site %in% c("Cal", "Nes") ~ "Warm", 
                                 Treatment == "veg_home" & Site %in% c("Nes","Pea","Cal") ~ "LocalControl")) %>%
    rename(destSiteID = Site, originSiteID = turf_type, Year = year, SpeciesName = Species_Name, destPlotID = plot_id) %>% 
    #add block ID 
    rename(destBlockID=Block) %>% 
    #add uniqueID
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_'))  %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("LocalControl", "Warm")) %>% 
    mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA) %>%
    filter(destPlotID != "CalNA.NA") %>%
    select(Year, Treatment, originSiteID, destSiteID, destBlockID, destPlotID, UniqueID, SpeciesName, Cover) %>%
    # Fix issue of double entries of species in two plots (Cal03.04, x2 in 2017 and Cal05.04, x2 in 2014). These look like duplicates
    distinct()
  
  dat2 <- dat %>%  
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other", Cover = pmax((100 - sum(Cover)), 0)) %>%
    bind_rows(dat) %>% 
    mutate(Total_Cover = sum(Cover, na.rm=T), Rel_Cover = Cover / Total_Cover) %>%
    ungroup()

  comm <- dat2 %>% filter(!SpeciesName %in% c('Other', 'Cetraria islandica')) %>%
    filter(Cover > 0)  
  cover <- dat2 %>% filter(SpeciesName %in% c('Other', 'Cetraria islandica')) %>% 
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover))
}


# Clean taxa list (add these to end of above)
CleanTaxa_CH_Calanda <- function(community_CH_Calanda) {
  taxa <- unique(community_CH_Calanda$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_CH_Calanda <- function(community_CH_Calanda) {
  dat <- community_CH_Calanda %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'Pea'=2800, 'Cal'=2000, 'Nes'=1400)),
           Gradient = "CH_Calanda",
           Country = "Switzerland",
           Longitude = as.numeric(recode(destSiteID, 'Pea'=9.47031, 'Cal'=9.48939, 'Nes'=9.49013)),
           Latitude = as.numeric(recode(destSiteID, 'Pea'=46.89326, 'Cal'=46.88778, 'Nes'=46.86923)),
           YearEstablished = 2012,
           PlotSize_m2 = 0.75) %>%
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country)
  
  return(dat)
}

#Clean trait data
CleanTrait_CH_Calanda <- function(trait_CH_Calanda_raw){
  trait <- trait_CH_Calanda_raw %>%
    rename(SpeciesName = species, Individual_number = individual, destSiteID = site, Collector = collector, PlantID = unique.code) %>% #not including Year/date because clear issues in file
    rename(Wet_Mass_g = leaf.fresh.weight, Dry_Mass_g = leaf.dry.weight, Leaf_Area_cm2 = leaf.area, Plant_Veg_Height_cm = height.veg.stretch, Plant_Rep_Height_cm = height.rep.stretch) %>%
    select(destSiteID, SpeciesName,Individual_number, PlantID, Plant_Veg_Height_cm, Plant_Rep_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Area_cm2) %>%
    mutate_all(~gsub(',','', .)) %>%
    mutate(Country = "Switzerland",
           Gradient = "CH_Calanda",
           destSiteID = recode(destSiteID, 'PEAK' = 'Pea', 'CAL' = 'Cal', 'NES' = 'Nes'),
           Plant_Veg_Height_cm =   gsub("[^0-9.-]", "", Plant_Veg_Height_cm),
           Plant_Veg_Height_cm = as.numeric(as.character(Plant_Veg_Height_cm))/10,
           Plant_Rep_Height_cm =   gsub("[^0-9.-]", "", Plant_Rep_Height_cm),
           Plant_Rep_Height_cm = as.numeric(as.character(Plant_Rep_Height_cm))/10,
           Dry_Mass_g =   gsub("[^0-9.-]", "", Dry_Mass_g),
           Dry_Mass_g = as.numeric(as.character(Dry_Mass_g)),
           Wet_Mass_g =   gsub("[^0-9.-]", "", Wet_Mass_g),
           Wet_Mass_g =   gsub("\\..", ".", Wet_Mass_g), #a .. present in data
           Wet_Mass_g =   gsub("NA", NA, Wet_Mass_g),
           Wet_Mass_g = as.numeric(as.character(Wet_Mass_g)),
           Leaf_Area_cm2 =   gsub("[^0-9.-]", "", Leaf_Area_cm2),
           Leaf_Area_cm2 =   gsub("0", "NA", Leaf_Area_cm2),
           Leaf_Area_cm2 =   gsub("NA", NA, Leaf_Area_cm2),
           Leaf_Area_cm2 = as.numeric(as.character(Leaf_Area_cm2)),
           LDMC = ifelse(!is.na(Dry_Mass_g)&!is.na(Wet_Mass_g), Dry_Mass_g/Wet_Mass_g, NA),
           SLA_cm2_g = ifelse(!is.na(Leaf_Area_cm2)&!is.na(Dry_Mass_g), Leaf_Area_cm2/Dry_Mass_g, NA)) %>%
    dplyr::select(Country, Gradient, destSiteID, SpeciesName, Individual_number, PlantID, Plant_Veg_Height_cm, Plant_Rep_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Area_cm2, SLA_cm2_g, LDMC) %>%
    pivot_longer(cols = Plant_Veg_Height_cm:LDMC, names_to = "Trait", values_to = "Value")%>%
    mutate(PlantID = paste(destSiteID, Individual_number, SpeciesName, sep = "_"))%>%
    filter(!is.na(Value))
  return(trait)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_CH_Calanda <- function(){
  
  ### IMPORT DATA
  community_CH_Calanda_raw = ImportCommunity_CH_Calanda()
  trait_CH_Calanda_raw = read_excel("./data/CH_Calanda/CH_Calanda_traitdata/funct_traits(101114)Excel.xlsx", sheet=1, na = "NA")
  
  ### CLEAN DATA SETS
  cleaned_CH_Calanda = CleanCommunity_CH_Calanda(community_CH_Calanda_raw)
  community_CH_Calanda = cleaned_CH_Calanda$comm
  cover_CH_Calanda = cleaned_CH_Calanda$cover
  meta_CH_Calanda = CleanMeta_CH_Calanda(community_CH_Calanda) 
  taxa_CH_Calanda = CleanTaxa_CH_Calanda(community_CH_Calanda)
  trait_CH_Calanda = CleanTrait_CH_Calanda(trait_CH_Calanda_raw)
 
  
  # Make list
  CH_Calanda = list(meta = meta_CH_Calanda,
                  community = community_CH_Calanda,
                  cover = cover_CH_Calanda,
                  taxa = taxa_CH_Calanda,
                  trait = trait_CH_Calanda)
  
  return(CH_Calanda)
}

