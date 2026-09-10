####################
### IT_MatschMazia2  ###
####################

source("R/functions/ImportData/community_IT_MatschMazia/loadcomm_IT.r")

#### Import Community ####
ImportCommunity_IT_MatschMazia2 <- function(){
  community_IT_MatschMazia2_raw<-load_cover_IT_MatschMazia2()
  return(community_IT_MatschMazia2_raw)
} 


#### Cleaning Code ####
# Cleaning Lautaret community data
CleanCommunity_IT_MatschMazia2 <- function(community_IT_MatschMazia2_raw){
  dat <- community_IT_MatschMazia2_raw %>% 
    rename(Year=year, Elevation=elevation) %>%
    gather(SpeciesName, Cover, -UniqueID, -Elevation, -treat, -Year) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\_', ' ', SpeciesName)) %>%
    mutate(destSiteID = case_when(Elevation == 1500 ~ "Low",
                                    Elevation == 1950 ~ "High"),
            Treatment = case_when(treat == "originControl" ~ "LocalControl", #there was a labeling mistake where they were both labelled origincontrol (one with an s)
                                 treat == "originControls"  ~ "LocalControl",
                                treat == "warmed" ~ "Warm"),
            originSiteID = case_when(Elevation == 1500 & Treatment == "LocalControl" ~ "Low",
                                   Elevation == 1950 ~ "High",
                                   Treatment == 'Warm' ~ 'High')) %>% 
    select(Year, destSiteID, originSiteID, UniqueID, Treatment, SpeciesName, Cover, -treat) %>% 
    # UniqueID used to be reconstructed without Year, so it identified a plot
    # (not a plot x year) - all years for the same plot collapsed onto one
    # UniqueID, which silently inflated Rel_Cover sums to ~(number of years)
    # once summed downstream. Include Year, as every other site does.
    extract(UniqueID, into = c("destPlotID", "year"), "(.*)_([^_]+)$") %>% 
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>%  
    select(-year) %>%
    mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA)
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other",Cover = pmax((100 - sum(Cover)), 0)) %>% 
    bind_rows(dat) %>% 
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other'))  %>% 
    filter(Cover > 0)
  cover <- dat2 %>% filter(SpeciesName %in% c('Other')) %>% 
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover)) 
  
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_IT_MatschMazia2 <- function(community_IT_MatschMazia2){
  taxa <- unique(community_IT_MatschMazia2$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_IT_MatschMazia2 <- function(community_IT_MatschMazia2){
  dat <- community_IT_MatschMazia2 %>%
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'High' = 1950, 'Low' = 1500)),
           Gradient = 'IT_MatschMazia2',
           Country = 'Italy',
           Longitude = as.numeric(recode(destSiteID, 'High' = 10.59195399, 'Low' = 10.5797899)), #ADD IN COORDS FOR LOW
           Latitude = as.numeric(recode(destSiteID, 'High' = 46.6916840, 'Low' = 46.6862599)), #ADD IN COORDS FOR LOW
           YearEstablished = 2010,
           PlotSize_m2 = 0.25) %>% 
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country) 
  
  return(dat)
}

#Clean trait data
CleanTrait_IT_MatschMazia2 <- function(trait_IT_MatschMazia2_raw){
  trait <- trait_IT_MatschMazia2_raw %>%
    rename(Elevation = elevation, SpeciesName = Species, Individual_number = `Rep-ID`) %>%
    filter(Elevation != "989.8") %>%
    rename(Wet_Mass_g = "Fresh weight, 1 leaf [g]", Dry_Mass_g = "Dry weight, 1 leaf [mg]", Leaf_Area_cm2 = "Leaf area [cm2], 1 leaf", Plant_Veg_Height_cm = "VegHt [cm]", Plant_Rep_Height_cm = "RepHt [cm]",
           LDMC = "LDMC [mg g-1]", SLA_mm2_mg = "SLA [mm2 mg-1]",  C_percent = "Carbon [%]", N_percent = "Nitrogen [%]", N_conc_mg_g = "LNC [mg g-1]", C_conc_mg_g = "LCC [mg g-1]"  ) %>%
    mutate_all(~gsub(',','', .)) %>%
    mutate(Country = "Italy",
           Gradient = "IT_MatschMazia2",
           destSiteID = recode(Elevation,  "1476.46" = "Low", "2026.09" = "High"),
           SLA_cm2_g = 10*as.numeric(SLA_mm2_mg),
           CN_ratio = as.numeric(C_percent)/as.numeric(N_percent),
           LDMC = as.numeric(LDMC)/1000) %>%
    mutate(across(Plant_Veg_Height_cm:C_conc_mg_g, ~as.numeric(.x)))%>%
    mutate(Individual_number = str_extract(Individual_number, "\\d+$"))%>%
    dplyr::select(Country, Gradient, destSiteID, SpeciesName, Individual_number, Plant_Veg_Height_cm:C_conc_mg_g) %>%
    pivot_longer(cols = Plant_Veg_Height_cm:C_conc_mg_g, names_to = "Trait", values_to = "Value")%>%
    mutate(PlantID = paste(destSiteID, Individual_number, SpeciesName, sep = "_"))%>%
    mutate(Individual_number = as.character(Individual_number), Value = as.numeric(Value)) %>%
    filter(!is.na(Value))
  return(trait)
}

#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_IT_MatschMazia2 <- function(){
  
  ### IMPORT DATA
  community_IT_MatschMazia2_raw = ImportCommunity_IT_MatschMazia2()
  trait_IT_MatschMazia2_raw = read_excel("./data/IT_MatschMazia/IT_MatschMazia_traitdata/Plant_Traits_Matsch_TRY_Contribution_names.xlsx", na="na")
  
  ### CLEAN DATA SETS
  cleaned_IT_MatschMazia2 = CleanCommunity_IT_MatschMazia2(community_IT_MatschMazia2_raw)
  community_IT_MatschMazia2 = cleaned_IT_MatschMazia2$comm
  cover_IT_MatschMazia2 = cleaned_IT_MatschMazia2$cover
  meta_IT_MatschMazia2 = CleanMeta_IT_MatschMazia2(community_IT_MatschMazia2) 
  taxa_IT_MatschMazia2 = CleanTaxa_IT_MatschMazia2(community_IT_MatschMazia2)
  trait_IT_MatschMazia2 = CleanTrait_IT_MatschMazia1(trait_IT_MatschMazia2_raw)
  
  
  # Make list
  IT_MatschMazia2 = list(meta = meta_IT_MatschMazia2,
                     community = community_IT_MatschMazia2,
                     cover = cover_IT_MatschMazia2,
                     taxa = taxa_IT_MatschMazia2,
                     trait = trait_IT_MatschMazia2)
  
  return(IT_MatschMazia2)
}

