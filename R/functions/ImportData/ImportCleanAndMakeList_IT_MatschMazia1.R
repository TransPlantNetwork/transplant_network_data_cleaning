####################
### IT_MatschMazia1  ###
####################

source("R/functions/ImportData/community_IT_MatschMazia/loadcomm_IT.r")

#### Import Community ####
ImportCommunity_IT_MatschMazia1 <- function(){
  community_IT_MatschMazia1_raw<-load_cover_IT_MatschMazia1()
  return(community_IT_MatschMazia1_raw)
} 


#### Cleaning Code ####
# Cleaning Lautaret community data
CleanCommunity_IT_MatschMazia1 <- function(community_IT_MatschMazia1_raw){
  dat <- community_IT_MatschMazia1_raw %>% 
    rename(Year=year, Elevation=elevation) %>%
    gather(SpeciesName, Cover, -UniqueID, -Elevation, -treat, -Year) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\_', ' ', SpeciesName)) %>%
    mutate(destSiteID = case_when(Elevation == 1000 ~ "Low",
                                    Elevation == 1500 ~ "High"),
            Treatment = case_when(treat == "destControl" ~ "LocalControl",
                                 treat == "originControls"  ~ "LocalControl",
                                treat == "warmed" ~ "Warm"),
            originSiteID = case_when(Elevation == 1000 & treat == "destControl" ~ "Low",
                                   Elevation == 1500 ~ "High",
                                   treat == 'warmed' ~ 'High')) %>% 
    select(Year, destSiteID, originSiteID, UniqueID, Treatment, SpeciesName, Cover, -treat) %>% 
    extract(UniqueID, into = c("destPlotID", "year"), "(.*)_([^_]+)$") %>% 
    mutate(UniqueID = paste(originSiteID, destSiteID, destPlotID, sep='_')) %>%  
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
CleanTaxa_IT_MatschMazia1 <- function(community_IT_MatschMazia1){
  taxa <- unique(community_IT_MatschMazia1$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_IT_MatschMazia1 <- function(community_IT_MatschMazia1){
  dat <- community_IT_MatschMazia1 %>%
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'High' = 1500, 'Low' = 1000)),
           Gradient = 'IT_MatschMazia1',
           Country = 'Italy',
           Longitude = as.numeric(recode(destSiteID, 'Low' = 10.5902491243, 'High' = 10.5797899)), #ADD IN COORDS FOR LOW
           Latitude = as.numeric(recode(destSiteID, 'Low' = 46.6612188656, 'High' = 46.6862599)), #ADD IN COORDS FOR LOW
           YearEstablished = 2010,
           PlotSize_m2 = 0.25) %>% 
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country) 
  
  return(dat)
}


#Clean trait data
CleanTrait_IT_MatschMazia1 <- function(trait_IT_MatschMazia1_raw){
  trait <- trait_IT_MatschMazia1_raw %>%
    rename(Elevation = elevation, SpeciesName = Species, Individual_number = `Rep-ID`) %>%
    filter(Elevation != 2026.09) %>%
    rename(Wet_Mass_g = "Fresh weight, 1 leaf [g]", Dry_Mass_g = "Dry weight, 1 leaf [mg]", Leaf_Area_cm2 = "Leaf area [cm2], 1 leaf", Plant_Veg_Height_cm = "VegHt [cm]", Plant_Rep_Height_cm = "RepHt [cm]",
           LDMC = "LDMC [mg g-1]", SLA_mm2_mg = "SLA [mm2 mg-1]",  C_percent = "Carbon [%]", N_percent = "Nitrogen [%]", N_conc_mg_g = "LNC [mg g-1]", C_conc_mg_g = "LCC [mg g-1]"  ) %>%
    mutate_all(~gsub(',','', .)) %>%
    mutate(Country = "Italy",
           Gradient = "IT_MatschMazia1",
           destSiteID = recode(Elevation,  "989.8" = "Low", "1476.46" = "High"),
           SLA_cm2_g = 10*as.numeric(SLA_mm2_mg),
           LDMC = as.numeric(LDMC)/1000,
           CN_ratio = as.numeric(C_percent)/as.numeric(N_percent)) %>%
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
ImportClean_IT_MatschMazia1 <- function(){
  
  ### IMPORT DATA
  community_IT_MatschMazia1_raw = ImportCommunity_IT_MatschMazia1()
  trait_IT_MatschMazia1_raw = read_excel("./data/IT_MatschMazia/IT_MatschMazia_traitdata/Plant_Traits_Matsch_TRY_Contribution_names.xlsx", na="na")
  
  ### CLEAN DATA SETS
  cleaned_IT_MatschMazia1 = CleanCommunity_IT_MatschMazia1(community_IT_MatschMazia1_raw)
  community_IT_MatschMazia1 = cleaned_IT_MatschMazia1$comm
  cover_IT_MatschMazia1 = cleaned_IT_MatschMazia1$cover
  meta_IT_MatschMazia1 = CleanMeta_IT_MatschMazia1(community_IT_MatschMazia1) 
  taxa_IT_MatschMazia1 = CleanTaxa_IT_MatschMazia1(community_IT_MatschMazia1)
  trait_IT_MatschMazia1 = CleanTrait_IT_MatschMazia1(trait_IT_MatschMazia1_raw)
  
  
  # Make list
  IT_MatschMazia1 = list(meta = meta_IT_MatschMazia1,
                     community = community_IT_MatschMazia1,
                     cover = cover_IT_MatschMazia1,
                     taxa = taxa_IT_MatschMazia1,
                     trait = trait_IT_MatschMazia1)
  
  return(IT_MatschMazia1)
}

