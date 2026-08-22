####################
### CN_Heibei  ###
####################

#### Import Community ####

ImportCommunity_CN_Heibei <- function(){
  community_CN_Heibei_raw<-read_excel(file_in("data/CN_Heibei/CN_Heibei_commdata/data to J Ecology.xlsx"))
  return(community_CN_Heibei_raw)
} 



#### Cleaning Code ####
# Cleaning Heibei community data

CleanCommunity_CN_Heibei <- function(community_CN_Heibei_raw){
  dat <- community_CN_Heibei_raw %>% 
    rename(SpeciesName = `species` , Cover = `Coverage(%)` , PlotID = Treatment , destSiteID = `away` , originSiteID = `home`, Year = year, plotNo = `replicate`)%>% 
    filter(!(destSiteID == 3600 | originSiteID == 3600)) %>% 
    mutate(originSiteID = as.character(originSiteID),
           destSiteID = as.character(destSiteID)) %>% 
    mutate(Treatment = case_when(destSiteID =="3200" & originSiteID == "3200" ~ "LocalControl" , 
                                 destSiteID =="3400" & originSiteID == "3400" ~ "LocalControl" , 
                                 destSiteID =="3800" & originSiteID == "3800" ~ "LocalControl" , 
                                 destSiteID =="3200" & originSiteID == "3400" ~ "Warm" , 
                                 destSiteID =="3200" & originSiteID == "3800" ~ "Warm" , 
                                 destSiteID =="3400" & originSiteID == "3200" ~ "Cold" , 
                                 destSiteID =="3400" & originSiteID == "3800" ~ "Warm" , 
                                 destSiteID =="3800" & originSiteID == "3200" ~ "Cold" ,
                                 destSiteID =="3800" & originSiteID == "3400" ~ "Cold")) %>% 
    mutate(destPlotID = paste(originSiteID, destSiteID, plotNo, sep='_')) %>% 
    mutate(UniqueID = paste(Year, destPlotID, sep='_')) %>% 
    group_by(UniqueID, Year, originSiteID, destSiteID, destPlotID, Treatment) %>%
    select(-PlotID, -plotNo) %>% ungroup() %>%
    mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA) %>%
    distinct() %>% #removed 9 instances of duplication
    group_by(Year, originSiteID, destSiteID, destBlockID, destPlotID, UniqueID, Treatment, SpeciesName) %>%
    summarize(Cover = sum(Cover, na.rm=T)) %>% #had one species which occured twice in a plot, summing across
    ungroup()
  
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other", Cover = pmax((100 - sum(Cover)), 0)) %>% #All total cover >100, pmax rebases this to zero
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

CleanMeta_CN_Heibei <- function(community_CN_Heibei){
  dat <- community_CN_Heibei %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, '3200'=3200, '3400'=3400, '3800'=3800)),
           Gradient = "CN_Heibei",
           Country = "China",
           Longitude = as.numeric(recode(destSiteID, '3800'=101.36922199, '3400'=101.331306, '3200'=101.313306)),
           Latitude = as.numeric(recode(destSiteID, '3800'=37.704917, '3400'=37.665306, '3200'=37.611750)),
           YearEstablished = 2007,
           PlotSize_m2 = 1) %>%
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country)
  
  return(dat)
}

# Cleaning Heibei species list

CleanTaxa_CN_Heibei <- function(community_CN_Heibei){
  taxa<-unique(community_CN_Heibei$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_CN_Heibei <- function(){
  
  
  ### IMPORT DATA
  community_CN_Heibei_raw = ImportCommunity_CN_Heibei()
  
  ### CLEAN DATA SETS
  cleaned_CN_Heibei = CleanCommunity_CN_Heibei(community_CN_Heibei_raw)
  community_CN_Heibei = cleaned_CN_Heibei$comm
  cover_CN_Heibei = cleaned_CN_Heibei$cover
  meta_CN_Heibei = CleanMeta_CN_Heibei(community_CN_Heibei) 
  taxa_CN_Heibei = CleanTaxa_CN_Heibei(community_CN_Heibei)
  
  
  # Make list
  CN_Heibei = list(meta = meta_CN_Heibei,
                    community = community_CN_Heibei,
                    cover = cover_CN_Heibei,
                    taxa = taxa_CN_Heibei)
  
  return(CN_Heibei)
}

