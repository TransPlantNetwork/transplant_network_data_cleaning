library(tidyverse)
library(data.table)
library(drake)
library(Hmisc)
library(taxize)


### FUNCTION TO IMPORT SPECIES -------------------------------------------------

# get_species <- function(){
#   # load drake environment
#   loadd()
#   alldat = tibble::lst(NO_Ulvhaugen, NO_Lavisdalen, NO_Gudmedalen, NO_Skjellingahaugen, 
#                                  CH_Lavey, CH_Calanda, CH_Calanda2,
#                                  US_Colorado, US_Montana, US_Arizona,
#                                  CN_Damxung, IN_Kashmir, CN_Gongga, CN_Heibei, 
#                                  DE_Grainau, DE_Susalps, DE_TransAlps, FR_AlpeHuez, SE_Abisko, FR_Lautaret, IT_MatschMazia1, IT_MatschMazia2)
#   # map all taxa to one unique vector
#   taxa <- alldat %>% 
#     map(~.$taxa) %>%
#     do.call(c, .) %>%
#     unique
#   # remove missing cases
#   taxa <- taxa[!is.na(taxa)]
#   # return
#   return(taxa)
# }

### MERGE TAXA LIST FROM SITE ABUNDANCE DATA AND LOOKUP TABLES ---------------------
merge_site_taxa_data <- function(sitedata) {
  
  
  #merge taxa data from abundances
  sitedata_taxa <- sitedata %>% 
    map_df("community", .id='Region') %>%
    ungroup() %>%
    dplyr::select(Region, destSiteID, SpeciesName) %>%
    base::unique() 
  
  return(sitedata_taxa) 
  
}

merge_all_taxa_data <- function(alldat) {
  
  #merge site data and species codes used for those three sites
  site_code <- left_join(alldat$sitetaxa, alldat$spcodes, by=c("Region", "SpeciesName"="code"), relationship = "many-to-many") %>% #sitetaxa is 4481, site_code is 4527 (check what is being added)
    mutate(original_name = ifelse(!is.na(taxa), taxa, SpeciesName))
  
  setdiff(c(alldat$sitetaxa$SpeciesName), c(site_code$SpeciesName))# both are zero?
  
  #site_code %>% filter(is.na(original_name) & is.na(SpeciesName)) #only 3 NAs in Species Name (from Heibei, this is a known issue)
  
  return(site_code) 
  
}


### FUNCTION TO CLEAN SPECIES NAMES WITH TNRS + GNR ----------------------------

resolve_species <- function(taxa){
  # copy taxa into modified version for database calling
  copy_taxa <- taxa$original_name
  # resolve easy fixes
  copy_taxa[copy_taxa == "Stellaria umbellata"] <- "Stellaria umbellata"
  copy_taxa[copy_taxa == "Stellaria_wide_leaf"] <- "Stellaria umbellata"
  copy_taxa[copy_taxa == "Entire Meconopsis"] <- "Meconopsis"
  copy_taxa[copy_taxa == "Leuc. Vulg."] <- "Leucanthemum vulgare"
  copy_taxa[copy_taxa == "unknown aster serrated"] <- "Aster"
  copy_taxa[copy_taxa == "Unknown aster serrated"] <- "Aster"
  copy_taxa[copy_taxa == "Haemarocalus fulva"] <- "Hemerocallis fulva"
  copy_taxa[copy_taxa == "Dracophaleum"] <- "Dracocephalum"
  copy_taxa[copy_taxa %in% c("Antoxanthum alpinum","Anthoxanthum alpinum")] <- "Anthoxanthum odoratum nipponicum"
  copy_taxa[copy_taxa == "Vaccinium gaultherioides"] <- "Vaccinium uliginosum"
  copy_taxa[copy_taxa == "Listera ovata"] <- "Neottia ovata"
  copy_taxa[copy_taxa == "Gentiana tenella"] <- "Gentianella tenella"
  copy_taxa[copy_taxa == "Nigritella nigra"] <- "Gymnadenia nigra"
  copy_taxa[copy_taxa == "Hieracium lactucela"] <- "Pilosella lactucella"
  copy_taxa[copy_taxa == "Agrostis schraderiana"] <- "Agrostis agrostiflora"
  copy_taxa[copy_taxa == "Festuca pratense"] <- "Festuca pratensis"
  copy_taxa[copy_taxa == "Ran. acris subsp. Friesianus"] <- "Ranunculus acris subsp. friesianus"
  copy_taxa[copy_taxa == "Symphyothricum_sp."] <- "Symphyotrichum"
  copy_taxa[copy_taxa == "Orchidacea spec"] <- "Orchidaceae"
  copy_taxa[copy_taxa == "Carex biggelowii"] <- "Carex bigelowii" 
  copy_taxa[copy_taxa == "Carex spec"] <- "Carex"
  copy_taxa[copy_taxa == "Potentilla stenophylla"] <- "Potentilla stenophylla"
  copy_taxa[copy_taxa == "Hol.lan"] <- "Holcus lanatus"
  copy_taxa[copy_taxa == "Dia.med"] <- "Dianthus deltoides"
  
  taxa$copy_taxa <- copy_taxa
  
  # call GNR (does not deal with duplicated data!)
  taxa_gnr <- gnr_resolve(names = base::unique(copy_taxa), 
                          best_match_only = T, 
                          data_source_ids = c(1, 12),
                          fields = "all",
                          with_context = T,
                          with_canonical_ranks = T)
  
  # subset for supplied name, unique ID, match score and matched name
  gnr_subset <- taxa_gnr %>% 
    dplyr::select(user_supplied_name, gni_uuid, score, matched_name2) %>%
    rename(matched_name = matched_name2)
  
  # construct data frame from taxa input, make character, bind GNR output
  taxa_tab <- data.frame(Region = taxa$Region,
                         destSiteID = taxa$destSiteID,
                         SpeciesName = taxa$SpeciesName, 
                         original_name = taxa$original_name,
                         submitted_name = taxa$copy_taxa) %>%
    mutate(submitted_name = as.character(submitted_name)) %>%
    left_join(., gnr_subset, by = c("submitted_name" = "user_supplied_name"))
  
  # add in ID for unidentified species
  taxa_out <- taxa_tab %>% filter(is.na(gni_uuid)) %>% group_by(Region) %>% 
    mutate(submitted_name = ifelse(is.na(gni_uuid), paste(Region, "NID", seq_len(nrow(.)), sep="_"), submitted_name),
           matched_name = ifelse(is.na(gni_uuid), paste(Region, "NID", seq_len(nrow(.)), sep="_"), matched_name)) %>%
    bind_rows(taxa_tab, .) %>% filter(!is.na(submitted_name))
  
  # fix odd stelumb issue for CN_Heibei (maybe a character issue in text?)
  taxa_out[taxa_out$submitted_name == "CN_Heibei_NID_1",]$gni_uuid <- "b8f58fe9-1a3b-5e54-bfd6-4d160f930b5e"
  taxa_out[taxa_out$submitted_name == "CN_Heibei_NID_1",]$score <- 0.988
  taxa_out[taxa_out$submitted_name == "CN_Heibei_NID_1",]$submitted_name <- "Stellaria umbellata"
  taxa_out[taxa_out$submitted_name == "CN_Heibei_NID_4",]$gni_uuid <- "b8f58fe9-1a3b-5e54-bfd6-4d160f930b5e"
  taxa_out[taxa_out$submitted_name == "CN_Heibei_NID_4",]$score <- 0.988
  taxa_out[taxa_out$submitted_name == "CN_Heibei_NID_4",]$submitted_name <- "Stellaria umbellata"
  taxa_out[taxa_out$submitted_name == "CN_Heibei_NID_6",]$gni_uuid <- "b8f58fe9-1a3b-5e54-bfd6-4d160f930b5e"
  taxa_out[taxa_out$submitted_name == "CN_Heibei_NID_6",]$score <- 0.988
  taxa_out[taxa_out$submitted_name == "CN_Heibei_NID_6",]$submitted_name <- "Stellaria umbellata"
  
  #grepping for 'NID' shows all correctly IDed :)
  
  # return
  return(taxa_out)
}


