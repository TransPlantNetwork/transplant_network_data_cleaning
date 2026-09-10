#############################
### TAXA DRAKE PLAN ########
#############################
#C. Chisholm, July 23 2021
# J. Lynn, 3 June 2021

# Load libraries
library("drake")
library("tidyverse")
library("readxl")
library("DBI")
library("visNetwork")

# drake configurations
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# trick
pn <- . %>% print(n = Inf)


# Source downstream trait scripts
source("R/drake_workflow/WrangleTaxaTraits/clean_taxonomy.R") # for cleaning species lists from 'taxa' list element for all sites
source("R/drake_workflow/WrangleTaxaTraits/site_taxa_codes.R") #for sp_codes for No, Arizona and Sweden where they use species codes in dat

# Import names from community dataframe
ImportSiteTaxaDrakePlan <- drake_plan(
  
  # Import site community species columns
  sitetaxa = merge_site_taxa_data(sitedata = tibble::lst(NO_Ulvhaugen, NO_Lavisdalen, NO_Gudmedalen,
                                                         NO_Skjellingahaugen, 
                                                         CH_Lavey, CH_Calanda, CH_Calanda2,
                                                         US_Colorado, US_Montana, US_Arizona,
                                                         CN_Damxung, IN_Kashmir, CN_Gongga, CN_Heibei, 
                                                         DE_Grainau, DE_Susalps, DE_TransAlps, FR_AlpeHuez, SE_Abisko,
                                                         FR_Lautaret, IT_MatschMazia1, IT_MatschMazia2)),
  
  # Import lookup tables for those sites with sp codes
  se_dat = load_SE_Abisko_sptable(),
  no_dat = load_Norway_sptable(),
  us_dat = load_US_Arizona_sptable(),
  spcodes =  merge_sptable(spdat = tibble::lst(se_dat, no_dat, us_dat)) # has regions as a column
  #spcodes_unlist = unnest(spcodes, cols = c(codes)),
  # Import taxa lists (list element $taxa) from all sites
  #taxa = get_species()
  
)

# Import Taxa lists from all sites and clean to accepted names
MergeTaxaDrakePlan <- drake_plan(
  
  mergedtaxa = merge_all_taxa_data(alldat = tibble::lst(sitetaxa, spcodes))

)


# Clean species names with GNI
CleanSpeciesNames <- drake_plan(
  
  cleanedspecies = resolve_species(mergedtaxa)
  
)

TaxaPlan <- bind_rows(ImportSiteTaxaDrakePlan, MergeTaxaDrakePlan, CleanSpeciesNames)

conf <- drake_config(TaxaPlan)
conf
# 
 make(TaxaPlan)
 
 loadd(cleanedspecies)


#cleanedspecies %>% filter(is.na(gni_uuid)) %>% View()
# the only issue remaining (that isn't Sweden and Norway) is Stellaria umbellata in CN_Heibei. It should be fine (gnr_resolve works on this species, why the NAs?)
#Stellaria umbellata fixed
#However, for some reason the gnr call adds some rows to the dataset (goes from 4527 to 4599). Where is this duplication coming from?