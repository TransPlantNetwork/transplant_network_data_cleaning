### FIND AND ADD TRY TRAIT DATA ----------------------------------------------------

## Data are from gap-filling operation performed for functional signal project. 
## TRY data used at observation level, filtered for obvious anomalies, merged 
## to species means, gap-filled using BHPMF (Schrodt et al. 2015) N = 93 times 
## with different input parameters to overcome problems with function getting 
## stuck in local max/min. Transformed back to original units, filtered for 
## outliers (CV > 1; values > 1.5 * IQR), took means of acceptable values. 
## Replaced missing values only.

load_wrangle_try <- function(cleaned) {
  ## LOAD TRY ##
  # specify directory, load try, give names
  try_dir <- paste0(getwd(), "/data/Traits/")
  try_data <- lapply(list.files(try_dir, full.names = T), read.csv)
  names(try_data) <- c("categorical", "numeric", "species")
  
  ## MATCH ROWS IN CLEANED SPECIES TO POSITIONS IN TRY SPECIES LIST ##
  # hierarchical matching: ID > accepted_name > submitted_name > original_name
  match_ids_ids <- match(cleaned$gni_uuid, try_data$species$gni_uuid)
  match_acc_acc <- match(cleaned$matched_name, try_data$species$accepted_name)
  match_sub_acc <- match(cleaned$submitted_name, try_data$species$accepted_name)
  match_ori_acc <- match(cleaned$original_name, try_data$species$accepted_name)
  match_acc_sub <- match(cleaned$matched_name, try_data$species$submitted_name)
  match_sub_sub <- match(cleaned$submitted_name, try_data$species$submitted_name)
  match_ori_sub <- match(cleaned$original_name, try_data$species$submitted_name)
  # create hierarchy, then add in iterative forms with ifelse statements
  hier_species <- cbind(match_ids_ids, match_acc_acc, match_sub_acc,
                        match_ori_acc, match_acc_sub, match_sub_sub,
                        match_ori_sub) %>%
    as.data.frame %>%
    mutate(consensus = match_ids_ids) %>%
    mutate(consensus = ifelse(is.na(consensus), match_acc_acc, consensus)) %>%
    mutate(consensus = ifelse(is.na(consensus), match_sub_acc, consensus)) %>%
    mutate(consensus = ifelse(is.na(consensus), match_ori_acc, consensus)) %>%
    mutate(consensus = ifelse(is.na(consensus), match_acc_sub, consensus)) %>%
    mutate(consensus = ifelse(is.na(consensus), match_sub_sub, consensus)) %>%
    mutate(consensus = ifelse(is.na(consensus), match_ori_sub, consensus))
  # create genus variables and match
  my_genus_acc <- str_split_fixed(cleaned$matched_name, 
                                  n = 2, 
                                  pattern = " ")[, 1]
  my_genus_sub <- str_split_fixed(cleaned$submitted_name, 
                                  n = 2, 
                                  pattern = " ")[, 1]
  my_genus_ori <- str_split_fixed(cleaned$original_name, 
                                  n = 2, 
                                  pattern = " ")[, 1]
  match_acc_genus <- match(my_genus_acc, try_data$species$genus)
  match_sub_genus <- match(my_genus_sub, try_data$species$genus)
  match_ori_genus <- match(my_genus_ori, try_data$species$genus)
  # apply same hierarchy to genus level data
  hier_genus <- cbind(match_acc_genus, match_sub_genus, match_ori_genus) %>%
    as.data.frame %>%
    mutate(consensus = match_acc_genus) %>%
    mutate(consensus = ifelse(is.na(consensus), match_sub_genus, consensus)) %>%
    mutate(consensus = ifelse(is.na(consensus), match_ori_genus, consensus))
  # bind hierarchies
  all_matches <- data.frame(species_match = hier_species$consensus) %>%
    mutate(genus_match = ifelse(is.na(species_match), 
                                hier_genus$consensus, 
                                species_match))
  
  ## SUBSET TRY DATA TO CLEANED SPECIES LIST ##
  # skeleton species list
  try_spp <- try_spp_gen <- cleaned %>% select(Region, destSiteID, original_name, 
                                               submitted_name, 
                                               accepted_name = matched_name)
  # subset try names
  try_names <- try_data$species %>% select(try_name = accepted_name, 
                                           genus:order)
  # species only
  try_spp <- try_spp %>%
    bind_cols(., try_names[all_matches$species_match, ]) %>%
    bind_cols(., try_data$categorical[all_matches$species_match, ]) %>%
    bind_cols(., try_data$numeric[all_matches$species_match, ])
  # genus plus species
  try_spp_gen <- try_spp_gen %>%
    bind_cols(., try_names[all_matches$genus_match, ]) %>%
    bind_cols(., try_data$categorical[all_matches$genus_match, ]) %>%
    bind_cols(., try_data$numeric[all_matches$genus_match, ])
  # return
  out <- list(species_only = try_spp,
              genus_species = try_spp_gen)
  return(out)
}




