#### Code to merge all community data (with metadata) together ####

merge_comm_data <- function(site_outputs) {
  
  
  #fix up community data
  community_data <- site_outputs |> 
    map_df("community", .id='Region') |>
    ungroup() |>
    filter(!Treatment %in% c('NettedControl', 'Cold', 'Control')) |>
    select(dplyr::any_of(c("Region", "Year", "originSiteID", "originBlockID", "destSiteID", "destBlockID",
                           "destPlotID", "Treatment", "turfID", "UniqueID", "SpeciesName", "Cover", "Rel_Cover"))) #Some unnecessary columns in NO and CH
  
  #add metadata to organize by elevations
  meta <- site_outputs |> 
    map("meta") |> 
    map(ungroup) |> 
    map_df(mutate, Gradient = as.character(Gradient), .id='Region') |> 
    mutate(Gradient=recode(Gradient, '1'='NO_Ulvhaugen', '2'='NO_Lavisdalen', '3'='NO_Gudmedalen', '4'='NO_Skjellingahaugen')) |>
    select(Region, destSiteID, Elevation) |> 
    distinct()
  
  #bind
  community_with_meta <- left_join(community_data, meta, by=c('Region', 'destSiteID'))
  
  #sanity checks:
  #unique(community_data$destSiteID) %in% unique(meta$destSiteID) #all true
  #unique(meta$destSiteID) %in% unique(community_data$destSiteID) #all true
  # community_with_meta[is.na(community_with_meta$Rel_Cover),] #no NA Rel_covers (cover yes, arizona only has rel_cover)
  # community_data[is.na(community_data$Treatment),] #no NA treatments
  #community_with_meta |> group_by(Region, destSiteID, Treatment) |> summarize(n=n()) |> View
  
  return(community_with_meta) 
  
}
