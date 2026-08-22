## Load CH_Calanda2 data from Sebastian M.
# C. Chisholm, June 11 2021

load_cover_CH_Calanda2 <- function(){
  # import data
  dat <- read.csv('./data/CH_Calanda2/CH_Calanda2_commdata/calanda_data_TransPlantNetwork.csv', stringsAsFactors = FALSE)
  cover <- dat %>% 
    filter(is_focal == FALSE) %>%
    # select columns
    select(year, site, block, plot, plot_id, quadrant_id, species, cover_cm2) %>%
    # group by plot and species to get summed cover
    group_by(year, site, block, plot, plot_id, species) %>%
    summarize(cover = sum(cover_cm2)) %>%
    ungroup()
    #keep in mind is_zombie denotes encroaching invaders
    
  return(cover)
}
