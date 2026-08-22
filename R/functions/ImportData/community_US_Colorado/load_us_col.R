###### Colorado community data functions ####
#Chelsea Chisholm, April 16, 2021
#adding in new 2019 data, merging to 2018

load_cover_US_Colorado <- function(){
  #import data
  dat18 <- read_csv(file = "./data/US_Colorado/US_Colorado_commdata/RMBLtransplant_speciesCover2018.csv")
  dat19 <- read_csv(file = "./data/US_Colorado/US_Colorado_commdata/SpeciesCover_AllSites_2019_forChelsea.csv") %>%
    rename(percentCover = pctCover)
  cover <- bind_rows('2018'=dat18, '2019'=dat19, .id="Year") %>% mutate(Year = as.numeric(Year)) %>%
    select(Year, turfID, species, percentCover)
  
  return(cover)
}
