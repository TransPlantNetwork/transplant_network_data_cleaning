###### IT community data functions ####
#Chelsea Chisholm, 3.11.2019, updated April 7 2021 to add corrected data from Georg (splitting into two gradients, older code below)

load_cover_IT_MatschMazia1 <- function(){
  #import data
  exp1 <- read_excel("data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13_corr.csv.xlsx", sheet=1)
  
  cover <- exp1 %>% mutate(UniqueID =`internal ID`, treat = `rec/Donor`) %>% 
    mutate(year = as.numeric(str_sub(`internal ID`,-2,-1))+2000) %>%
    select(-`Gradient Name`, -`internal ID`, -`rec/Donor`) 
  
  return(cover)
}


load_cover_IT_MatschMazia2 <- function(){
  #import data
  exp2 <- read_excel("data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13_corr.csv.xlsx", sheet=2)

  cover <- exp2 %>% mutate(UniqueID =`internal ID`, treat = `rec/Donor`) %>% 
    mutate(year = as.numeric(str_sub(`internal ID`,-2,-1))+2000) %>%
    select(-`Gradient Name`, -`internal ID`, -`rec/Donor`) 
  
  return(cover)
}

# load_cover_IT_MatschMazia <- function(){
#   #import data
#   exp1 <- read_excel("data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13.csv.xlsx", sheet=1)
#   colnames(low)[3] <- 'treat'
#   exp2 <- read_excel("data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13.csv.xlsx", sheet=2)
#   colnames(high)[2] <- 'treat'
#   
#   low <- low %>% mutate(destSiteID='Low', destPlotID=`internal ID`) %>% 
#     mutate(year = as.numeric(str_sub(`internal ID`,-2,-1))+2000) %>%
#     select(-Bezeichnung, -`Gradient Name`, -`internal ID`) 
#   high <- high %>% mutate(destSiteID='High', destPlotID=`internal ID`) %>% 
#     mutate(year = as.numeric(str_sub(`internal ID`,-2,-1))+2000) %>%
#     select(-`Gradient Name`, -`internal ID`) 
#   
#   cover <- bind_rows(low, high)
#   
#   return(cover)
# }