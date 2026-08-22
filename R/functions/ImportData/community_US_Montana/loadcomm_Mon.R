###### US_Montana community data functions ####
#Chelsea Chisholm, 03.11.2019, edited April 7 2021 to add new Montana data in (old code preserved below)

load_cover_US_Montana <- function(){
  #import data
  dat<- read.csv('./data/US_Montana/US_Montana_commdata/MT_transplant_relevee_200421.csv')

  cover <- dat %>% 
    rename(destSiteID=Elevation, turfID=Plot, SpeciesName=Species)  %>%
    filter(!grepl("soil",Disturb_treat)) %>% 
    mutate(Treatment = case_when(Disturb_treat == "lowland_community" & destSiteID=="low" ~ "LocalControl",
                                 Disturb_treat == "Alpine_community" & destSiteID == "low" ~ "Warm",
                                 Disturb_treat == "Alpine_community" & destSiteID == "mid" ~ "LocalControl")) %>% #I remember Tim saying they transplanted alpine turfs downward to both middle and low. so here we are...
    filter(Treatment %in% c('LocalControl', 'Warm'), !is.na(Cover)) %>%
    mutate(originSiteID=recode(Disturb_treat, "lowland_community"='Low', "Alpine_community"='High'), #middle is the actual high site
           destSiteID=recode(destSiteID, 'low'='Low', "mid"='High')) %>%
    select(-Disturb_treat)

  return(cover)
}

# load_cover_US_Montana <- function(){
#   #import data
#   dat13 <- read_excel('./data/US_Montana/US_Montana_commdata/Fall_2013_Cover_Data.xlsx', sheet=1)
#   dat14_15 <- read_excel('./data/US_Montana/US_Montana_commdata/Montana_MIREN_trans_relevee.xlsx', sheet=1)
#   
#   cover13 <- dat13 %>%
#     filter(site !='high') %>% #removing the very high community which we aren't interested in
#     gather(SpeciesName, cover, -site, -plot, -rep) %>%
#     mutate(Year=2013, Region='Montana') %>%
#     rename(destSiteID=site, turfID=plot, reps=rep, Cover=cover) %>%
#     mutate(Treatment = case_when(reps == "LC" & destSiteID=="low" ~ "LocalControl",
#       reps == "AC" & destSiteID == "low" ~ "Warm",
#       reps == "AC" & destSiteID == "middle" ~ "LocalControl")) %>%
#     filter(Treatment %in% c('LocalControl', 'Warm'), !is.na(Cover)) %>%
#     mutate(destSiteID=recode(destSiteID, low='Low', middle='High'), #middle is the actual high site
#            originSiteID=recode(reps, 'LC'='Low', "AC"='High')) 
#     
#   cover14_15 <- dat14_15 %>% select(1:8) %>% #seems to be only low and middle communities here
#     rename(destSiteID=Elevation, turfID=Plot, SpeciesName=Species)  %>%
#     filter(!grepl("soil",Disturb_treat)) %>% #what to do about inside vs outside in column Location? ignored for now
#     select(-Location) %>%
#     mutate(Treatment = case_when(Disturb_treat == "lowland_community" & destSiteID=="Low" ~ "LocalControl",
#                                  Disturb_treat == "Alpine_community" & destSiteID == "Low" ~ "Warm",
#                                  Disturb_treat == "Alpine_community" & destSiteID == "Middle" ~ "LocalControl")) %>% #I remember Tim saying they transplanted alpine turfs downward to both middle and low. so here we are...
#     filter(Treatment %in% c('LocalControl', 'Warm'), !is.na(Cover)) %>%
#     mutate(originSiteID=recode(Disturb_treat, "lowland_community"='Low', "Alpine_community"='High'), #middle is the actual high site
#            destSiteID=recode(destSiteID, 'Low'='Low', "Middle"='High')) %>%
#     select(-Disturb_treat)
#   
#   cover <- bind_rows(cover13, cover14_15)
#   
#   return(cover)
# }
