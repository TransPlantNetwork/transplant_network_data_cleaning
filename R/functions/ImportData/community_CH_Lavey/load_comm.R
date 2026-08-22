###### Lavey community data functions ####
#Chelsea Chisholm, 19.02.2019

#### STILL MAJOR ISSUE WITH MULTIPLE SAME SPECIES IN SAME SUBSHEET--> Loic says it's just an input mistake, they are the same species.
load_cover_CH_Lavey <- function(){
  
#Function to load sheets and sum abundance across pin drop points per turf
  collector17 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x), #has to add name repair because adding unique names. Just combine them I think...
         .id = "turfID") #no idea why but needed to break pipeline here 
    dat1 <- dat %>% dplyr::select(-'NA', -contains('FOCAL')) %>%
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID))) %>%
      group_by(turfID) %>%
      summarise_if(is.numeric, sum) %>%
      gather(key='SpeciesName', value='cover', -turfID ) %>% 
      mutate(SpeciesName=str_replace(SpeciesName, "\\...+[\\d]$", "")) %>% #remove unique IDS and sum across groups
      group_by(turfID, SpeciesName) %>%
      summarise(cover=sum(cover)) %>%
      filter(!grepl('FOCAL', SpeciesName)) %>% #remove focals from counts
      mutate(SpeciesName = if_else(SpeciesName=='NA', 'Other', SpeciesName)) %>% #NA is bare ground (or litter, etc!)
      filter(!is.na(cover)) #remove non essential rows
    return(dat1)
  }
  
  speciescorr18 <- function(y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x),
        .id = "turfID") 
    dat1 <- dat %>%
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID))) %>%
      dplyr::select(turfID, sp_2017, sp_2018) 
    return(dat1)
  }
  
  collector18 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x),
        .id = "turfID") 
    dat1 <- dat %>% dplyr::select(-sp_2017) %>%
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID)), cover=rowSums(select_if(., is.numeric), na.rm=T)) %>%
      dplyr::select(-starts_with('CRE')) %>% #remove all rows and keep sums
      filter(!is.na(cover)) %>% #remove non essential rows
      filter(!grepl('FOCAL', sp_2018)) %>% #remove focals from counts
      mutate(SpeciesName = if_else(sp_2018=='NA', 'Other', sp_2018)) %>% #NA is bare ground (or litter, etc!)
      select(turfID, SpeciesName, cover, -sp_2018) %>%
      group_by(turfID, SpeciesName) %>% #again make sure unique IDs are accounted for
      summarise(cover=sum(cover))
    return(dat1)
  }
  
  collector19 <- function (y) {
    dat <- excel_sheets(y) %>%
      .[-1] %>%
      map_dfr(~ read_excel(path = y, sheet = .x, col_types = 'text')) %>%
      pivot_longer(cols = 'Vegetation %':'Biscutella laevigata', names_to = 'SpeciesName', values_to = 'cover')
    dat1 <- dat %>%
      mutate(cover = recode(cover, `+` = 0.5 , `r` = 0.1 , `1` = 3.5 , `2a` = 10 , `2b` = 20 , `3` = 37.5 , `4` = 62.5 , `5` = 87.5)) %>% 
      filter(!is.na(cover)) %>% #remove non essential rows
      filter(!grepl('FOCAL', SpeciesName)) %>% #remove focals from counts %>%
      mutate(...1 = gsub('RIO_' , 'RIO_RIOturf_', ...1)) %>%
      mutate(turfID = paste0('T', substr(...1, start = 14, stop=nchar(...1))), siteID = substr(...1, start = 1, stop = 7)) %>%
      select(siteID, turfID, SpeciesName, cover) %>%
      mutate(SpeciesName = str_replace(SpeciesName, "\\...+[\\d]$", "")) 
    return(dat1)
  }
  
  #import data
  files17 <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2017/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2017/'))
  files18 <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2018/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2018/'))
  files18 <- files18[-8]
  files19 <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2019/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2019/'))
  
  sitenames <-c('CRE_CRE','CRE_RIO','MAR_MAR','MAR_RIO','PRA_PRA','PRA_RIO','RIO_RIO')
  
  #Cover17 is site x species
  cover17 <- tibble(siteID=files17) %>% 
    mutate(file_contents = map(siteID, collector17)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=2017) %>%
    unnest(cols = c(file_contents)) 
  
  #Species corrections for 2017 data
  corr <- tibble(siteID=files18) %>% 
    mutate(file_contents = map(siteID, speciescorr18)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=2017) %>%
    unnest(cols = c(file_contents)) %>%
    filter(!is.na(sp_2017)) %>%
    group_by(year, siteID, turfID) %>%
    unique 
  
  cover17_corr <- cover17 %>% 
    left_join(., corr, by = c("year", "siteID", "turfID", "SpeciesName" = "sp_2017")) %>%
    mutate(SpeciesName = ifelse(is.na(sp_2018), SpeciesName, sp_2018)) %>%
    select(-sp_2018) 
  
  #Cover18 is species x site
  cover18 <- tibble(siteID=files18) %>% 
    mutate(file_contents = map(siteID, collector18)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=2018) %>%
    unnest(cols = c(file_contents)) %>%
    dplyr::select(siteID, year, turfID, SpeciesName, cover) 

  #Cover19 is a single excel doc with sheets for each site
  cover19 <- tibble(dat = files19) %>% 
    mutate(file_contents = map(dat, collector19)) %>%
    mutate(year=2019) %>%
    select(-dat) %>%
    unnest(cols = c(file_contents)) 
  
  alldat <- bind_rows(cover17_corr, cover18, cover19) 

  return(alldat)
}
