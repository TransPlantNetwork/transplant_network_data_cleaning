###### Calanda community data functions ####
#Chelsea Chisholm, 11.03.2019

load_cover_CH_Calanda <- function(){
  #import data
  dat <- read.csv('./data/CH_Calanda/CH_Calanda_commdata/relevee_database.csv', sep=';', stringsAsFactors = FALSE)
  cover <- dat %>% 
    select(year, Treatment, Block, plot_id, Site, turf_type, Species_Name, Cov_Rel1, Cov_Rel2) %>%
    #remove NF and na values
    mutate(Cov_Rel1=na_if(Cov_Rel1, '\xa7'), Cov_Rel1 = na_if(Cov_Rel1, "NF"), Cov_Rel2= na_if(Cov_Rel2, '\xa7'), Cov_Rel2= na_if(Cov_Rel2, "NF")) %>% 
    mutate(Cov_Rel1 = gsub(',|-', '.', Cov_Rel1), Cov_Rel2 = gsub(',|-', '.', Cov_Rel2)) %>%
    mutate(Cov_Rel1=as.numeric(Cov_Rel1), Cov_Rel2=as.numeric(Cov_Rel2)) %>%
    group_by(year, Treatment, Block, plot_id, Site, turf_type, Species_Name) %>%
    mutate(Cover = mean(c(Cov_Rel1, Cov_Rel2), na.rm=T)) %>%
    filter(!is.na(Cover)) 
    
  return(cover)
}
