#############################
### TRANSPLANT DRAKE PLAN ###
#############################

# Load libraries
library("drake")
library("tidyverse")
library("vegan")
library("readxl")
library("lubridate")
library("e1071")
library("DBI")
library("RSQLite")
library("visNetwork")

# drake configurations
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# trick
pn <- . %>% print(n = Inf)

# source cleaning scripts
path <- c('./R/functions/ImportData/')    
source_files <- list.files(path, "\\.R$")  
map(paste0(path, source_files), source)

#Source downstream scripts
source("R/merge_community.R")

# Import Data
ImportDrakePlan <- drake_plan(

  NO_Ulvhaugen = ImportClean_NO_Norway(g = 1), 
  NO_Lavisdalen = ImportClean_NO_Norway(g = 2),
  NO_Gudmedalen = ImportClean_NO_Norway(g = 3),
  NO_Skjellingahaugen = ImportClean_NO_Norway(g = 4),
  
  CH_Lavey = ImportClean_CH_Lavey(),
  CH_Calanda = ImportClean_CH_Calanda(),
  CH_Calanda2 = ImportClean_CH_Calanda2(), 

  US_Colorado = ImportClean_US_Colorado(), 
  US_Montana = ImportClean_US_Montana(), 
  US_Arizona = ImportClean_US_Arizona(), 

  CN_Gongga = ImportClean_CN_Gongga(), 
  CN_Damxung = ImportClean_CN_Damxung(), 
  IN_Kashmir = ImportClean_IN_Kashmir(), 
  CN_Heibei = ImportClean_CN_Heibei(),

  DE_Grainau = ImportClean_DE_Grainau(), 
  DE_Susalps = ImportClean_DE_Susalps(),
  DE_TransAlps = ImportClean_DE_TransAlps(),
  FR_AlpeHuez = ImportClean_FR_AlpeHuez(), 
  SE_Abisko = ImportClean_SE_Abisko(),
  FR_Lautaret = ImportClean_FR_Lautaret(), 
  IT_MatschMazia1 = ImportClean_IT_MatschMazia1(),
  IT_MatschMazia2 = ImportClean_IT_MatschMazia2()  
)

#Make taxa vectors all a dataframe with column name 'SpeciesName', keep consistent across all dataframes
#destBlockID needs to be added to Norway data, Gongga, etc. destBlockID <- NA


MergeDrakePlan <- drake_plan(
  dat = merge_comm_data(alldat = tibble::lst(NO_Ulvhaugen, NO_Lavisdalen, NO_Gudmedalen, NO_Skjellingahaugen, 
    CH_Lavey, CH_Calanda, CH_Calanda2,
    US_Colorado, US_Montana, US_Arizona,
    CN_Damxung, IN_Kashmir, CN_Gongga, CN_Heibei, 
    DE_Grainau, DE_Susalps, DE_TransAlps, FR_AlpeHuez, SE_Abisko, FR_Lautaret, IT_MatschMazia1, IT_MatschMazia2))
)

MyPlan <- bind_rows(ImportDrakePlan, MergeDrakePlan)

conf <- drake_config(MyPlan)
conf

# loadd(dat)
# map(dat, ~sum(is.na(.))) %>% as.data.frame(.) %>% t() %>% View()
