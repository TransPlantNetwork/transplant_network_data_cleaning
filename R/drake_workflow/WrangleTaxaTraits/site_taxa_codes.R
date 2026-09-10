### COLLATE SPECIES LOOKUP TABLES FROM THREE SITES ---------------------------------------------
## Script for species name matching for sites with species codes (SE_Abisko, Norway, US_Arizona)

## SE_Abisko
load_SE_Abisko_sptable <- function() {

    # 2012 species list (incomplete)
    splist2012 <- read_excel("data/SE_Abisko/SE_Abisko_commdata/Vegetation data Abisko transplantation experiment (2012 + 2013 + 2014 + 2015)_for Chelsea.xlsx", sheet = "Species list 2012")
    taxa <- c(splist2012$Name...2, splist2012$Name...4)
    code <- c(splist2012$Code...1, splist2012$Code...3)
    df <- data.frame(code=code, taxa=taxa)
    df <- df[complete.cases(df),]
    
    
    # 2017 species (BUT THIS STILL DOESN'T FIX ABOUT 10 SPECIES!)
    splist2017 <- read_excel("data/SE_Abisko/SE_Abisko_commdata/Veenetal_splist.xlsx", sheet=1)
    splist2017 <- splist2017 %>% separate(species, c("A", "B", "C", "D"), sep = " ") %>% 
      filter(!A %in% c('moss', 'litter', 'lichen')) %>%
      mutate(code = paste(substr(A, 1, 3), substr(B, 1, 3), sep=" "),
             taxa = paste(A, B, sep=" ")) %>%
      select(taxa, code)
    
    allyears <- bind_rows(df, splist2017)
    
    se_dat <- allyears[!allyears$taxa %in% c('Lichen', 'Litter', 'Moss'),] 
    
    se_dat <- se_dat %>% distinct()
    
    # Clean up individual species issues
    se_dat <- se_dat %>% 
      mutate(code = recode(code, 'Hie alp' = "Hie sect. Alp", 'Luz mul' = "Luz mul (subsp fri)", 'Luz wah' = "Luz wal",
             'Rum ace'= "Rum ace subsp lap", 'Cry fra' = 'Cys fra', 'Epi lac' = 'Epi lac', 'Equ pal' = 'Equ pal/pra', 'Ste nem' = 'Ste nem',
             'Tri spi' = 'Tri spi', 'Ver fru' = 'Ver fru', 'Ver sp.' = 'Ver spe'))
    
    return(se_dat)

}

## Norway
# source code needs to be outside of the function to avoid self invalidation? 
source("R/functions/ImportData/community_NO_Norway/loadCover.r")

load_Norway_sptable <- function() {
  con <- src_sqlite(path = file_in("data/NO_Norway/seedclim.sqlite"), create = FALSE)
  #get taxonomy table
  taxa <- tbl(con, "taxon") %>%
    collect() 
  taxa <- unique(taxa$speciesName)
  no <- str_split_fixed(taxa, ' ', 2)
  no[,2] <- str_replace_all(no[,2], c('sp?'='sp', 'sp.'='sp', 'sp??'='sp', 'sp1?'='sp',
                                      'spp'='sp', 'spp.'='sp', 'spp1?'='sp'))
  no[,2] <- gsub('[[:punct:] ]+','',no[,2])
  code <- paste(substr(no[,1], 1, 3), substr(no[,2], 1, 3), sep='.')  
  no_dat <- data.frame(code=code, taxa=taxa)
  no_dat <- no_dat %>% 
    mutate(code = recode(code, Aco.spe = "Aco.sep", Car.spx = "Car.sax", Car.car = "Caru.car", 
                         Gal.spx = "Gal.sax", Ger.spl = "Ger.syl", Hup.spl = "Hup.sel", Kob.spm = "Kob.sim",
                         Luz.spc = "Luz.spi", Mel.spl = "Mel.syl", Nar.spr = "Nar.str", Oma.spn = "Oma.sup",
                         Oma.spl = "Oma.syl", Pim.spx = "Pim.sax", Pin.spl = "Pin.syl", Sel.spl = "Sel.sel",
                         Val.spm = "Val.sam", Ver.spr = "Ver.ser")) %>%
    mutate(code = if_else(code == "Hyp.mac" & taxa == "Hypericum maculatum", "Hype.mac", 
                           if_else(code == "Hyp.mac" & taxa == "Hypochaeris maculata", "Hypo.mac", code))) %>%
    mutate(code = if_else(code == "Rum.ace" & taxa == "Rumex acetosella", "Rum.acl", code))
    
  return(no_dat)
}

##Arizona
load_US_Arizona_sptable <- function() {
  splist <- read_excel(file_in("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx"), sheet = "Species List ") %>%
    mutate(Species_FullName = paste(Genus, Species)) 
  
  us_dat <- data.frame(code = splist$Code, taxa=splist$Species_FullName)
  
  return(us_dat)
}

##Function to merge all
merge_sptable <- function(spdat) {
  sp_codes <- bind_rows(list("SE_Abisko" = spdat$se_dat, "NO_Ulvhaugen"=spdat$no_dat, "NO_Lavisdalen"=spdat$no_dat, "NO_Gudmedalen"=spdat$no_dat,
                             "NO_Skjellingahaugen"=spdat$no_dat, "US_Arizona" = spdat$us_dat), .id="Region") %>%
    filter(!taxa %in% c(NA, "NA NA")) 
  
  return(sp_codes)
}