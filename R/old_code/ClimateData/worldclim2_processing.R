#### Read in worldclim data (downloaded to /climate/Worldclim2)
library(rgdal)
library(raster)
library(sp)

# Read in metadata (using meta list item from individual sites)
alldat <- tibble::lst(NO_Ulvhaugen, NO_Lavisdalen, NO_Gudmedalen, NO_Skjellingahaugen, 
                     CH_Lavey, CH_Calanda, 
                     US_Colorado, US_Montana, US_Arizona,
                     CN_Damxung, IN_Kashmir, CN_Gongga, CN_Heibei, 
                     DE_Grainau, DE_Susalps, DE_TransAlps, FR_AlpeHuez, SE_Abisko, FR_Lautaret, IT_MatschMazia1, IT_MatschMazia2)
meta <- alldat %>% 
  map_df("meta", .id='Region') %>%
  ungroup() 
  
# Create spatial points coordinates data
coords <- data.frame(x=as.numeric(meta$Longitude),y=as.numeric(meta$Latitude))
points <- SpatialPoints(coords, proj4string = CRS("+proj=longlat"))

#### Get elevation data ####
#  Generate a stack object with elevation used for worldclim2
E_filenames <- "./climate/Worldclim2/wc2.1_30s_elev.tif"
elev <- stack(E_filenames)
values <- extract(elev, points)
colnames(values) <- "wc2.1_30s_elev"

## Get temperature data 
T_filenames<- paste("./climate/Worldclim2/wc2.1_30s_tavg/wc2.1_30s_tavg_", c(paste(0,1:9, sep=""), 10,11,12),
                    ".tif",sep="")
temp30<- stack(T_filenames)
temps <- extract(temp30, coords)

## Get vapor pressure deficit data 
V_filenames<- paste("./climate/Worldclim2/wc2.1_30s_vapr/wc2.1_30s_vapr_", c(paste(0,1:9, sep=""), 10,11,12),
                    ".tif",sep="")
vapr30<- stack(V_filenames)
vapr <- extract(vapr30, coords)

## Get annual precip data 
P_filenames<- paste("./climate/Worldclim2/wc2.1_30s_prec/wc2.1_30s_prec_", c(paste(0,1:9, sep=""), 10,11,12),
                    ".tif",sep="")
prec30<- stack(P_filenames)
prec <- extract(prec30, coords)

df <- cbind.data.frame(coordinates(points), values, temps, vapr, prec)
colnames(df)[1:2] <- c('x','y')
dat <- cbind.data.frame(meta, df)

# Calculate lapse rate for each site for temp and include average vapr (how to make this cumulative? does that even make sense?)
dat2 <- dat %>% rowwise() %>%
  # average temp
  mutate(T_ann = mean(c(wc2.1_30s_tavg_01:wc2.1_30s_tavg_12))) %>%
  mutate(T_sum = mean(c(wc2.1_30s_tavg_05:wc2.1_30s_tavg_09))) %>%
  mutate(T_win = mean(c(wc2.1_30s_tavg_10, wc2.1_30s_tavg_11, wc2.1_30s_tavg_12, wc2.1_30s_tavg_01, wc2.1_30s_tavg_02, wc2.1_30s_tavg_03))) %>%
  mutate(T_ann_cor = T_ann+(0.0098*(wc2.1_30s_elev-Elevation)),
         T_sum_cor = T_sum+(0.0098*(wc2.1_30s_elev-Elevation)),
         T_win_cor = T_win+(0.0098*(wc2.1_30s_elev-Elevation))) %>%
  # VPD
  mutate(V_ann = mean(c(wc2.1_30s_vapr_01:wc2.1_30s_vapr_12))) %>%
  mutate(V_sum = mean(c(wc2.1_30s_vapr_04:wc2.1_30s_vapr_09))) %>%
  mutate(V_win = mean(c(wc2.1_30s_vapr_10, wc2.1_30s_vapr_11, wc2.1_30s_vapr_12, wc2.1_30s_vapr_01, wc2.1_30s_vapr_02, wc2.1_30s_vapr_03))) %>%
  
  # annual precip
  mutate(P_ann = sum(c(wc2.1_30s_prec_01:wc2.1_30s_prec_12))) %>%
  mutate(P_sum = sum(c(wc2.1_30s_prec_04:wc2.1_30s_prec_09))) %>%
  mutate(P_win = sum(c(wc2.1_30s_prec_10, wc2.1_30s_prec_11, wc2.1_30s_prec_12, wc2.1_30s_prec_01, wc2.1_30s_prec_02, wc2.1_30s_prec_03))) %>%
  dplyr::select(Gradient, destSiteID, Latitude, Longitude, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country, Elevation, Elevation_st = wc2.1_30s_elev, T_ann, T_sum, T_win, T_ann_cor, T_sum_cor, T_win_cor, V_ann, V_sum, V_win, P_ann, P_sum, P_win)

# Issue with correcting temp for US_Arizona, actually overcorrects so that the T_ann_cor at low site is colder than high site. Use summer temperature instead!

write.csv(dat2, './climate/worlclim2_processedclimate.csv')
