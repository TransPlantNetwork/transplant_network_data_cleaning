#### READING IN TERRACLIM ####
#4 km downscaled data worldwide (using worldclim (1 km) and cruts (0.5 deg))

# Read in lat lon metadata
metadata <- read_excel(path = '~/Dropbox/projects/SNF Experiment/Transplant_Incline/data/overview/MetadataOverview_Gradient_Site.xlsx', 
                       sheet='Site level', skip = c(2))
metadat <- data.frame(gradient = metadata[,1], site = metadata[,2], lat =  metadata[,5], long = metadata[,6], elev=metadata[4], year1=metadata[7], yearn=metadata[8])
colnames(metadat) <- c('gradient', 'site', 'lat', 'long', 'elev', 'year1', 'yearn') 
metadat <- metadat %>% group_by(gradient,site) %>% summarize(lat=mean(as.numeric(lat)), long=mean(as.numeric(long)),
                                                             yearn=mean(yearn), year1=mean(year1), 
                                                             elev=elev, Yearrange=yearn-year1)
coords <- data.frame(x=metadat$long,y=metadat$lat)
points <- SpatialPoints(coords, proj4string = pre@crs)
write.csv(coords, '~/Desktop/coords.csv')

# Download data
# enter in longitude, latitude here
x<-coords[1,]

# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
var="aet"

library(ncdf4)
# Remote access for download
baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")

nc <- nc_open(baseurlagg)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
flat = match(abs(lat - x[2]) < 1/48, 1)
latindex = which(flat %in% 1)
flon = match(abs(lon - x[1]) < 1/48, 1)
lonindex = which(flon %in% 1)
start <- c(lonindex, latindex, 1)
count <- c(1, 1, -1)


# read in the full period of record using aggregated files

data <- as.numeric(ncvar_get(nc, varid = var,start = start, count))

