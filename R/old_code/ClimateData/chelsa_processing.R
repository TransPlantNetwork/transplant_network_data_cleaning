library(raster)
chelsaFiles <- list.files(path=...,
                          pattern=...,
                          full.names=T)
chelsaRasts <- stack(chelsaFiles)
chelsaDat <- extract(chelsaRasts, myXYpoints)