#using Richard's function (mainly for Lavey)

# install.packages("devtools")
#devtools::install_github("Between-the-Fjords/turfmapper", force=TRUE)

library(turfmapper)
library("tidyverse")

dat <- CH_Lavey$community

#set up subturf grid
grid <- make_grid(ncol = 1)

#A single plot example (forcing using subturf=1)
dat %>%
  filter(destPlotID=='CRE_CRE_T1') %>%
  mutate(subturf=1) %>%
  make_turf_plot(
    year = Year, species = SpeciesName, cover = Cover, subturf = subturf, 
    title = glue::glue("Site {.$destSiteID}: plot {.$destPlotID}"), 
    grid_long = grid)

#Make separate figs per plot (forcing using subturf=1)
figs <- dat %>% 
  arrange(destSiteID, destPlotID) %>% 
  mutate(subturf = 1) %>%
  group_by(destSiteID, destPlotID) %>% 
  nest() %>% 
  {map2(
    .x = .$data, 
    .y = glue::glue("Site {.$destSiteID}: plot {.$destPlotID}"),
    .f = ~make_turf_plot(
      data = .x, year = Year, species = SpeciesName, cover = Cover, subturf = subturf, title = .y, grid_long = grid)
   )} #%>% 
  # walk(print) #print all maps

#Print into single pdf file for Loic
pdf(file = "~/Desktop/lavey_specieschecks.pdf") 
print(figs)
dev.off()
