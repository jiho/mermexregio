library("sp")
library("raster")
library("plyr")
library("tidyverse")

source("lib_plot.R")
source("lib_cluster.R")
source("lib_coastline.R")

## Read data ----

# rasters
r <- list()
# polygons
p <- list()

# Bathymetry
message("Add bathy")
b <- read.table("bathy/med_4min.xyz.gz", col.names=c("lon", "lat", "z"))
br <- xyz2raster(b, "z")
br4 <- mask(br, med_mask)
b <- read.table("bathy/med_1min.xyz.gz", col.names=c("lon", "lat", "z"))
br <- xyz2raster(b, "z")
br1 <- mask(br, med_mask)
r[["Bathymetry"]] <- list("4 min"=br4, "1min"=br1)

# Congruence
message("Add frontiers")
load("consensus.Rdata")
r[["Frontiers congruence"]] <- list()
r[["Frontiers congruence"]][["Smoothed"]] <- mask(smoothed_congruence, med_mask)
r[["Frontiers congruence"]][["Count"]] <- congruence

# Retained regions
message("Add regions")
load("regions_retained.RData")
d <- select(d, id, lon, lat, cluster)
r[["Retained regionalisations"]] <- dlply(d, ~id, xyz2raster, z="cluster")
p[["Retained regionalisations"]] <- llply(r[["Retained regionalisations"]], raster2poly)

# All regionalisations
load("regions_filtered.RData")
d <- select(d, id, lon, lat, cluster)
r[["Any regionalisation"]] <- dlply(d, ~id, xyz2raster, z="cluster")
p[["Any regionalisation"]] <- llply(r[["Any regionalisation"]], raster2poly)

# Gridded regionalisations
load("regions_gridded.RData")
d <- select(d, id, lon, lat, cluster)
r[["Raw regionalisations"]] <- dlply(d, ~id, xyz2raster, z="cluster")

# Threats
message("Add threats")
process_threat_layer <- function(path, recenter) {
  # read file
  threats <- raster(paste0("threats/medthreats/", path))
  # force centering on 18,38
  projection(threats) <- "+proj=laea +lat_0=38 +lon_0=18 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  # reproject at at the same resolution as the rest
  threats <- projectRaster(threats, res=0.1, crs=projection(med_mask))
  # mask regions out of the med
  threats <- mask(threats, med_mask)
  return(threats)
}
# all individual layers
l <- read.csv("threats/medthreats/layers.csv") %>% filter(category=="threat") %>% select(description, path_to)
threats <- llply(l$path_to, process_threat_layer, .progress="text")
# prepend the cumulative model
threats <- c(process_threat_layer("model/model_all_med_annual_sst_clean.tif"), threats)
# add names
names(threats) <- c("Cumulative", l$description)
# and store
r[["Threats (Micheli et al 2013)"]] <- threats

# Project all raster layers (to speed up display)
message("Reproject all layers")
r <- llply(r, function(x) {
  llply(x, function(x) {
    projectRaster(x, projectExtent(x, crs = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")), method="ngb")
  })
}, .progress="text")

# save data for the shiny app
save(r, p, regions, ridges, clr, file="webapp/data.Rdata")
