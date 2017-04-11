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
br <- mask(br, med_mask)
r[["Bathymetry"]] <- list("4 min"=br)

# Congruence
message("Add frontiers")
load("consensus.Rdata")
r[["Frontiers congruence"]] <- list()
r[["Frontiers congruence"]][["Count"]] <- congruence
r[["Frontiers congruence"]][["Smoothed"]] <- mask(smoothed_congruence, med_mask)

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
r[["Gridded regionalisations"]] <- dlply(d, ~id, xyz2raster, z="cluster")

# Project all raster layers (to speed up display)
message("Reproject all layers")
r <- llply(r, function(x) {
  llply(x, function(x) {
    projectRaster(x, projectExtent(x, crs = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")), method="ngb")
  })
}, .progress="text")

# save data for the shiny app
save(r, p, regions, ridges, clr, file="webapp/data.Rdata")
