library("sp")
library("raster")
library("plyr")
library("stringr")
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
r[["Retained regionalisations (cleaned)"]] <- dlply(d, ~id, xyz2raster, z="cluster")
p[["Retained regionalisations (cleaned)"]] <- llply(r[["Retained regionalisations (cleaned)"]], raster2poly)

# All regionalisations
load("regions_filtered.RData")
d <- select(d, id, lon, lat, cluster)
r[["Cleaned regionalisations"]] <- dlply(d, ~id, xyz2raster, z="cluster")
p[["Cleaned regionalisations"]] <- llply(r[["Cleaned regionalisations"]], raster2poly)

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


# Protected areas
message("Add protected areas")
load("protection.RData")
protect <- list(
  Existing=protect_existing,
  Proposed=protect_proposed,
  Both=protect_tot
)
# remove pixels outside of Med
protect <- llply(protect, mask, med_mask)
# and store
r[["Protection areas (Micheli et al 2013)"]] <- protect


## Process data ---

# Save all layers to files
# write rasters as geoTIFF
rs <- r[c("Frontiers congruence", "Retained regionalisations", "Any regionalisation", "Raw regionalisations")]
rs <- unlist(rs)
names(rs) <- str_replace_all(names(rs), "[ \\.\\(\\)=’']", "_")
names(rs) <- str_replace_all(names(rs), "à", "a")
names(rs) <- str_replace_all(names(rs), "__", "_")
names(rs) <- str_replace_all(names(rs), "_$", "")
l_ply(names(rs), function(x) {
  writeRaster(rs[[x]], filename=str_c("webapp/www/", x, ".tif"), format="GTiff", overwrite=TRUE)
}, .progress="text")

# write polygons as shapefiles
writeOGR(regions, dsn="webapp/www/", layer="Consensus_regions", driver="ESRI Shapefile", overwrite_layer=T)
zip("webapp/www/Consensus_regions.zip", files=list.files("webapp/www/", pattern="Consensus_regions", full=T))
writeOGR(ridges, dsn="webapp/www/", layer="Consensus_frontiers", driver="ESRI Shapefile", overwrite_layer=T)
zip("webapp/www/Consensus_frontiers.zip", files=list.files("webapp/www/", pattern="Consensus_frontiers", full=T))


# Project all raster layers for leaflet (to speed up display)
message("Reproject all layers")
r <- llply(r, function(x) {
  llply(x, function(x) {
    projectRaster(x, projectExtent(x, crs = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")), method="ngb")
  })
}, .progress="text")


## Save data ---

# save data for the shiny app
save(r, p, regions, ridges, clr, file="webapp/data.Rdata")
