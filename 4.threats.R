#
# Read cumulative threats from Micheli et al 2013, process them and compute stats per regions
#
# (c) 2016 Jean-Olivier Irisson, GNU General Public License v3
#

library("rgdal")
library("raster")
library("broom")
library("plyr")
library("dplyr")
library("ggplot2")

source("lib_plot.R")
source("lib_coastline.R")


## Read threats ----

# read cumulative threats from Micheli
threats <- raster("threats/medthreats/model/model_all_med_annual_sst_clean.tif", p4s="+proj=laea +lat_0=38 +lon_0=18 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# reproject threats raster onto the same CRS as the one used by med_mask, regions (and all the rest)
threats_proj <- projectRaster(threats, res=0.1, crs=projection(med_mask))
# plot(threats_proj)

# mask pixels near the coast (and in the black sea)
threats_proj <- mask(threats_proj, med_mask)
# plot(threats_proj)

# convert to data.frame for plotting
threats_proj_df <- data.frame(coordinates(threats_proj), threats=getValues(threats_proj))
threats_proj_df <- rename(threats_proj_df, lon=x, lat=y)
threats_proj_df <- na.omit(threats_proj_df)

# get full range of values over the Med
range(threats_proj_df$threats)
# [1]  1.467439 19.141973


## Compute and plot stats per region ----

# get final region and frontiers for plotting
load("synthetic_regions_and_frontiers.RData")

# get regions info
glance.SpatialPolygons <- function(x) {
  d1 <- as.data.frame(x)
  d2 <- ldply(x@polygons, function(x) {
    labs <- data.frame(t(x@labpt))
    names(labs) <- c("lon", "lat")
    data.frame(labs, plotOrder=x@plotOrder, area=x@area)
  })
  cbind(d1, d2)
}
regions_info <- glance(regions)

# extract threats per region
per_region <- raster::extract(threats_proj, regions)
# and compute summary statistics
stat_per_region <- ldply(per_region, function(x) {
  data.frame(
    mean=mean(x, na.rm=T),
    sd=sd(x, na.rm=T),
    median=median(x, na.rm=T),
    mad=mad(x, na.rm=T)
  )
})
# associate with names and other characteristics of the regions
(stat_per_region <- cbind(select(regions_info, lon, lat, area), stat_per_region) %>% arrange(mean))
#          lon      lat     area     mean        sd   median       mad
# 1   6.348255 42.45426 5.118689 6.521065 0.9355536 6.327923 0.6928974
# 2  24.499041 38.91974 2.761458 7.244163 0.8408299 7.101298 0.5403883
# 3  33.040386 33.37509 9.557849 7.663863 1.6673566 7.415904 1.3329226
# 4  11.020561 33.99404 1.314587 7.720535 3.2528174 7.541304 4.3141335
# 5  18.412114 33.69915 4.935625 8.137292 0.7201071 8.273689 0.4793287
# 6  18.975654 35.90877 2.483750 8.274811 0.5366429 8.275125 0.3187360
# 7  17.071558 42.17324 7.188485 8.526723 2.4699770 8.567819 2.0183637
# 8   6.648030 38.14334 8.198385 8.559400 1.9196471 8.049050 1.5638004
# 9  25.667252 36.33054 4.446379 8.628716 1.2310602 8.431601 0.7734671
# 10 12.881931 44.91081 1.221160 8.988919 3.2618803 8.866291 4.3241646
# 11 30.964493 35.98848 2.039734 9.093476 0.8728047 9.214047 0.6883602

# plot to check which is most impacted
bd + geom_point(aes(colour=mean), data=stat_per_region, size=10) + scale_colour_distiller(palette="RdYlGn")

# figure for paper
p <- base + theme_dark(8) + map +
  # threats
  geom_raster(aes(fill=threats), data=threats_proj_df) +
  # regions
  geom_polygon(aes(group=group), data=regions_df, colour="black", size=0.2, fill="black", alpha=0.1) +
  # frontiers
  geom_path(aes(group=group), data=ridges_df, size=0.4, alpha=1, colour="black", lineend="round") +
  geom_path(aes(group=group), data=ridges_df, size=0.2, alpha=1  , colour="white", lineend="round", linetype="dashed") +
  scale_fill_distiller(palette="RdYlGn") +
  labs(fill="Cumulative impacts") +
  # setup
  coastd + legend_inside()
# ggsave(p, file="fig5.png", width=3.5, height=1.8) # dashed line incorrect
ggsave(p, file="fig5.pdf", width=3.5, height=1.8)
system("convert -density 300 fig5.pdf fig5.png")
pg <- remove_last_layer(p) + scale_fill_distiller(palette="Greys", direction=1) + coastl + legend_inside(text="black")
# ggsave(pg, file="fig5-bw.png", width=3.5, height=1.8)
ggsave(pg, file="fig5-bw.pdf", width=3.5, height=1.8)
system("convert -density 300 fig5-bw.pdf fig5-bw.png")
