#
# Read coastline and define which points are in sea
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#


# read coastline
med <- read.csv("coastline/gshhg_medit_i.csv")

# define domain
domain_lon <- range(med$lon, na.rm=T)
domain_lat <- range(med$lat, na.rm=T)

# read sea mask
library("rgdal")
# mask <- readOGR("coastline", "mask_coastline", verb=F)
med_mask <- readOGR("coastline", "mask_coastline_buffered_0.05", verb=F)

# convert to data.frame
med_maskdf <- suppressMessages(fortify(med_mask))
names(med_maskdf)[1] <- "lon"

# Check if points are in sea
# points  data.frame with lon, lat of points
# mask    sea mask, data.frame with lon, lat, and hole status
in_sea <- function(points, mask=med_maskdf, ...) {
  library("sp")
  library("plyr")

  # check all points for each piece
  in_mask <- daply(mask, ~piece, function(x) {
    # should be outside holes and inside the rest => != hole (when hole is FALSE, should be in polygon; and conversely, when hole is TRUE, should be outside)
    point.in.polygon(point.x=points$lon, point.y=points$lat, pol.x=x$lon, pol.y=x$lat) != x$hole[1]
  }, ...)

  # check if all are true
  colSums(in_mask) == max(colSums(in_mask))
}


# get some size orders for distances
library("oce")
mlon <- mean(domain_lon)
mlat <- mean(domain_lat)
geodDist(longitude1=mlon, longitude2=mlon, latitude1=mlat, latitude2=mlat+1)
# [1] 111.006
geodDist(longitude1=mlon, longitude2=mlon+1, latitude1=mlat, latitude2=mlat)
# [1] 87.83204

# 1º ~ 100 km
