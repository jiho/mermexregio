#
# Superposition of boundaries to define a new global regionalisation
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("doParallel")
registerDoParallel(cores=detectCores()-1)

library("raster")
library("rgeos")
library("rgdal")
library("ncdf4helpers")
library("broom")
library("stringr")

library("plyr")
library("reshape2")
library("tidyverse")

source("lib_plot.R")
source("lib_coastline.R")
source("lib_custer.R")


## Load and choose data ----

load("regions_filtered.RData")

# choose which regionalisations to keep
possib <- unique(d$id)
row.names(possib) <- NULL
possib
(possib <- possib[c(1, 3, 7, 10, 12, 13, 17, 18)])

# keep only those
d <- d[d$id %in% possib,]
(n_studies <- length(possib))

# plot the retained regions
p <- bd + geom_raster(aes(fill=clr), data=d) + facet_wrap(~id) + scale_fill_identity()
ggsave(p, file="regions_retained.png", width=10, height=6)

# cleaner version for the paper figure
# add letter label
ids <- unique(d$id)
d$label <- factor(d$id, levels=ids, labels=str_c("  ", LETTERS[1:n_studies], ".     ", ids))
p <- bd + facet_wrap(~label) +
  geom_raster(aes(fill=clr), data=d) + scale_fill_identity() +
  coastd +
  theme(strip.text=element_text(hjust=0))
ggsave(p, file="fig2.png", width=7.2, height=4.3)
ggsave(p, file="fig2.pdf", width=7.2, height=4.3)

# greyscale version
pg <- bo + facet_wrap(~label) +
  geom_raster(aes(fill=cluster), data=d) +
  scale_fill_regions_bw() +
  coasto +
  theme(strip.text=element_text(hjust=0))
ggsave(pg, file="fig2-bw.png", width=7.2, height=4.3)
ggsave(pg, file="fig2-bw.pdf", width=7.2, height=4.3)

# save data for later use
save(d, file="regions_retained.RData")


## Compute frontiers ----

load("regions_retained.RData")

# compute frontiers between regions
frontiers <- ddply(d, ~id, function(x) {
  # x <- filter(d, study=="dortenzio")

  xm <- x

  # consider just numbers
  xm$cluster <- as.numeric(xm$cluster)
  # convert to matrix
  xm <- acast(xm, lat~lon, value.var="cluster")

  # detect shifts horizontally and vertically
  h <- aaply(xm, 1, function(xx) {c(NA, diff(xx)!=0)})
  v <- aaply(xm, 2, function(xx) {c(NA, diff(xx)!=0)})

  # reconvert to a data.frame
  h <- melt(h, varnames=c("lat", "lon"), value.name="is_frontier")
  v <- melt(t(v), varnames=c("lat", "lon"), value.name="is_frontier")

  # remove data out of the sea
  h <- na.omit(h)
  v <- na.omit(v)

  # because of the way we detect them, the border pixels are those *after* the border, in lon and lat
  # to find their actual coordinates, they should be shifted by minus half a grid cell
  cell <- unique(round(diff(sort(unique(x$lon))),10))
  h$lon <- h$lon - cell/2
  h1 <- h2 <- h
  h1$lat <- h1$lat - cell/2
  h2$lat <- h2$lat + cell/2
  h <- rbind(h1, h2)

  v$lat <- v$lat - cell/2
  v1 <- v2 <- v
  v1$lon <- v1$lon - cell/2
  v2$lon <- v2$lon + cell/2
  v <- rbind(v1, v2)

  # combine vertical and horizontal frontiers
  f <- rbind(h, v)
  # NB: round up to avoid pbs with high level precision
  f$lat <- round(f$lat, 10)
  f$lon <- round(f$lon, 10)
  f <- unique(f)
  # the same border can be seen as a frontier horizontally but not vertically
  # reduce to just one status
  f <- f %>% group_by(lat, lon) %>% summarise(is_frontier=any(is_frontier)) %>% ungroup()

  # # check
  # bl + geom_raster(aes(fill=cluster), data=x) + geom_point(data=f, size=0.5, alpha=0.3)
  # bl + geom_raster(data=f)
  # bl + geom_raster(aes(fill=cluster), data=x) + geom_point(data=filter(f, is_frontier), size=0.5)

  return(f)
}, .progress="text", .parallel=T)

# # plot to check
# clr <- brewer.colors(11, name="Set3")
# clr <- c(clr, darker(saturate(clr)))
# p <- base + geom_raster(aes(fill=cluster), data=d) + geom_point(data=filter(frontiers, is_frontier), size=0.01, colour="red") + facet_wrap(~study) + scale_fill_manual(values=clr, guide="none") + coastd + theme_dark()
# ggsave(p, file="frontiers_on_clusters.png", width=40, height=20)

# compute:

# boundary count
f <- frontiers %>% group_by(lon, lat) %>% summarise(count=sum(is_frontier, na.rm=T))

# frequency
f$freq <- f$count / n_studies

# frequency weighted by number of clusters per regionalisation
n_clust <- d %>% group_by(id) %>% summarise(n=length(unique(cluster)))
fw <- frontiers %>%
  # give weights inversely proportional to the number of clusters
  left_join(n_clust) %>% mutate(w=as.numeric(is_frontier)/n) %>%
  # compute weighted frontiers frequency
  group_by(lon, lat) %>% summarise(f_w_nb_cluster=sum(w, na.rm=T)) %>% ungroup()
# scale it by the maximum value it could take (if all regionalisation agreed)
fw$f_w_nb_cluster <- fw$f_w_nb_cluster / sum(1/n_clust$n)
# add that to the non weighted version
f <- left_join(f, fw)

# frequency weighted by number of frontier pixels per regionalisation
# remove Spalding that has too much weight
f_front <- frontiers %>% group_by(id) %>% summarise(f=sum(is_frontier)/n())
fw <- frontiers %>%
  # give weights inversely proportional to proportion of frontiers
  left_join(f_front) %>% mutate(w=as.numeric(is_frontier)/f) %>%
  # compute weighted frontiers frequency
  group_by(lon, lat) %>% summarise(f_w_f_frontier=sum(w, na.rm=T)) %>% ungroup()
# scale it by the maximum value it could take (if all regionalisation agreed)
fw$f_w_f_frontier <- fw$f_w_f_frontier / sum(1/f_front$f)
# add that to the non weighted version
f <- left_join(f, fw)

f <- ungroup(f)

# compute a binned version of frontiers, to give a broad frontier strength estimate
f$strength <- cut(f$count, breaks=c(0,1.5,3.5,5), include.lowest=TRUE, right=TRUE, labels=c("No/weak", "Medium", "Strong"))

# plot all frontier weighting schemes
ft <- gather(f, key="frontier_type", value="freq", starts_with("f"))
ft$frontier_type <- factor(ft$frontier_type, levels=c("freq", "f_w_nb_cluster", "f_w_f_frontier"), labels=c("Frequency of frontier", "Freq. weighted by nb clusters", "Freq. weigthed by prop of frontier pixels"))
bd + geom_raster(aes(fill=freq), data=ft) + scale_fill_distiller(palette="YlGnBu") + facet_wrap(~frontier_type, ncol=1)

# plot binned frontiers
bd + geom_raster(aes(fill=strength), data=f) + scale_fill_manual(values=brewer.colors(3, name="YlGnBu", rev=T))

# -> use the basic count, which is clearer and simpler

# plot frontiers map for paper
p <- base + theme_dark(8) + map +
  geom_raster(aes(fill=count), data=f) +
  scale_fill_distiller(palette="YlGnBu", guide="legend", breaks=0:5) +
  labs(fill="Congruence") +
  coastd + legend_inside(guide="l")
ggsave(p, file="fig3.png", width=3.5, height=1.8)
ggsave(p, file="fig3.pdf", width=3.5, height=1.8)
pg <- remove_last_layer(p) +
  scale_fill_gradient(low="black", high="grey90", breaks=0:5) +
  coastl + legend_inside(guide="l", text="black")
ggsave(pg, file="fig3-bw.png", width=3.5, height=1.8)
ggsave(pg, file="fig3-bw.pdf", width=3.5, height=1.8)

save(f, file="frontiers.RData")


## Smooth and define consensus regions and frontiers ----

load("frontiers.RData")

# convert frontier count into a raster
congruence <- SpatialPixelsDataFrame(select(f, lon, lat), select(f, count), proj4string=CRS("+proj=longlat +datum=WGS84"))
congruence <- raster(congruence)

# increase resolution to later smooth
congruence_i <- disaggregate(congruence, 6, method="bilinear")
# -> 1 cell = 0.15º/6 = 0.025º
#    1º lon ~ 85km  => 0.025º ~  2.1 km in lon
#    1º lat ~ 111km => 0.025º ~  2.7 km in lat


# Smooth a raster with a gaussian kernel
# @param x the input raster
# @param diameter the diameter of the smoothing window, in number of raster cells
# @return a raster, possibly extended to fit the whole smoothed image
gaussian_smooth <- function(x, diameter) {
  # define a gaussian smoothing kernel
  dn <- dnorm(seq(-1, 1, length.out=diameter), mean=0, sd=0.8)
  m <- t(t(dn)) %*% t(dn)
  m <- m - min(m)
  m <- m / max(m)

  # perform the smooth
  xs <- focal(x, w=m, mean, na.rm=TRUE, pad=TRUE)
  
  return(xs)
}

# Compute a "contour" on a raster (i.e. all cells below or above a given level), turn it into a polygon and mask it with the mediterranean coastline
# @param x input raster
# @param level contour level
# @param below consider all cells below `level`
# @retrun a SpatialPolygonDataFrame
contour_in_med <- function(x, level, below=TRUE) {
  # extract the interval we want
  if (below) {
    breaks <- c(-Inf, level)
  } else  {
    breaks <- c(level, +Inf)
  }
  rc <- cut(x, breaks=breaks)
  # convert it to polygons
  # NB: raster::rasterToPolygons does the same but much more slowly
  rp <- raster2poly(rc)
  # mask with med
  rpm <- gIntersection(rp, med_mask)
  if (class(rpm) == "SpatialCollections") {
    rpm <- rpm@polyobj
  }
  return(rpm)
}

# Extract label points (centres) of polygons
# @param x SpatialPolygons or SpatialPolygonsDataFrame object
# @return A SpatialPoints object, with the same CRS as the input
labpts <- function(x) {
  pts <- coordinates(x)
  SpatialPoints(pts, proj4string=CRS(proj4string(x)))
}

# Extract polygon points
# @param x SpatialPolygons or SpatialPolygonsDataFrame object
# @return A SpatialPoints object, with the same CRS as the input
coords <- function(x) {
  pts <- ldply(x@polygons, function(x) {
    ldply(x@Polygons, function(x) {
      if (x@hole) {
        out <- NULL
      } else {
        out <- data.frame(x@coords)
      }
      return(out)
    })
  })
  SpatialPoints(pts, proj4string=CRS(proj4string(x)))
}


# smooth congruence surface at various levels
radius <- c(0.45, 0.50, 0.55)           # in degrees
diameters <- radius * 2 / (0.15/6)      # in pixels
diameters <- round(diameters - 1)       # must be odd and integer
smoothed_congruence <- llply(diameters, function(diam) {
  gaussian_smooth(congruence_i, diam)
}, .parallel=TRUE, .inform=F)
# add (nicely formatted) names to track what is what
radius <- str_c("smoothing radius = ", sprintf("%.2f", radius), "º")
names(smoothed_congruence) <- radius

# Compute regions as contour lines
# @param xs smoothed congruence surface
# @param levels vector of contour levels. The first one will define the cores of regions of interest. Any regions defined by other levels which does not have such a core inside it will be eliminated.
# @param below see contour_in_med
# @param small.area minimum area of regions, including cores
# @param large.area minimum area of final regions, once regions without cores have been eliminated
contour_regions <- function(xs, levels, below, small.area=0.01, large.area=0.5) {

  # compute contours of regions
  xc <- llply(levels, function(l) {
    # compute contours and mask with med
    x <- contour_in_med(xs, l, below)
    # remove small regions
    x <- disaggregate(x)
    x <- x[sapply(x@polygons, function(x) { x@area > small.area })]
    return(x)
  }, .parallel=TRUE)
  names(xc) <- levels

  # keep only regions which have an intense core
  # determine the points that make up the first level contour = the core
  cores <- coords(xc[[1]])
  # then remove it from the list and, in the others, only keep polygons which have have a core inside them are are large enough
  xc <- xc[-1]
  xc <- llply(xc, function(x) {
    # match a core
    matches <- over(x, cores)
    x <- x[!is.na(matches)]
    # and be large enough
    x <- x[sapply(x@polygons, function(x) { x@area > large.area })]
    return(x)
  })
  
  return(xc)
}

# cut homogeneous regions at various levels, for each smoothing level
levels_regions <- c(0.02, 0.28, 0.3, 0.32)
regions <- llply(smoothed_congruence, contour_regions, levels=levels_regions, below=TRUE, .progress="text")
names(regions) <- radius

# do the same for consensus frontiers
levels_frontiers <- c(0.98, 0.78, 0.8, 0.82)
frontiers <- llply(smoothed_congruence, contour_regions, levels=levels_frontiers, below=FALSE, large.area=0.3, .progress="text")
names(frontiers) <- radius

# Convert a Raster to data.frame
# @param x a raster object
tidy.RasterLayer <- function(x) {
  coords <- as.data.frame(coordinates(x))
  content <- as.data.frame(x)
  return(data.frame(coords, content))
}

# convert all sp objects to data.frames for plotting
smoothed_congruence_df <- ldply(radius, function(r) {
  xd <- na.omit(tidy(smoothed_congruence[[as.character(r)]]))
  names(xd) <- c("lon", "lat", "congruence")
  xd$radius <- r
  return(xd)
})

regions_df <- ldply(radius, function(r) {
  ldply(levels_regions[-1], function(l) {
    xd <- tidy(regions[[as.character(r)]][[as.character(l)]])
    xd <- rename(xd, lon=long)
    xd$radius <- r
    xd$level <- l
    return(xd)
  })
})

frontiers_df <- ldply(radius, function(r) {
  ldply(levels_frontiers[-1], function(l) {
    xd <- tidy(frontiers[[as.character(r)]][[as.character(l)]])
    xd <- rename(xd, lon=long)
    xd$radius <- r
    xd$level <- l
    return(xd)
  })
})

# plot result
p <- bd + facet_wrap(~radius, ncol=1) +
  # smoothed congruence surface
  geom_raster(aes(fill=congruence), data=smoothed_congruence_df) +
  scale_fill_distiller(palette="YlGnBu", guide="none") +
  # consensus regions
  geom_path(aes(group=interaction(group, level)), data=regions_df, colour="white", alpha=0.5, size=0.2) +
  # consensus frontiers
  geom_path(aes(group=interaction(group, level)), data=frontiers_df, colour="red", alpha=0.5, size=0.2) +
  coastd
ggsave(p, file="sup_fig4.png", width=3.5*1.5, height=5*1.5)

# chose some levels
my_radius <- "smoothing radius = 0.50º"
level_r <- "0.3"
level_f <- "0.8"
# and select the data
smoothed_congruence <- smoothed_congruence[[my_radius]]
smoothed_congruence_df <- filter(smoothed_congruence_df, radius==my_radius)
regions <- regions[[my_radius]][[level_r]]
regions_df <- filter(regions_df, radius==my_radius, level==level_r)
frontiers <- frontiers[[my_radius]][[level_f]]
frontiers_df <- filter(frontiers_df, radius==my_radius, level==level_f)


# Now regions need to be edited a but (remove some islands etc.) and frontier zones need to be converted to ridges

# invert the values, for the computation of a catchment area to define frontier ridges (water flows *down*, towards low values)
smoothed_congruence_inv <- smoothed_congruence
values(smoothed_congruence_inv) <- 2 - values(smoothed_congruence_inv)

# write data for QGis
dir.create("consensus", showWarnings=FALSE)
writeRaster(smoothed_congruence_inv, filename="consensus/frontiers_smoothed_inv.geo.tif", overwrite=TRUE)
writeOGR(as(frontiers, "SpatialPolygonsDataFrame"), "consensus", "frontiers", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(as(regions, "SpatialPolygonsDataFrame"), "consensus", "regions", driver="ESRI Shapefile", overwrite_layer=TRUE)

# Edit files in QGis
# Cleanup regions (insides)
# - select layer
# - save as > regions_cleaned
# - edit layer
# - remove central nodes with node tool
# Extract ridge lines
# - import inverted smoothed congruence
# - import outline of consensus frontiers
# - Processing > Toolbox > SAGA > Catchment area; Method 5
# - display result as single band gray, white to black, from quantile 1 to quantile 99%, blending mode = multiply
# - trace ridges manually (new polyline layer and draw)

# read edited frontiers back
regions <- readOGR("consensus", "regions_cleaned", verbose=F)
regions_df <- tidy(regions)
regions_df <- rename(regions_df, lon=long)

# read ridges back
ridges <- readOGR("consensus", "frontiers_lines", verbose=F)
ridges_df <- tidy(ridges)
ridges_df <- rename(ridges_df, lon=long)

# plot regions and frontier ridges on top of smoothed congruence
p <- base + theme_dark(8) + map +
  # raster
  geom_raster(aes(fill=congruence), data=smoothed_congruence_df) +
  scale_fill_distiller(palette="YlGnBu", breaks=c(0, 0.5, 1)) +
  labs(fill="Smoothed congruence") +
  legend_inside() +
  # regions
  geom_path(aes(group=group), data=regions_df, size=0.2, colour="white") +
  # frontiers
  geom_path(aes(group=group), data=ridges_df, size=0.6, alpha=0.5, colour="black", lineend="round") +
  geom_path(aes(group=group), data=ridges_df, size=0.2, alpha=1  , colour="white", lineend="round", linetype="dashed") +
  # coast
  coastd
# ggsave(p, file="fig4.png", width=3.5*1.5, height=1.8*1.5)
ggsave(p, file="fig4_base.pdf", width=3.5*1.5, height=1.8*1.5)
pg <- remove_last_layer(p) + 
  scale_fill_gradient(low="black", high="grey90", breaks=c(0, 0.5, 1)) + 
  coastl + legend_inside(text="black")
ggsave(pg, file="fig4-bw_base.pdf", width=3.5*1.5, height=1.8*1.5)


## Save data for later use ----

save(congruence, smoothed_congruence, smoothed_congruence_df, regions, regions_df, frontiers, frontiers_df, ridges, ridges_df, file="consensus.RData")
