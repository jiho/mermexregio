#
# Recompute all regions on the same regular grid and cleanup the data
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("doParallel")
registerDoParallel(cores=detectCores()-1)

library("plyr")
library("dplyr")
library("ggplot2")

source("lib_plot.R")
source("lib_coastline.R")

load("regions.RData")


## Regrid ----
message("Regrid")

# inspect resolution, for each study
res <- function(x) {data.frame(t(as.matrix(summary(diff(sort(unique(x)))))))}
d %>% group_by(study) %>% do(res(.$lon))
d %>% group_by(study) %>% do(res(.$lat))
# -> All but Berline, Rossi are OK for 0.2º
#    All but Berline, Rossi, Reygondeau are OK for 0.15º
#    All but Berline, Rossi, Reygondeau, Palmieri are OK for 0.1º
#    0.1º ~ 8.5km in lon, 11km in lat


# define new gridded coordinates
coord <- list()
step <- 0.15
coord$lon <- seq(domain_lon[1], domain_lon[2], by=step)
coord$lat <- seq(domain_lat[1], domain_lat[2], by=step)
grid <- expand.grid(lon=coord$lon, lat=coord$lat)
# keep only sea parts
sea <- in_sea(grid, .parallel=T)
grid <- grid[sea,]
# bl + geom_point(data=grid, colour="#16B2B4", size=0.1)


# use nearest neighbour interpolation to put data on the same grid for all
# (removes some information for high resolution regionalisation, adds points for low resolution ones)

nni <- function(data, grid, cutoff=0.5, ...) {
  # Nearest neighbour interpolation
  # data  data.frame with columns lon, lat and others
  # grid  data.frame with columns lon and lat, defining grid points
  # cutoff  maximum distance (in degrees in +/- around each grid point) to look for neighbours in the original data

  library("oce")

  # for each point on the new grid, find the closest neighbour
  interpolated <- adply(grid, 1, function(x) {
    # reduce data to neighbourhood
    neighbourhood <- filter(data, lon > x$lon-cutoff, lon < x$lon+cutoff, lat > x$lat-cutoff, lat < x$lat+cutoff)
    # bd + geom_point(aes(colour=cluster), data=neighbourhood, size=0.1)
    # base + geom_point(aes(colour=cluster), data=neighbourhood, size=0.1)
    # if points are too far, return NA
    if (nrow(neighbourhood) == 0) {
      i <- NULL
    # otherwise pick data at closest point
    } else {
      dists <- geodDist(neighbourhood$lon, neighbourhood$lat, x$lon, x$lat)
      i <- neighbourhood[which.min(dists),]
      i$lon <- x$lon
      i$lat <- x$lat
    }
    return(i)
  }, .expand=F, ...)
  # remove first column (index)
  interpolated <- interpolated[,-1]

  return(interpolated)
}

# perform the interpolation
di <- group_by(d, id, study, type) %>% do(nni(., grid, .parallel=TRUE))

# plot the result
p <- bd + geom_raster(aes(fill=clr), data=di) + facet_wrap(~id) + scale_fill_identity()
ggsave(p, file="regions_gridded.png", width=10, height=5)

# save data for later use
d <- as.data.frame(di)
save(d, file="regions_gridded.RData")


## Remove speckles with a majority filter ----
message("Cleanup")

load("regions_gridded.RData")

majority_filter <- function(d, n=5, f=0.2) {
  # Filter map of clusters (factors) to remove speckles
  #
  # Move a square window over the data matrix, detect which clusters appear with low frequency and replace them with the cluster which represents the majority of the window
  #
  # d   data.frame with columns lon, lat, and cluster
  # n   size of the sides of the window to be moved over space
  # f   frequency under which clusters are considered low frequency and replaced by the majority cluster
  #

  # define an integer grid
  d$i <- as.numeric(factor(d$lat))
  d$j <- as.numeric(factor(d$lon))
  ii <- max(d$i)
  jj <- max(d$j)

  # move a window over the grid
  dd <- ldply(1:(ii-n), function(i) {
    ldply(1:(jj-n), function(j) {
      # select points in the window
      x <- d[d$i %in% i:(i+n) & d$j %in% j:(j+n),]

      # if there are enough
      if (nrow(x) > 5) {
        # detect the frequency of each cluster
        counts <- table(x$cluster)
        freq <- counts / sum(counts)
        # detect most frequent and low frequency clusters
        most_frequent <- names(freq)[which.max(freq)]
        low_freq <- names(freq)[freq < f]
        # replace low frequency ones with the most frequent
        x$cluster[x$cluster %in% low_freq] <- most_frequent
      }

      return(x)
    })
  }, .progress="none", .parallel=TRUE)

  # each x,y point will have been in many windows and the value of the cluster assign to it may change between windows. Assign the majority cluster in each location
  majority <- function(x) { which.max(table(x)) }
  # NB: this works only because x is a factor and factor levels are always kept in order (and empty levels are given 0 in the output of table)
  dm <- group_by(dd, lon, lat) %>% summarise(cluster=majority(cluster))

  # convert the result into a factor and keep the levels intact to ensure continuity of colors
  dm$cluster <- factor(dm$cluster, levels=levels(d$cluster))

  return(dm)
}

# # tests
# df <- filter(d, study == "D'Ortenzio and Ribera d’Alcalà (2009)", lon > 5, lon < 10, lat > 38, lat < 42)
# ggplot() + geom_raster(aes(lon, lat, fill=cluster), data=dt)
# df <- majority_filter(df, n=8)
# ggplot() + geom_raster(aes(lon, lat, fill=cluster), data=df)

# Filter data iteratively
# on a small subset region, for tests
# df <- ddply(filter(d, lon > 0, lon < 3, lat > 38, lat < 40), ~id, function(dd) {
# on the whole thing
df <- ddply(d, ~id, function(dd) {

  message(dd$id[1])

  if (dd$study[1] %in% c("Spalding et al (2007)", "Berline et al (2014)", "Rossi et al (2014)")) {
    # do not filter
    d_new <- select(dd, lon, lat, cluster)

  } else {
    # start filtering with a ~1º window
    n <- 7
    # initialise containers
    d_new <- d_old <- dd
    # bd + geom_raster(aes(fill=cluster), data=d_new) + scale_fill_regions() + labs(title="orig")

    # filter over a maximum of 10 steps
    for (i in 1:10) {
      # filter
      d_old <- d_new
      d_new <- majority_filter(d_old, n=n, f=0.25)
      # count how many pixels changed
      n_diff <- sum(as.character(d_old$cluster) != as.character(d_new$cluster), na.rm=TRUE)
      message("  ", i, " n=", n, " ndiff=", n_diff)
      # print(bd + geom_raster(aes(fill=cluster), data=d_new) + scale_fill_regions() +labs(title=i))

      # when few pixels change, make the window smaller, to remove speckles
      if (n_diff < 200) { n <- 3 }
      # when very few pixels change, just stop
      if (n_diff < 50) { break }
    }
  }

  return(d_new)
})

d <- df
# re-assign cluster colours
d$clr <- clr[as.numeric(as.character(d$cluster))]


## Inspect result and save modified data ----

# plot
p <- bd + geom_raster(aes(fill=clr), data=d, na.rm=TRUE) + facet_wrap(~id) + scale_fill_identity()
ggsave(p, file="regions_filtered.png", width=10, height=5)

# count clusters and their size
dlply(d, ~id, function(x) {table(x$cluster)})
d %>% group_by(id) %>% summarise(n=n())

# save data for later use
save(d, file="regions_filtered.RData")
