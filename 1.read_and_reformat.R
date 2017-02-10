#
# Read data communicated by all authors
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("doParallel")
registerDoParallel(cores=detectCores()-1)

library("oce")
library("rgdal")
library("raster")

library("stringr")
library("reshape2")
library("plyr")
library("tidyverse")

source("lib_plot.R")
source("lib_coastline.R")
source("lib_cluster.R")


## spalding ----

s <- readOGR("spalding", "meow_ecos", verb=F)

# extract Med
s <- s[s$PROVINCE == "Mediterranean Sea",]

# rasterize over 0.15º resolution
r <- raster(extent(s), resolution=0.15)
r <- rasterize(s, r)

# get data as a data.frame
d <- data.frame(coordinates(r), factor(getValues(r)))
names(d) <- c("lon", "lat", "cluster")
# remove missing values
d <- na.omit(d)
# and values on land
d <- d[in_sea(d[,1:2], .parallel=TRUE),]

d$study <- "Spalding et al (2007)"

bd + geom_raster(aes(fill=cluster), data=d) + scale_fill_regions()

spalding <- d


## dortenzio ----

# read
lon <- read.table("dortenzio/Longitude.txt")
lat <- read.table("dortenzio/Latitude.txt")
cluster <- read.table("dortenzio/Fig_4_DR09.txt", na.strings="NaN")

# reformat
lonT <- gather(lon)
latT <- gather(lat)
clusterT <- gather(cluster)

d <- data.frame(lon=lonT$value, lat=latT$value, cluster=factor(clusterT$value))

# remove empty cluster
d <- na.omit(d)

# remove the point in the middle of turkey
d <- filter(d, !(lon > 30 & lat > 37))

# remove points outside of med in the atlantic
d <- filter(d, lon >= domain_lon[1])

bd + geom_point(aes(colour=cluster), data=d, size=0.1) + scale_colour_regions()

# store this filtered data and identify it
d$study <- "D'Ortenzio and Ribera d’Alcalà (2009)"

dortenzio <- d


## mayot ----

# read
lon <- read.table("mayot/Longitude.txt")
lat <- read.table("mayot/Latitude.txt")

avg_cluster <- read.table("mayot/Fig_5a.txt")
cluster_on_clim <- read.table("mayot/Fig_8.txt")

# reformat
lon <- gather(lon)[,2]
lat <- gather(lat)[,2]
avg_cluster <- gather(avg_cluster)[,2]
cluster_on_clim <- gather(cluster_on_clim)[,2]

d <- data.frame(lon, lat, "Cluster on climato."=cluster_on_clim, "Average cluster"=avg_cluster, check.names=FALSE)
# NB: the pixels are not completely regular and this results in blank lines on a tile/raster plot

# remove points outside the med
d <- na.omit(d)
d <- filter(d, lon > domain_lon[1])

# remove the point in the middle of turkey
d <- filter(d, !(lon > 30 & lat > 37))

# reformat cluster codes
d$`Average cluster`[d$`Average cluster` == 13] <- NA
# d$`Average cluster`[d$`Average cluster` == 12] <- "no regime"

# convert to tall format
d <- gather(d, key="type", value="cluster", -lon, -lat)
d$cluster <- factor(d$cluster)

d$study <- "Mayot et al (2016)"

bd + geom_point(aes(colour=cluster), size=0.1, data=d) + facet_wrap(~type, ncol=1) + scale_colour_regions(guide="legend")

mayot <- d


## palmieri ----

# read data
lon <- read.table("palmieri/Chl_max/lon_PHY_STD_4.txt")
lat <- read.table("palmieri/Chl_max/lat_PHY_STD_4.txt")

chl_max <- read.table("palmieri/Chl_max/Clust_STD_2.txt")
chl_surf_pisces <- read.table("palmieri/chl_surf/PISCES_MED12/Clust_STD_4.txt")
chl_tot <- read.table("palmieri/Chl_tot/Clust_STD_4.txt")
mld_nemo <- read.table("palmieri/mld/NEMO_MED12/Clust_MLD_4.txt")

# reformat
vars <- list(lon, lat, chl_max, chl_surf_pisces, chl_tot, mld_nemo)
d <- llply(vars, function(x) { gather(x)[,2] })
d <- do.call(cbind, d)
d[!is.finite(d)] <- NA

d <- data.frame(d)
names(d) <- c("lon", "lat", "Chl max", "Chl surf. PISCES", "Chl tot", "MLD")
d <- melt(d, id.vars=c("lon", "lat"), variable.name="type", value.name="cluster")
d <- na.omit(d)
d$cluster <- factor(d$cluster)
d$study <- "Palmieri (2014)"

bd + geom_point(aes(lon, lat, colour=cluster), size=0.3, data=d) + facet_wrap(~type) + scale_colour_regions()

palmieri <- d


## reygondeau bioregions ----

# read
d <- read.csv("reygondeau/biogeochemical_regions.csv")
d$bioregion <- factor(d$bioregion)

# reformat
d <- d %>% filter(layer!="seafloor") %>% rename(type=layer, cluster=bioregion)
d <- na.omit(d)

# identify
d$study <- "Reygondeau et al (2017)"
d$type <- str_c("Bioregions ", d$type)

bd + geom_point(aes(lon, lat, colour=cluster), size=0.3, data=d) + facet_wrap(~type) + scale_colour_regions()

reygondeau_bio <- d


## reygondeau ecoregions ----

# read
d <- read.csv("reygondeau/ecoregions.csv", stringsAsFactors=T)
d$ecoregion <- factor(d$ecoregion)

# choose the regionalisation of interest
d <- d %>% filter(layer != "benthic", trophic == "all") %>% rename(type=layer, cluster=ecoregion)

d %>% group_by(type) %>% summarise(n=length(unique(cluster)))
d %>% group_by(type, cluster) %>% summarise(n=n(), small=n()<100)

# reformat
d <- na.omit(d)
d <- select(d, -trophic)

# identify
d$study <- "Reygondeau et al (2014)"
d$type <- str_c("Ecoregions ", d$type)
d$type <- str_replace(d$type, " all", "")

reygondeau_eco <- d


## berline ----

# read
d <- read.csv("berline/LonLatGroup.csv", header=F, sep=";", col.names=c("lon", "lat", "cluster"))

# reformat
d$study <- "Berline et al (2014)"
d$cluster <- factor(d$cluster)

# remove similar clusters to get to a more manageable number
# plot with labels
bl + geom_point(aes(colour=cluster), data=d)
bl + geom_text(aes(label=cluster), data=d, size=2)

# cluster layers are in order of "dissimilarity"
# 1 and 2 are the weakest difference
# 3 and 4 are the second weakest
df <- d
for (i in seq(1, 13, by=2)) {
  df$cluster[df$cluster == (i+1)] <- i
}
# relabel
df$cluster <- relabel(df$cluster)
nlevels(df$cluster)
# [1] 15

# compare
bd + geom_point(aes(colour=cluster), data=d) + scale_colour_regions()
bd + geom_point(aes(colour=cluster), data=df) + scale_colour_regions()

berline <- df


## rossi ----

# read data
d30 <- read.table("rossi/Mean_Provinces_PLD30_wintersummer2002-2011_PiO.txt", col.names=c("lon", "lat", "cluster"))
d30$type <- "PLD=30"

d60 <- read.table("rossi/Mean_Provinces_PLD60_wintersummer2002-2011_PiO.txt", col.names=c("lon", "lat", "cluster"))
d60$type <- "PLD=60"

# combine and reformat
d <- rbind(d30, d60)
d$cluster <- factor(d$cluster)

# remove Atlantic extension
d <- filter(d, lon > domain_lon[1])

bd + geom_point(aes(colour=cluster), size=0.5, data=d) + facet_wrap(~type, ncol=1)

# remove small clusters
count <- count(d, type, cluster)
d <- left_join(filter(count, n > 20), d, by=c("type", "cluster"))
d <- select(d, -n)

# relabel clusters
d$cluster <- relabel(d$cluster)
summarise(group_by(d, type), n_clust=length(unique(cluster)))

bd + geom_point(aes(colour=cluster), size=0.5, data=d) + facet_wrap(~type, ncol=1) + scale_colour_regions()

d$study <- "Rossi et al (2014)"

rossi <- d


## nieblas ----

# extract data from netCDF files
library("ncdf4")

nc <- nc_open("nieblas/varrclass_clim_thresh_0.05_final.nc")
x <- ncvar_get(nc, "X")
y <- ncvar_get(nc, "Y")
cluster <- ncvar_get(nc, "Cluster")
nc_close(nc)
d <- melt(cluster, varnames=c("lon", "lat"), value.name="Classical")
# add lon and lat
lon <- seq(from=-6, to=42, along=x)
lat <- seq(from=30, to=47, along=y)
d$lon <- lon[d$lon]
d$lat <- lat[d$lat]

nc <- nc_open("nieblas/varrmeso_clim_thresh_0.05_final.nc")
cluster <- ncvar_get(nc, "Cluster")
nc_close(nc)
dd <- melt(cluster, value.name="cluster")
d$Mesoscale <- dd$cluster

nc <- nc_open("nieblas/varrfull_clim_thresh_0.05_final.nc")
cluster <- ncvar_get(nc, "Cluster")
nc_close(nc)
dd <- melt(cluster, value.name="cluster")
d$Full <- dd$cluster

# remove data in the Black Sea and Bosphorus
d <- filter(d, ! (lon > 27 & lat > 40) )

# remove Atlantic extension
d <- filter(d, lon > domain_lon[1])

# reformat
d <- gather(d, key=type, value=cluster, -lon, -lat)
d <- na.omit(d)
d$cluster <- factor(d$cluster, levels=unique(d$cluster))

# identify
d$study <- "Nieblas et al (2014)"

bd + geom_point(aes(colour=cluster), size=0.1, data=d) + facet_wrap(~type, ncol=2) + scale_colour_regions()

nieblas <- d


## Save data ----

d <- rbind.fill(spalding, dortenzio, mayot, palmieri, reygondeau_bio, berline, rossi, nieblas, reygondeau_eco)

# create single, readable identifier for each regionalisation
d$type <- str_replace_na(d$type, "")
d$id <- str_c(d$study, " ", d$type)
d$id <- str_trim(d$id)

# force study order
d$study <- factor(d$study, levels=unique(d$study))
d$id <- factor(d$id, levels=unique(d$id))

# assign a fixed colour to each cluster
d$clr <- clr[as.numeric(d$cluster)]

# plot all
p <- bd + geom_point(aes(colour=clr), data=d, size=0.1, shape=16, na.rm=TRUE) + facet_wrap(~id) + scale_colour_identity()
ggsave(p, file="regions.png", width=10, height=5)

save(d, file="regions.RData")
