#
# Read data.distribution from WOA
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("doParallel")
registerDoParallel(cores=detectCores()-1)

library("ncdf4")
library("ncdf4helpers")
library("stringr")
library("plyr")
library("dplyr")
library("lubridate")

source("lib_plot.R")
source("lib_coastline.R")


## Read data from WOA ----

# get number of observation over OpenDAP
get_n_obs <- function(path) {
  # get variable name and abbreviation from path
  name <- str_split_fixed(path, "/", 2)[,1]
  abbrev <- str_replace(str_split_fixed(path, "_", 5)[,3], "00", "")

  # get data in the region of interest, over the 70 first m of the water column
  nc <- nc_open(str_c("https://data.nodc.noaa.gov/thredds/dodsC/woa/WOA13/DATAv2/", path))
  d_dd <- ncvar_slice(nc, str_c(abbrev, "_dd"), lat=domain_lat, lon=domain_lon, depth=0:100)
  nc_close(nc)

  # compute average nb of observations per pixel
  d <- as.data.frame(d_dd)
  names(d)[ncol(d)] <- "n"
  d <- d %>% group_by(lon, lat) %>% summarise(n=mean(n, na.rm=T)) %>% ungroup()

  # extract variable name
  d$variable <- name

  return(data.frame(d))
}

# do that for several variables
d <- ldply(
  c(
    "temperature/netcdf/decav/0.25/woa13_decav_t00_04v2.nc",
    "salinity/netcdf/decav/0.25/woa13_decav_s00_04v2.nc",
    "o2sat/netcdf/all/1.00/woa13_all_O00_01.nc",
    "nitrate/netcdf/all/1.00/woa13_all_n00_01.nc",
    # "phosphate/netcdf/all/1.00/woa13_all_p00_01.nc",
    "silicate/netcdf/all/1.00/woa13_all_i00_01.nc"
  ),
  get_n_obs, .progress="text"
)

save(d, file="data_density.RData")

## Inspect data ----

load("data_density.RData")

# remove useless points
ds <- d
ds <- na.omit(ds)

# remove points outside of med
# ds <- ds[in_sea(select(ds, lon, lat)),]
# -> too drastic
# be more gentle
ds <- filter(ds, lon > -5.375)
ds <- filter(ds, lat > 30.15)
ds <- filter(ds, !(lon < 0 & lat > 42))
ds <- filter(ds, !(lon > 27 & lat > 40.5))

# make all n obs >= 1 (for log10 scaling)
ds$n[ds$n < 1] <- 1

# manually bin the number of observations
ds$n_bin <- cut(ds$n, breaks=c(0,1,10,50,100,1000,2500), include_lowest=TRUE, dig.lab = 4)

# rename variables
ds$variable <- str_to_title(ds$variable)
ds$variable <- str_replace(ds$variable, "O2sat", "Oxygen saturation")

# force order
ds$variable <- factor(ds$variable, levels=unique(ds$variable))

# plot

# look at the distribution of data density
qplot(ds$n) + scale_x_log10()

# pick nice colors
colors <- brewer.colors(name="RdYlBu", 6)

# base + facet_wrap(~variable) +
#   geom_raster(aes(fill=n), data=ds) +
#   coastd + theme_dark(8) +
#   scale_fill_gradientn(
#     colours=colors,
#     values=scales::rescale(log10(c(1,5,10,100,1000,max(ds$n))),
#                            from=log10(range(ds$n))),
#     trans="log10",
#     breaks=c(1, 10, 100, 1000),
#     labels=c("<1", 10, 100, 1000))
# ggsave(file="data_density.png", width=12, height=5)
#
# base + facet_wrap(~variable) +
#   geom_raster(aes(fill=n_bin), data=ds) +
#   coastd + theme_dark(8) +
#   scale_fill_manual(name="Nb obs", values=colors)
# ggsave(file="data_density_binned.png", width=12, height=5)

# plot for paper

p <- base + theme_dark(8) + map +
  facet_wrap(~variable) +
  geom_raster(aes(fill=n_bin), data=ds) +
  coastd +
  scale_fill_manual(values=colors) + labs(fill="Number of observations") +
  # force discrete legend
  theme(
    # legend inside plot
    legend.position = c(0.85, 0.25),
    legend.margin = margin(0,0,0,0, "pt"),
    # smaller
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.3, "cm")
  )
ggsave(p, file="fig6.png", width=7.2, height=2.8)
ggsave(p, file="fig6.pdf", width=7.2, height=2.8)

pg <- remove_last_layer(p) + scale_fill_manual(values=interp_colors(6, colors=c("grey90", "black"))) + coasto + theme(panel.background=element_rect(fill="white"), panel.grid=element_blank())
ggsave(pg, file="fig6-bw.png", width=7.2, height=2.8)
ggsave(pg, file="fig6-bw.pdf", width=7.2, height=2.8)


## Read data from WOD ----

# list downloaded variables
variables <- list.dirs("data_density/world_ocean_database", full=T, rec=F)

D <- ldply(variables, function(var_dir) {
  var <- basename(var_dir)
  message(var)

  # extract the nc file describing the content of the archive, which has all lats and longs
  tar_files <- list.files(var_dir, pattern=glob2rx("*.tar.gz"), full=TRUE)
  l_ply(tar_files, function(f) {
    # the "contents" file has the same name as the archive, with an .nc extension
    untar(f, files=str_replace(basename(f), fixed(".tar.gz"), ".nc"), exdir=dirname(f))
  })
  
  # read all those netCDF files
  nc_files <- str_replace(tar_files, fixed(".tar.gz"), ".nc")
  d <- ldply(nc_files, function(f) {
    # get data
    nc <- nc_open(f)
    lat <- ncvar_get(nc, "lat")
    lon <- ncvar_get(nc, "lon")
    date <- ncvar_get(nc, "time")
    nc_close(nc)
  
    # convert date
    date <- ymd_hms("1770-01-01 00:00:00") + date*3600*24
    # document variable and instrument
    variable <- var
    instrument <- str_split_fixed(f, fixed("."), 4)[,3]
    data.frame(variable, instrument, lat, lon, date)
  }, .progress="text")

  return(d)
})

# remove points outside of Med Sea
D <- D[in_sea(select(D, lon, lat), .parallel=TRUE),]

# inspect general aspect of data density for both variables
bd + geom_bin2d(aes(x=lon, y=lat), binwidth=0.25, data=D) + facet_wrap(~variable) + scale_fill_distiller(palette="RdYlBu", trans="log10", direction=1)
# -> similar to WOA summary, despite WOA filtering out much data

# inspect which instruments contribute to which variable
bd + geom_bin2d(aes(x=lon, y=lat), binwidth=0.25, data=D) + facet_grid(instrument~variable) + scale_fill_distiller(palette="RdYlBu", trans="log10", direction=1)
# -> The XBT contribute much to the data density in the centre of the basin for temperature