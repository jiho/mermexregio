#
# Draw a map of Mediterranean bathymetry
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("plyr")
library("dplyr")

source("lib_plot.R")

# read data
file <- "bathy/med_1min.xyz.gz"
d <- read.table(file, col.names=c("lon", "lat", "z"))
dim(d)

# remove outside of land mask
d <- filter(d, lon > domain_lon[1]+0.05, lon < domain_lon[2]-0.05, lat > domain_lat[1]+0.05, lat < domain_lat[2])
d <- filter(d, z <= 100)

# define blue colour scale

# colorbrewer's Blues
show_col(brewer_colors(3, name="Blues"))
cb_blues <- rev(brewer_colors(3, name="Blues"))

# based on colorbrewer's YlGnBu used in the frontiers maps
show_col(brewer_colors(9, name="YlGnBu"))
show_col(brewer_colors(8, name="YlGnBu"))
show_col(brewer_colors(7, name="YlGnBu"))
ygb_blues <- rev(brewer_colors(7, name="YlGnBu"))[1:3]

# based on viridis blue
show_col(viridis_colors(9))
show_col(viridis_colors(8))
show_col(viridis_colors(7))
vir_blue <- viridis_colors(7)[3]
vir_blues <- c(darker(vir_blue, 2), vir_blue, lighter(vir_blue, 1))
show_col(vir_blues)


# Google Maps blue
gm_blue <- "#93BFFE"
gm_blues <- c(darker(gm_blue), gm_blue, lighter(gm_blue, 0.5))
show_col(gm_blues)

show_col(
  interp_colors(20, colors=cb_blues),
  interp_colors(20, colors=vir_blues),
  interp_colors(20, colors=ygb_blues),
  interp_colors(20, colors=gm_blues)
)

blues <- vir_blues

# make the plot
p <- base + theme_dark(8) + map + legend_inside() +
  # bathy
  geom_raster(aes(fill=z), data=d) +
  scale_fill_gradientn("Depth", colours=blues, breaks=c(-5000, -2500, 0)) +
  # coast
  # geom_polygon(data=med, fill="white", colour=blues[2], size=0.1)
  # geom_polygon(data=med, fill=desaturate(blues[3], 10), colour=desaturate(blues[1], 10), size=0.1)
  coastd
print(p)
ggsave(p, file="fig1_base_1.5col.pdf", width=3.5*1.5, height=2.7)
ggsave(p, file="fig1_base_2col.pdf", width=7.2, height=3.6)
# and now edit the figure

# convert the final figure into greyscale
system("convert -type Grayscale fig1.png fig1-bw.png")
