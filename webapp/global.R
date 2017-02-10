library("shiny")
library("leaflet")
library("raster")
library("chroma")

load("data.Rdata")

rasters <- r
polygons <- p

# colors for bathy
vir_blue <- viridis_colors(7)[3]
blues <- c(darker(vir_blue, 2), vir_blue, lighter(vir_blue, 1))
# show_col(vir_blues)

# and congruence
BuGnYl <- brewer_colors(6, "YlGnBu", rev=T)
