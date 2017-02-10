#
# Prepare backgrounds, colours, legends, etc. for plots
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("ggplot2")

# read coastline
med <- read.csv("coastline/gshhg_medit_i.csv")

## Plot backgrounds ----

# customise theme_dark and theme_light (theme_light here is meant to resemble theme_gray with the lines thicknesses of theme_dark)
theme_light <- function(...) {
  ggplot2::theme_light(...) +
    theme(
      panel.border = element_blank(),
      panel.spacing = unit(1, "pt"),
      panel.background = element_rect(fill="grey92", colour = NA),
      panel.grid.major = element_line(colour="white", size = 0.25),
      panel.grid.minor = element_blank(),
      strip.text.x = element_text(margin=margin(4,0,4,0, "pt"))
    )
}
theme_dark <- function(...) {
  ggplot2::theme_dark(...) +
    theme(
      panel.spacing = unit(1, "pt"),
      panel.grid.minor = element_blank(),
      strip.text.x = element_text(margin=margin(4,0,4,0, "pt")),
      strip.background = element_rect(fill="grey20", colour=NA)
    )
}

# setup for maps = more compact, no axes titles
degree_label <- function(x) {paste0(x, "\u00B0")}
# degree_label <- function(x) {paste0(x, "º")}
map <- list(
  coord_quickmap(),
  scale_x_continuous(expand=c(0,0), labels=degree_label),
  scale_y_continuous(expand=c(0,0), labels=degree_label),
  theme(
    axis.title = element_blank(),
    plot.margin = unit(c(0,0,0,0), "pt")
  )
)

# Put the legend inside the plot, somewhere between Morocco and Algeria
# @param guide type of guide to plot
# @text.colour colour of the text
# by default, uses a colorbar on a dark background
legend_inside <- function(guide=c("colorbar", "legend"), text.colour="white", ...) {
  guide <- match.arg(guide)
  list(
    theme(
      # legend inside plot
      legend.position = c(0.0, 0.15),
      legend.background = element_blank(),
      # horizontal
      legend.direction = "horizontal",
      legend.justification = "left",
      # white text
      legend.title=element_text(colour=text.colour),
      legend.text=element_text(colour=text.colour),
      # smaller
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(ifelse(guide=="colorbar", 0.5, 0.3), "cm")
    ),
    if (guide=="colorbar") {
      guides(
        # title on top of legend
        colour = guide_colorbar(title.position = "top", label.vjust=1),
        fill = guide_colorbar(title.position = "top", label.vjust=1)
      )
    } else {
      guides(
        # title on top of legend
        colour = guide_legend(title.position = "top", label.hjust=0.5, label.position = "bottom", nrow=1),
        fill = guide_legend(title.position = "top", label.hjust=0.5, label.position = "bottom", nrow=1)
      )
    }
  )
}

# Remove the last layer of a ggplot
# Useful to remove coastline from ggplots
# @param p a ggplot object
remove_last_layer <- function(p) {
  p$layers <- p$layers[-length(p$layers)]
  return(p)
}

# prepare plot
base <- ggplot(mapping=aes(x=lon, y=lat))

# coast polygons with various looks
coastg <- geom_polygon(fill=theme_light()$panel.background$fill, data=med)
coasto <- geom_polygon(fill="white", colour="grey70", size=0.1, data=med)
coastl <- geom_polygon(fill="white", colour=NA, data=med)
coastd <- geom_polygon(data=med)

bl <- base + theme_light(8) + map + coastl
bd <- base + theme_dark(8)  + map + coastd
bo <- base + theme_light(8) + map + coasto
bg <- base + theme_light(8) + map + coastg


## Colours ----

# distinct colours for a maximum of 33 regions
library("chroma")
clr <- brewer.colors(12, name="Set3")[-9]
clr <- c(clr, darker(saturate(clr), 2), lighter(desaturate(clr), 0.5))
length(clr)
# show_col(clr)

scale_fill_regions <- function(guide="none", ...) {
  scale_fill_manual(values=clr, guide=guide, ...)
}
scale_colour_regions <- function(guide="none",...) {
  scale_colour_manual(values=clr, guide=guide, ...)
}
scale_color_regions <- scale_colour_regions

# contrasting palette of greys for a maximum of 30 regions
greys <- grey(0:30/34)
# show_col(greys)
# reorder them to be most different at the beginning
greys <-  greys[c(1,30,15,8,23,4,19,11,26,6,13,17,21,28,3,10,25,2,5,7,9,12,14,16,18,20,22,24,27,29)]
scale_fill_regions_bw <- function(...) {
  scale_fill_manual(values=greys, guide="none", ...)
}
scale_colour_regions_bw <- function(...) {
  scale_colour_manual(values=greys, guide="none", ...)
}
scale_color_regions_bw <- scale_colour_regions_bw
