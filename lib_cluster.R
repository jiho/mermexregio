#
# Utility functions to deal with clusters and rasters of such
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

# Relabel factor levels (dropping unused levels and keeping the remaining one in lexical order)
# @param x a factor
relabel <- function(x) {
  current_levels <- sort(unique(x))
  n_levels <- 1:length(current_levels)
  factor(x, levels=current_levels, labels=n_levels)
}


# Convert a binary raster into a polygon
# uses gdal because it is much faster than raster::rasterToPolygon
# @param x input raster
# @return a SpatialPolygonDataFrame
raster2poly <- function(x, verbose=FALSE) {
  # get path to gdal utility
  gdal_poly_path <- Sys.which('gdal_polygonize.py')
  if (!file.exists(gdal_poly_path)) {
    stop("Can't find gdal_polygonize.py on your system.")
  }

  # write the raster to disk temporarily
  raster_path <- tempfile(fileext=".tif")
  require(raster)
  writeRaster(x, raster_path)

  # prepare storage for the output shapefile
  shape_path <- tempfile()

  # convert the raster to shape
  system2("python", args=paste0(gdal_poly_path, " "," -mask ", raster_path, " ", raster_path, " -f \"ESRI Shapefile\" ", shape_path, ".shp"), stdout=NULL)
  
  # read the output shapefile
  shp <- readOGR(dirname(shape_path), layer=basename(shape_path), verbose=verbose)
  
  # remove temporary files
  file.remove(raster_path)
  file.remove(paste0(shape_path, c(".dbf", ".prj", ".shp", ".shx")))
  
  return(shp)
}


# Convert a data.frame with xyz info into a raster
# @param x data.frame with columns lon, lat and `z_name`
# @param z_name name of the z column
xyz2raster <- function(x, z_name) {
  require("raster")
  x <- x[,c("lon", "lat", z_name)]
  r <- raster::rasterFromXYZ(xyz=x, crs=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), digits=3)
  return(r)
}

