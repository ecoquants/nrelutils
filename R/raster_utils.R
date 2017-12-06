#' Wrap raster from [-180,180] to [0,360]
#'
#' @param r raster object [-180,180]
#'
#' @return wrapped raster [0, 360]
#' @export
raster_wrap = function(r){
  shift(rotate(shift(r, 180)), 180)
}

#' Unwrap raster [0,360] to [-180,180]
#'
#' @param raster raster object
#'
#' @return unwrapped raster
#' @export
raster_unwrap = function(r){
  shift(r, -360)
}

#' Clip raster with crop & mask
#'
#' @param x raster object to be clipped
#' @param y raster object to clip by
#'
#' @return raster x with crop(x, y) and mask(x, y)
#' @export
raster_clip <- function(x, y){
  x <- crop(x, y)
  x <- mask(x, y)
  x
}

#' Plot raster bisected around antimeridian [0,360] to [-180,180]
#'
#' @param r raster
#' @param aggregate_factor factor to aggregate
#'
#' @return leaflet map with bisected raster and graticule. Pan to get other half.
#' @export
r_map <- function(r, aggregate_factor=10){
  # [Raster over international date line · Issue #225 · rstudio/leaflet](https://github.com/rstudio/leaflet/issues/225#issuecomment-347721709)

  da <- aggregate(r, fact=aggregate_factor) # plot(da); lines(eez_wgcs_sp)
  b  <- raster_unwrap(da)
  b1 <- crop(b, extent(-180, 180,-90,90), snap="in") %>% trim()
  b2 <- crop(b, extent(-360,-180,-90,90), snap="in") %>% shift(360) %>% trim()

  # get bounding box on bigger raster
  xr1 = extent(b1) %>% as.vector() %>% .[1:2] %>% diff()
  xr2 = extent(b2) %>% as.vector() %>% .[1:2] %>% diff()
  bb = c(extent(b1), extent(b2))[[which.max(c(xr1,xr2))]]

  leaflet(options=leafletOptions(worldCopyJump=T)) %>%
    addProviderTiles("Stamen.TonerLite", group = "B&W") %>%
    addRasterImage(b1) %>%
    addRasterImage(b2) %>%
    addGraticule() %>% # interval=1
    addScaleBar() %>%
    fitBounds(bb@xmin, bb@ymin, bb@xmax, bb@ymax)
}

#' Polygons (ply) to raster(s) on filesystem (tif)
#'
#' @param x sf polygon object
#' @param y raster object to use as template
#' @param ter territory name prefix
#' @param key output directory and used in prefix="{ter}_{key}"
#' @param by character. Name of column in sf object by which to aggregate layers. Default=NULL
#' @param sfx character. Suffix of output tif. Default="epsg4326.tif"
#'
#' @return return leaflet map
#' @export
ply_to_tifs <- function(x, y, ter, key, field="one", by=NA, sfx="epsg4326.tif"){
  library(raster)
  library(fasterize)
  library(sf)
  library(glue)
  library(readr)

  dir.create(key, showWarnings=F)
  pfx <- glue("{ter}_{key}")

  if (is.na(by)) by=NULL

  if (nrow(x) == 0){
    write_lines("0", digest_txt)
    return(NULL)
  }

  r <- fasterize(x, y, field=field, fun="first", by=by)

  if (!is.null(by)){
    tifs <- glue("{pfx}_{names(r)}_{sfx}")
    for (i in 1:length(tifs)){ # i <- 1
      lyr <- names(r)
      writeRaster(raster(r, lyr), file.path(key, tifs[i]), overwrite=T) # hapc_b[[lyr]] # plot(raster(hapc_b, lyr))
    }
    write_lines(glue("{names(r)}:{tifs}"), digest_txt)
  } else {
    tif <- glue("{pfx}_{sfx}")
    writeRaster(r, file.path(key, tif), overwrite=T)
    write_lines(glue("{key}:{tif}"), digest_txt)
  }
}


