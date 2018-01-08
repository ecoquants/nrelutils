#' Wrap raster from [-180,180] to [0,360]
#'
#' @param r raster object [-180,180]
#'
#' @return wrapped raster [0, 360]
#' @export
raster_wrap = function(r){
  raster::shift(raster::rotate(raster::shift(r, 180)), 180)
}

#' Unwrap raster [0,360] to [-180,180]
#'
#' @param raster raster object
#'
#' @return unwrapped raster
#' @export
raster_unwrap = function(r){
  raster::shift(r, -360)
}

#' Clip raster with crop & mask
#'
#' @param x raster object to be clipped
#' @param y raster object to clip by
#'
#' @return raster x with crop(x, y) and mask(x, y)
#' @export
raster_clip <- function(x, y){
  x <- raster::crop(x, y)
  x <- raster::mask(x, y)
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

  da <- raster::aggregate(r, fact=aggregate_factor) # plot(da); lines(eez_wgcs_sp)
  b  <- raster_unwrap(da)
  b1 <- raster::crop(b, raster::extent(-180, 180,-90,90), snap="in") %>% raster::trim()
  b2 <- raster::crop(b, raster::extent(-360,-180,-90,90), snap="in") %>% raster::shift(360) %>% raster::trim()

  # get bounding box on bigger raster
  xr1 = raster::extent(b1) %>% as.vector() %>% .[1:2] %>% diff()
  xr2 = raster::extent(b2) %>% as.vector() %>% .[1:2] %>% diff()
  bb = c(raster::extent(b1), raster::extent(b2))[[which.max(c(xr1,xr2))]]

  leaflet::leaflet(options=leafletOptions(worldCopyJump=T)) %>%
    leaflet::addProviderTiles("Stamen.TonerLite", group = "B&W") %>%
    leaflet::addRasterImage(b1) %>%
    leaflet::addRasterImage(b2) %>%
    leaflet::addGraticule() %>% # interval=1
    leaflet::addScaleBar() %>%
    leaflet::fitBounds(bb@xmin, bb@ymin, bb@xmax, bb@ymax)
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
ply_to_tifs <- function(x, y, ter, lyr, field="one", by=NA, sfx="epsg4326.tif"){

  #x=ply; y=ter_depth_wgcs_r; ter; key=lyr; field=lyr_p$field; by=lyr_p$by; sfx="epsg4326.tif"
  dir_lyr <- glue::glue("{dir_lyrs}/{lyr}")
  dir.create(lyr, showWarnings=F)
  pfx <- glue::glue("{ter}_{lyr}")
  lyr_info <- get_lyr_info(lyr)

  if (is.na(by)) by=NULL

  if (nrow(x) == 0){
    lyr_info$territories[[ter]] <- NA
    set_lyr_info(lyr_info)
    return(NULL)
  }

  # TODO: consider 0-1 for % of cell, try volex for raster operations
  # TODO: points for tide
  if (lyr == "tide")
    browser()
  r <- fasterize::fasterize(x, y, field=field, fun="first", by=by)

  if (!is.null(by)){
    tifs <- glue("{pfx}_{names(r)}_{sfx}")
    for (i in 1:length(tifs)){ # i <- 3
      component <- names(r)[i]
      msg(g("    Writing: {tifs[i]}"))

      raster::writeRaster(raster(r, component), file.path(dir_lyr, tifs[i]), overwrite=T) # r[[lyr]] # plot(raster(r, lyr))
    }

    lyr_info$territories[[ter]]$components <- setNames(tifs, names(r)) %>% as.list()
    set_lyr_info(lyr_info)
  } else {
    tif <- glue::glue("{pfx}_{sfx}")
    msg(g("    Writing: {tif}"))

    raster::writeRaster(r, file.path(dir_lyr, tif), overwrite=T)

    lyr_info$territories[[ter]]$components <- setNames(tif, lyr) %>% as.list()
    set_lyr_info(lyr_info)
  }
}

raster_trim <- function(r){
  # raster::trim() crazy slow compared to this
  r_m          <- is.na(raster::as.matrix(r))
  col_notna    <- which(colSums(r_m) != nrow(r))
  row_notna    <- which(rowSums(r_m) != ncol(r))
  extent_notna <- raster::extent(
    r,
    row_notna[1], row_notna[length(row_notna)],
    col_notna[1], col_notna[length(col_notna)])
  raster::crop(r, extent_notna)
}

raster_project_leaflet_nearest <- function(x){
  raster::projectRaster(x, raster::projectExtent(x, crs = sp::CRS(leaflet:::epsg3857), method="ngb"))
}
