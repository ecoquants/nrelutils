#' Wrap sf geometry [-180,180] to [0,360]
#'
#' @param x sf object [-180,180]
#'
#' @return wrapped sf object [0,360]. The geometry column gets renamed to geom.
#' @export
sf_wrap = function(x){ #

  fld_geom <- attr(x, "sf_column")

  if (fld_geom != "geom"){
    x[,"geom"] <- x[,fld_geom]
    x <- sf::st_set_geometry(x, "geom")
  }
  x <- x %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(
      geom = (geom + c(360,90)) %% c(360) - c(0,90)) %>%
    sf::st_set_geometry("geom") %>%
    sf::st_set_crs(4326)
  if (fld_geom != "geom"){
    x <- dplyr::select(x, -one_of(fld_geom))
  }
  x
}

#' Unwrap sf geometry [0,360] to [-180,180]
#'
#' @param x sf object [0,360]
#'
#' @return unwrapped sf object [-180,180] but could be outside range? (TODO: check!). The geometry column gets renamed to geom.
#' @export
sf_unwrap = function(x){

  if (fld_geom != "geom"){
    x[,"geom"] <- x[,fld_geom]
    x <- sf::st_set_geometry(x, "geom")
  }
  x <- x %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(
      geom = (geom + c(360,90)) %% c(-360) - c(0,-360+90)) %>%
    sf::st_set_geometry("geom") %>%
    sf::st_set_crs(4326)
  if (fld_geom != "geom"){
    x <- dplyr::select(x, -one_of(fld_geom))
  }
  x
}

#' Return intersecting geometries
#'
#' @param a sf object to slice
#' @param b sf object to intersect
#'
#' @return sliced sf object `a` based on intersecting with geometrys from `b`,
#' vs `sf::st_intersects()` which returns a boolean matrix (`sparse = FALSE`)
#' or a sparse list (a boolean matrix (`sparse = FALSE`) containing indices matched
#' in `b`.
#' @export
sf_intersects = function(a, b){
  i <- sf::st_intersects(a, b)
  dplyr::slice(a, which(sapply(i, length) > 0))
}

#' Wrap and intersect
#'
#' @param x sf object to intersect, eg layer
#' @param y sf object to intersect, eg eez
#'
#' @return wrapped and intersected sf object with a one field as a valid MULTIPOLYGON
#' @export
sf_wrap_intersection <- function(x, y){

  # merge into single wrapped geojson for ease of use across territories
  if (nrow(x) == 0) return(x)

  cat(glue::glue("    Intersecting"), "\n")
  x %>%
    sf_intersects(y) %>%
    sf::st_make_valid() %>%
    sf::st_intersection(y) %>%
    dplyr::mutate(
      one = 1,
      geometry = st_cast(geometry, "MULTIPOLYGON"))
}

sf_lyr_ply <- function(lyr_p){

  lyr      <- lyr_p$key
  lyr_info <- get_lyr_info(lyr)
  ply_geo  <- glue::glue("{dir_prep_data}/{lyr}/_{lyr}_epsg4326.geojson")

  if (!file.exists(ply_geo)){
    cat(glue::glue("  Assembling: {ply_geo}"), "\n")

    paths <- stringr::str_split(lyr_p$paths, ";")[[1]]
    for (k in 1:length(paths)){ # k <- 2

      path <- paths[k]
      cat(glue::glue("    sf_wrap (& rbind): {basename(path)}"), "\n")

      ply1 <- sf::read_sf(path) %>%
        sf_wrap() %>%
        dplyr::rename_all(tolower)

      if (k == 1){
        ply <- ply1
      } else {
        # TODO: add this exception to `prep_eval` fld in prep_params.csv and fxn to prep_layer_functions.R
        if (p$key == "wind" & basename(path) == "atlantic_coast_90mwindspeed_off.shp"){
          ply1 <- ply1 %>% dplyr::rename(gridcode=wpc)
        }

        # assign missing fields in order to rbind
        for (fld in setdiff(names(ply), names(ply1))){ # fld = setdiff(names(ply), names(ply1))[1]
          ply1[[fld]] <- NA
          class(ply1[[fld]]) <- class(ply[[fld]])
        }
        for (fld in setdiff(names(ply1), names(ply))){ # fld = setdiff(names(ply), names(ply1))[1]
          ply[[fld]] <- NA
          class(ply[[fld]]) <- class(ply1[[fld]])
        }

        # bind
        ply <- sf::rbind(ply, ply1)
      }
      #browser()
      cat(glue::glue("      {length(names(ply))}: {paste(names(ply), collapse=',')}"),"\n")
    }
    #browser()
    sf::write_sf(ply, ply_geo)
    # TODO: fix sf_wrap() to output sf_column geometry since automatic name for geojson
  }
  #cat(glue::glue("    Reading: {basename(ply_geo)}"), "\n")
  ply <- sf::read_sf(ply_geo)
  ply
}

#' Quick map of polygon layers
#'
#' @param ply
#'
#' @return init for efh assuming polygons with layers column, categorical
#' @export
ply_map <- function(ply, fld_color="layer"){
  library(leaflet)

  m <- leaflet::leaflet(
    options = leaflet::leafletOptions(worldCopyJump=T)) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap, group="Color") %>%
    leaflet::addProviderTiles(leaflet::providers$Stamen.TonerLite, group="B&W")

  layers <- sort(unique(ply[[fld_color]]))
  v_i <- 1:length(layers)
  pal <- leaflet::colorFactor("Spectral", v_i)
  for (i in v_i){
    lyr <- layers[i]

    m <- m %>%
      leaflet::addPolygons(
        data = filter_at(ply, fld_color, any_vars(. == lyr)),
        color = pal(i), group = lyr)
  }

  m %>%
    leaflet::addLegend(colors = pal(v_i), labels = layers, position = "bottomright") %>%
    leaflet::addLayersControl(
      baseGroups = c("B&W", "Color"),
      overlayGroups = layers)
}
