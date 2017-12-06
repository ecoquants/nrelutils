#' Wrap sf geometry [-180,180] to [0,360]
#'
#' @param x sf object [-180,180]
#'
#' @return wrapped sf object [0,360]. The geometry column gets renamed to geom.
#' @export
sf_wrap = function(x){ #
  library(dplyr)
  library(sf)

  fld_geom <- attr(x, "sf_column")

  if (fld_geom != "geom"){
    x[,"geom"] <- x[,fld_geom]
    x <- st_set_geometry(x, "geom")
  }
  x <- x %>%
    st_transform(4326) %>%
    mutate(
      geom = (geom + c(360,90)) %% c(360) - c(0,90)) %>%
    st_set_geometry("geom") %>%
    st_set_crs(4326)
  if (fld_geom != "geom"){
    x <- select(x, -one_of(fld_geom))
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
  library(dplyr)
  library(sf)

  if (fld_geom != "geom"){
    x[,"geom"] <- x[,fld_geom]
    x <- st_set_geometry(x, "geom")
  }
  x <- x %>%
    st_transform(4326) %>%
    mutate(
      geom = (geom + c(360,90)) %% c(-360) - c(0,-360+90)) %>%
    st_set_geometry("geom") %>%
    st_set_crs(4326)
  if (fld_geom != "geom"){
    x <- select(x, -one_of(fld_geom))
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
  i <- st_intersects(a, b)
  slice(a, which(sapply(i, length) > 0))
}

#' Wrap and intersect
#'
#' @param paths_str character. colon `;` seperated paths to sf readable data (shp, gdb, geojson, etc)
#' @param y sf object to intersect
#'
#' @return wrapped and intersected sf object with a one field as a valid MULTIPOLYGON
#' @export
sf_wrap_intersection <- function(paths_str, y){

  # merge into single wrapped geojson for ease of use across territories
  ply_geo <- glue("./{p$key}/_{p$key}_epsg4326.geojson")
  if (!file.exists(ply_geo)){
    cat(glue("  Assembling: {ply_geo}"), "\n")

    paths_v <- str_split(paths_str, ";")[[1]]
    for (k in 1:length(paths_v)){ # k <- 2

      path <- paths_v[k]
      cat(glue("    sf_wrap (& rbind): {basename(path)}"), "\n")

      ply1 <- read_sf(path) %>%
        sf_wrap()

      if (k == 1){
        ply <- ply1
      } else {
        # TODO: add this exception to `prep_eval` fld in prep_params.csv and fxn to prep_layer_functions.R
        if (p$key == "wind" & basename(path) == "atlantic_coast_90mwindspeed_off.shp"){
          ply1 <- ply1 %>% rename(GRIDCODE=WPC)
        }
        ply <- rbind(ply, ply1)
      }
    }
    write_sf(ply, ply_geo)
    # TODO: fix sf_wrap() to output sf_column geometry since automatic name for geojson
  }
  cat(glue("    Reading: {ply_geo}"), "\n")
  ply <- read_sf(ply_geo)

  if (nrow(ply) == 0) return(ply)

  cat(glue("    Intersecting({ply_geo}, y)"), "\n")
  ply  %>%
    sf_intersects(y) %>%
    st_make_valid() %>%
    st_intersection(y) %>%
    mutate(
      one = 1,
      geometry = st_cast(geometry, "MULTIPOLYGON"))
}

#' Quick map of polygon layers
#'
#' @param ply
#'
#' @return init for efh assuming polygons with layers column, categorical
#' @export
ply_map <- function(ply, fld_color="layer"){
  library(leaflet)

  m <- leaflet(
    options = leafletOptions(worldCopyJump=T)) %>%
    addProviderTiles(providers$Esri.OceanBasemap, group="Color") %>%
    addProviderTiles(providers$Stamen.TonerLite, group="B&W")

  layers <- sort(unique(ply[[fld_color]]))
  v_i <- 1:length(layers)
  pal <- leaflet::colorFactor("Spectral", v_i)
  for (i in v_i){
    lyr <- layers[i]

    m <- m %>%
      addPolygons(
        data = filter_at(ply, fld_color, any_vars(. == lyr)),
        color = pal(i), group = lyr)
  }

  m %>%
    addLegend(colors = pal(v_i), labels = layers, position = "bottomright") %>%
    addLayersControl(
      baseGroups = c("B&W", "Color"),
      overlayGroups = layers)
}
