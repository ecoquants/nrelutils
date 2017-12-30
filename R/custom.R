get_ter_eez_wgcs_sf <- function(ter){
  read_sf(eez_wgcs_geo) %>%
    filter(territory==ter)
}

get_ter_depth_wgcs_r <- function(ter){

  dir.create("./depth", showWarnings=F)
  # ter = "Great Lakes"
  ter_depth_wgcs_tif <- sprintf("./depth/%s_depth_epsg4326.tif", ter)
  if (!file.exists(ter_depth_wgcs_tif)){
    cat("    creating", ter_depth_wgcs_tif, "\n")

    depth_wgcs_tif <- "depth/_depth_epsg4326.tif"
    if (!file.exists(depth_wgcs_tif)){
      cat("    creating", depth_wgcs_tif, "\n")

      depth_gcs  <- raster(depth_nc, layer = "elevation") # depth_gcs_0 <- depth_gcs

      # burn in [Great Lakes Bathymetry | NCEI](https://www.ngdc.noaa.gov/mgg/greatlakes/greatlakes.html)
      gl_tifs  <- list.files(
        "/Volumes/Best HD/nrel_data_big/greatlakes_ngdc",
        ".*\\.tif$", recursive = T, full.names = T)
      r_gl      <- do.call(raster::merge, lapply(gl_tifs, raster::raster)) # pl
      r_gl_gcs  <- raster::projectRaster(r_gl, depth_gcs) # plot(r_gl_gcs)
      depth_gcs <- raster::merge(r_gl_gcs, depth_gcs)

      depth_wgcs <- raster_wrap(depth_gcs)
      depth_wgcs <- depth_wgcs * -1
      writeRaster(depth_wgcs, depth_wgcs_tif, overwrite=T)
    } else {
      depth_wgcs <- raster(depth_wgcs_tif)
    }

    ter_eez_wgcs_sf <- get_ter_eez_wgcs_sf(ter) # plot(ter_eez_wgcs_sf["territory"])
    ter_eez_wgcs_r <- fasterize(ter_eez_wgcs_sf, depth_wgcs) %>% trim() # plot(ter_eez_wgcs_sf["territory"]); plot(ter_eez_wgcs_r, add=T)
    ter_depth_wgcs <- raster_clip(depth_wgcs, ter_eez_wgcs_r) # plot(ter_eez_wgcs_sf["territory"]); plot(ter_depth_wgcs, add=T)
    ter_depth_wgcs[ter_depth_wgcs < 0] <- NA # plot(ter_eez_wgcs_sf["territory"]); plot(ter_depth_wgcs >= 0, add=T)
    ter_depth_wgcs <- trim(ter_depth_wgcs) # plot(ter_eez_wgcs_sf["territory"]); plot(ter_depth_wgcs, add=T)

    ter_depth_wgcs_tif <- sprintf("./depth/%s_depth_epsg4326.tif", ter)
    writeRaster(ter_depth_wgcs, ter_depth_wgcs_tif, overwrite=T)
  }
  ter_depth_wgcs <- raster(ter_depth_wgcs_tif) # plot(ter_depth_wgcs)

  ter_depth_wgcs # plot(ter_depth_wgcs)
}

ter_stack <- function(ter, lyr_params, dir_data=here::here("prep/data"), debug=F){
  # ter = "West"; params_csv = "./prep_params.csv"

  keys <- sort(lyr_params$key)

  #if (exists("s")) rm(s)
  i_key = 0
  for (key in keys){ # key <- "mpa" # keys[1] "efh" mpa" "wind"
    if (debug) cat(glue::glue("{key}"),"\n")
    digest_txt <- glue::glue("{dir_data}/{key}/{ter}_{key}_epsg4326.txt")

    if (suppressMessages(readr::read_lines(digest_txt)[1]) == "0"){
      if (debug) msg("  0, skipping")
      next
    }
    i_key = i_key + 1

    df <- suppressMessages(readr::read_delim(digest_txt, ":", col_names = c("name","path"))) %>%
      dplyr::mutate(
        name = ifelse(name == key, name, glue::glue("{key}_{name}")),
        #name = glue("{key}_{name}"),
        path = glue::glue("{dir_data}/{key}/{path}"))
    s1 <- raster::stack(as.list(df$path))
    names(s1) <- df$name

    if (debug) cat(" ", paste0(df$name, collapse="\n  "),"\n")

    if (i_key == 1){
      sA <- s1
    } else {
      if (!raster::compareRaster(sA, s1, stopiffalse=F)){
        stop(glue::glue("WHOAH! {key} has diff't raster size/origin/extent/resolution/projection!"))
        #next() # browser()
      }
      sA <- raster::stack(sA, s1)
    }
  }

  # add area
  sA <- raster::stack(sA, raster::area(sA))
  names(sA)[raster::nlayers(sA)] <- "area_km2"

  sA
}

ter_stack_tbl <- function(s){

  tbl_s <- raster::as.data.frame(s) %>%
    tibble::as_tibble()
}

ter_bin <- function(tbl_s, bin_by = "depth"){
  lims <- nrel_limits[[bin_by]]

  tbl_b <- tbl_s %>%
    dplyr::filter(!is.na(depth))

  if (bin_by != "depth"){
    tbl_b <- tbl_b %>%
      dplyr::filter(
        depth  >= lims$depth$min,
        depth  <= lims$depth$max)
  }

  tbl_b$vals <- tbl_b[[bin_by]]
  tbl_b <- tbl_b %>%
    dplyr::mutate(
      bin = cut(vals, lims$breaks, lims$break_labels, include.lowest = T)) %>%
    dplyr::filter(
      !is.na(bin))
  tbl_b
}

ter_bin_lyr <- function(tbl_b){

  tbl_b <- tbl_b %>%
    dplyr::mutate(
      bin = factor(as.character(bin), c(levels(bin), "Total"), ordered=T))

  lyrs_rm <- c(names(nrel_limits), "vals")
  tbl_b <- suppressWarnings(dplyr::select(tbl_b, -dplyr::one_of(lyrs_rm)))

  tbl_b
}

ter_bin_lyr_smry <- function(tbl_b){

  # totals
  dt <- tbl_b %>%
    #dplyr::mutate(
    #  bin = as.character(bin)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      area_km2 = sum(area_km2))
  dt <- dt %>%
    tibble::add_row(
      bin      = "Total",
      area_km2 = sum(dt$area_km2)) %>%
    tidyr::spread(bin, area_km2)
  dt <- tibble::tibble(
    layer = " Total Area") %>%
    dplyr::bind_cols(dt)
  dt$rank <- 0

  # dl: layer x bin = area_km2
  dl <- tbl_b %>%
    tidyr::gather(key = layer, value = value, -bin, -area_km2) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(layer, bin) %>%
    dplyr::summarise(
      area_km2 = sum(area_km2)) %>%
    tidyr::spread(bin, area_km2) %>%
    dplyr::ungroup()
  dl$Total <- rowSums(dl[,-1], na.rm = TRUE)

  # rank
  dl <- dl %>%
    dplyr::mutate(
      rank = dplyr::dense_rank(1/Total))

  # bind totals
  tbl_bls <- dplyr::bind_rows(dt, dl)
  tbl_bls
}

svg_bar <- function(pct, val){
  pct <- ifelse(is.na(pct), 0, round(pct * 100, 3))
  #cat("val before", val,"\n")
  val <- ifelse(is.na(val), "0", format(round(val), big.mark = ","))
  #cat("val after", val,"\n")
  glue::glue('
             <svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="100%" height="14px" preserveAspectRatio="xMaxYMin meet">
             <g>
             <rect width="{pct}%" height="16" fill="DarkGray"></rect>
             <text text-anchor="end" x="100%" y="12">{val}</text>
             </g>
             </svg>
             ')
  # <text text-anchor="end" x="100%" y="11" font-family="Courier New" font-size="14px" color="black">{val}</text>
}

ter_bin_lyr_smry_plus <- function(ter_bin_lyr_csv, lyr_params, lyr_categories){

  tbl_bls <- suppressMessages(read_csv(ter_bin_lyr_csv, trim_ws=F))
  bins <- tbl_bls %>%
    select(-one_of(c("layer","rank"))) %>%
    names()

  dt <- tbl_bls %>%
    filter(layer == " Total Area")

  # percent, svg
  #browser()
  for (bin in bins){ # bin = bins[1]
    col_pct <- glue::glue("{bin}_PCT")
    col_svg <- glue::glue("{bin}_SVG")

    tbl_bls[[col_pct]] <- tbl_bls[[bin]] / dt[[bin]]
    tbl_bls[[col_svg]] <- purrr::map2_chr(tbl_bls[[col_pct]], tbl_bls[[bin]], svg_bar)
  }

  # key, component
  tbl_bls <- tbl_bls %>%
    dplyr::mutate(
      key       = stringr::str_replace(layer, "(.*)_(.*)", "\\1"),
      component = ifelse(
        key == layer, "",
        stringr::str_replace(layer, "(.*)_(.*)", "\\2") %>% stringr::str_replace_all("\\.", " ")))

  # title, category
  tbl_blsp <- tbl_bls %>%
    dplyr::left_join(
      lyr_params %>%
        dplyr::select(key, title) %>%
        dplyr::bind_rows(tibble::tibble(
          key = " Total Area",
          title = " Total Area")),
      by = "key") %>%
    left_join(
      lyr_categories %>%
      select(key, use),
    by="key")

  tbl_blsp
}

print_ter_bin_lyr_smry_plus <- function(tbl_blsp){
  # variables used, but not declared
  # i_tbl (global, write)
  # ter
  # bin_html
  # TODO: if html: sortable/searchable header
  fmt <- get_fmt()
  bins <- names(tbl_blsp)[2:which(names(tbl_blsp)=="Total")]
  #browser()

  th_bins <- htmltools::tagList(lapply(bins, htmltools::tags$th))
  hdr = htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(colspan=3, "Ocean Use"),
        th(colspan=5, HTML(glue("Area (km<sup>2</sup>) by {bin_html}")))),
      tr(
        th("Category"),
        th("Layer"),
        th("Component"),
        th_bins,
        th("Rank")))))

  i_tbl <<- i_tbl + 1
  caption <- HTML(glue('Table {i_tbl}: Area (km<sup>2</sup>) of Ocean Use by {bin_html} bin for {ter} region. Width of gray horizontal bars from left of cell indicate percent area occupied in given depth bin.'))

  tbl_blsp %>%
    select(-one_of(glue("{bins}"))) %>%
    rename_at(
      vars(one_of(glue("{bins}_SVG"))),
      ~stringr::str_replace(., "_SVG", "")) %>%
    select(layer, one_of(glue("{bins}")))

  d <- tbl_blsp[1,] %>%
    bind_rows(
      tbl_blsp[-1,] %>%
        arrange(use, title, component)) %>%
    rename(
      Category  = use,
      Layer     = title,
      Component = component,
      Rank      = rank) %>%
    select(-one_of(glue("{bins}"))) %>%
    rename_at(
      vars(one_of(glue("{bins}_SVG"))),
      ~stringr::str_replace(., "_SVG", "")) %>%
    select(one_of(c("Category", "Layer", "Component", bins, "Rank")))

  datatable(
    d,
    escape    = F,
    rownames  = F,
    container = hdr,
    caption   = caption,
    options   = list(
      pageLength = nrow(tbl_blsp),
      dom=c(
        html_document = 'lftir',
        pdf_document  = 't',
        word_document = 't')[[fmt]],
      ordering=c(
        html_document = TRUE,
        pdf_document  = FALSE,
        word_document = FALSE)[[fmt]],
      autoWidth = TRUE,
      columnDefs = list(
        list(width     = '80px'    , targets = 3:(2+length(bins))),
        list(className = 'dt-right', targets = 3:(3+length(bins))))))
}

ter_bin_cnt <- function(tbl_b){
  # table for cumulative count plot
  cols_tally <- setdiff(names(tbl_b), c(names(nrel_limits), "bin", "vals", "area_km2"))
  tbl_b$tally <- apply(tbl_b[, cols_tally], 1, function(x) sum(!is.na(x)))
  tbl_b <- tbl_b %>%
    dplyr::group_by(bin, tally) %>%
    dplyr::summarise(
      area_km2 = sum(area_km2)) #%>%
    #filter(tally>0) # View(tbl_df2)

  lyrs_rm <- c(names(nrel_limits), "vals")
  tbl_bc <- suppressWarnings(dplyr::select(tbl_b, -dplyr::one_of(lyrs_rm)))
  tbl_bc
}

plot_caption_ter_bin_cnt <- function(ter, bin_html){
  glue::glue("Area of Cumulative Ocean Use by {bin_html} for {ter} region. Cumulative use are counted and summarized by {bin_html} and area.")
}

plot_ter_bin_cnt <- function(ter_bin_cnt_csv, bin_html){
  fmt <- get_fmt()
  tbl_bc  <- suppressMessages(read_csv(ter_bin_cnt_csv, trim_ws=F))
  tbl_bc$bin <- with(tbl_bc, factor(bin, unique(bin), ordered=T))

  g <- ggplot(data = tbl_bc, aes(x = bin, y = area_km2, fill = tally)) +
    geom_col() +
    scale_fill_distiller(palette = "Spectral", name="Count")

  if (fmt == "html_document"){
    g <- g +
      labs(x = bin_html, y = "Area (km<sup>2</sup>)")
    g <- plotly::ggplotly(g)
  } else {
    g <- g +
      labs(x = bin_html, y = expression(Area (km^2)))
  }
  g
}

ter_cnt_raster <- function(s){
  lyrs_rm <- c(names(nrel_limits), "area_km2")
  s_cnt   <- raster::dropLayer(s, which(names(s) %in% lyrs_rm))
  r_cnt   <- sum(!is.na(s_cnt), na.rm=T) %>%
    raster::mask(raster::raster(s, "depth"))
  r <- raster_unwrap(r_cnt) # plot(r)

  r_3857 <- leaflet::projectRasterForLeaflet(r)
  r_3857
}

ter_cnt_raster2 <- function(s){
  # TODO: use yaml for epsg4326:*.tif \n epsg3857:\n  big:*.tif\n  small:*.tif
  #ter_cnt_raster <- function(s){
  lyrs_rm <- c(names(nrel_limits), "area_km2")
  s_cnt   <- raster::dropLayer(s, which(names(s) %in% lyrs_rm))
  r_cnt   <- sum(!is.na(s_cnt), na.rm=T) %>%
    raster::mask(raster::raster(s, "depth"))
  r <- raster_unwrap(r_cnt) # plot(r)

  ext <- raster::extent(s)
  ext@xmin < 180
  r <- ter_cnt_raster(s)
  r
}

map_caption_ter_cnt <- function(ter){
  glue::glue("Map of cumulative ocean use in {ter} region.")
  # TODO: if ( fmt==html_document & length(info$) ),
  #         pan to other side of dateline to see rest of map
}

#' Leaflet map of territory's ocean use count
#'
#' @param info
#'
#' @return
#' @export
#'
#' @examples
lmap_ter_cnt <- function(info){
  b <- info$raster$epsg3857_init_bounds
  r <- raster::raster(info$raster$epsg3857[1])

  vals <- getValues(r)
  if (length(info$raster$epsg3857) == 2){
    r2 <- raster::raster(info$raster$epsg3857[2])
    vals <- c(vals, getValues(r2))
  }
  pal <- colorNumeric(
    palette = 'Spectral', na.color="#00000000",
    reverse=T, domain = vals)

  m <- leaflet::leaflet(
    options=leafletOptions(worldCopyJump=T)) %>%
    #options=c(leafletOptions(), attributionControl=F, zoomControl=F, worldCopyJump=T)) %>%
    leaflet::addProviderTiles(providers$Stamen.TonerLite, group = "B&W") %>%
    leaflet::addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
    leaflet::addRasterImage(r, project=F, colors=pal, opacity=0.8, group="Count")

  if (length(info$raster$epsg3857) == 2){
    m <- m %>%
      leaflet::addRasterImage(r2, project=F, colors=pal, opacity=0.8, group="Count")
  }

  m <- m %>%
    leaflet::addGraticule() %>% # interval=1
    leaflet::addScaleBar("bottomleft") %>%
    leaflet::addLegend("bottomright", pal, vals, opacity=0.8, title="Count") %>%
    leaflet::fitBounds(b[1], b[3], b[2], b[4]) %>%
    leaflet::addLayersControl(baseGroups = c("B&W", "Ocean"), overlayGroups=c("Count"))
  m
}

#' Ggplot map of territory's ocean use count
#'
#' @param r
#'
#' @return
#' @export
#'
#' @examples
gmap_ter_cnt <- function(info){

  r       <- raster::raster(info$raster$epsg4326)
  b       <- raster::extent(r) %>% as.vector()
  bb      <- extent(r) %>% as("SpatialPolygons") %>% fortify()
  world2  <- ggplot2::map_data("world2")

  # TODO: contour raster to ggplot2?
  #r_depth <- raster::raster(here(glue("prep/data/depth/{ter}_depth_epsg4326.tif")))
  #max(nrel_limits$depth$breaks)
  # contourplot(r_depth, at=max(nrel_limits$depth$breaks), labels=F, lwd=0.4, col="gray", lwd=0.1)

  #browser()
  # TODO: rnaturalearth world, clip to USA EEZ, burn Great Lakes
  # TODO: depth
  p <- rasterVis::gplot(r) +
    ggplot2::geom_polygon(
      data = world2, aes(x=long, y=lat, group=group),
      color = "black", fill = "darkgray", size=0.2) +
    ggplot2::geom_tile(aes(fill=value), alpha=0.8) +
    ggplot2::scale_fill_distiller(
      palette = "Spectral", name = "Count", direction = -1, na.value = NA) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::coord_fixed(xlim = b[1:2],  ylim = b[3:4], ratio = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = element_text(size = ggplot2::rel(0.6))) +
    ggsn::scalebar(
      data = bb, location="bottomleft", dist=100, dd2km=T, model="WGS84", st.size=1)
  print(p)
}

get_fmt <- function(){
  rmd <- knitr::current_input()
  if (!is.null(rmd)){
    fmt <- rmarkdown::default_output_format(rmd)$name
  } else {
    fmt <- "html_document" # for interactive output
  }
  fmt
}

map_ter <- function(ter_info_yml, type="cnt"){

  info  <- yaml::yaml.load_file(ter_cnt_yml)
  fmt   <- get_fmt()
  b     <- info$raster$epsg4326_bounds
  dpi   <- 300
  w     <- 6
  h_max <- 8
  h     <- min(c(h_max, diff(b[3:4])/diff(b[1:2])*w))
  redo_figs <- F

  ter_info <- get_ter_info(ter_info_yml, type)

  message(glue::glue("info$figure$png: {info$figure$png}. "))
  if (!file.exists(info$figure$png) | redo_figs){
    message(glue::glue("  png({info$figure$png}). "))
    png(filename=info$figure$png, res=dpi, width=w*dpi, height=h*dpi)
    gmap_ter_cnt(info)
    dev.off()
  }
  message(glue::glue("info$figure$pdf: {info$figure$pdf}. "))
  if (!file.exists(info$figure$pdf) | redo_figs){
    message(glue::glue("  pdf({info$figure$pdf}). "))
    pdf(file=info$figure$pdf, width=w, height=h)
    gmap_ter_cnt(info)
    dev.off()
  }
  #browseURL(info$figure$png)

  res <- switch(
    fmt,
    html_document = lmap_ter_cnt(info),
    word_document = knitr::include_graphics(info$figure$png),
    pdf_document  = knitr::include_graphics(info$figure$pdf))
  return(res)
}

get_lyr_info <- function(lyr){
  dir_lyr      <- glue::glue("{dir_prep_data}/{lyr}")
  lyr_info_yml <- glue::glue("{dir_prep_data}/{lyr}/_{lyr}.yml")

  if (!dir.exists(dir_lyr)){
    dir.create(dir_lyr)
  }

  if (file.exists(lyr_info_yml)){
    lyr_info <- yaml::read_yaml(lyr_info_yml)
  } else {
    lyr_info <- list(
      key = lyr)
    #browser()
    lyr_info$territories <- setNames(
      rep(list(NULL), length(eez_wgcs_sf$territory)),
      eez_wgcs_sf$territory) %>% as.list()
    yaml::write_yaml(lyr_info, lyr_info_yml)
  }

  lyr_info
}

set_lyr_info <- function(lyr_info){
  lyr <- lyr_info$key
  lyr_info_yml <- glue("{dir_prep_data}/{lyr}/_{lyr}.yml")
  yaml::write_yaml(lyr_info, lyr_info_yml)
}

prep_lyr_ter <- function(lyr_p, lyr_ply, ter){

  lyr          <- lyr_p$key
  lyr_info     <- get_lyr_info(lyr)
  lyr_ter_info <- lyr_info$territories[[ter]]

  #browser()
  if (is.null(lyr_ter_info) | lyr_p$redo){

    # get eez
    ter_eez_wgcs_sf  <- get_ter_eez_wgcs_sf(ter)  # ply_map(ter_eez_wgcs_sf, "territory")

    # intersect with eez and wrap
    ply <- sf_wrap_intersection(lyr_ply, ter_eez_wgcs_sf)
    
    if (nrow(ply) == 0){
      lyr_info$territories[[ter]] <- NA
      set_lyr_info(lyr_info)
    } else {
      
      # TODO: write lyr_info: components=paths

      # modify as needed
      if (!is.na(lyr_p$mod_eval)){
        ply <- eval(parse(text=lyr_p$mod_eval)) # ply_map(ply)
      }
      
      # get depth
      ter_depth_wgcs_r <- get_ter_depth_wgcs_r(ter) # r_map(ter_depth_wgcs_r)
      
      # convert to raster tif(s)
      ply_to_tifs(ply, ter_depth_wgcs_r, ter, lyr, field=lyr_p$field, by=lyr_p$by)
    }
  }
}

get_ter_info <- function(ter_info_yml, dir_data, ter, type="cnt"){

  browser()
  if (file.exists(ter_info_yml)){
    ter_info  <- yaml::yaml.load_file(ter_info_yml)
  } else {
    ter_info <- list()
  }
  ter_info_yml$territory <- ter

  # quick fixes
  if ("raster" %in% names(info)){
    ter_info_yml$cnt <- ter_info_yml$raster
    ter_info_yml     <- ter_info_yml[-which(names(ter_info_yml)=="raster")]
  }
  if ("figure" %in% names(info)){
    ter_info_yml$cnt <- c(ter_info_yml$cnt, info$figure)
    ter_info_yml     <- ter_info_yml[-which(names(ter_info_yml)=="figure")]
  }
  yaml::write_yaml(info, ter_cnt_yml)

  s     <- ter_stack(ter, lyr_params, dir_prep_data)

  if (type == "cnt"){
    #info$cnt$layers
    #if (!file.exists(info$cnt$epsg4326) ||

    lyrs_rm <- c(names(nrel_limits), "area_km2")
    s_cnt   <- raster::dropLayer(s, which(names(s) %in% lyrs_rm))
    r_val   <- sum(!is.na(s_cnt), na.rm=T) %>%
      raster::mask(raster::raster(s, "depth"))

    info$cnt <- list(
        layers          = names(s),
        layers_not_cnt  = lyrs_rm,
        n_layers_cnt    = raster::nlayers(s_cnt),
        cnt_max         = raster::maxValue(r_cnt),
        epsg4326        = glue::glue("{dir_data}/territories/{ter}_cnt_epsg4326.tif"),
        epsg4326_bounds = raster::extent(r_cnt) %>% as.vector())
  } else {
    bin <- type # bin <- "depth"

    lims <- nrel_limits[[bin]]

    tbl_b <- tbl_s %>%
      dplyr::filter(!is.na(depth))

    if (bin_by != "depth"){
      tbl_b <- tbl_b %>%
        dplyr::filter(
          depth  >= lims$depth$min,
          depth  <= lims$depth$max)
    }

    r_val <- raster::raster(s, bin)
    nrel_limits[bin]
  }

  #browser()
  raster::writeRaster(r_val, info$cnt$epsg4326, overwrite=T)

  r <- nrelutils::raster_unwrap(r_cnt)
  if (extent(r)@xmin < -180){

    info$raster$epsg3857 <- glue::glue("{dir_data}/territories/{ter}_cnt_epsg3857_{c('left','right')}.tif")
    r_l <- raster::crop(r, raster::extent(-360,-180,-90,90), snap="in") %>% raster::shift(360) %>% raster::trim()
    r_r <- raster::crop(r, raster::extent(-180, 180,-90,90), snap="in") %>% raster::trim()
    r_l_epsg3857 <- leaflet::projectRasterForLeaflet(r_l)
    r_r_epsg3857 <- leaflet::projectRasterForLeaflet(r_r)
    raster::writeRaster(r_l_epsg3857, info$raster$epsg3857[1], overwrite=T)
    raster::writeRaster(r_r_epsg3857, info$raster$epsg3857[2], overwrite=T)

    # determine biggest longitudinal range for initial map display
    e_l <- raster::extent(r_l)
    e_r <- raster::extent(r_r)
    x_l  <- e_l %>% as.vector() %>% .[1:2] %>% diff()
    x_r  <- e_r %>% as.vector() %>% .[1:2] %>% diff()
    b <- c(e_l, e_r)[[which.max(c(x_l,x_r))]]
    #browser()
  } else {
    info$raster$epsg3857 <- glue::glue("{dir_data}/territories/{ter}_cnt_epsg3857.tif")
    r_epsg3857 <- leaflet::projectRasterForLeaflet(r)
    raster::writeRaster(r_epsg3857, info$raster$epsg3857, overwrite=T)

    b <- raster::extent(r)
  }
  info$raster$epsg3857_init_bounds <- b %>% as.vector()
  yaml::write_yaml(info, ter_cnt_yml)
}

fix_prep_lyr_ter <- function(){
  # quick fix in prep.R: from digest ({ter}_{lyr}_epsg4326.txt) to yaml (_{ter}.yml)
  lyr_ter_txt <- glue("{dir_prep_data}/{lyr}/{ter}_{lyr}_epsg4326.txt")
  if (file.exists(lyr_ter_txt)){
    txt <- read_lines(lyr_ter_txt)

    if (txt[1] == "0"){
      lyr_info$territories[[ter]] <- NA
    } else {
      component_tif <- str_split(txt, ":", simplify=T)
      component_tif <- setNames(component_tif[,2], component_tif[,1]) %>% as.list()
      lyr_info$territories[[ter]]$components <- component_tif
    }
    # devtools::load_all("~/github/nrelutils")
    set_lyr_info(lyr_info)
    file.remove(lyr_ter_txt)
  }
}
