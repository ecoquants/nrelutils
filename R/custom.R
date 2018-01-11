get_ter_eez_wgcs_sf <- function(ter){
  read_sf(eez_wgcs_geo) %>%
    filter(territory==ter)
}

get_ter_depth_wgcs_r <- function(ter){

  dir_depth <- glue("{dir_lyrs}/depth")

  dir.create(dir_depth, showWarnings=F)
  # ter = "Great Lakes"
  ter_depth_wgcs_tif <- glue::glue("{dir_depth}/{ter}_depth_epsg4326.tif")
  if (!file.exists(ter_depth_wgcs_tif)){
    cat("    creating", ter_depth_wgcs_tif, "\n")

    depth_wgcs_tif <- glue::glue("{dir_depth}/_depth_epsg4326.tif")
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

ter_stack <- function(ter, lyr_params){
  # ter = "West"

  lyrs        <- sort(lyr_params %>% filter(run==T) %>% .$key)
  lyrs_in_ter <- setNames(rep(NA, length(lyrs)), lyrs) %>% as.list()

  if (exists("s_ter", inherits=F)) rm(s_ter)
  for (lyr in lyrs){ # lyr = "aquaculture" # "wind" # key <- "mpa" # keys[1] "efh" mpa" "wind"
    lyr_info_yml <- glue::glue("{dir_lyrs}/{lyr}/_{lyr}.yml")
    lyr_info     <- yaml::read_yaml(lyr_info_yml)

    # skip if layer not in territory
    if (is.na(lyr_info$territories[[ter]])){
      next
    }

    # get raster stack of layer components
    msg(g("    {ter} - {lyr}"))
    lyrs_in_ter[[lyr]] <- names(lyr_info$territories[[ter]]$components)
    tifs               <- purrr::map_chr(
      lyr_info$territories[[ter]]$components,
      ~glue::glue("{dir_lyrs}/{lyr}/{.x[1]}"))

    #if (lyr == "oceanuseatlas.hi" & ter == "Hawaii") browser("TODO: extend all to depth r")
    #r_a <- fasterize::fasterize(x, y, field=field, fun="first", by=by)

    s_lyr <- raster::stack(tifs)
    if (length(lyrs_in_ter[[lyr]]) == 1 && lyr == lyrs_in_ter[[lyr]]){
      names(s_lyr) <- lyr
    } else {
      names(s_lyr) <- glue::glue("{lyr}_{lyrs_in_ter[[lyr]]}")
    }

    # add layer stack to territory stack
    if (!exists("s_ter", inherits=F)){
      s_ter <- s_lyr
    } else {
      s_ter <- raster::stack(s_ter, s_lyr)
    }
  }

  # add area to territory stack
  s_ter <- raster::stack(s_ter, raster::area(s_ter))
  names(s_ter)[raster::nlayers(s_ter)] <- "areakm2"
  lyrs_in_ter$areakm2 <- "areakm2"

  # add attribute of layers in territory stack
  attr(s_ter, "lyrs_in_ter") <- lyrs_in_ter
  s_ter
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
      areakm2 = sum(areakm2))
  dt <- dt %>%
    tibble::add_row(
      bin      = "Total",
      areakm2 = sum(dt$areakm2)) %>%
    tidyr::spread(bin, areakm2)
  dt <- tibble::tibble(
    layer = " Total Area") %>%
    dplyr::bind_cols(dt)
  dt$rank <- 0

  # dl: layer x bin = areakm2
  #if (ter=="Atlantic Islands" & type == "wave" & element == "count")
  #browser()
  dl <- tbl_b %>%
    tidyr::gather(key = layer, value = value, -bin, -areakm2) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(layer, bin) %>%
    dplyr::summarise(
      areakm2 = sum(areakm2))
  if (nrow(dl) == 0){
    dl <- NULL
  } else {
    dl <- dl %>%
      tidyr::spread(bin, areakm2) %>%
      dplyr::ungroup()
    dl$Total <- rowSums(dl[,-1], na.rm = TRUE)

    # rank
    dl <- dl %>%
      dplyr::mutate(
        rank = dplyr::dense_rank(1/Total))
  }

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

ter_bin_lyr_smry_plus <- function(ter, bin, lyr_params, lyr_categories){

  ter_info <- get_ter_info(ter)
  ter_bin_lyr_csv <- ter_info[[bin]]$count$layers_csv
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
        th("Dataset"),
        th("Layer"),
        th_bins,
        th("Rank")))))

  i_tbl <<- i_tbl + 1
  caption <- HTML(glue('Table {i_tbl}: Area (km<sup>2</sup>) of Ocean Use by {bin_html} bin for {ter} region.
                       Width of gray horizontal bars from left of cell indicate percent area occupied in given {bin_html} bin.'))

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
  cols_tally <- setdiff(names(tbl_b), c(names(nrel_limits), "bin", "vals", "areakm2"))
  tbl_b$tally <- apply(tbl_b[, cols_tally], 1, function(x) sum(!is.na(x)))
  tbl_b <- tbl_b %>%
    dplyr::group_by(bin, tally) %>%
    dplyr::summarise(
      areakm2 = sum(areakm2)) #%>%
    #filter(tally>0) # View(tbl_df2)

  lyrs_rm <- c(names(nrel_limits), "vals")
  tbl_bc <- suppressWarnings(dplyr::select(tbl_b, -dplyr::one_of(lyrs_rm)))
  tbl_bc
}

plot_caption_ter_bin_cnt <- function(ter, bin_html){
  glue::glue("Area of Cumulative Ocean Use by {bin_html} for {ter} region. Cumulative use are counted and summarized by {bin_html} and area.")
}

plot_ter_bin_cnt <- function(ter, bin){

  fmt <- get_fmt()
  ter_info <- get_ter_info(ter)
  ter_bin_cnt_csv <- ter_info[[bin]]$count$cumulative_csv
  tbl_bc  <- suppressMessages(read_csv(ter_bin_cnt_csv, trim_ws=F))
  tbl_bc$bin <- with(tbl_bc, factor(bin, unique(bin), ordered=T))

  g <- ggplot(data = tbl_bc, aes(x = bin, y = areakm2, fill = tally)) +
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
  lyrs_rm <- c(names(nrel_limits), "areakm2")
  s_cnt   <- raster::dropLayer(s, which(names(s) %in% lyrs_rm))
  r_cnt   <- sum(!is.na(s_cnt), na.rm=T) %>%
    raster::mask(raster::raster(s, "depth"))
  r <- raster_unwrap(r_cnt) # plot(r)

  r_3857 <- raster_project_leaflet_nearest(r)
  r_3857
}

ter_cnt_raster2 <- function(s){
  # TODO: use yaml for epsg4326:*.tif \n epsg3857:\n  big:*.tif\n  small:*.tif
  #ter_cnt_raster <- function(s){
  lyrs_rm <- c(names(nrel_limits), "areakm2")
  s_cnt   <- raster::dropLayer(s, which(names(s) %in% lyrs_rm))
  r_cnt   <- sum(!is.na(s_cnt), na.rm=T) %>%
    raster::mask(raster::raster(s, "depth"))
  r <- raster_unwrap(r_cnt) # plot(r)

  ext <- raster::extent(s)
  ext@xmin < 180
  r <- ter_cnt_raster(s)
  r
}

map_ter_caption <- function(ter, type, element){
  str_type <- ifelse(element == "count", "cumulative ocean use", glue::glue("viable {type}"))
  glue::glue("Map of {str_type} in {ter} region.")
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
lmap_ter <- function(ter, type, element){
  # ter='East'; type='wind'; element='limits'

  ter_info <- get_ter_info(ter)
  el       <- ter_info[[type]][[element]]
  b        <- el$epsg3857_extent_init
  r        <- raster::raster(el$epsg3857_tif[1])

  # TODO: fix creation vs here
  if (b[1] > 180){
    b[1:2] = b[1:2] - 360
  }

  vals <- getValues(r)
  if (length(el$epsg3857_tif) == 2){
    r2 <- raster::raster(el$epsg3857_tif[2])
    vals <- c(vals, getValues(r2))
  }

  if (element == "limits"){
    # TODO: for limits project to leaflet without interpolation so integer
    brk_labels <<- nrel_limits[[type]]$break_labels
    legend_title <- stringr::str_to_title(type)
    vals <- round(vals)
    r    <- round(r)
    if (length(el$epsg3857_tif) == 2){
      r2 <- round(r2)
    }

    pal_col <- ifelse(type == "depth", "Blues", "Greens")
    pal <- leaflet::colorFactor(
      palette = pal_col,
      domain = factor(round(vals), 1:length(brk_labels), brk_labels),
      levels = 1:length(brk_labels),
      na.color="#00000000", reverse=F)

  } else {
    legend_title <- "Count"

    pal <- colorNumeric(
      palette = 'Spectral', na.color="#00000000",
      reverse=T, domain = vals)
  }

  m <- leaflet::leaflet(
    options=leafletOptions(worldCopyJump=T)) %>%
    #options=c(leafletOptions(), attributionControl=F, zoomControl=F, worldCopyJump=T)) %>%
    leaflet::addProviderTiles(providers$Stamen.TonerLite, group = "B&W") %>%
    leaflet::addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
    leaflet::addRasterImage(r, project=F, colors=pal, opacity=0.8, group="Count")

  if (length(el$epsg3857_tif) == 2){
    m <- m %>%
      leaflet::addRasterImage(r2, project=F, colors=pal, opacity=0.8, group="Count")
  }

  m <- m %>%
    leaflet::addGraticule() %>% # interval=1
    leaflet::addScaleBar("bottomleft") %>%
    leaflet::fitBounds(b[1], b[3], b[2], b[4]) %>%
    leaflet::addLayersControl(baseGroups = c("B&W", "Ocean"), overlayGroups=c("Count"))

  if (element == "limits"){
    m <- m %>%
      leaflet::addLegend(
        "bottomright",
        colors = pal(1:length(brk_labels)),
        labels = brk_labels,
        opacity = 0.8, title = legend_title)

  } else {
    m <- m %>%
      leaflet::addLegend("bottomright", pal, vals, opacity=0.8, title=legend_title)
  }
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
gmap_ter <- function(ter, type, element){
  # ter <- "Alaska"; type <- "depth"; element <- "limits"
  #gmap_ter("Alaska", "depth", "limits")
    # ter <- ; type <- "depth"; element <-


  ter_info <- get_ter_info(ter)
  el       <- ter_info[[type]][[element]]
  # devtools::load_all("~/github/nrelutils")
  r        <- raster::raster(el$epsg4326_tif) %>% raster_trim()
  b        <- raster::extent(r) %>% as.vector()
  bb       <- extent(r) %>% as("SpatialPolygons") %>% fortify()
  world2   <- ggplot2::map_data("world2")

  p <- rasterVis::gplot(r) +
    ggplot2::geom_polygon(
      data = world2, aes(x=long, y=lat, group=group),
      color = "black", fill = "darkgray", size=0.2) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggsn::scalebar(
      data = bb, location="bottomleft", dist=100, dd2km=T, model="WGS84", st.size=1) +
    ggplot2::coord_fixed(xlim = b[1:2],  ylim = b[3:4], ratio = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = element_text(size = ggplot2::rel(0.6)))


  # TODO: add units to legend_title
  #if (type == "depth" & element == "limits") browser()

  if (element == "limits"){
    brk_labels <<- nrel_limits[[type]]$break_labels
    msg(g("        {type}: {paste(brk_labels, collapse=',')}"))
    pal <- ifelse(type == "depth", "Blues", "Greens")
    legend_title <- stringr::str_to_title(type)

    p <- p +
      ggplot2::geom_tile(aes(
        fill=factor(value, levels=1:length(brk_labels), labels=brk_labels)), alpha=0.8) +
      ggplot2::scale_fill_brewer(
        palette = pal, name = legend_title, direction = 1, na.value = NA)
  } else {
    legend_title <- "Count"

    p <- p +
      ggplot2::geom_tile(aes(fill=value), alpha=0.8) +
      ggplot2::scale_fill_distiller(
        palette = "Spectral", name = legend_title, direction = -1, na.value = NA)
  }

  print(p)
}

get_fmt <- function(){
  #rmd <- knitr::current_input()
  pandoc_to <- opts_knit$get("rmarkdown.pandoc.to")
  #if (!is.null(rmd)){
  if (!is.null(pandoc_to)){
    #fmt <- rmarkdown::default_output_format(rmd)$name
    fmt <- switch(
      pandoc_to,
      html = "html_document",
      latex = "pdf_document",
      docx  = "word_document")
  } else {
    fmt <- "html_document" # for interactive output
  }
  fmt
}

map_ter <- function(ter, type="all", element="count", redo_figs=F, return_null=F){

  #msg(g("      {ter}_{type}_{element}_map.[png|pdf]"))

  ter_info  <- get_ter_info(ter)

  if (is.na(ter_info[[type]][1])) return(NULL)

  el <- ter_info[[type]][[element]]

  map_pfx <- glue::glue("{dir_data}/territories/{ter}_{type}_{element}")
  map_png <- glue::glue("{map_pfx}_map.png")
  map_pdf <- glue::glue("{map_pfx}_map.pdf")
  ter_info[[type]][[element]]$map_png <- map_png
  ter_info[[type]][[element]]$map_pdf <- map_pdf
  set_ter_info(ter_info)

  fmt       <- get_fmt()
  b         <- el$epsg4326_extent
  dpi       <- 300
  w         <- 6
  h_max     <- 8
  h         <- min(c(h_max, diff(b[3:4])/diff(b[1:2])*w))

  msg(g("  map_ter({ter}, {type}, {element}); fmt={fmt}"))

  if (!file.exists(map_png) | redo_figs){
    message(glue::glue("        png({basename(map_png)})"))
    png(filename=map_png, res=dpi, width=w*dpi, height=h*dpi)
    gmap_ter(ter, type, element)
    dev.off()
  }

  if (!file.exists(map_pdf) | redo_figs){
    message(glue::glue("        pdf({basename(map_pdf)})"))
    pdf(file=map_pdf, width=w, height=h)
    gmap_ter(ter, type, element)
    dev.off()
  }
  #browseURL(info$figure$png)

  if (return_null) return(NULL)

  res <- switch(
    fmt,
    html_document = lmap_ter(ter, type, element),
    word_document = knitr::include_graphics(map_png),
    pdf_document  = knitr::include_graphics(map_pdf))
  return(res)
}

get_lyr_info <- function(lyr){
  dir_lyr      <- glue::glue("{dir_lyrs}/{lyr}")
  lyr_info_yml <- glue::glue("{dir_lyrs}/{lyr}/_{lyr}.yml")

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
  lyr_info_yml <- glue("{dir_lyrs}/{lyr}/_{lyr}.yml")
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

    # check if any intersection
    ply <- sf_intersects(lyr_ply, ter_eez_wgcs_sf)
    if (nrow(ply) == 0){
      lyr_info$territories[[ter]] <- NA
      set_lyr_info(lyr_info)
      return(NULL)
    }

    # save ter lyr
    lyr_ter_geo <- glue::glue("{dir_lyrs}/{lyr}/{ter}_{lyr}_epsg4326.geojson")
    if (!file.exists(lyr_ter_geo)){
       # ply <- sf_intersection(lyr_ply, ter_eez_wgcs_sf) # SLOW!
       write_sf(ply, lyr_ter_geo)
    } else {
       ply <- read_sf(lyr_ter_geo)
    }

    # if (nrow(ply) == 0){
    #   lyr_info$territories[[ter]] <- NA
    #   set_lyr_info(lyr_info)
    # } else {

    # modify as needed
    if (!is.na(lyr_p$mod_eval)){
      ply <- eval(parse(text=lyr_p$mod_eval)) # ply_map(ply)
    }

    # get depth
    ter_depth_wgcs_r <- get_ter_depth_wgcs_r(ter) # r_map(ter_depth_wgcs_r)

    # convert to raster tif(s)
    ply_to_tifs(ply, ter_depth_wgcs_r, ter, lyr, field=lyr_p$field, by=lyr_p$by)

  }
  #}
}

set_ter_info <- function(ter_info){
  ter <- ter_info$territory
  ter_info_yml <- glue("{dir_data}/territories/{ter}.yml")
  yaml::write_yaml(ter_info, ter_info_yml)
}

get_ter_info <- function(ter){
  ter_info_yml <- glue("{dir_data}/territories/{ter}.yml")
  if (file.exists(ter_info_yml)){
    ter_info  <- yaml::read_yaml(ter_info_yml)
  } else {
    ter_info <- list(
      territory = ter)
  }
  ter_info
}

make_ter_type_element <- function(ter, ter_s, ter_s_tbl, lyr_params, type, element){
  # ter="Gulf of Mexico"; type="wind"; element="count"

  msg(g("  make_ter_type_element({ter}, ..., {type}, {element})"))

  ter_info <- get_ter_info(ter)
  lyrs_not_count <- c(names(nrel_limits), "areakm2")

  # skip if bin not in territory
  if (type != "all"){
    # TODO: make_ter_type_element(Alaska, ..., wave, limits)  - 2018-01-04 16:32:25
    # Quitting from lines 19-102 (nrel-uses.Rmd)
    # Error in if (is.na(ter_info$layers[[type]])) { :
    #     argument is of length zero
    #   Calls: <Anonymous> ... eval -> eval -> make_ter_info -> make_ter_type_element
    if (is.na(ter_info$layers[[type]])){
      ter_info[[type]] <- NA
      set_ter_info(ter_info)
      msg(g("    skipping b/c {type} not in {ter}"))
      return(ter_info)
    }
  }

  # get raster of count or bin limits
  if (element == "count"){
    if (type == "all"){
      msg("    calc all count")
      ter_s_cnt <- raster::dropLayer(ter_s, which(names(ter_s) %in% lyrs_not_count))
      # TODO: later, sum values after setting to 0 (but slow) vs assuming value of 1 if not NA
      #       ter_s_cnt[is.na(ter_s_cnt)] <- 0
      r <- sum(!is.na(ter_s_cnt), na.rm=T) %>%
        raster::mask(raster::raster(ter_s, "depth"))
    } else {
      # read raster from all for speedup. presumes make_ter_type_element() run for type='all' before others
      r <- raster::raster(ter_info[["all"]][[element]]$epsg4326_tif)
    }
  } else {
    # bin limits
    r <- raster::raster(ter_s, type)
  }

  # filter to nrel_limits
  msg("    filter to nrel_limits")
  if (type != "all"){
    r_depth <- raster::raster(ter_s, "depth")
    # TODO: rerun with new is.na(r_bin) condition
    if (type != "depth"){
      r_bin <- raster::raster(ter_s, type)
      r_na <- r_depth < nrel_limits[[type]]$depth$min |
        r_depth > nrel_limits[[type]]$depth$max |
        is.na(r_bin) |
        r_bin < min(nrel_limits[[type]]$breaks)
      r[r_na] <- NA
    } else {
      r[r_depth < min(nrel_limits$depth$breaks) |
          r_depth > max(nrel_limits$depth$breaks)] <- NA
    }
  }

  # assign breaks to bin limit rasters
  msg("    assign breaks to bin limit rasters")
  if (element == "limits"){
    r <- raster::cut(r, breaks = nrel_limits[[type]]$breaks, include.lowest = T)
  }

  pfx  <- glue::glue("{dir_data}/territories/{ter}_{type}_{element}")

  # if all NA, set to NA, eg make_ter_type_element(Atlantic Islands, ..., tide, count)
  if (is.na(raster::minValue(r)) & is.na(raster::maxValue(r))){
    ter_info[[type]][[element]] = NA
    set_ter_info(ter_info)
    return(NULL)
  }

  msg("    trimming raster")
  r <- raster_trim(r)

  # register raster
  msg("    register wrapped geographic raster")
  tif_epsg4326 <- glue::glue("{pfx}_epsg4326.tif")
  ter_info[[type]][[element]] <- list(
    val_range       = c(raster::minValue(r), raster::maxValue(r)),
    epsg4326_tif    = tif_epsg4326,
    epsg4326_extent = raster::extent(r) %>% as.vector())
  # write wrapped geographic raster
  raster::writeRaster(r, tif_epsg4326, overwrite=T)

  # handle mercator raster for leaflet
  msg("    handle mercator raster for leaflet")
  r_u <- raster_unwrap(r)
  if (raster::extent(r_u)@xmin < -180){
    # split into rasters around dateline and project for leaflet
    tifs_epsg3857 <- glue::glue("{pfx}_epsg3857_{c('left','right')}.tif")
    r_l <- raster::crop(r_u, raster::extent(-360,-180,-90,90), snap="in") %>% raster::shift(360) %>% raster_trim()
    r_r <- raster::crop(r_u, raster::extent(-180, 180,-90,90), snap="in") %>% raster_trim()
    r_l_epsg3857 <- raster_project_leaflet_nearest(r_l)
    r_r_epsg3857 <- raster_project_leaflet_nearest(r_r)

    # write rasters
    raster::writeRaster(r_l_epsg3857, tifs_epsg3857[1], overwrite=T)
    raster::writeRaster(r_r_epsg3857, tifs_epsg3857[2], overwrite=T)

    # extent based on biggest longitudinal range for initial map display
    e_l <- raster::extent(r_l)
    e_r <- raster::extent(r_r)
    x_l  <- e_l %>% as.vector() %>% .[1:2] %>% diff()
    x_r  <- e_r %>% as.vector() %>% .[1:2] %>% diff()
    b_epsg3857 <- c(e_l, e_r)[[which.max(c(x_l,x_r))]] %>% as.vector()
  } else {
    # project for leaflet
    tifs_epsg3857 <- glue::glue("{pfx}_epsg3857.tif")
    r_epsg3857    <- raster_project_leaflet_nearest(r_u)

    # write raster
    raster::writeRaster(r_epsg3857, tifs_epsg3857, overwrite=T)

    # extent
    b_epsg3857 <- raster::extent(r_u) %>% as.vector()
  }
  ter_info[[type]][[element]]$epsg3857_tif         <- tifs_epsg3857
  ter_info[[type]][[element]]$epsg3857_extent_init <- b_epsg3857
  set_ter_info(ter_info)

  # update map figures
  msg("    update map figures")
  map_ter(ter, type, element, redo_figs=T, return_null=F)

  # table summaries
  if (type != "all" & element == "count"){
    msg("    table summaries")
    bin <- type
    ter_bin_lyr_csv <- glue("{pfx}_layers.csv")
    ter_bin_cnt_csv <- glue("{pfx}_cumulative.csv")
    ter_info[[type]][[element]]$layers_csv     <- ter_bin_lyr_csv
    ter_info[[type]][[element]]$cumulative_csv <- ter_bin_cnt_csv

    tbl_b   <- ter_bin(ter_s_tbl, bin)
    tbl_bl  <- ter_bin_lyr(tbl_b)
    # devtools::load_all("../nrelutils")
    tbl_bls <- ter_bin_lyr_smry(tbl_bl)
    write_csv(tbl_bls, ter_bin_lyr_csv)

    tbl_bc  <- ter_bin_cnt(tbl_b)
    write_csv(tbl_bc, ter_bin_cnt_csv)
  }

  set_ter_info(ter_info)
}

make_ter_info <- function(ter, lyr_params, bins){

  ter_info       <- get_ter_info(ter)
  lyrs_not_count <- c(names(nrel_limits), "areakm2")
  types          <- c("all", bins)

  # # quick fixes
  # if ("raster" %in% names(ter_info)){
  #   ter_info$cnt <- ter_info$raster
  #   ter_info     <- ter_info[-which(names(ter_info)=="raster")]
  #   names(ter_info$cnt)[which(names(ter_info$cnt)=="layers")] <- "layer_components"
  #   ter_info$cnt$layers_considered <- lyr_params$key[lyr_params$key != "aquaculture"]
  #   ter_info$cnt$layers_contained  <- sort(unique(str_split(ter_info$cnt$layer_components, "_", simplify=T)[,1])) %>%
  #     setdiff("area")
  # }
  # if ("figure" %in% names(ter_info)){
  #   ter_info$cnt <- c(ter_info$cnt, ter_info$figure)
  #   ter_info     <- ter_info[-which(names(ter_info)=="figure")]
  # }
  # yaml::write_yaml(ter_info, ter_info_yml)

  # fetch and update territory if missing layers or bins
  missing_layers <- setdiff(
    lyr_params %>% filter(run==T) %>% .$key,
    setdiff(names(ter_info$layers), lyr_params %>% filter(redo==T)))
  # check that missing layers are available in
  #browser()
  for (lyr in missing_layers){ # lyr <- missing_layers[1]
    lyr_in_ter <- !is.na(get_lyr_info(lyr)$territories[[ter]])
    if (!lyr_in_ter){
      msg(g("  missing or redo layer {lyr}, but not in {ter}, so ter_info$layers${lyr} = NA"))
      ter_info$layers[[lyr]] <- NA
      set_ter_info(ter_info)
      ter_info <- get_ter_info(ter)
      missing_layers <- setdiff(missing_layers, lyr)
    }
  }
  missing_types <- setdiff(types, names(ter_info))
  # TEMP: hard coded for new nrel bins
  if (get_fmt()=="html_document") missing_types <- "wind"

  # temp add map figures
  # msg("    TEMP update map figures")
  # map_ter(ter, "all", "count", redo_figs=T, return_null=T)
  # for (bin in bins){
  #   map_ter(ter, bin, "count" , redo_figs=T, return_null=T)
  #   map_ter(ter, bin, "limits", redo_figs=T, return_null=T)
  # }

  if (length(missing_layers) > 0 | length(missing_types) > 0){
    msg(g("  missing_layers: {paste(missing_layers, collapse=', ')}"))
    msg(g("  missing_types : {paste(missing_types, collapse=', ')}"))

    msg("  loading ter_s, ter_s_tbl")
    ter_s     <- ter_stack(ter, lyr_params)
    ter_s_tbl <- ter_stack_tbl(ter_s)

    # initiate layers
    #if (!"layers" %in% names(ter_info)){
    ter_info$layers           <- attr(ter_s, "lyrs_in_ter")
    ter_info$layers_not_count <- lyrs_not_count
    set_ter_info(ter_info)
    #}

    if (length(missing_types) > 0 & length(missing_layers) == 0){
      # if only missing type, redo needed type
      for (type in missing_types){ # type = "wave"
        make_ter_type_element(ter, ter_s, ter_s_tbl, lyr_params, type, "count")
        if (type %in% bins){
          make_ter_type_element(ter, ter_s, ter_s_tbl, lyr_params, type, "limits")
        }
      }
    } else {
      # otherwise if missing layer, redo all types
      make_ter_type_element(ter, ter_s, ter_s_tbl, lyr_params, "all", "count")
      for (bin in bins){
        make_ter_type_element(ter, ter_s, ter_s_tbl, lyr_params, bin, "count")
      }
    }
    # update layers into ter_info
    ter_info <- get_ter_info(ter)
    ter_info$layers           <- attr(ter_s, "lyrs_in_ter")
    ter_info$layers_not_count <- lyrs_not_count
    set_ter_info(ter_info)
  }

  get_ter_info(ter)
}

fix_prep_lyr_ter <- function(){
  # quick fix in prep_layers.R: from digest ({ter}_{lyr}_epsg4326.txt) to yaml (_{ter}.yml)
  lyr_ter_txt <- glue("{dir_lyrs}/{lyr}/{ter}_{lyr}_epsg4326.txt")
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

dt_lyrs_ter <- function(lyr_params){
  fmt <- get_fmt()

  lyrs_ter_tbl <- map_df(
    1:nrow(lyr_params),
    function(i){
      lyr_p        <- lyr_params[i,]
      lyr          <- lyr_p$key
      lyrs_notna   <- !is.na(get_lyr_info(lyr)$territories)
      lyrs_checked <- ifelse(lyrs_notna, "✔", "")

      tibble(
        Source = lyr_p$source,
        lyr     = lyr,
        Dataset = lyr_p$title) %>%
        bind_cols(
          lyrs_checked %>%
            as.list() %>% as.tibble())
    }) %>%
    arrange(Source, Dataset) %>%
    select(-lyr)

  datatable(
    lyrs_ter_tbl,
    caption = "Existence of ocean use datasets across regions.",
    options = list(
      pageLength = nrow(lyrs_ter_tbl),
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
        list(className = 'dt-center', targets = 2:(ncol(lyrs_ter_tbl))))))
}
