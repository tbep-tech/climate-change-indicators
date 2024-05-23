get_prism_r <- function(dates, var){  # dates = dates_then
  d <- d_prism_r |>
    filter(
      date     %in% !!dates,
      variable  ==  !!var) |>
    select(path_tif, lyr)

  rast(unique(d$path_tif)) |>
    subset(d$lyr) |>
    mean() |>
    project(leaflet:::epsg3857)
}

get_sst_r <- function(dates){  # dates = dates_then
  lyrs <- d_sst_r |>
    filter(
      date %in% !!dates) |>
    pull(lyr)

  r_sst |>
    subset(lyrs) |>
    mean() |>
    project(leaflet:::epsg3857)
}

map_then_now <- function(
    r_then,
    r_now,
    tiles = providers$CartoDB.DarkMatter,
    lgnd_then,
    lgnd_now,
    var_lbl,
    palette     = "Spectral",
    palette_rev = TRUE){

  vals <- c(values(r_now, na.rm=T), values(r_then, na.rm=T))
  pal  <- colorNumeric(
    palette, vals, reverse = palette_rev, na.color = "transparent")

  leaflet() |>
    addMapPane("left",  zIndex = 0) |>
    addMapPane("right", zIndex = 0) |>
    addProviderTiles(
      tiles,
      options = pathOptions(pane = "left"),
      group   = "base",
      layerId = "base_l") |>
    addProviderTiles(
      tiles,
      options = pathOptions(pane = "right"),
      group   = "base",
      layerId = "base_r") |>
    addRasterImage(
      r_then, colors = pal, opacity = 0.8, project = F,
      options = leafletOptions(pane = "left"),
      group = "r_then") |>
    addRasterImage(
      r_now, colors = pal, opacity = 0.8, project = F,
      options = leafletOptions(pane = "right"),
      group = "r_now") |>
    addLayersControl(overlayGroups = c("r_then", "r_now")) |>
    addSidebyside(
      layerId = "sidecontrols",
      leftId  = "base_l",
      rightId = "base_r") |>
    addControl(
      HTML(lgnd_then),
      position = "topleft") |>
    addControl(
      HTML(lgnd_now),
      position = "topright") |>
    addPolygons(
      data = tbeptools::tbsegshed,
      # label = ~long_name,
      color="white", weight = 2, fillOpacity=0) |>
    addLegend(
      pal    = pal,
      values = vals,
      title  = var_lbl)
}
