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

map_sl <- function(
    dark_mode = T){

  tiles = ifelse(
    dark_mode,
    providers$CartoDB.DarkMatter,
    providers$CartoDB.Positron)

  color_tbsegshed = ifelse(
    dark_mode,
    "white",
    "black")

  leaflet() |>
    addProviderTiles(
      tiles,
      group   = "basemap",
      layerId = "basemap") |>
    addPolygons(
      data         = tbsegshed,
      label        = tbsegshed$long_name,
      labelOptions = labelOptions(
        interactive = T),
      color        = color_tbsegshed,
      weight       = 2,
      fillOpacity  = 0) |>
    addMarkers(
      data    = tbeptools::sealevelstations,
      lng     = ~longitude,
      lat     = ~latitude,
      layerId = ~station_id,
      group   = "sl_stations",
      label   = ~glue(
        "<b>{station_name}</b><br>
        sea level station") |>
        lapply(HTML),
      popup   = ~glue(
        "<b>{station_name}</b><br>
        sea level station<br>
        ID: {station_id}<br>
        est.: {date_est}"),
      options = markerOptions(

      ))
}

map_then_now <- function(
    r_then,
    r_now,
    dark_mode = T,
    lgnd_then,
    lgnd_now,
    var_lbl,
    palette     = "Spectral",
    palette_rev = TRUE){

  tiles = ifelse(
    dark_mode,
    providers$CartoDB.DarkMatter,
    providers$CartoDB.Positron)

  color_tbsegshed = ifelse(
    dark_mode,
    "white",
    "black")

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
      data         = tbsegshed,
      label        = tbsegshed$long_name,
      labelOptions = labelOptions(
        interactive = T),
      color        = color_tbsegshed,
      weight       = 2,
      fillOpacity  = 0) |>
    fitBounds(b[1], b[2], b[3], b[4]) |>
    addLegend(
      pal    = pal,
      values = vals,
      title  = var_lbl)
}

plot_doy <- function(
    df, # required columns: time, val
    days_smooth      = 7,
    color_thisyear   = "red",
    color_lastyear   = "orange",
    color_otheryears = "gray",
    size_thisyear    = 1.5,
    size_lastyear    = 1,
    size_otheryears  = 0.5,
    alpha_otheryears = 0.5,
    interactive      = T){

  # DEBUG:
  # df = d_temp; days_smooth = 7
  # color_thisyear = "red"; color_lastyear = "orange"; color_otheryears = "gray"
  # size_thisyear = 1.5; size_lastyear = 1; size_otheryears = 0.5; alpha_otheryears = 0.5
  # interactive = T

  # get years
  yr_this <- max(year(df$time))
  yr_last <- yr_this - 1
  yrs_oth <- setdiff(
    unique(year(df$time)),
    c(yr_this, yr_last))

  # add doy
  d <- df |>
    mutate(
      year = year(time),
      doy  = yday(time),
      grp  = case_when(
        year == yr_this ~ "this",
        year == yr_last ~ "last",
        T               ~ "other"))

  # get mean per doy per year
  d_doy <- d |>
    group_by(year, doy) |>
    summarize(
      val = mean(val),
      grp = first(grp),
      .groups = "drop")

  # smooth per year
  if (days_smooth > 0){
    d_doy <- d_doy |>
      group_by(year) |>
      arrange(doy) |>
      mutate(
        val = zoo::rollmean(
          val,
          k      = days_smooth,
          fill   = NA,
          align  = "center")) |>
      ungroup()
  }

  # get min, max per doy
  d_doy_rng <- d_doy |>
    filter(grp == "other") |>
    group_by(doy) |>
    summarize(
      min = min(val, na.rm=T),
      max = max(val, na.rm=T),
      .groups = "drop")

  # get mean per doy for other years
  d_doy_oth <- d_doy |>
    filter(grp == "other") |>
    group_by(doy) |>
    summarize(
      val = mean(val, na.rm=T),
      .groups = "drop")

  # get data for this and last year
  d_doy_tl <- d_doy |>
    filter(grp != "other")

  # plot
  g <- ggplot() +
    geom_ribbon(
      data = d_doy_rng,
      aes(x = doy, ymin = min, ymax = max),
      alpha = alpha_otheryears,
      fill  = color_otheryears) +
    geom_line(
      data = d_doy_oth,
      aes(x = doy, y = val),
      color = color_otheryears,
      size  = size_otheryears) +
    geom_line(
      data = d_doy_tl |> filter(grp == "last"),
      aes(x = doy, y = val),
      color = color_lastyear,
      size  = size_lastyear) +
    geom_line(
      data = d_doy_tl |> filter(grp == "this"),
      aes(x = doy, y = val),
      color = color_thisyear,
      size  = size_thisyear) +
    labs(
      x = "Day of Year",
      y = "Value") +
    scale_x_continuous(
      breaks = seq(1, 365, 30),
      labels = function(x) format(as.Date(x, origin = "2000-01-01"), "%b %d"))

  if (!interactive)
    return(g)

  ggplotly(g)
}


