get_hurricanes_nc <- function(
    in_nc           = "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/netcdf/IBTrACS.NA.v04r01.nc",
    out_nc          = here::here(glue::glue("data/storms/{basename(in_nc)}"))){
  # only download if in_nc (url) newer than out_nc (destfile)

  resp <- curl::multi_download(
    urls          = in_nc,
    destfile      = out_nc,
    timecondition = 1,
    timevalue     = as.numeric(file.info(out_nc)$mtime))
  if (!resp$success)
    stop(resp$error)

  out_nc
}


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

plot_sl <- function(stn_id, interactive = T) {

  d <- d_sl |>
    filter(
      station_id == !!stn_id,
      !is.na(msl))

  m             <- lm(msl~date, data = d)
  rsq           <- summary(m)$r.squared
  m_per_day     <- m$coefficients[["date"]]
  cm_per_decade <- m_per_day * 100 * 365 * 10

  txt_g <- sprintf(
    "atop(
      Sea~level~rise:~%0.2f~cm/decade,
      (R^2:~%0.2f))", cm_per_decade, rsq)
  txt_p <- sprintf(
    "Sea level rise: %0.2f cm/decade\n
    (R<sup>2</sup>: %0.2f)", cm_per_decade, rsq)

  g <-  d |>
    ggplot(aes(x = date, y = msl)) +
    geom_point() +
    geom_smooth(method = "lm", formula = "y ~ x") + # 95% confidence interval of the predicted mean
    # TODO: settings slider for wiggles (0 = lm)
    # geom_smooth(
    #   method = lm, formula = y ~ splines::bs(x, 5)) +
    labs(
      x = "Date",
      y = "Mean Sea Level (m)")

  if (!interactive)
    return(
      g +
        annotate(
          "text",
          x=min(d$date), y=max(d$msl), hjust=0, vjust=1,
          label = txt_g, parse = T))

  ggplotly(g) |>
    add_annotations(
      x = 0,
      y = 1,
      xref = "paper",
      yref = "paper",
      text = txt_p,
      xanchor = "left",
      showarrow = F)
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
    interactive      = TRUE,
    ylab             = "Temperature (ºC)"){
  # bay_segment = "BCB"
  # df = d_sst_z

  # check args ----
  stopifnot(c("time","val") %in% names(df))

  # days_smooth
  stopifnot(is.numeric(days_smooth) & days_smooth >= 0 & days_smooth <= 365)
  if (days_smooth == 0){
    days_sm_before <- 0
    days_sm_after  <- 0
  } else {
    h <- (days_smooth - 1)/2
    days_sm_before <- ceiling(h) |> as.integer()
    days_sm_after  <-   floor(h) |> as.integer()
  }

  yrs       <- range(year(df$time))
  yr_last   <- yrs[2] - 1
  yrs_other <- glue("{yrs[1]} to {yr_last}")
  yr_cols <- setNames(
    c(color_thisyear, color_lastyear, color_otheryears),
    c(        yrs[2],        yr_last,        yrs_other))
  yr_szs <- setNames(
    c( size_thisyear,  size_lastyear, size_otheryears),
    c(        yrs[2],        yr_last,       yrs_other))

  md_lims <- sprintf(
    "%d-%02d-%02d",
    year(today()), c(1,12), c(1,31) ) |>
    as.POSIXct()

  d <- df |>
    mutate(
      year  = year(time),
      doy   = sprintf(
        "%d-%02d-%02d",
        year(today()), month(time), day(time) ) |>
        as.POSIXct(),
      yr_cat = case_when(
        year == yrs[2]  ~ yrs[2] |> as.character(),
        year == yr_last ~ yr_last |> as.character(),
        .default = yrs_other) |>
        factor()) |>
    select(time, year, doy, yr_cat, val) |>
    arrange(year, doy, val) |>
    group_by(year) |>
    mutate(
      val_sl = slider::slide_mean(
        val,
        before   = days_sm_before,
        after    = days_sm_after,
        step     = 1L,
        complete = F,
        na_rm    = T),
      date  = as.Date(time),
      value = round(val_sl, 2) ) |>
    select(-time) |>
    ungroup()

  g <- ggplot(
    d,
    aes(
      x     = doy,
      y     = val_sl,
      group = year,
      color = yr_cat,
      size  = yr_cat,
      date  = date,
      value = value)) +  # frame = yday
    geom_line(
      # aes(text  = text),
      alpha = 0.6) +
    scale_colour_manual(
      name   = "Year",
      values = yr_cols) +
    scale_size_manual(
      values = yr_szs, guide="none") +
    # theme(legend.position = "") +
    theme(
      legend.position = c(0.5, 0.15)) +
    scale_x_datetime(
      labels = date_format("%b %d"),
      limits = md_lims,
      expand = c(0, 0)) +
    labs(
      x = "Day of year",
      y = ylab)

  if (!interactive)
    return(g)

  ggplotly(g, tooltip=c("date","value")) |>
    layout(legend = list(x = 0.5, y = 0.15))
}

plot_hist <- function(
    df,                                           # requires columns: date, value
    month_day    = format(max(df$date), "%m-%d"), # in "%m-%d" format as in "10-31" for Oct 31
    years_now    = lubridate::year(max(df$date)),
    years_then   = lubridate::year(min(df$date)):(lubridate::year(min(df$date)) + 20),
    color_then   = "white",
    color_now    = "red",
    value_units   = "ºF",
    value_glue   = "{sign_symbol} {round(abs(avg_diff), 2)} {value_units}",
    sign_positive = "warmer",
    sign_negative = "colder",
    caption_glue = "
      The air temperature as of {dates_now} is {round(abs(avg_diff),2)} {value_units} {sign} than the
      previously recorded average during the years {years_then_text}.",
    interactive   = TRUE){

  # DEBUG
  # df           = d_temp
  # month_day    = format(max(df$date), "%m-%d")
  # years_now    = lubridate::year(max(df$date))
  # years_then   = lubridate::year(min(df$date)):(lubridate::year(min(df$date)) + 20)
  # color_then   = "white"
  # color_now    = "red"
  # value_glue   = "{sign_symbol} {round(abs(avg_diff), 2)} {value_units}"
  # caption_glue = "
  #     The air temperature as of {dates_now} is {round(abs(avg_diff),2)} {value_units} {sign} than the
  #     previously recorded average during the years {years_then_text}."
  # sign_positive = "warmer"
  # sign_negative = "colder"
  # value_units   = "ºF"
  # interactive   = TRUE

  # df = d_prism_z |>
  #   filter(
  #     bay_segment           == "TB",
  #     variable              == "tdmean") |>
  #   rename(value = mean)

  stopifnot(c("date","value") %in% names(df))

  d = df |>
    filter(
      format(date, "%m-%d") == month_day) |>
    select(date, value) |>
    mutate(
      yr       = year(date),
      interval = case_when(
        yr %in% years_now  ~ "Now",
        yr %in% years_then ~ "Then")) |>
    filter(
      !is.na(interval))

  # handle units ----
  if (!inherits(d$value, "units")){
    browser() # DEBUG
    stop("d$value must have units::units() set.")
  }
  value_units_0 <- deparse_unit(d$value)
  if (value_units != value_units_0){

    if (value_units == "ºF"){
      # temperature: ºC -> ºF
      d$value <- d$value |>
        set_units("degree_F")
    } else if (value_units == "ºC"){
      # temperature: ºF -> ºC
      d$value <- d$value |>
        set_units("degree_C")
    } else if (value_units == "in"){
      # precipitation: mm -> in
      d$value <- d$value |>
        set_units("inch")
    } else if (value_units == "mm"){
      # precipitation: in -> mm
      d$value <- d$value |>
        set_units("mm")
    } else {
      stop(glue("Unit conversion from {value_units_0} to {value_units} not yet supported."))
    }
  }

  # ggplot ----
  g <- d |>
    drop_units() |>
    ggplot(aes(x = value, fill = interval, color = interval)) +
    scale_fill_manual(
      values = c(
        Then = color_then,
        Now  = color_now)) +
    scale_color_manual(
      values = c(
        Then = color_then,
        Now  = color_now)) +
    geom_density(
      alpha = 0.7, color = NA) +
    stat_summary(
      aes(xintercept = after_stat(x), y = 0),
      fun = mean, geom = "vline", orientation = "y") +
    # theme_minimal() +
    theme(
      axis.text=element_text(size=16),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title                = element_blank(),
      legend.position.inside      = c(0.1, 0.9),
      legend.justification.inside = c(0, 1)) +
    guides(
      fill  = guide_legend(position = "inside")) +
    scale_x_continuous(
      breaks = function(x) pretty(x, min.n = 3, n=5, high.u.bias = 7),
      expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  # interactive ----
  if (!interactive){
    o <- g
  } else {
    g <- g +
      theme(legend.position = "none")
    o <- ggplotly(g)
  }

  # render caption ----
  # attach(caption_list)

  if (length(years_now) == 1){
    dates_now <- format(as.Date(glue("{years_now}-{month_day}")), "%b %d, %Y")
  } else {
    dates_now_1 <- format(as.Date(glue("{years_now[1]}-{month_day}")), "%b %d, %Y")
    dates_now <- glue("{dates_now_1} to {years_now[length(years_now)]}")
  }

  avg_diff <- d |>
    drop_units() |>
    group_by(interval) |>
    summarise(
      avg = mean(value)) |>
    pull(avg) |>
    rev() |>
    diff()
  sign_symbol <- ifelse(avg_diff > 0, "+", "-")

  # browser() # DEBUG
  value <- glue(value_glue)

  sign <- ifelse(avg_diff > 0, sign_positive, sign_negative)
  years_then_text <- glue("{years_then[1]} to {years_then[length(years_then)]}")
  caption <- glue(caption_glue)
  # detach(caption_list)

  attr(o, "value")   <- value
  attr(o, "caption") <- caption
  attr(o, "value_caption") <- tagList(
    value, p(caption))

  o
}
