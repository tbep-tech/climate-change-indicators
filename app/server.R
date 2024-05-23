function(input, output, session) {

  # dark_mode ----
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light ))

  # map_prism_temp ----
  output$map_prism_temp <- renderLeaflet({

    # DEBUG
    # input <- list(
    #   sld_md       = as.Date("2024-07-22"),
    #   sld_yrs_now  = c(2024, 2024),
    #   sld_yrs_then = c(1981, 2001))

    var        = "tmax"
    var_lbl    = "Temperature (°C)"
    md         = sprintf("%02d-%02d", month(input$sld_t_md), day(input$sld_t_md))
    yrs_now    = input$sld_t_yrs_now[1]:input$sld_t_yrs_now[2]
    yrs_then   = input$sld_t_yrs_then[1]:input$sld_t_yrs_then[2]
    dates_now  = as.Date(glue("{yrs_now}-{md}"))
    dates_then = as.Date(glue("{yrs_then}-{md}"))

    if (any(dates_now > now_prism))
      dates_now[dates_now > now_prism] <- dates_now[dates_now > now_prism] - years(1)

    yrs_now_rng  <- year(dates_now)
    yrs_then_rng <- year(dates_then)
    if (length(yrs_now_rng) > 2)
      yrs_now_rng <- range(yrs_now_rng)
    if (length(yrs_then_rng) > 2)
      yrs_then_rng <- range(yrs_then_rng)

    r_now  <- get_prism_r(dates_now,  var)
    r_then <- get_prism_r(dates_then, var)

    lgnd_now <- glue(
      "<b>Now</b><br>
          {format(input$sld_t_md, '%b %d')},
          {paste(yrs_now_rng, collapse = ' to ')}")
    lgnd_then <- glue(
      "<b>Then</b><br>
           {format(input$sld_t_md, '%b %d')},
           {paste(yrs_then_rng, collapse = ' to ')}")

    map_then_now(
      r_then,
      r_now,
      tiles = ifelse(
        isTRUE(input$dark_mode),
        providers$CartoDB.DarkMatter,
        providers$CartoDB.Positron),
      lgnd_then,
      lgnd_now,
      var_lbl)

  })

  # map_prism_rain ----
  output$map_prism_ppt <- renderLeaflet({

    # DEBUG
    # input <- list(
    #   sld_md       = as.Date("2024-07-22"),
    #   sld_yrs_now  = c(2024, 2024),
    #   sld_yrs_then = c(1981, 2001))

    var        = "ppt"
    var_lbl    = "Rain (mm)"
    md         = sprintf("%02d-%02d", month(input$sld_p_md), day(input$sld_p_md))
    yrs_now    = input$sld_p_yrs_now[1]:input$sld_p_yrs_now[2]
    yrs_then   = input$sld_p_yrs_then[1]:input$sld_p_yrs_then[2]
    dates_now  = as.Date(glue("{yrs_now}-{md}"))
    dates_then = as.Date(glue("{yrs_then}-{md}"))

    if (any(dates_now > now_prism))
      dates_now[dates_now > now_prism] <- dates_now[dates_now > now_prism] - years(1)

    yrs_now_rng  <- year(dates_now)
    yrs_then_rng <- year(dates_then)
    if (length(yrs_now_rng) > 2)
      yrs_now_rng <- range(yrs_now_rng)
    if (length(yrs_then_rng) > 2)
      yrs_then_rng <- range(yrs_then_rng)

    r_now  <- get_prism_r(dates_now,  var)
    r_then <- get_prism_r(dates_then, var)

    lgnd_now <- glue(
      "<b>Now</b><br>
          {format(input$sld_p_md, '%b %d')},
          {paste(yrs_now_rng, collapse = ' to ')}")
    lgnd_then <- glue(
      "<b>Then</b><br>
           {format(input$sld_p_md, '%b %d')},
           {paste(yrs_then_rng, collapse = ' to ')}")

    map_then_now(
      r_then,
      r_now,
      tiles = ifelse(
        isTRUE(input$dark_mode),
        providers$CartoDB.DarkMatter,
        providers$CartoDB.Positron),
      lgnd_then,
      lgnd_now,
      var_lbl,
      palette     = "Blues",
      palette_rev = F)

  })

  # map_sst ----
  output$map_sst <- renderLeaflet({

    # DEBUG
    # input <- list(
    #   sld_o_md       = as.Date("2024-07-22"),
    #   sld_o_yrs_now  = c(2024, 2024),
    #   sld_o_yrs_then = c(1981, 2001))

    var_lbl    = "Sea Surface Temperature (°C)"
    md         = sprintf("%02d-%02d", month(input$sld_o_md), day(input$sld_o_md))
    yrs_now    = input$sld_o_yrs_now[1]:input$sld_o_yrs_now[2]
    yrs_then   = input$sld_o_yrs_then[1]:input$sld_o_yrs_then[2]
    dates_now  = as.Date(glue("{yrs_now}-{md}"))
    dates_then = as.Date(glue("{yrs_then}-{md}"))

    if (any(dates_now > now_sst))
      dates_now[dates_now > now_sst] <- dates_now[dates_now > now_sst] - years(1)

    yrs_now_rng  <- year(dates_now)
    yrs_then_rng <- year(dates_then)
    if (length(yrs_now_rng) > 2)
      yrs_now_rng <- range(yrs_now_rng)
    if (length(yrs_then_rng) > 2)
      yrs_then_rng <- range(yrs_then_rng)

    r_now  <- get_sst_r(dates_now)
    r_then <- get_sst_r(dates_then)

    lgnd_now <- glue(
      "<b>Now</b><br>
          {format(input$sld_o_md, '%b %d')},
          {paste(yrs_now_rng, collapse = ' to ')}")
    lgnd_then <- glue(
      "<b>Then</b><br>
           {format(input$sld_o_md, '%b %d')},
           {paste(yrs_then_rng, collapse = ' to ')}")

    map_then_now(
      r_then,
      r_now,
      tiles = ifelse(
        isTRUE(input$dark_mode),
        providers$CartoDB.DarkMatter,
        providers$CartoDB.Positron),
      lgnd_then,
      lgnd_now,
      var_lbl)

  })

  # plot_sst ----
  output$plot_sst <- plotly::renderPlotly({

    bay_segment <- "BCB"

    d <- d_sst_z |>
      filter(bay_segment == !!bay_segment) |>
      mutate(
        year  = year(time),
        date  = sprintf(
          "%d-%02d-%02d",
          year(today()), month(time), day(time) ) |>
          as.POSIXct(),
        color = case_when(
          year == year(today())     ~ "red",
          year == year(today()) - 1 ~ "orange",
          .default = "gray") ) |>
      select(time, year, date, color, val) |>
      arrange(year, date, val)

    yrs    <- as.character(unique(d$year))
    colors <- setNames(rep("darkgray", length(yrs)), yrs)
    colors[as.character(year(today()))]     <- "red"
    colors[as.character(year(today()) - 1)] <- "orange"

    d <- d |>
      group_by(year) |>
      mutate(
        val_sl = slider::slide_mean(
          val, before = 3L, after = 3L, step = 1L,
          complete = F, na_rm = T),
        txt_date = as.Date(time),
        txt_val  = round(val_sl, 2) ) |>
      select(-time) |>
      ungroup()

    # TODO: darkly theme w/ bslib
    g <- ggplot(
      d,
      aes(
        x     = date,
        y     = val_sl,
        group = year,
        color = factor(year),
        date  = txt_date,
        value = txt_val)) +  # frame = yday
      geom_line(
        # aes(text  = text),
        alpha = 0.6) +
      scale_colour_manual(
        values = colors) +
      theme(legend.position = "none") +
      scale_x_datetime(
        labels = date_format("%b %d")) +
      labs(
        x = "Day of year",
        y = "Temperature ºC")
    g
    # x, y, alpha, color, group, linetype, size

    # add color theming
    # https://rstudio.github.io/thematic/articles/auto.html

    ggplotly(g, tooltip=c("date","value"))
  })

}
