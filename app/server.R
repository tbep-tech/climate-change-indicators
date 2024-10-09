function(input, output, session) {

  # sw_dark ----
  observe(session$setCurrentTheme(
    if (isTRUE(input$sw_dark)) dark else light ))

  # sw_imperial ----
  observeEvent(input$sw_imperial, {
    update_switch(
      "sw_imperial",
      label = ifelse(
        input$sw_imperial,
        "ºF, in",
        "ºC, mm"),
      value = input$sw_imperial)
  })

  # Overview ----

  # * Air Temperature ----

  # ·· rx_temp ----
  rx_temp <- reactive({
    suppressWarnings({
      plot_hist(
        d_temp,
        value_units = ifelse(
          input$sw_imperial,
          "ºF",
          "ºC"))
    })
  })

  # ·· value_temp ----
  output$value_temp <- renderUI({
    # browser() # DEBUG
    rx_temp() |>
      attr("value")
  })

  # ·· caption_temp ----
  output$caption_temp <- renderUI({
    rx_temp() |>
      attr("caption")
  })

  # ·· hist_temp ----
  output$hist_temp <- renderPlotly({
    suppressWarnings({
      plot_hist(
        d_temp,
        value_units = ifelse(
          input$sw_imperial,
          "ºF",
          "ºC"))
    })
  })

  # * Rain ----

  # ·· rx_rain [t] ----
  rx_rain <- reactive({
    suppressWarnings({
      plot_hist(
        d_rain,
        value_units   = ifelse(
          input$sw_imperial,
          "in",
          "mm"),
        value_glue    = "{sign_symbol} {round(abs(avg_diff), 1)} {value_units}",
        sign_positive = "wetter",
        sign_negative = "drier",
        caption_glue  = "
      The rainfall this year up to {dates_now} is {round(abs(avg_diff),2)} {value_units} {sign} than the
      previously recorded average year to date rainfall during the years {years_then_text}.")
    })
  })

  # ·· value_rain [t] ----
  output$value_rain <- renderUI({
    rx_rain() |>
      attr("value")
  })

  # ·· caption_rain [t] ----
  output$caption_rain <- renderUI({
    rx_rain() |>
      attr("caption")
  })

  # ·· hist_rain [t] ----
  output$hist_rain <- renderPlotly({
    suppressWarnings({
      plot_hist(
        d_rain,
        value_units   = ifelse(
          input$sw_imperial,
          "in",
          "mm"))
    })
  })

  # * Ocean Temperature ----

  # ·· rx_sst [t] ----
  rx_sst <- reactive({
    suppressWarnings({
      plot_hist(
        d_sst,
        value_units = ifelse(
          input$sw_imperial,
          "ºF",
          "ºC"))
    })
  })

  # ·· value_sst [t] ----
  output$value_sst <- renderUI({
    rx_sst() |>
      attr("value")
  })

  # ·· caption_sst [t] ----
  output$caption_sst <- renderUI({
    rx_sst() |>
      attr("caption")
  })

  # ·· hist_sst [t] ----
  output$hist_sst <- renderPlotly({
    suppressWarnings({
      plot_hist(
        d_sst,
        value_units = ifelse(
          input$sw_imperial,
          "ºF",
          "ºC"))
    })
  })

  # Air Temperature [t] ----

  # * map_temp ----
  output$map_temp <- renderLeaflet({

    # DEBUG
    # input <- list(
    #   sld_md       = as.Date("2024-07-22"),
    #   sld_yrs_now  = c(2024, 2024),
    #   sld_yrs_then = c(1981, 2001))

    var        = "tmax"
    var_lbl    = "Temperature (°C)"
    md         = format(input$sld_t_md, "%m-%d")
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
      dark_mode = isTRUE(input$sw_dark),
      lgnd_then,
      lgnd_now,
      var_lbl)

  })

  # * plot_temp ----
  output$plot_temp <- renderPlotly({

    d_prism_z |>
      filter(
        variable    == input$sel_t_var,
        bay_segment == input$sld_t_seg) |>
      mutate(
        time = as.POSIXct(date)) |>
      select(time, val = mean) |>
      plot_doy(
        days_smooth = input$sld_t_days_smooth)
  })

  # Rain [r] ----

  # * map_rain ----
  output$map_rain <- renderLeaflet({

    var        = "pptytd"
    var_lbl    = "Rain (mm)<br>year to date"

    # DEBUG
    # input = list(
    #   sld_r_md       = as.Date("2024-10-07"),
    #   sld_r_yrs_now  = c(2024, 2024),
    #   sld_r_yrs_then = c(1981, 2001))

    md         = format(input$sld_r_md, "%m-%d")
    yrs_now    = input$sld_r_yrs_now[1]:input$sld_r_yrs_now[2]
    yrs_then   = input$sld_r_yrs_then[1]:input$sld_r_yrs_then[2]
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
          {format(input$sld_r_md, '%b %d')},
          {paste(yrs_now_rng, collapse = ' to ')}")
    lgnd_then <- glue(
      "<b>Then</b><br>
           {format(input$sld_r_md, '%b %d')},
           {paste(yrs_then_rng, collapse = ' to ')}")

    map_then_now(
      r_then,
      r_now,
      dark_mode = isTRUE(input$sw_dark),
      lgnd_then,
      lgnd_now,
      var_lbl,
      palette     = "Blues",
      palette_rev = F)

  })

  # * plot_rain ----
  output$plot_rain <- renderPlotly({

    d_prism_z |>
      filter(
        variable    == "pptytd",
        bay_segment == input$sld_r_seg) |>
      mutate(
        time = as.POSIXct(date)) |>
      select(time, val = mean) |>
      plot_doy(
        days_smooth    = input$sld_r_days_smooth,
        color_thisyear = "purple",
        color_lastyear = "darkblue",
        ylab           = "Rain, year to date (mm)")
  })

  # Sea Level [l] ----

  # * map_sl ----
  output$map_sl <- renderLeaflet({
    map_sl()
  })

  # * plot_sl ----
  output$plot_sl <- renderPlotly({
    # input$map_sl$click
    # TODO: update input$sel_l_stn based on map stn click
    #       update map highlighted marker based on ∆ input$sel_l_stn

    # input <- list(sel_l_stn = 8726724)

    plot_sl(input$sel_l_stn)

  })

  # Ocean Temperature [o] ----

  # * map_sst ----
  output$map_sst <- renderLeaflet({

    # DEBUG
    # input <- list(
    #   sld_o_md       = as.Date("2024-07-22"),
    #   sld_o_yrs_now  = c(2024, 2024),
    #   sld_o_yrs_then = c(1981, 2001))

    var_lbl    = "Sea Surface Temperature (°C)"
    md         = format(input$sld_o_md, "%m-%d")
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
      dark_mode = isTRUE(input$sw_dark),
      lgnd_then,
      lgnd_now,
      var_lbl)
  })

  # * plot_sst ----
  output$plot_sst <- renderPlotly({

    d_sst_z |>
      filter(
        bay_segment == input$sld_o_seg) |>
      plot_doy(
        days_smooth = input$sld_o_days_smooth)
  })

  # Hurricanes [h] ----

  # * map_h ----
  output$map_h <- renderLeaflet({

    h_st |>
      h_filt_yrs(input$sld_h_yrs) |>
      plotStorms(dynamicPlot = T)

  })

  # * plot_h ----
  output$plot_h <- renderPlotly({

    # browser()
    d <- h_d_sum |>
      select(year, scale_sum, label_md) |>
      mutate(
        yr_grp = case_when(
          year > input$sld_h_yr_split  ~ "Now",
          TRUE                         ~ "Then"))

    d_g <- d |>
      group_by(yr_grp) |>
      summarise(
        yr_min = min(year),
        yr_max = max(year),
        avg    = mean(scale_sum))

    p <- d |>
      ggplot(aes(x = year, y = scale_sum, text = label_md)) +
      geom_line(aes(group = 1)) +
      geom_point() +
      scale_x_continuous(
        limits = h_yrs,
        expand = c(0, 0)) +
      geom_segment(
        aes(
          x     = yr_min,
          xend  = yr_max,
          y     = avg,
          yend  = avg,
          text  = NULL,
          group = yr_grp),
        data = d_g,
        linetype = "dashed")

    ggplotly(p, tooltip = list("text"))
  })

}
