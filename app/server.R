function(input, output, session) {

  # dark_mode ----
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light ))

  # Overview ----

  # * Air Temperature ----

  # ·· rx_temp ----
  rx_temp <- reactive({
    suppressWarnings({
      plot_hist(d_temp)
    })
  })

  # ·· value_temp ----
  output$value_temp <- renderUI({
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
      plot_hist(d_temp)
    })
  })

  # * Rain ----

  # ·· rx_rain [t] ----
  rx_rain <- reactive({

    suppressWarnings({
      plot_hist(
        d_rain,
        caption_list = list(
          value_glue   = "{sign_symbol} {round(abs(avg_diff), 1)} {units}",
          caption_glue = "
      The rainfall as of {dates_now} is {round(abs(avg_diff),2)} {units} {sign_text} than the
      previously recorded average during the years {years_then_text}.",
          positive = "wetter",
          negative = "drier",
          units    = "mm"))
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
      plot_hist(d_rain)
    })
  })

  # * Ocean Temperature ----

  # ·· rx_sst [t] ----
  rx_sst <- reactive({
    suppressWarnings({
      plot_hist(d_sst)
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
      plot_hist(d_sst)
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
      dark_mode = isTRUE(input$dark_mode),
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

    # DEBUG
    # input <- list(
    #   sld_md       = as.Date("2024-07-22"),
    #   sld_yrs_now  = c(2024, 2024),
    #   sld_yrs_then = c(1981, 2001))

    var        = "ppt"
    var_lbl    = "Rain (mm)"
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
      dark_mode = isTRUE(input$dark_mode),
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
        variable    == "ppt",
        bay_segment == input$sld_r_seg) |>
      mutate(
        time = as.POSIXct(date)) |>
      select(time, val = mean) |>
      plot_doy(
        days_smooth    = input$sld_r_days_smooth,
        color_thisyear = "purple",
        color_lastyear = "darkblue",
      )
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
      dark_mode = isTRUE(input$dark_mode),
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

}
