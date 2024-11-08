thematic_shiny()
page_navbar(
  title = "Tampa Bay Climate Change Indicators",
  theme = dark,

  # Overview [v] ----
  nav_panel(
    title = tagList(
      "Overview", bs_icon("compass-fill")),

    sliderInput(
      "sld_date_split",
      "Date to split comparison",
      min        = as.Date("1980-01-01"),
      value      = Sys.Date() - years(1),
      max        = Sys.Date(),
      timeFormat = "%F",
      step       = 1,
      animate    = T,
      width      = "100%"),

    layout_column_wrap(
      width = "400px",

      # * Air Temperature ----
      vb(
        title    = span("Air Temperature", bs_icon("thermometer-half")),
        value    = uiOutput("value_temp"), p(uiOutput("caption_temp")),
        showcase = plotlyOutput("hist_temp")),

      # * Rain ----
      vb(
        title    = span("Rain", bs_icon("cloud-rain-fill")),
        value    = uiOutput("value_rain"), p(uiOutput("caption_rain")),
        showcase = plotlyOutput("hist_rain")),

      # * Ocean Temperature ----
      vb(
        title    = span("Ocean Temperature", bs_icon("thermometer-low")),
        value    = uiOutput("value_sst"), p(uiOutput("caption_sst")),
        showcase = plotlyOutput("hist_sst")),

      # * Hurricanes ----
      vb(
        title    = span("Hurricanes", bs_icon("tornado")),
        value    = uiOutput("value_hurricanes"), p(uiOutput("caption_hurricanes")),
        showcase = plotlyOutput("bar_hurricanes"))
        # showcase = bpexploderOutput("bar_hurricanes", height = "auto"))

    )),

  # Air Temperature [t] ----
  nav_panel(
    title = tagList(
      "Air Temperature", bs_icon("thermometer-half")),
    navset_card_underline(

    # * Map ----
    nav_panel(
      tagList(
        "Map ", bs_icon("map")),
      card(
        full_screen = T,
        card_header(
          class = "d-flex",       # r-align gear icon
          span(
            "Air Temperature - Map of Then vs Now",
            class = "me-auto"),   # r-align gear icon

          popover(
            title = "Settings",
            bs_icon("gear"),

            selectInput(
              "sel_t_var",
              "Variable",
              c("Max"     = "tmax",
                "Average" = "tmean",
                "Min"     = "tmin"),
              selected = "tmax"),

            sliderInput(
              "sld_t_yrs_then",
              "Year(s), Then",
              min     = yrs_prism[1],
              value   = c(yrs_prism[1], yrs_prism[1]+20),
              max     = yrs_prism[2] - 1,
              step    = 1,
              animate = T,
              sep     = ""),

            sliderInput(
              "sld_t_yrs_now",
              "Year(s), Now",
              min     = yrs_prism[1] + 1,
              value   = c(yrs_prism[2], yrs_prism[2]),
              max     = yrs_prism[2],
              step    = 1,
              animate = T,
              sep     = "")) ),

        leafletOutput("map_temp"),

        absolutePanel(
          id        = "pnl_t_md",
          bottom    = "5%", left = "5%", right = "5%",
          width     = "90%",

          sliderInput(
            "sld_t_md",
            "Month and day of year",
            min        = as.Date(glue("{year(now_prism)}-01-01")),
            value      = now_prism, # TODO: c(now_prism, now_prism)
            max        = as.Date(glue("{year(now_prism)}-12-31")),
            timeFormat = "%b %d",
            animate    = T,
            width     = "100%") ) ) ),

    # * Plot ----
    nav_panel(
      tagList(
        "Plot ", bs_icon("graph-up-arrow")),
      card(
        full_screen = T,
        card_header(
          class = "d-flex",       # r-align gear icon
          span(
            "Air Temperature - Day of year for all years",
            class = "me-auto"),   # r-align gear icon

          popover(
            title = "Settings",
            bs_icon("gear"),

            sliderInput(
              "sld_t_days_smooth",
              "# of days for smoothing average",
              min        = 0,
              value      = 7,
              max        = 90,
              animate    = T) ) ),

        selectInput(
          "sld_t_seg",
          "Bay Segment",
          prism_zones,
          selected = "TB"),

        plotlyOutput("plot_temp") ) ) ) ),

  # Rain [r] ----
  nav_panel(
    title = tagList(
      "Rain", bs_icon("cloud-rain-fill")),
    navset_card_underline(

      # * Map ----
      nav_panel(
        tagList(
          "Map ", bs_icon("map")),

        card(
          full_screen = T,
          card_header(
            class = "d-flex",       # r-align gear icon
            span(
              "Precipitation - Map of Then vs Now",
              class = "me-auto"),   # r-align gear icon

            popover(
              title = "Settings",
              bs_icon("gear"),

              sliderInput(
                "sld_r_md",
                "Month and day of year",
                min        = as.Date(glue("{year(now_prism)}-01-01")),
                value      = now_prism,
                max        = as.Date(glue("{year(now_prism)}-12-31")),
                timeFormat = "%b %d",
                animate    = T),

              sliderInput(
                "sld_r_yrs_then",
                "Year(s), Then",
                min     = yrs_prism[1],
                value   = c(yrs_prism[1], yrs_prism[1]+20),
                max     = yrs_prism[2] - 1,
                step    = 1,
                animate = T,
                sep     = ""),

              sliderInput(
                "sld_r_yrs_now",
                "Year(s), Now",
                min     = yrs_prism[1] + 1,
                value   = c(yrs_prism[2], yrs_prism[2]),
                max     = yrs_prism[2],
                step    = 1,
                animate = T,
                sep     = "")) ),

          leafletOutput("map_rain") ) ),

      # * Plot ----
      nav_panel(
        tagList(
          "Plot ", bs_icon("graph-up-arrow")),
        card(
          full_screen = T,
          card_header(
            class = "d-flex",       # r-align gear icon
            span(
              "Rain, year to date - Day of year for all years",
              class = "me-auto"),   # r-align gear icon

            popover(
              title = "Settings",
              bs_icon("gear"),

              sliderInput(
                "sld_r_days_smooth",
                "# of days for smoothing average",
                min        = 0,
                value      = 7,
                max        = 90,
                animate    = T) ) ),

          selectInput(
            "sld_r_seg",
            "Bay Segment",
            prism_zones,
            selected = "TB"),

          plotlyOutput("plot_rain") ) ) ) ),

  # Sea Level [l] ----
  nav_panel(
    title = tagList(
      "Sea Level", bs_icon("water")),

      # * map ----
        card(
          full_screen = T,
          card_header(
            tagList(
              "Map of sea level stations ", bs_icon("map") ) ),
          # TODO: helpText("Click on a different station to see the data."),
          leafletOutput("map_sl") ),

      # * plot ----
        card(
          full_screen = T,
          card_header(
            tagList(
              "Plot of sea levels ", bs_icon("graph-up-arrow") ) ),
          selectInput(
            "sel_l_stn",
            "Sea level station",
            sl_stations),
          plotlyOutput("plot_sl") ) ),

  # Ocean Temperature [o] ----
  nav_panel(
    title = tagList(
      "Ocean Temperature", bs_icon("thermometer-low")),
    navset_card_underline(

      # * Map ----
      nav_panel(
        tagList(
          "Map ", bs_icon("map")),
        card(
          full_screen = T,
          card_header(
            class = "d-flex",       # r-align gear icon
            span(
              "Sea Surface Temperate - Map of Then vs Now",
              class = "me-auto"),   # r-align gear icon

            popover(
              title = "Settings",
              placement = "right",
              bs_icon("gear"),

              sliderInput(
                "sld_o_md",
                "Month and day of year",
                min        = as.Date(glue("{year(now_prism)}-01-01")),
                value      = now_prism,
                max        = as.Date(glue("{year(now_prism)}-12-31")),
                timeFormat = "%b %d",
                animate    = T),

              sliderInput(
                "sld_o_yrs_then",
                "Year(s), Then",
                min     = yrs_prism[1],
                value   = c(yrs_prism[1], yrs_prism[1]+20),
                max     = yrs_prism[2] - 1,
                step    = 1,
                animate = T,
                sep     = ""),

              sliderInput(
                "sld_o_yrs_now",
                "Year(s), Now",
                min     = yrs_prism[1] + 1,
                value   = c(yrs_prism[2], yrs_prism[2]),
                max     = yrs_prism[2],
                step    = 1,
                animate = T,
                sep     = "")) ),

          leafletOutput("map_sst") ) ),

      # * Plot ----
      nav_panel(
        tagList(
          "Plot ", bs_icon("graph-up-arrow")),
        card(
          full_screen = T,
          card_header(
            class = "d-flex",       # r-align gear icon
            span(
              "Sea Surface Temperature - Day of year for all years",
              class = "me-auto"),   # r-align gear icon

            popover(
              title = "Settings",
              bs_icon("gear"),

              sliderInput(
                "sld_o_days_smooth",
                "# of days for smoothing average",
                min        = 0,
                value      = 7,
                max        = 90,
                animate    = T) ) ),

          selectInput(
            "sld_o_seg",
            "Bay Segment",
            sst_zones),

          plotlyOutput("plot_sst") ) )
    )
  ),

  # Hurricanes [h] ----
  nav_panel(
    title = tagList(
      "Hurricanes", bs_icon("tornado")),

    # * map ----
    card(
      full_screen = T,
      card_header(
        tagList(
          "Map of hurricane tracks", bs_icon("map") ) ),
      # TODO: configure option to limit map of hurricane tracks by year
      leafletOutput("map_h"),

      absolutePanel(
        id        = "pnl_h_yrs",
        bottom    = "5%", left = "5%", right = "5%",
        width     = "90%",

        sliderInput(
          "sld_h_yrs",
          "Years",
          min        = h_yrs[1],
          value      = c(h_yrs[2] - 1, h_yrs[2]),
          max        = h_yrs[2],
          sep        = "",
          round      = T,
          step       = 1,
          animate    = T,
          width      = "100%") )

      ),

    # * plot ----
    card(
      full_screen = T,
      card_header(
        class = "d-flex",       # r-align gear icon
        span(
          "Plot of hurricanes over time", bs_icon("graph-up-arrow"),
          class = "me-auto"),   # r-align gear icon
        popover(
          title     = "Settings",
          bs_icon("gear"),

          sliderInput(
            "sld_h_yr_split",
            "Year Split",
            min     = min(h_d_sum$year),
            value   = h_yr_split,
            max     = max(h_d_sum$year),
            step    = 1,
            animate = T,
            sep     = "") ) ),
      plotlyOutput("plot_h") ) ),

  # More ----
  nav_spacer(),
  nav_menu(
    title = "More",
    align = "right",
    nav_item(
      tags$a(
        shiny::icon("info-circle"),
        "About",
        href   = "https://tbep-tech.github.io/climate-change-indicators/",
        target = "_blank") ),
    nav_item(
      tags$a(
        shiny::icon("envelope", class = "fa-solid"),
        "Contact",
        href   = "https://tbep-tech.github.io/climate-change-indicators/",
        target = "_blank") ) ),

  # sw_dark ----
  nav_item(
    input_switch("sw_dark", bs_icon("moon-stars-fill"), TRUE) ),

  # sw_imperial ----
  nav_item(
    input_switch("sw_imperial", "ºF, in", TRUE) ),

)
