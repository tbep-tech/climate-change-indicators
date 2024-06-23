thematic_shiny()
page_navbar(
  title = "Tampa Bay Climate Change Indicators",
  theme = dark,
  
  nav_panel(
    title = tagList(
      "Overview", bs_icon("compass-fill")),
    layout_column_wrap(
      width = "250px",
      !!!vbs ) ),

  # Air Temperature ----
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
          "Air Temperature - Map of Then vs Now",

          popover(
            title = "Settings",
            bs_icon("gear", class = "ms-auto"),

            selectInput(
              "sel_t_var",
              "Variable",
              c("Max"     = "tmax",
                "Average" = "tmean",
                "Min"     = "tmin"),
              selected = "tmax"),

            sliderInput(
              "sld_t_yrs_then",
              "Then year(s)",
              min     = yrs_prism[1],
              value   = c(yrs_prism[1], yrs_prism[1]+20),
              max     = yrs_prism[2] - 1,
              step    = 1,
              animate = T,
              sep     = ""),

            sliderInput(
              "sld_t_yrs_now",
              "Now year(s)",
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
          "Air Temperature - Day of year for all years",

          popover(
            title = "Settings",
            bs_icon("gear", class = "ms-auto"),

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

  # Rain ----
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
            "Precipitation - Map of Then vs Now",

            popover(
              title = "Settings",
              bs_icon("gear", class = "ms-auto"),

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
                "Then year(s)",
                min     = yrs_prism[1],
                value   = c(yrs_prism[1], yrs_prism[1]+20),
                max     = yrs_prism[2] - 1,
                step    = 1,
                animate = T,
                sep     = ""),

              sliderInput(
                "sld_r_yrs_now",
                "Now year(s)",
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
            "Air Temperature - Day of year for all years",

            popover(
              title = "Settings",
              bs_icon("gear", class = "ms-auto"),

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

  # Sea Level ----
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

  # Ocean Temperature ----
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
            "Sea Surface Temperate - Map of Then vs Now",

            popover(
              title = "Settings",
              placement = "right",
              bs_icon("gear", class = "ms-auto"),

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
                "Then year(s)",
                min     = yrs_prism[1],
                value   = c(yrs_prism[1], yrs_prism[1]+20),
                max     = yrs_prism[2] - 1,
                step    = 1,
                animate = T,
                sep     = ""),

              sliderInput(
                "sld_o_yrs_now",
                "Now year(s)",
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
            "Sea Surface Temperature - Day of year for all years",

            popover(
              title = "Settings",
              bs_icon("gear", class = "ms-auto"),

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

  # dark_mode ----
  nav_item(
    input_switch("dark_mode", bs_icon("moon-stars-fill"), TRUE) )
)
