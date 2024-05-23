page_navbar(
  title = "Tampa Bay Climate Change Indicators",
  theme = dark,

  nav_panel(
    title = tagList(
      "Air Temperature", bs_icon("thermometer-half")),

    card(
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
            "sld_t_md",
            "Month and day of year",
            min        = as.Date(glue("{year(now_prism)}-01-01")),
            value      = now_prism,
            max        = as.Date(glue("{year(now_prism)}-12-31")),
            timeFormat = "%b %d",
            animate    = T),
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

      leafletOutput("map_prism_temp") ) ),

  nav_panel(
    title = tagList(
      "Rain", bs_icon("cloud-rain-fill")),

    card(
      card_header(
        "Precipitation - Map of Then vs Now",
        popover(
          title = "Settings",
          bs_icon("gear", class = "ms-auto"),
          sliderInput(
            "sld_p_md",
            "Month and day of year",
            min        = as.Date(glue("{year(now_prism)}-01-01")),
            value      = now_prism,
            max        = as.Date(glue("{year(now_prism)}-12-31")),
            timeFormat = "%b %d",
            animate    = T),
          sliderInput(
            "sld_p_yrs_then",
            "Then year(s)",
            min     = yrs_prism[1],
            value   = c(yrs_prism[1], yrs_prism[1]+20),
            max     = yrs_prism[2] - 1,
            step    = 1,
            animate = T,
            sep     = ""),
          sliderInput(
            "sld_p_yrs_now",
            "Now year(s)",
            min     = yrs_prism[1] + 1,
            value   = c(yrs_prism[2], yrs_prism[2]),
            max     = yrs_prism[2],
            step    = 1,
            animate = T,
            sep     = "")) ),

    leafletOutput("map_prism_ppt") ) ),

  nav_panel(
    title = tagList(
      "Sea Level", bs_icon("water")) ),

  nav_panel(
    title = tagList(
      "Ocean Temperature", bs_icon("thermometer-low")),

    navset_card_underline(
      nav_panel(
        "Map",

        card(
          card_header(
            "Sea Surface Temperate - Map of Then vs Now",
            popover(
              title = "Settings",
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

      nav_panel(
        "Plot",

        card(
          card_header(
            "Sea Surface Temperate - Time Series",
            # popover(
            #   title = "Settings",
            #   bs_icon("gear", class = "ms-auto"),
            #   sliderInput(
            #     "sld_o_md",
            #     "Month and day of year",
            #     min        = as.Date(glue("{year(now_prism)}-01-01")),
            #     value      = now_prism,
            #     max        = as.Date(glue("{year(now_prism)}-12-31")),
            #     timeFormat = "%b %d",
            #     animate    = T),
            #   sliderInput(
            #     "sld_o_yrs_then",
            #     "Then year(s)",
            #     min     = yrs_prism[1],
            #     value   = c(yrs_prism[1], yrs_prism[1]+20),
            #     max     = yrs_prism[2] - 1,
            #     step    = 1,
            #     animate = T,
            #     sep     = ""),
            #   sliderInput(
            #     "sld_o_yrs_now",
            #     "Now year(s)",
            #     min     = yrs_prism[1] + 1,
            #     value   = c(yrs_prism[2], yrs_prism[2]),
            #     max     = yrs_prism[2],
            #     step    = 1,
            #     animate = T,
            #     sep     = ""))
            ),

          plotlyOutput("plot_sst") ) )
    )
  ),

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

  nav_item(
    input_switch("dark_mode", bs_icon("moon-stars-fill"), TRUE) )
)
