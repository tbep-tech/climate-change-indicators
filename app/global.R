# TODO:
# - [ ] make sld_{t}_tmp 2-values and process
# - [ ] handle tmean w/ reactive; sel_t_var implement in map_prism_temp w/ avg
# - [ ] reuse time slider processing with a functions.R
# - [ ] convert prism units: ºC -> ºF, mm -> in
# - [ ] prism: show `version`, `date_updated`
# - [ ] show sources for prism, sst, etc with About and links per card
# - [ ] observe and update map_sl <-> plot_sl with click and highlight station in map_sl
# - [ ] move gears to right with Settings label
# - [ ] temp: heat index using dewpoint (`tdmean` -> humidity)
# - [ ] + density histogram below map for selected now & then times
# - [ ] migrate viz functions.R to tbeptools
# - [ ] create static figures for reports (auto parameterize rendered)
# - [ ] swiper maps: leaflet-proxy-map updates to layers so can zoom / pan without refresh
# - [x] add sea level plot with linear fit and map for stations
# - [x] icons for Map | Plot
# - [x] swiper maps: precipitation, ocean temperature
# - [x] style ggplotly https://rstudio.github.io/thematic/#shiny
#          https://rstudio.github.io/thematic/articles/auto.html

# devtools::install_local(here::here("../tbeptools"), force = T)
# devtools::load_all(here::here("../tbeptools"))
librarian::shelf(
  bsicons, bslib, dplyr, glue, here, htmltools, leaflet, leaflet.extras2,
  lubridate, plotly, readr, scales, sf, shiny, slider,
  tbep-tech/tbeptools,
  terra, thematic, tibble, tidyr)
source(here("app/functions.R"))
options(readr.show_col_types = F)

# themes ----
light <- bs_theme(preset = "flatly")
dark  <- bs_theme(preset = "darkly")

# prism ----
dir_prism     <- here("data/prism")
prism_csv     <- here("data/prism.csv")

d_prism_r     <- read_prism_rasters(dir_prism)
yrs_prism     <- range(year(d_prism_r$date))
now_prism     <- max(d_prism_r$date)
d_prism_z     <- read_csv(prism_csv)
prism_zones   <- d_prism_z |>
  distinct(bay_segment) |>
  left_join(
    tbsegshed |>
      st_drop_geometry() |>
      select(long_name, bay_segment) |>
      bind_rows(
        tibble(
          bay_segment = "TB",
          long_name   = "Tampa Bay") ),
    by = "bay_segment") |>
  select(long_name, bay_segment) |>
  deframe()

# sst ----
sst_tif   <- here("data/sst/tb_sst.tif")
sst_csv   <- here("data/sst/tb_sst.csv")

r_sst     <- rast(sst_tif)
d_sst_r   <- tibble(
  lyr = names(r_sst)) |>
  separate(lyr, c("var", "date"), sep = "\\|", remove = F) |>
  mutate(
    date = as.Date(date))
yrs_sst   <- range(year(d_sst_r$date))
now_sst   <- max(d_sst_r$date)
d_sst_z   <- read_csv(sst_csv) |>
  mutate(
    date = as.Date(time))
sst_zones <- d_sst_z |>
  distinct(long_name, bay_segment) |>
  deframe()

# sealevel ----
d_sl <- read_csv(here("data/slr/monthly_sea_levels.csv"))
sl_stations <- d_sl |>
  distinct(station_name, station_id) |>
  deframe()

# map ----
b <- st_bbox(tbsegshed) |> as.numeric()

# overview ----
sparkline <- plot_ly(economics) |> 
  add_lines(
    x = ~date, y = ~psavert,
    color = I("white"), span = I(1),
    fill = 'tozeroy', alpha = 0.2) |> 
  layout(
    xaxis = list(visible = F, showgrid = F, title = ""),
    yaxis = list(visible = F, showgrid = F, title = ""),
    hovermode = "x",
    margin = list(t = 0, r = 0, l = 0, b = 0),
    font = list(color = "white"),
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent") |> 
  config(displayModeBar = F) |> 
  htmlwidgets::onRender(
    "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }")

# value boxes
vbs <- list(
  value_box(
    title = "Personal Savings Rate",
    value = "7.6%",
    p("Started at 12.6%"),
    p("Averaged 8.6% over that period"),
    p("Peaked 17.3% in May 1975"),
    showcase = sparkline,
    full_screen = TRUE,
    theme = "success"
  ),
  
  value_box(
    title = "1st value",
    value = "123",
    showcase = bs_icon("bar-chart"),
    theme = "purple",
    p("The 1st detail")
  ),
  
  value_box(
    title = "2nd value",
    value = "456",
    showcase = bs_icon("graph-up"),
    theme = "teal",
    p("The 2nd detail"),
    p("The 3rd detail")
  ),
  
  value_box(
    title = "3rd value",
    value = "789",
    showcase = bs_icon("pie-chart"),
    theme = "pink",
    p("The 4th detail"),
    p("The 5th detail"),
    p("The 6th detail")
  )
)
