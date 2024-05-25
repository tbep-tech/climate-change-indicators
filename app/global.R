# TODO:
# - [ ] add sea level plot with linear fit and map for stations
# - [ ] make sld_{t}_tmp 2-values and process
# - [ ] handle tmean w/ reactive; sel_t_var implement in map_prism_temp w/ avg
# - [ ] reuse time slider processing with a functions.R
# - [ ] convert prism units: ºC -> ºF, mm -> in
# - [ ] prism: show `version`, `date_updated`
# - [ ] show sources for prism, sst, etc with About and links per card
# - [ ] move gears to right with Settings label
# - [ ] temp: heat index using dewpoint (`tdmean` -> humidity)
# - [ ] + density histogram below map for selected now & then times
# - [ ] migrate viz functions.R to tbeptools
# - [ ] create static figures for reports (auto parameterize rendered)
# - [ ] swiper maps: leaflet-proxy-map updates to layers so can zoom / pan without refresh
# - [x] icons for Map | Plot
# - [x] swiper maps: precipitation, ocean temperature
# - [x] style ggplotly https://rstudio.github.io/thematic/#shiny
#          https://rstudio.github.io/thematic/articles/auto.html

# devtools::install_local(here::here("../tbeptools"), force = T)
# devtools::load_all(here::here("../tbeptools"))
librarian::shelf(
  bsicons, bslib, dplyr, glue, here, leaflet, leaflet.extras2, lubridate, plotly,
  readr, scales, sf, shiny, slider,
  tbep-tech/tbeptools,
  terra, thematic, tibble, tidyr)
source(here("app/functions.R"))
options(readr.show_col_types = F)

light <- bs_theme(preset = "flatly")
dark  <- bs_theme(preset = "darkly")

dir_prism <- here("data/prism")
prism_csv <- here("data/prism.csv")
sst_tif   <- here("data/sst/tb_sst.tif")
sst_csv   <- here("data/sst/tb_sst.csv")

d_prism_r     <- read_prism_rasters(dir_prism)
yrs_prism     <- range(year(d_prism_r$date))
now_prism     <- max(d_prism_r$date)
d_prism_z     <- read_csv(prism_csv)
prism_zones <- d_prism_z |>
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

r_sst <- rast(sst_tif)
d_sst_r <- tibble(
  lyr = names(r_sst)) |>
  separate(lyr, c("var", "date"), sep = "\\|", remove = F) |>
  mutate(
    date = as.Date(date))
yrs_sst <- range(year(d_sst_r$date))
now_sst <- max(d_sst_r$date)
d_sst_z <- read_csv(sst_csv) |>
  mutate(
    date = as.Date(time))
sst_zones <- d_sst_z |>
  distinct(long_name, bay_segment) |>
  deframe()

b <- st_bbox(tbsegshed) |> as.numeric()
