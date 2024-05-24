# TODO:
# - [ ] style ggplotly https://rstudio.github.io/thematic/#shiny
#          https://rstudio.github.io/thematic/articles/auto.html
# - [ ] icons for Map | Plot
# - [ ] show sources for prism, sst, etc with About and links per card
# - [ ] convert prism units: ºC -> ºF, mm -> in
# - [ ] sel_t_var implement in map_prism_temp w/ avg
# - [ ] swiper maps: precipitation, ocean temperature
# - [ ] temp: heat index using dewpoint (`tdmean` -> humidity)
# - [ ] prism: show `version`, `date_updated`
# - [ ] time series cards / panels
# - [ ] create static figures for reports (auto parameterize rendered)
# - [x] swiper maps: leaflet-proxy-map updates to layers so can zoom / pan without refresh

# devtools::install_local(here::here("../tbeptools"), force = T)
# devtools::load_all(here::here("../tbeptools"))
librarian::shelf(
  bsicons, bslib, dplyr, glue, here, leaflet, leaflet.extras2, lubridate, plotly,
  readr, scales, shiny, slider,
  tbep-tech/tbeptools,
  terra, thematic, tidyr)
source(here("app/functions.R"))
options(readr.show_col_types = F)

light <- bs_theme(preset = "flatly")
dark  <- bs_theme(preset = "darkly")

dir_prism <- here("data/prism")
prism_csv <- here("data/prism.csv")
sst_tif   <- here("data/sst/tb_sst.tif")
sst_csv   <- here("data/sst/tb_sst.csv")

d_prism_r <- read_prism_rasters(dir_prism)
yrs_prism <- range(year(d_prism_r$date))
now_prism <- max(d_prism_r$date)
d_prism_z <- read_csv(prism_csv)

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
