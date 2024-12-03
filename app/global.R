# TODO:
# - [ ] landing: splash page
# - [ ] tour: with cicero
# - [ ] about: show sources for prism, sst, etc with About and links per card
# - [ ] load: prep data so app loads faster
# - [ ] performance: debounce() for slider inputs, like sld_t_md
# - [ ] temp: option to select min/max/mean as sel_t_var
# - [ ] sl: observe and update map_sl <-> plot_sl with click/select and highlight station in map_sl
# - [ ] temp: heat index using dewpoint (`tdmean` -> humidity)
# - [ ] download: data, static figures
# - [ ] maps: leaflet-proxy-map updates to layers so can zoom / pan without refresh
# - [ ] prism: show `version`, `date_updated`

# devtools::install_local(here::here("../tbeptools"), force = T)
# devtools::load_all(here::here("../tbeptools"))
librarian::shelf(
  bsicons, bslib, dplyr, glue, here, htmltools, leaflet, leaflet.extras2,
  lubridate, markdown, plotly, purrr, readr, scales, sf, shiny, slider,
  tbep-tech/tbeptools,
  StormR, terra, thematic, tibble, tidyr, units)
set.seed(42)
source(here("app/functions.R"))
options(readr.show_col_types = F)

# themes ----
light <- bs_theme(preset = "flatly")
dark  <- bs_theme(preset = "darkly")

# prism ----
dir_prism     <- here("data/prism")
dir_prismytd  <- here("data/prism_ytd")
prism_csv     <- here("data/prism.csv")
prismytd_csv  <- here("data/prism.csv")

d_prism_r     <- read_prism_rasters(dir_prism)
d_prismytd_r  <- read_prism_rasters(dir_prismytd)
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
# devtools::load_all("~/Github/marinebon/extractr")

sst_tif   <- here("data/sst/tb_sst.tif")
sst_csv   <- here("data/sst/tb_sst.csv")

sf_tb <- tbeptools::tbsegshed |>
  bind_rows(
    tbeptools::tbshed |>
      mutate(
        long_name   = "Tampa Bay",
        bay_segment = "TB") |>
      select(-Acres)) |>
  sf::st_make_valid()

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
  distinct(bay_segment) |>
  deframe()

# sealevel ----
d_sl <- read_csv(here("data/slr/monthly_sea_levels.csv"))
sl_stations <- d_sl |>
  distinct(station_name, station_id) |>
  deframe()

sl_station_default <- sl_stations["St. Petersburg"]

sl_yr_rng <- d_sl |>
  filter(station_id == sl_station_default) |>
  select(year) |>
  range()
sl_yr_default <- 2000

# map bounding box ----
b <- st_bbox(tbsegshed) |> as.numeric()

# get centroid point of Tampa Bay watershed
ctr <- tbeptools::tbshed |>
  suppressWarnings({
    st_centroid() })
# st_coordinates(ctr) |> as.numeric()  # -82.35778  27.86714

# hurricanes ----
h_url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/netcdf/IBTrACS.NA.v04r01.nc"
h_nc  <- here(glue("data/storms/{basename(h_url)}"))

h_sds    <- defStormsDataset(h_nc, basin = "NA", verbose = 0) # NA: North Atlantic basin
h_buf_km <- 1000
h_ply <- ctr |>
  st_buffer(h_buf_km * 1000)
# mapview::mapView(h_ply) + mapview::mapView(ctr)

h_yrs <- c(h_sds@seasons["min"], h_sds@seasons["max"]) |> as.integer() # 1980 2024
h_st <- defStormsList(
  h_sds,
  ctr,
  maxDist     = h_buf_km,
  removeUnder = 1,
  seasons     = h_yrs,
  verbose     = 0)

h_d <- tibble(
  storm    = getNames(h_st),
  year     = getSeasons(h_st) |> as.numeric(),
  date_beg = map_chr(h_st@data, ~.@obs.all$iso.time[[1]]) |>
    as.Date(),
  yday_beg = yday(date_beg),
  scale    = getScale(h_st)   |> as.numeric())  # A tibble: 221 × 3

h_d_sum <- h_d |>
  group_by(year) |>
  summarize(
    scale_sum = sum(scale),
    scale_avg = mean(scale) |> round(1),
    storms_n  = n(),
    storms_md = glue(
      "- {storm} ({scale})") |>
      paste(collapse = "\n"),
    .groups = "drop") |>
  mutate(
    label_md   = glue(
      "<b>{year}</b>, {scale_sum} scale sum
      {storms_n} storms (avg scale: {scale_avg}):
      {storms_md}"),
    label_html = markdownToHTML(label_md, fragment.only = T) )

stopifnot(length(setdiff(h_yrs[1]:h_yrs[2], h_d_sum$year)) == 0) # TODO: add missing years

h_filt_yrs <- function(st, yrs){
  st_yrs <- getSeasons(st) |> as.numeric()
  i_yrs <- st_yrs >= yrs[1] & st_yrs <= yrs[2]
  st@data <- st@data[i_yrs]
  st
}

h_yr_split <- 2000

# overview ----

vb <- function(...){
  value_box(
    showcase_layout = showcase_bottom(
      height     = 0.5,
      max_height = "200px"),
    max_height   = "350px",
    full_screen  = TRUE,
    theme        = "primary",
    ...)
}

# default to whole Tampa Bay (vs bay_segment) ----
d_temp <- d_prism_z |>
  filter(
    bay_segment == "TB",
    variable    == "tdmean") |>
  rename(value = mean) |>
  mutate(
    value = set_units(value, "degree_C"))

d_rain <- d_prism_z |>
  filter(
    bay_segment == "TB",
    variable    == "pptytd") |>
  rename(value = mean) |>
  mutate(
    value = set_units(value, "mm"))

d_sst <- d_sst_z |>
  filter(
    bay_segment == "TB") |>
  rename(value = mean) |>
  mutate(
    value = set_units(value, "degree_C"))

