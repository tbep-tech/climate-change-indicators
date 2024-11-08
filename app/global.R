# TODO:
# - [ ] Calculate collective min/max dates for Overview sld_date_split across data: prism, sst, slr, storms
# - [ ] Overview bar plot, default: bar plot with error bars, zoomed into extent of min/max for exploded
# - [ ] Overview bar plot, click to explode: add geom_sina point labels for min/max/avg years to plots, per [r - How to align position of geom\_sina point with geom\_text\_repel - Stack Overflow](https://stackoverflow.com/questions/69498840/how-to-align-position-of-geom-sina-point-with-geom-text-repel)
# - [ ] add landing page and tour with cicero
# - [ ] add month-day slider to other maps besides air temperature
# - [ ] add prism_ytd processing and map
# - [ ] debounce() for slider inputs, like sld_t_md
# - [ ] try absolutePanel(draggable = T) for sliders (eg sld_t_md); try nesting wellPanel
#       https://shiny.posit.co/r/layouts/panels-cards/
#       https://github.com/rstudio/shiny-examples/blob/main/027-absolutely-positioned-panels/ui.R
# - [ ] add avg (before 2 yrs ago) to plot_doy() colored blue
# - [ ] make sld_{t}_tmp 2-values and process
# - [ ] handle tmean w/ reactive; sel_t_var implement in map_prism_temp w/ avg
# - [ ] reuse time slider processing with a functions.R
# - [ ] prism: show `version`, `date_updated`
# - [ ] show sources for prism, sst, etc with About and links per card
# - [ ] observe and update map_sl <-> plot_sl with click and highlight station in map_sl
# - [ ] temp: heat index using dewpoint (`tdmean` -> humidity)
# - [ ] + density histogram below map for selected now & then times
# - [ ] migrate viz functions.R to tbeptools
# - [ ] create static figures for reports (auto parameterize rendered)
# - [ ] swiper maps: leaflet-proxy-map updates to layers so can zoom / pan without refresh
# DONE:
# - [x] switch SST to https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html
# - [x] move settings (gear icon) to right of card headers
# - [x] convert prism units: ºC -> ºF, mm -> in
# - [x] add sea level plot with linear fit and map for stations
# - [x] icons for Map | Plot
# - [x] swiper maps: precipitation, ocean temperature
# - [x] style ggplotly https://rstudio.github.io/thematic/#shiny
#          https://rstudio.github.io/thematic/articles/auto.html

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
# sst_new_csv   <- here("data/sst/tb_sst_new.csv")

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
# d_sst_z_new   <- read_csv(sst_new_csv) |>
  mutate(
    date = as.Date(time))
# d_sst_z_old <- d_sst_z
# table(d_sst_z_old$bay_segment)
#   BCB    HB   LTB    MR   MTB   OTB   TCB
# 13169 13169 13169 13169 13169 13169 13169

sst_zones <- d_sst_z |>
  distinct(bay_segment) |>
  deframe()

# sealevel ----
d_sl <- read_csv(here("data/slr/monthly_sea_levels.csv"))
sl_stations <- d_sl |>
  distinct(station_name, station_id) |>
  deframe()

# map bounding box ----
b <- st_bbox(tbsegshed) |> as.numeric()

# get centroid point of Tampa Bay watershed
ctr <- tbeptools::tbshed |>
  suppressWarnings({
    st_centroid() })

# st_coordinates(pt) |> as.numeric()
# -82.35778  27.86714

# hurricanes ----
h_url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/netcdf/IBTrACS.NA.v04r01.nc"
h_nc  <- here(glue("data/storms/{basename(h_url)}"))

# download if online newer than local
download_new(h_url, h_nc)

h_sds <- defStormsDataset(h_nc, basin = "NA", verbose = 0) # NA: North Atlantic basin
# str(h_sds)
# length(unique(paste(h_sds@database$names, h_sds@database$seasons, " "))) # 622

# tibble(
#   name     = h_sds@database$names,
#   season   = h_sds@database$seasons,
#   isotime1 = h_sds@database$isotimes[1,],
#   isotime360 = h_sds@database$isotimes[360,]) |>
#   arrange(desc(isotime1))
# # A tibble: 712 × 4
#   name         season isotime1            isotime360
#   <chr[1d]> <int[1d]> <chr>               <chr>
# 1 BERYL          2024 2024-06-26 00:00:00 ""
# 2 CHRIS          2024 2024-06-24 18:00:00 ""
# 3 ALBERTO        2024 2024-06-16 12:00:00 ""
# 4 UNNAMED        2023 2023-11-16 18:00:00 ""
# 5 UNNAMED        2023 2023-10-23 12:00:00 ""

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
# getNbStorms(h_st)  # 221

h_d <- tibble(
  storm    = getNames(h_st),
  year     = getSeasons(h_st) |> as.numeric(),
  date_beg = map_chr(h_st@data, ~.@obs.all$iso.time[[1]]) |>
    as.Date(),
  yday_beg = yday(date_beg),
  scale    = getScale(h_st)   |> as.numeric())  # A tibble: 221 × 3
# tail(h_d)

# split_date <- Sys.Date() - years(1)
# devtools::load_all(here::here("../tbeptools"))
# y <- anlz_splitstorms(h_d, split_date)
# y |>
#   show_splitbarplot("period", "year", "sum")
# table(y$year)
# tail(y)
#
#
# hurricanes <- data.frame(
#   date_beg = as.Date(c(
#     "1980-07-31", "1980-09-04", "1980-11-07",
#     "1981-05-06", "1981-08-07", "1981-11-12")),
#   scale = c(6, 1, 3, 1, 2, 1))
# # Basic analysis with default statistics (sum, average and count)
# split_date <- Sys.Date() - years(1)
# anlz_splitstorms(hurricanes, split_date)
#
# # Analysis with custom statistics
# anlz_splitstorms(hurricanes, split_date,
#                      stats = list(
#                        max = max,
#                        min = min))

# h_d$yday_beg

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

# summary(h_d_sum)
#      year        scale_sum       scale_avg        storms_n       storms_md
# Min.   :1980   Min.   : 3.00   Min.   :1.000   Min.   : 1.000   Length:45
# 1st Qu.:1991   1st Qu.: 6.00   1st Qu.:1.800   1st Qu.: 3.000   Class :character
# Median :2002   Median : 9.00   Median :2.400   Median : 4.000   Mode  :character
# Mean   :2002   Mean   :11.93   Mean   :2.447   Mean   : 4.911
# 3rd Qu.:2013   3rd Qu.:14.00   3rd Qu.:2.875   3rd Qu.: 6.000
# Max.   :2024   Max.   :36.00   Max.   :6.000   Max.   :14.000

# h_d_sum |>
#   select(year, scale_sum) |>
#   plot(type = "b")

# (h_yr_max <- h_d_sum$year[which.max(h_d_sum$scale_sum)])
# 2005
# h_d |>
#   filter(year == h_yr_max)
#    storm     year scale
#    <chr>    <dbl> <dbl>
#  1 ARLENE    2005     1
#  2 CINDY     2005     2
#  3 DENNIS    2005     5
#  4 EMILY     2005     6
#  5 FRANKLIN  2005     1
#  6 KATRINA   2005     6
#  7 OPHELIA   2005     2
#  8 RITA      2005     6
#  9 TAMMY     2005     1
# 10 WILMA     2005     6

h_filt_yrs <- function(st, yrs){
  # st = h_st; yrs = c(2000, 2024)
  st_yrs <- getSeasons(st) |> as.numeric()
  i_yrs <- st_yrs >= yrs[1] & st_yrs <= yrs[2]
  st@data <- st@data[i_yrs]
}

# plotStorms(h_filt_yrs(h_st, c(2005, 2005)), dynamicPlot = T)
# plotStorms(h_st, dynamicPlot = T)

# h_yr_split <- 2000
# h_s <- h_d_sum |>
#   select(year, scale_sum) |>
#   mutate(
#     yr_grp = case_when(
#       year >= h_yr_split ~ glue(">= {h_yr_split}"),
#       TRUE              ~ glue("< {h_yr_split}")) |>
#       as.factor())
# h_s <- tibble::tribble(
#   ~year,  ~cat, grp,
#   2000,    10,  "Then",
#   2001,    12,  "Then",
#   2002,    8,   "Then",
#   2020,    14,   "Now",
#   2021,    18,   "Now",
#   2022,    15,   "Now")

# h_g <- h_s |>
#   group_by(yr_grp) |>
#   summarise(
#     yr_min = min(year),
#     yr_max = max(year),
#     avg    = mean(scale_sum),
#     sd     = sd(scale_sum))
#
# # h_bar: column plot with error bars ----
# h_bar <- h_g |>
#   ggplot(aes(x = yr_grp, y = avg)) +
#   geom_col() +
#   geom_errorbar(
#     aes(ymin=avg-sd, ymax=avg+sd), width=.2, alpha=0.8) +
#   ggplot2::labs(x = NULL, y = NULL) +
#   coord_cartesian(ylim = range(h_s$scale_sum)) + # expand_range
#   theme(
#     axis.text=element_text(size=12))
#
# # h_v: violin plot with geom_sina points ---
# h_v <- h_bar +
#   # geom_violin(
#   #   data = h_s,
#   #   aes(x = yr_grp, y = scale_sum),
#   #   fill = "gray80", linewidth = 1, alpha = .5) +
#   geom_sina(
#     data = h_s,
#     aes(x = yr_grp, y = scale_sum),
#     alpha = .25)
#
# h_p <- ggplot_build(h_v)$data[[3]] |>
#   bind_cols(
#     h_s |>
#       # select(year, scale_sum, yr_grp) |>
#       mutate(
#         lbl = glue(
#           "year: {year}
#            sum(cat): {scale_sum}")))
# # highlight points with most, least and closest to average,
# #   subset to yr_grp
# librarian::shelf(ggrepel)
# dplyr::top_n(h_p, 3, scale_sum)
#
# devtools::load_all("~/Github/tbep-tech/tbeptools")
# anlz_filter_extreme_and_mean_rows(sample_data, group, value)
#
#
# h_p2 |> select(year, scale_sum, yr_grp, rn) |> View()
#
#   filter(dense_rank(scale_sum) %in% c(1, n(), which.min(abs(scale_sum - h_g$avg))) ) |>
#   slice(n=1)
#   slice(c(1, n(), which.min(abs(scale_sum - h_g$avg))) ) |>
#   ggplot(aes(x = x, y = y, text = lbl)) +
#   geom_point(size = 3, color = "red", shape = 22, fill = NA) +
#   geom_label_repel(
#     aes(label = lbl), color = "red",
#     box.padding = 2, min.segment.length = 0.5, size=3)
# h_p |>
#   arrange(scale_sum) |>
#   slice(c(1, n(), which.min(abs(scale_sum - h_g$avg))) ) |>
#   ggplot(aes(x = x, y = y, text = lbl)) +
#   geom_point(size = 3, color = "red", shape = 22, fill = NA) +
#   geom_label_repel(
#     aes(label = lbl), color = "red",
#     box.padding = 2, min.segment.length = 0.5, size=3)
#
#
# a + geom_point(data = a_guts, aes(x = x, y = y),
#                size = 3, color = "red", shape = 22, fill = NA) +
#   geom_label_repel(data = a_guts,
#                    aes(x = x, y = y, label = county), color = "red",
#                    box.padding = 2, min.segment.length = 0.5, size=3)
#
#
#
# h_pg <- suppressWarnings(
#   h_bar +
#   geom_point(
#     data = h_p,
#     aes(x = x, y = y, text = lbl)))
# ggplotly(h_pg, tooltip = "text")
#     # size = 3, color = "red", shape = 22, fill = NA) +
#
# suppressWarnings(
#   h_bar +
#     geom_point(
#       data = h_p,
#       aes(x = x, y = y, text = lbl))) |>
#   ggplotly(tooltip = "text")
#
# h_v <- h_s |>
#   ggplot(
#     aes(
#       x = yr_grp, y = scale_sum, text = year)) +
#   # labs(x = "Season", y = "Ozone") +
#   # scale_color_brewer(palette = "Dark2", guide = "none") +
#   # geom_boxplot() +
#   geom_violin(fill = "gray80", linewidth = 1, alpha = .5) +
#   geom_sina(alpha = .25)
# a <- ggplot(midwest, aes(state, area)) +
#   geom_violin() +
#   geom_sina()
#
# geom_violin(
#   data = h_s,
#   aes(x = yr_grp, y = scale_sum),
#   fill = "gray80", linewidth = 1, alpha = .5) +
#   geom_sina(
#     data = h_s,
#     aes(x = yr_grp, y = scale_sum, text = year),
#     alpha = .25)
#
#
# a_guts <- ggplot_build(a)$data[[2]] %>%
#   bind_cols(midwest %>%
#               mutate(state_num = as.integer(as.factor(state))) %>%
#               select(state_num, area, county, poptotal)) %>%
#   subset(poptotal>1E6)
#
#
# set.seed(0)
# a + geom_point(data = a_guts, aes(x = x, y = y),
#                size = 3, color = "red", shape = 22, fill = NA) +
#   geom_label_repel(data = a_guts,
#                    aes(x = x, y = y, label = county), color = "red",
#                    box.padding = 2, min.segment.length = 0.5, size=3)
#
#
#
# h_bar +
#   # geom_violin(
#   #   data = h_s,
#   #   aes(x = yr_grp, y = scale_sum),
#   #   fill = "gray80", linewidth = 1, alpha = .5) +
#   geom_sina(
#     data = h_s,
#     aes(x = yr_grp, y = scale_sum, text = year),
#     alpha = .25)


# librarian::shelf("homerhanumat/bpexploder")
# h_bar <- bpexploder(
#   data = iris,
#   settings = list(
#     groupVar = "Species",
#     levels = levels(iris$Species),
#     yVar = "Petal.Length",
#     tipText = list(
#       Petal.Length = "Petal Length"),
#     relativeWidth = 0.75))
# bpexploder(
#   data = h_s,
#   settings = list(
#     groupVar = "yr_grp",
#     # levels   = levels(h_s$yr_grp),
#     yVar     = "scale_sum",
#     # tipText = list(
#     #   scale_sum = "sum(Cat)",
#     #   year      = "Year"),
#     relativeWidth = 0.75))

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

# TODO: make reactive to settings for bay_segment, var and years/date
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
    bay_segment == "OTB") |>
  rename(value = mean) |>
  mutate(
    value = set_units(value, "degree_C"))

