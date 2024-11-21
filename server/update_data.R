#!/usr/bin/env Rscript

log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste(timestamp, "-", msg)
  cat(message, "\n", file = "/var/log/climate_data_update.log", append = TRUE)
  cat(message, "\n") # Also print to console
}

log_message("Loading packages...")
librarian::shelf(
  curl, dplyr, marinebon/extractr, gert, glue, here, lubridate, sf,
  tbeptech/tbeptools,
  quiet = TRUE)

log_message("Setting up variables...")
tb_zones <- tbeptools::tbsegshed |>
  bind_rows(
    tbeptools::tbshed |>
      mutate(
        long_name   = "Tampa Bay",
        bay_segment = "TB") |>
      select(-Acres)) |>
  st_make_valid()

# Update functions ----
update_sea_level <- function() {
  sl_csv <- here("data/slr/monthly_sea_levels.csv")

  read_importsealevels(sl_csv)
}

update_sst <- function() {
  dir_nc <- here("data/sst/tb_sst_nc")
  if (dir.exists(dir_nc))
    unlink(dir_nc, recursive = TRUE)

  extractr::ed_extract(
    ed        = extractr::ed_info("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html"),
    var       = "CRW_SST",
    bbox      = c(xmin = -83.0, ymin = 27.2 , xmax = -82.3, ymax= 28.5),
    sf_zones  = tb_zones,
    fld_zones = "bay_segment",
    zonal_csv = here::here("data/sst/tb_sst.csv"),
    rast_tif  = here::here("data/sst/tb_sst.tif"),
    mask_tif = F)
}

update_hurricane <- function() {
  url  <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/netcdf/IBTrACS.NA.v04r01.nc"
  file <- here("data/storms", basename(url))

  resp <- curl::multi_download(
    urls          = url,
    destfile      = file,
    timecondition = 1,
    timevalue     = as.numeric(file.info(file)$mtime),
    progress      = F)

  if (!resp$success)
    stop(resp$error)
}

update_prism <- function() {
  d <- read_importprism(
    vars      = c("tmin", "tmax", "tdmean", "ppt"),
    vars_ytd  = c("ppt"),
    date_beg  = as.Date("1981-01-01"),
    date_end  = Sys.Date(),
    bbox      = c(xmin = -82.9, ymin = 27.2, xmax = -81.7, ymax = 28.6),
    dir_tif   = here("data/prism"),
    sf_zones  = tb_zones,
    fld_zones = "bay_segment",
    zonal_csv = here("data/prism.csv"),
    verbose   = T)
}

git_update <- function() {
  # Configure git credentials
  git_config_set("user.name", Sys.getenv("GIT_USER"))
  git_config_set("user.email", Sys.getenv("GIT_EMAIL"))

  # Set up GitHub authentication
  pat <- Sys.getenv("GITHUB_PAT")
  if (pat == "")
    stop("GITHUB_PAT environment variable not set")
  git_config_set("credential.helper", "store")
  git_config_global_set("github.token", pat)

  # Add all changes in data directory
  git_add(here("data"), repo = here())

  # Commit with timestamp
  commit_msg <- glue("update_data.R: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")
  git_commit(commit_msg, repo = here())

  # Push changes using PAT
  git_push(repo = here(), password = pat)
}

# Main Execution ----

main <- function() {
  log_message("Starting data update process")

  # List of update functions to run
  update_fns <- list(
    prism     = update_prism,
    sea_level = update_sea_level,
    sst       = update_sst,
    hurricane = update_hurricane,
    git       = git_update)

  # Run each function with error handling
  lapply(names(update_fns), function(fn_name) {
    tryCatch({
      log_message(glue::glue("Starting {fn_name} step..."))
      update_fns[[fn_name]]()
      log_message(glue::glue("Successfully completed {fn_name} step"))
    }, error = function(e) {
      log_message(glue::glue("Error in {fn_name} step: {conditionMessage(e)}"))
    })
  })

  log_message("Data update process completed")
}

main()
