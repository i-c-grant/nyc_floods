library(terra)
library(R.utils)

build_date_query <- function(datetime) {
  datetime <- with_tz(datetime, "UTC")

  query_hour <- hour(datetime)

  query_hour <- ifelse(query_hour < 10,
                         paste0("0", query_hour),
                         query_hour) %>%
    as.character()
  
  query_date <- date(datetime) %>%
    str_replace_all("-", "")

  query_type <- ".01h" # get 1 hour precip (as opposed to 6 hour)
  
  query <- paste0(query_date,
                  query_hour,
                  query_type)

  return(query)
}


build_date_queries <- function(start, end) {
  dts <- seq(from = start, to = end, by = "hour")
  queries <- map_chr(dts, build_date_query)
  return(queries)
}


get_dir_names <- function(start, end) {
  ## get monthly directories, e.g. 202001
  dts <- seq(from = start,
                to = end,
                by = "month")

  years <- year(dts) %>%
    as.character

  months <- month(dts)

  months <- ifelse(months < 10,
                     paste0("0", months),
                   months) %>%
    as.character

  dirs <- paste0(years, months)

  return(dirs)
}


get_stage4_paths <- function(start,
                             end,
                             data_dir) {

  dirs <- file.path(data_dir,
                    get_dir_names(start, end))

  queries <- build_date_queries(start,
                                end)
  
  all_stage4_files <- list.files(dirs,
                                 full.names = TRUE,
                                 recursive = TRUE)

  exclude <- paste("st4_pr", "st4_ak", ".24h", ".06h", sep = "|")

  ## filter out Alaska and Puerto Rico files and 6 and 24 hour precip
  stage4_files <-
    all_stage4_files[!grepl(exclude, all_stage4_files)]

  ## filter out files that don't match the queries
  stage4_files <-
    stage4_files[grepl(paste(queries, collapse = "|"), stage4_files)]

  return(stage4_files)
}


## get_stage4_fn <- function(query,
##                           file_list) {

##   stage4_files <- list.files(data_dir,
##                              pattern = query,
##                              full.names = TRUE,
##                              recursive = TRUE)
  
##   ## filter out Alaska and Puerto Rico files
##   keep <- !str_detect(stage4_files, "st4_pr|st4_ak")
##   stage4_files <- stage4_files[keep]

##   ## check that we have exactly one file
##   if (length(stage4_files) > 1) {
##     stop("Multiple stage4 files found for query: ", query)
##   }

##   if (length(stage4_files) == 0) {
##     stop("No stage4 files found for query: ", query)
##   }

##   return(stage4_files)
## }


load_stage4_file <- function(path) {
  ## get extension of file
  ext <- tools::file_ext(path)

  ## if file is compressed, uncompress it
  if ((ext == "Z") || (ext == "gz")) {
    path <- gunzip(path,
                 temporary = TRUE,
                 remove = FALSE,
                 overwrite = TRUE)
  }

  r <- rast(path)

  return(r)
}

clip_to_nyc <- function(r, boundary_name) {
  nyc_boundary <- load_nyc_boundary(layer = boundary_name)
  
  r_crs <- terra::crs(r)

  if (r_crs != terra::crs(nyc_boundary)) {
    nyc_boundary <- sf::st_transform(nyc_boundary, crs = r_crs)
  }

  r <- terra::crop(r, nyc_boundary)

  return(r)
}

load_nyc_stage4_df <- function(dt_start,
                               dt_end,
                               data_dir,
                               disagg) {
 
  dts <- seq(from = dt_start,
             to = dt_end,
             by = "hour")

  paths <- get_stage4_paths(dt_start,
                            dt_end,
                            data_dir)

  rasts <- map(paths, load_stage4_file)
  ## initial clip to general NYC area to reduce size of reprojection
  rasts <- map(rasts, \(r) clip_to_nyc(r, "nyc_box"))
  ## reproject clipped rasters to NYC state plane
  rasts <- map(rasts, \(r) terra::project(r, "EPSG:2263"))
  ## final clip to NYC area to get a rectangluar area
  rasts <- map(rasts, \(r) clip_to_nyc(r, "nyc_state_plane"))
    
  if (disagg) {
    rasts <- map(rasts, \(r) terra::disagg(r, 16, method = "bilinear"))
  }

  df <- map2(rasts, dts,
             \(r, dt) as.data.frame(r, xy = TRUE) %>%
                      as_tibble %>%
                      mutate(time = dt)) %>%
    bind_rows %>%
    rename(hourly_precip = 3)

  return(df)
}

load_nyc_stage4_stars <- function(start_datetime,
                                  end_datetime,
                                  data_dir,
                                  disagg) {

  df_precip <- load_nyc_stage4_df(start_datetime,
                                  end_datetime,
                                  data_dir,
                                  disagg)

  stars_precip <- st_as_stars(df_precip,
                              dims = c("x", "y", "time"))

  st_crs(stars_precip) <- st_crs("EPSG:2263")

  return(stars_precip)
}

load_precip_calls_stars <- function(start_datetime,
                                    end_datetime,
                                    data_dir,
                                    disagg = FALSE) {

  precip_stars <- load_nyc_stage4_stars(start_datetime,
                                        end_datetime,
                                        data_dir,
                                        disagg)

  precip_calls_stars <- precip_stars %>%
    add_calls_to_stage4_stars()

  return(precip_calls_stars)
  
}

add_cumulative_precip <- function(stars_stage4) {
## add cumulative precipitation
stars_precip_calls <-
  stars_precip_calls %>%
  as_tibble %>%
  group_by(x, y) %>%
  arrange(time) %>%
  mutate(cumulative_precip =
           cumsum(hourly_precip)) %>%
  st_as_stars(dims = c("x", "y", "time"))

  st_crs(stars_precip_calls) <- st_crs("EPSG:2263")

  return(stars_precip_calls)
}

calc_decay <- function(x, decay) {
  ans <- numeric(length(x))
  ans[1] <- x[1]
  
  for (i in 2:length(x)) {
    ans[i] <- max(0, decay * (ans[i - 1] + x[i]))
  }
  
  return(ans)
}

add_decay_precip <- function(stars_stage4, decay) {
## add cumulative precipitation
stars_precip_calls <-
  stars_precip_calls %>%
  as_tibble %>%
  group_by(x, y) %>%
  arrange(time) %>%
  mutate(decay_precip = calc_decay(hourly_precip, decay)) %>%
  st_as_stars(dims = c("x", "y", "time"))

  st_crs(stars_precip_calls) <- st_crs("EPSG:2263")

  return(stars_precip_calls)
}
