library(httr)
library(jsonlite)
library(sf)
library(mapview)
library(glue)
library(ggplot2)
library(tidyverse)

API_BASE <- "https://api.dev.floodlabs.nyc/api/rest/"

floodnet_request <- function(endpoint, ...) {
  
  response <- GET(url = paste0(API_BASE, endpoint), query = list(...))
  if (response$status_code != 200) {
    stop(paste("API request failed with status code", response$status_code))
  }
  return(fromJSON(content(response, "text")))
}

get_deployments <- function() {
  response <- floodnet_request("deployments/flood")

  ## Convert to tibble
  deployments <- as_tibble(response)[[1]]

  deployments$lon <- map_dbl(deployments$location$coordinates,
                             \(x) x[[1]])
  
  deployments$lat <- map_dbl(deployments$location$coordinates,
                             \(x) x[[2]])

  sf_deployments <-
    deployments %>%
    select(-location) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

  return(sf_deployments)
}

query_deployment <- function(deployment_id, start_time, end_time) {
  response <- floodnet_request(glue("deployments/flood/{deployment_id}/depth"),
                               start_time = start_time,
                               end_time = end_time)

  return(as_tibble(response))
}


query_depth_data <- function(start_time, end_time) {
  deployments <- get_deployments()

  active_deployments <- deployments %>%
    filter(date_deployed <= start_time,
           sensor_status == "good")

  depth_data <-
    map(active_deployments$deployment_id,
        \(id) query_deployment(id,
                               start_time=start_time,
                               end_time=end_time))

  depth_data <- depth_data[map_lgl(depth_data, \(x) nrow(x) > 0)]

  depth_data <- bind_rows(depth_data)$depth_data

  depth_data$time <- ymd_hms(depth_data$time)

  return(as_tibble(depth_data))

}
