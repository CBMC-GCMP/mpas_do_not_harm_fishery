## This script shows how area used data are created from the preprocessed modelled files,
# all the intermediate data are available by the authors under reasonable request, or can be
# reproduced independently following the README instruction at the main repository page. 


# Loading libraries -------------------------------------------------------

library(tidyverse)
library(dafishr)
library(furrr)
library(fst)

## Load dataset
load("outputs/month_vessel_hrs.RData")

sf::sf_use_s2(FALSE) ## Setting spatial options

## Loading an eastern pacific shapefile to filter out area use
estpa <- sf::st_read("data/eastern_pacific.gpkg")

## Shapefile of the Mexican EEZ
EEZ <- dafishr::mx_eez_pacific

## Revillagigedo MPA polygon from the dafishr package
Revimpa <- dafishr::all_mpas |> 
      filter(str_detect(NOMBRE, "Revillagigedo"))


## Here I create a list of the data that were preprocessed and modeled according to instruction and stored in a data folder. 
file_list <- list.files("data/", pattern = ".fst", full.names = T)


permits <- dafishr::pelagic_vessels_permits |> 
      mutate(str_split(vessel_name, "\\(")) |> 
      pull(vessel_name) |> 
      unique()

revi_list <- month_vessel_hrs |> 
      filter(year > 2005) |> 
      filter(str_detect(zone, "Revillagigedo")) |> 
      pull(vessel_name) |> 
      unique()

plan(multisession, gc = TRUE, workers = 10)


rasteringmonths <- function(x) {
      out <- return(tryCatch(
            {
                  
                  sf::sf_use_s2(FALSE)
                  
                  torast <- read_fst(x) %>% 
                        mutate(RNP = as.character(RNP)) %>% 
                        filter(vessel_state == "hauling") %>% 
                        filter(vessel_name %in% revi_list) %>% 
                        mutate(hour = lubridate::hour(date)) %>% 
                        group_by(year, month, day, hour, longitude, latitude, vessel_name) %>%
                        summarise(latitude = mean(latitude), longitude = mean(longitude))  %>% 
                        ungroup() %>% 
                        mutate(date = as.Date(paste0(year, "-", month, "-", max(day)), "%Y-%m-%d")) %>% 
                        group_by(date, longitude, latitude) %>%
                        summarise(hrs = n(), vessels = n_distinct(vessel_name)) %>%
                        sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
                        sf::st_join(., estpa) %>%
                        filter(!is.na(Ocean_Area)) %>%
                        sf::st_join(., EEZ) %>%
                        as.data.frame() %>%
                        select(-geometry, -Name) %>%
                        mutate(longitude = round(longitude, 5), latitude = round(latitude, 5)) %>%
                        group_by(date, longitude, latitude) %>%
                        summarise(hrs = sum(hrs), vessels = sum(vessels)) %>%
                        mutate(effort = hrs/vessels)
                  
                  ext <- raster::extent(-155, -75, -12, 35)
                  gridsize <- 0.1
                  r <- raster::raster(ext, res = gridsize)
                  
                  torast <- sf::st_as_sf(torast, coords = c("longitude", "latitude"), crs = 4326)
                  
                  raster_res <- raster::rasterize(torast,
                                                  r,
                                                  'effort',
                                                  fun = sum)
                  
                  raster::crs(raster_res) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
                  raster::writeRaster(raster_res, 
                                      filename = paste0("rasters/", "raster_revilla_", unique(torast$date), "_hrs.tiff"),
                                      format = "GTiff",
                                      overwrite = TRUE)
                  
                  tot_area_raster <- function(r) {
                        cell_size <- raster::area(r, na.rm = TRUE, weights = FALSE)
                        #delete NAs from vector of all raster cells
                        ##NAs lie outside of the rastered region, can thus be omitted
                        cell_size <- cell_size[!is.na(cell_size)]
                        #compute area [km2] of all cells in geo_raster
                        raster_area <- length(cell_size) * median(cell_size)
                        print(round(raster_area, digits = 1))
                  }
                  
                  result <- data.frame(
                        date = unique(torast$date),
                        area = tot_area_raster(raster_res))
                  
                  print(result)
                  
            },
            error = function(cond) {
                  message(paste("Can't calculate area in ", x, "\n"))
                  result <- data.frame(
                        date = unique(torast$date),
                        area = NA)
                  # Choose a return value in case of error
                  return(result)
            }
      )
      )
      return(out)
}


areas <- future.apply::future_lapply(file_list, rasteringmonths)
beepr::beep()

areas_results_revi <- do.call(rbind, areas) |> 
      mutate(revi = "Yes")



rasteringmonths <- function(x) {
      out <- return(tryCatch(
            {
                  
                  sf::sf_use_s2(FALSE)
                  
                  torast <- read_fst(x) %>% 
                        mutate(RNP = as.character(RNP)) %>% 
                        filter(vessel_state == "hauling") %>% 
                        filter(vessel_name %in% permits) %>% 
                        filter(!vessel_name %in% revi_list) %>% 
                        mutate(hour = lubridate::hour(date)) %>% 
                        group_by(year, month, day, hour, longitude, latitude, vessel_name) %>%
                        summarise(latitude = mean(latitude), longitude = mean(longitude))  %>% 
                        ungroup() %>% 
                        mutate(date = as.Date(paste0(year, "-", month, "-", max(day)), "%Y-%m-%d")) %>% 
                        group_by(date, longitude, latitude) %>%
                        summarise(hrs = n(), vessels = n_distinct(vessel_name)) %>%
                        sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
                        sf::st_join(., estpa) %>%
                        filter(!is.na(Ocean_Area)) %>%
                        sf::st_join(., EEZ) %>%
                        as.data.frame() %>%
                        select(-geometry, -Name) %>%
                        mutate(longitude = round(longitude, 5), latitude = round(latitude, 5)) %>%
                        group_by(date, longitude, latitude) %>%
                        summarise(hrs = sum(hrs), vessels = sum(vessels)) %>%
                        mutate(effort = hrs/vessels)
                  
                  ext <- raster::extent(-155, -75, -12, 35)
                  gridsize <- 0.1
                  r <- raster::raster(ext, res = gridsize)
                  
                  torast <- sf::st_as_sf(torast, coords = c("longitude", "latitude"), crs = 4326)
                  
                  raster_res <- raster::rasterize(torast,
                                                  r,
                                                  'effort',
                                                  fun = sum)
                  
                  raster::crs(raster_res) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
                  raster::writeRaster(raster_res, 
                                      filename = paste0("rasters/", "raster_notrevilla_", unique(torast$date), "_hrs.tiff"),
                                      format = "GTiff",
                                      overwrite = TRUE)
                  
                  tot_area_raster <- function(r) {
                        cell_size <- raster::area(r, na.rm = TRUE, weights = FALSE)
                        #delete NAs from vector of all raster cells
                        ##NAs lie outside of the rastered region, can thus be omitted
                        cell_size <- cell_size[!is.na(cell_size)]
                        #compute area [km2] of all cells in geo_raster
                        raster_area <- length(cell_size) * median(cell_size)
                        print(round(raster_area, digits = 1))
                  }
                  
                  result <- data.frame(
                        date = unique(torast$date),
                        area = tot_area_raster(raster_res))
                  
                  print(result)
                  
            },
            error = function(cond) {
                  message(paste("Can't calculate area in ", x, "\n"))
                  result <- data.frame(
                        date = unique(torast$date),
                        area = NA)
                  # Choose a return value in case of error
                  return(x)
            }
      )
      )
      return(out)
}


areas <- future.apply::future_lapply(file_list[2:430], rasteringmonths)
beepr::beep()

areas_results_notrevi <- do.call(rbind, areas) |> 
      mutate(revi = "No")


areas_results <- bind_rows(areas_results_revi, areas_results_notrevi)



save(object = areas_results, file = "outputs/area_results.RDS") 