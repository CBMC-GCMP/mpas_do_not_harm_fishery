library(tidyverse)
library(fst)
library(dafishr)
library(furrr)
library(ggspatial)


sf::sf_use_s2(FALSE)
estpa <- sf::st_read("data/eastern_pacific.gpkg")
EEZ <- dafishr::mx_eez_pacific
Revimpa <- dafishr::all_mpas %>% 
      filter(str_detect(NOMBRE, "Revillagigedo"))

file_list <- list.files("data/", pattern = ".fst", full.names = T)

permits <- readRDS("metadata/pelagic_vessels_permits.RDS") %>%  
      mutate(str_split(vessel_name, "\\(")) %>%  
      pull(vessel_name) %>% 
      unique()

revi_list <- readRDS("outputs/montlhy_vessel_hrs.RDS") %>% 
      filter(year > 2005) %>% 
      filter(str_detect(zone, "Revillagigedo")) %>% 
      pull(vessel_name) %>% 
      unique()

plan(multisession, gc=TRUE, workers = 10)

before <- future_map_dfr(file_list[177:322], function(x) {
      sf::sf_use_s2(FALSE)
      read_fst(x) %>% 
            mutate(RNP = as.character(RNP)) %>% 
            filter(vessel_state == "hauling") %>% 
            filter(vessel_name %in% permits) %>%
            filter(!vessel_name %in% revi_list) %>% 
            mutate(hour = lubridate::hour(date)) %>% 
            group_by(year, month, day, hour, longitude, latitude, vessel_name) %>%
            summarise(latitude = mean(latitude), longitude = mean(longitude))  %>% 
            filter(year < 2018) %>% 
            filter(year >= 2009) %>%  
            group_by(longitude, latitude) %>%  
            summarise(hrs = n(), vessels = n_distinct(vessel_name)) %>% 
            sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
            sf::st_join(., estpa) %>%
            filter(!is.na(Ocean_Area)) %>%
            sf::st_join(., EEZ) %>%
            as.data.frame() %>%
            select(-geometry, -Name)
})



ext <- raster::extent(-155, -75, -12, 35)
gridsize <- 0.1
r <- raster::raster(ext, res = gridsize)

torast_before <- before %>% 
      mutate(longitude = round(longitude, 5), latitude = round(latitude, 5)) %>%  
      group_by(longitude, latitude) %>% 
      summarise(hrs = sum(hrs/24), vessels = sum(vessels)) %>%  
      mutate(effort = hrs/vessels)

torast_before_sf <- sf::st_as_sf(torast_before, coords = c("longitude", "latitude"), crs = 4326)

raster_before <- raster::rasterize(torast_before_sf, 
                                   r,
                                   'effort', 
                                   fun = sum)

raster::plot(raster_before)
beepr::beep()


# convert to a df for plotting in two steps,
# First, to a SpatialPointsDataFrame
tot_p <- raster::rasterToPoints(raster_before, spatial = TRUE)
# Then to a 'conventional' dataframe
tot_p  <- data.frame(tot_p)|>  
      mutate(time = "before")


p1 <- ggplot() +
      geom_tile(data = tot_p, aes(x = x, y = y, col = log1p(layer), fill = log1p(layer)), alpha = 0.9) +
      geom_sf(data = EEZ, fill = NA, color = "black") +
      coord_sf() +
      scale_fill_gradient2(name = "log(Fishing Effort + 1)", 
                           high = "#ef8a62", 
                           mid = "gray80",
                           guide = guide_colourbar(direction = "horizontal",
                                                   title.position = "top")) +
      scale_color_gradient2(name = "log(Fishing Effort + 1)", 
                            high = "#ef8a62", 
                            mid = "gray80",
                            guide = guide_colourbar(direction = "horizontal",
                                                    title.position = "top")) +
      theme_void() +
      theme(legend.position = "bottom")

# AFTER  ------------------------------------------------------------------


after <- future_map_dfr(file_list[323:430], function(x) {
      sf::sf_use_s2(FALSE)
      read_fst(x) %>% 
            mutate(RNP = as.character(RNP)) %>% 
            filter(vessel_state == "hauling") %>% 
            filter(vessel_name %in% permits) %>%
            filter(!vessel_name %in% revi_list) %>% 
            mutate(hour = lubridate::hour(date)) %>% 
            group_by(year, month, day, hour, longitude, latitude, vessel_name) %>%
            summarise(latitude = mean(latitude), longitude = mean(longitude))  %>% 
            group_by(longitude, latitude) %>%  
            summarise(hrs = n(), vessels = n_distinct(vessel_name)) %>% 
            sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
            sf::st_join(., estpa) %>%
            filter(!is.na(Ocean_Area)) %>%
            sf::st_join(., EEZ) %>%
            as.data.frame() %>%
            select(-geometry, -Name)
})


ext <- raster::extent(-155, -75, -12, 35)
gridsize <- 0.1
r <- raster::raster(ext, res = gridsize)

torast_after <- after %>% 
      mutate(longitude = round(longitude, 5), latitude = round(latitude, 5)) %>%  
      group_by(longitude, latitude) %>% 
      summarise(hrs = sum(hrs/24), vessels = sum(vessels)) %>%  
      mutate(effort = hrs/vessels)

torast_after_sf <- sf::st_as_sf(torast_after, coords = c("longitude", "latitude"), crs = 4326)

raster_after <- raster::rasterize(torast_after_sf, 
                                  r,
                                  'effort', 
                                  fun = sum)

# convert to a df for plotting in two steps,
# First, to a SpatialPointsDataFrame
tot_after <- raster::rasterToPoints(raster_after, spatial = TRUE)
# Then to a 'conventional' dataframe
tot_after  <- data.frame(tot_after) |>  
      mutate(time = "after")


p2 <- ggplot() +
      geom_tile(data = tot_after, aes(x = x, y = y, col = log1p(layer), fill = log1p(layer)), alpha = 0.9) +
      geom_sf(data = EEZ, fill = NA, color = "black") +
      geom_sf(data = Revimpa, fill = NA, col = "black", linetype = 2, alpha = .2) +
      coord_sf() +
      scale_fill_gradient2(name = "log(Fishing Effort + 1)", 
                           high = "#ef8a62", 
                           mid = "gray80",
                           guide = guide_colourbar(direction = "horizontal",
                                                   title.position = "top")) +
      scale_color_gradient2(name = "log(Fishing Effort + 1)", 
                            high = "#ef8a62", 
                            mid = "gray80",
                            guide = guide_colourbar(direction = "horizontal",
                                                    title.position = "top")) +
      theme_void() +
      theme(legend.position = "bottom")
