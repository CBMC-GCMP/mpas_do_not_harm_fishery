## This scripts shows how monthly data are created from the preprocessed modelled files,
# all the intermediate data are available by the authors under reasonable request, or can be
# reproduced independently following the README instruction at the main repository page. 

# Loading libraries
library(tidyverse)
library(fst)
library(dafishr)

# Some spatial settings to aid intersection
sf::sf_use_s2(FALSE)

# Loading a polygon that allows filtering by the eastern pacific
estpa <- sf::st_read("data/eastern_pacific.gpkg")

# Loading the Mexican EEZ polygon
EEZ <- dafishr::mx_eez_pacific

# This creates a list of files that should correspond to the preprocessed and modeled data on your end. For me it was a data folder. 
file_list <- list.files("data/", pattern = ".fst", full.names = T)


# Loop function to create and clean data
total <- map_dfr(file_list, function(x) { 
      read_fst(x) %>% 
            mutate(RNP = as.character(RNP)) %>% 
            filter(vessel_state == "hauling") %>% 
            mutate(hour = lubridate::hour(date)) %>% 
            sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
            sf::st_join(., estpa) %>% 
            filter(!is.na(Ocean_Area)) %>% 
            sf::st_join(., EEZ) %>% 
            as.data.frame() %>% 
            mutate(zone = ifelse(zone == "open area" & Name == "Mexican Pacific Exclusive Economic Zone", "EEZ", zone)) %>% 
            mutate(zone = ifelse(is.na(zone), "High seas", zone)) %>% 
            select(year, month, day, hour, vessel_name, zone) %>%
            distinct() %>%
            unique() %>%
            group_by(year, month, vessel_name, zone) %>%
            summarise(hrs = n()) |> 
            filter(year > 2000)
})

## Saving the final data
saveRDS(total, "outputs/montlhy_vessel_hrs.RDS")