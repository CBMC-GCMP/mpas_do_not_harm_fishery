library(dafishr)
library(dplyr)
library(ggplot2)
library(future.apply)
library(purrr)
library(sf)

## Download data from 2022
vms_download(2022)

## Create a list of files to process
files <- list.files("VMS-data/", recursive = T, pattern = ".csv", full.names = T)


## Set up a parallel session 
plan(multisession, workers = 10)

## Use the preprocessing_vms function wrapper
future_lapply(files, preprocessing_vms, future.seed = NULL)

## Load a file to check 
vms <- fst::read_fst("preprocessed/vms_2022_1_1_15_preprocessed.fst")
glimpse(vms)
unique(vms$mpa_decree)

files_preprocessed <- list.files("preprocessed/", recursive = T, pattern = ".fst", full.names = T)

## Create modeled files
vms_modeled  <- map_dfr(files_preprocessed, 
                        function(x)
                              fst::read_fst(x) |>
                              mutate(RNP = as.character(RNP)) |> 
                              model_vms())

## Plot to observe
vms_modeled |> 
      filter(vessel_state == "hauling") |> 
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
      ggplot() +
      geom_sf(pch = ".") +
      geom_sf(data = dafishr::all_mpas, fill = NA, col = "red") +
      coord_sf(xlim = c(min(vms_modeled$longitude), max(vms_modeled$longitude)), 
               ylim = c(min(vms_modeled$latitude), max(vms_modeled$latitude)))
