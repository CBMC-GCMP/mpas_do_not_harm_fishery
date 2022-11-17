Supplementary information for – The largest fully protected marine area
in North America does not harm industrial fishing
================

The repository store code to support findings for a submitted manuscript
titled “The largest fully protected marine area in North America does
not harm industrial fishing”. We provide here instruction to replicate
the analysis over sample raw data and reproduce the results presented in
the manuscript and in the supplementary materials. If reproducing the
results using cleaned datasets is the main interest the first section
can be skipped.

## Replicate the analysis

Raw data from the Mexican Vessel Monitoring system are available through
the “[Datos
Abiertos](https://datos.gob.mx/busca/dataset/localizacion-y-monitoreo-satelital-de-embarcaciones-pesqueras)”
initiative and can be downloaded and wrangled using the `dafishr`
package. Here, we provide a quick intro.

### Step 1. Row data download

First there are some packages that needs to be installed:

``` r
library(dafishr)
library(readr)
library(dplyr)
library(ggplot2)
library(future.apply)
library(purrr)
library(sf)
library(CausalImpact)
```

Once installed the packages, we can use `dafishr` functions to download
the last set of data.

``` r
vms_download(2022)
```

The `vms_download` function will create a folder in the working
directory called `VMS-data` that contains a series of `.csv` files from
the Mexican SISMEP data from 2022. These data are raw information on
geolocation of the industrial vessels. Different years can be selected
at once using a vector inside the function. You can find more
information on dafishr documentation `help(vms_download)`.

We can take a look at one of these files.

``` r
glimpse(read_csv('VMS-data/RLMSEP_2022/01. ENERO/01-15 ENE 2022.csv'))
```

    Rows: 525,478
    Columns: 9
    $ Nombre                          <chr> "12 DE DICIEMBRE I", "12 DE DICIEMBRE …
    $ RNP                             <dbl> 54213, 54213, 54213, 54213, 54213, 542…
    $ `Puerto Base`                   <chr> "SALINA CRUZ", "SALINA CRUZ", "SALINA …
    $ `Permisionario o Concesionario` <chr> "PESQUERA PERLA DE SALINA CRUZ, S.A. D…
    $ Fecha                           <chr> "01/01/2022 00:04", "01/01/2022 01:04"…
    $ Latitud                         <dbl> 16.17215, 16.17217, 16.17217, 16.17213…
    $ Longitud                        <dbl> -95.19392, -95.19390, -95.19390, -95.1…
    $ Velocidad                       <chr> "0", "0", "0", "0", "0", "0", "0", "0"…
    $ Rumbo                           <chr> "0", "0", "0", "0", "0", "0", "0", "0"…

The columns are:

- `Nombre` = name of the vessel

- `RNP` = unique vessel registration code

- `Puerto base` = base port where vessels report catch

- `Permisionario o Concesionario` = permit owner name or company name

- `Fecha` = is the date of each geoposition

- `Latitud` = is the WGS83 (4326) latitudinal degree

- `Longitud` = is the WGS83 (4326) longitudinal degree

- `Velocidad` = is the speed of the vessel at the time recorded

- `Rumbo` = is the direction of navigation of the vessel in degrees

There are some evident parsing issues in speed and direction column, all
these are considered and corrected in the pre-processing phase, as well
as other errors.

The pre-processing goes through a series of
[steps](https://cbmc-gcmp.github.io/dafishr/articles/dafisr.html#downloading-raw-data-on-your-computer)
that are wrapped inside a unique `preprocessing_vms` function. For
simplicity, we use that wrapper here.

The function can be used on a single file, but we can loop it using
`lapply` or even better use a parallel approach:

``` r
## Create a list of files to process
files <- list.files("VMS-data/", recursive = T, pattern = ".csv", full.names = T)

## Set up a parallel session 
plan(multisession, workers = 2) ## Set Cores according to laptop characteristics

## Use the preprocessing_vms function wrapper
future_lapply(files, preprocessing_vms, future.seed = NULL)
```

The `future_lapply` function will loop the `preprocessing_vms` function
on the list of files that were downloaded in the `VMS-data` folder and
save cleaned files in a `.fst` format (see specs
[here](https://www.fstpackage.org/)) in a preprocessed folder that is
automatically created in the working directory.

If you apply the preprocessing on the full scale of the VMS data (from
2008 to 2022) it will take a long time to process on a personal
computer. However, the approach of keeping small chunks of data
separated allows to complete all the analysis without the need of a
significant computing power.

We can load a file to check the results:

``` r
## Load a file to check 
vms <- fst::read_fst("preprocessed/vms_2022_1_1_15_preprocessed.fst")

glimpse(vms)
```

    Rows: 488,781
    Columns: 20
    $ id           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
    $ year         <dbl> 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 202…
    $ month        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    $ day          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    $ date         <dttm> 2022-01-01 00:04:00, 2022-01-01 01:04:00, 2022-01-01 02:…
    $ vessel_name  <chr> "12 DE DICIEMBRE I", "12 DE DICIEMBRE I", "12 DE DICIEMBR…
    $ RNP          <dbl> 54213, 54213, 54213, 54213, 54213, 54213, 54213, 54213, 5…
    $ port_base    <chr> "SALINA CRUZ", "SALINA CRUZ", "SALINA CRUZ", "SALINA CRUZ…
    $ owner        <chr> "PESQUERA PERLA DE SALINA CRUZ, S.A. DE C.V.", "PESQUERA …
    $ latitude     <dbl> 16.17215, 16.17217, 16.17217, 16.17213, 16.17217, 16.1721…
    $ longitude    <dbl> -95.19392, -95.19390, -95.19390, -95.19390, -95.19390, -9…
    $ speed        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    $ direction    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    $ file_name    <chr> "VMS-data//RLMSEP_2022/01. ENERO/01-15 ENE 2022.csv", "VM…
    $ location     <chr> "port_visit", "port_visit", "port_visit", "port_visit", "…
    $ zone         <chr> "open area", "open area", "open area", "open area", "open…
    $ mpa_decree   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ state        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ municipality <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ region       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

Column names are now in English, the parsing is now as expected and
several other columns are added:

- `file_name` = the name of the raw file which is useful for future
  references and bug/errors detection

- `location` = has two levels “port_visit”, “at_sea”, meaning that the
  vessels was at port or navigating at sea respectively. Information on
  how this is obtained can be found
  [here](https://cbmc-gcmp.github.io/dafishr/reference/join_ports_locations.html).

- `zone` = can be “open area” meaning that the vessel was outside an MPA
  polygon, or it can be a name of an MPA where the vessels was
  operating. The intersection method with all the MPA in Mexico can be
  found
  [here](https://cbmc-gcmp.github.io/dafishr/reference/join_mpa_data.html).

- `mpa_decree` = the decree of the MPA is coded as “NA” if absent, “PN”
  for National Park, “PMN” for National Marine Park, “RB” Biosphere
  reserve, “APFF” Area of Protection for the Flora and Fauna.

- `state` = the state that the MPA belongs to administratively

- `municipality` = the municipality that the MPA belongs to
  administratively

- `region` = the region that the MPA belongs to administratively

We use this intersection to discriminate vessels that historically were
fishing in Revillagigedo and vessels that did not.

### Modeling VMS data

Now that the preprocessing is over, we can model VMS data according to
the speed to have a sense of where vessels were probably fishing.
Modeling details can be found
[here](https://cbmc-gcmp.github.io/dafishr/reference/model_vms.html) and
the full code
[here](https://github.com/CBMC-GCMP/dafishr/blob/HEAD/R/model_vms.R).

Here, we first create a list of all the preprocessed files and then we
loop using `map_dfr` function from the `purrr` package the `model_vms`
function. We add an additional parsing rule to the `RNP` column as when
reading back some of the files it is sometimes parsed as character
drawing an error in the final merge.

The approach can also be parallelized using `furrr` version of the
function: `future_map_dfr`.

``` r
## Create a list of all the files that were preprocessed

files_preprocessed <- list.files("preprocessed/", recursive = T, pattern = ".fst", full.names = T)

## Model files and create a new data frame
vms_modeled  <- map_dfr(files_preprocessed, 
                        function(x)
                              fst::read_fst(x) |>
                              mutate(RNP = as.character(RNP)) |> 
                              model_vms())
```

We can now create a plot of the results to see where the vessels were
probably fishing.

``` r
## Plot to observe
vms_modeled |> 
      filter(vessel_state == "hauling") |> 
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
      ggplot() +
      geom_sf(pch = ".") +
      geom_sf(data = dafishr::all_mpas, fill = NA, col = "red") +
      coord_sf(xlim = c(min(vms_modeled$longitude), max(vms_modeled$longitude)), 
               ylim = c(min(vms_modeled$latitude), max(vms_modeled$latitude)))
```

## Reproduce the results

The road to raw data to processed files on all the historical data is
long and we are available for questions and details on to further
proceed if you want to fully reproduce all the details. Beware, however,
that all significant steps are presented above and now we follow up on
the final steps of the analysis and results production using
intermediate datasets that we make available.
