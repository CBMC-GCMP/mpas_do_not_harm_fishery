
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(CausalImpact)


# Data upload -------------------------------------------------------------

permits <- dafishr::pelagic_vessels_permits|> 
      mutate(str_split(vessel_name, "\\("))|> 
      pull(vessel_name)|>
      unique()

vessels_list <- readRDS("outputs/month_vessel_hrs.RDS")|>
      pull(vessel_name)|>
      unique()

revi_list <- readRDS("outputs/month_vessel_hrs.RDS")|>
      filter(str_detect(zone, "Revillagigedo"))|>
      pull(vessel_name)|>
      unique()

load(file = "outputs/area_results.RDS")

landings <- readRDS("outputs/pacific_landings.RDS") 

landings[c('First_Name', 'Second_Name')] <- str_split_fixed(landings$vessel_name, "\\(", n = 2)


# Fishing activity inside Revillagigedo polygon (Figure 1A) ----------------------------------------------------------------

revispatial <- readRDS("outputs/month_vessel_hrs.RDS")|> 
      filter(str_detect(zone, "Revillagigedo"))|> 
      group_by(year, month, vessel_name)|>
      summarise(hrs = sum(hrs))|>
      group_by(year, month)|> 
      summarise(vessel = n_distinct(vessel_name), hrs = sum(hrs))|>
      mutate(effort = hrs/vessel, 
             date = as.Date(paste0(year, "-", month, "-01"), "%Y-%m-%d"))

x <- revispatial|> 
      as.data.frame()|>
      mutate(time = 1:length(date))

tomtx <- x|>ungroup()|>dplyr::select(effort, time)

revi_CI_mtx <- as.ts(as.matrix(tomtx), frequency = 1)


dates <- x|>ungroup()|> pull(date)

tomod <- zoo(revi_CI_mtx, dates)
tomod

tt <- seq(min(dates), max(dates), "month")

tomod <- merge(tomod, zoo(, tt), fill = 0)

tomod$time <- 1:length(tomod$time)
tomod$effort <- (tomod$effort)
pre.period <- as.Date(c("2008-01-01", "2017-11-01"))
post.period <- as.Date(c("2017-12-01", "2021-12-31"))


impact <- CausalImpact(tomod, pre.period, post.period, 
                       model.args = list(niter = 1000, nseasons = 30, season.duration = 2))

(revilla_hrs <- plot(impact) +
            labs(subtitle = "Fishing activity inside the Revillagigedo polygon", y = "") +
            scale_x_date(breaks = "1 year", date_labels = "%Y") +
            theme(panel.grid = element_blank(), 
                  text = element_text(size=11),
                  strip.background = element_rect(color = NA, fill = NA)))


summary(impact, "report")


# CPUE Revillagigedo fleet (Figure 1B) ----------------------------------------------------------------

x <- landings|> 
      ungroup()|>
      dplyr::select(date, vessel_name = First_Name, catch, days_declared)|>
      mutate(vessel_name = str_trim(vessel_name))|>
      filter(vessel_name %in% permits)|> 
      mutate(revi = ifelse(vessel_name %in% revi_list, "Yes", "No"))|> 
      filter(date >= "2008-01-01")|>
      mutate(year = lubridate::year(date), month = lubridate::month(date))|> 
      mutate(effort = catch/days_declared) |> 
      group_by(year, month, revi)|> 
      summarise(fishing_effort = mean(effort/1000))|>
      mutate(date = as.Date(paste0(year, "-", month, "-01"), "%Y-%m-%d")) |> 
      as.data.frame()|>
      mutate(time = 1:336) |> 
      filter(revi == "Yes") |> 
      ungroup() |> 
      dplyr::select(-revi)



tomtx <- x|>ungroup()|>dplyr::select(fishing_effort, time)

revi_CI_mtx <- as.ts(as.matrix(tomtx), frequency = 1)


dates <- x|>ungroup()|> pull(date)

tomod <- zoo(revi_CI_mtx, dates)
tomod

tt <- seq(min(dates), max(dates), "month")

#tomod <- merge(tomod, zoo(, tt), fill = 0)

tomod$time <- 1:length(tomod$time)
tomod$area <- (tomod$area)
pre.period <- as.Date(c("2008-01-01", "2017-11-01"))
post.period <- as.Date(c("2017-12-01", "2021-12-31"))


impact <- CausalImpact(tomod, pre.period, post.period, 
                       model.args = list(niter = 1000, nseasons = 30, season.duration = 2))

(revilla <- plot(impact) +
            #scale_x_date(breaks = "1 year", date_labels = "%Y") +
            labs(subtitle = "CPUE - Historically active in MPA polygon", y = "") +
            scale_x_date(breaks = "1 year", date_labels = "%Y") +
            theme(panel.grid = element_blank(), 
                  text = element_text(size=11),
                  strip.background = element_rect(color = NA, fill = NA)))

summary(impact, "report")


# CPUE Not Revillagigedo fleet (Figure 1B) ----------------------------------------------------------------

x <-landings|> 
      ungroup()|>
      dplyr::select(date, vessel_name = First_Name, catch, days_declared)|>
      mutate(vessel_name = str_trim(vessel_name))|>
      filter(vessel_name %in% permits)|> 
      mutate(revi = ifelse(vessel_name %in% revi_list, "Yes", "No"))|> 
      mutate(year = lubridate::year(date), month = lubridate::month(date))|> 
      mutate(effort = catch/days_declared) |> 
      group_by(year, month, revi)|> 
      summarise(fishing_effort = mean(effort/1000))|>
      mutate(date = as.Date(paste0(year, "-", month, "-01"), "%Y-%m-%d")) |> 
      as.data.frame()|>
      mutate(time = 1:336) |> 
      filter(revi == "No") |> 
      ungroup() |> 
      dplyr::select(-revi)



tomtx <- x|>ungroup()|>dplyr::select(fishing_effort, time)

revi_CI_mtx <- as.ts(as.matrix(tomtx), frequency = 1)


dates <- x|>ungroup()|> pull(date)

tomod <- zoo(revi_CI_mtx, dates)
tomod

tt <- seq(min(dates), max(dates), "month")

#tomod <- merge(tomod, zoo(, tt), fill = 0)

tomod$time <- 1:length(tomod$time)
tomod$area <- (tomod$area)
pre.period <- as.Date(c("2008-01-01", "2017-11-01"))
post.period <- as.Date(c("2017-12-01", "2021-12-31"))


impact <- CausalImpact(tomod, pre.period, post.period, 
                       model.args = list(niter = 1000, nseasons = 30, season.duration = 2))

(revilla <- plot(impact) +
            #scale_x_date(breaks = "1 year", date_labels = "%Y") +
            labs(subtitle = "CPUE - Not historically active in MPA polygon", y = "") +
            scale_x_date(breaks = "1 year", date_labels = "%Y") +
            theme(panel.grid = element_blank(), 
                  text = element_text(size=11),
                  strip.background = element_rect(color = NA, fill = NA)))


summary(impact, "report")


# Area used Figure 1C Revillagigedo fleet -------------------------------------------------------------------

x <- areas_results|> 
      filter(revi == "yes") |> 
      mutate(year = lubridate::year(date), month = lubridate::month(date))|>
      group_by(year, month)|>
      summarise(area = sum(area)/1000)|>
      mutate(date = as.Date(paste0(year, "-", month, "-01"), "%Y-%m-%d"))|>
      as.data.frame()|>
      mutate(time = 1:164)

tomtx <- x|>ungroup()|>dplyr::select(area, time)

revi_CI_mtx <- as.ts(as.matrix(tomtx), frequency = 1)


dates <- x|>ungroup()|> pull(date)

tomod <- zoo(revi_CI_mtx, dates)
tomod

tt <- seq(min(dates), max(dates), "month")

#tomod <- merge(tomod, zoo(, tt), fill = 0)

tomod$time <- 1:length(tomod$time)
tomod$area <- (tomod$area)
pre.period <- as.Date(c("2008-01-01", "2017-11-01"))
post.period <- as.Date(c("2017-12-01", "2021-12-31"))


impact <- CausalImpact(tomod, pre.period, post.period, 
                       model.args = list(niter = 1000, nseasons = 30, season.duration = 2))

(revilla <- plot(impact) +
            #scale_x_date(breaks = "1 year", date_labels = "%Y") +
            labs(subtitle = "Area used by vessels historically in MPA polygon", y = "") +
            scale_x_date(breaks = "1 year", date_labels = "%Y") +
            theme(panel.grid = element_blank(), 
                  text = element_text(size=11),
                  strip.background = element_rect(color = NA, fill = NA)))

summary(impact, "report")

# Area used Figure 1C Not Revillagigedo fleet -------------------------------------------------------------------

x <- areas_results|> 
      filter(revi == "no") |> 
      mutate(year = lubridate::year(date), month = lubridate::month(date))|>
      group_by(year, month)|>
      summarise(area = sum(area)/1000)|>
      mutate(date = as.Date(paste0(year, "-", month, "-01"), "%Y-%m-%d"))|>
      as.data.frame()|>
      mutate(time = 1:164)

tomtx <- x|>ungroup()|>dplyr::select(area, time)

revi_CI_mtx <- as.ts(as.matrix(tomtx), frequency = 1)


dates <- x|>ungroup()|> pull(date)

tomod <- zoo(revi_CI_mtx, dates)
tomod

tt <- seq(min(dates), max(dates), "month")

#tomod <- merge(tomod, zoo(, tt), fill = 0)

tomod$time <- 1:length(tomod$time)
tomod$area <- (tomod$area)
pre.period <- as.Date(c("2008-01-01", "2017-11-01"))
post.period <- as.Date(c("2017-12-01", "2021-12-31"))


impact <- CausalImpact(tomod, pre.period, post.period, 
                       model.args = list(niter = 1000, nseasons = 30, season.duration = 2))

(revilla <- plot(impact) +
            #scale_x_date(breaks = "1 year", date_labels = "%Y") +
            labs(subtitle = "Area used by vessels that did not historically in MPA polygon", y = "") +
            scale_x_date(breaks = "1 year", date_labels = "%Y") +
            theme(panel.grid = element_blank(), 
                  text = element_text(size=11),
                  strip.background = element_rect(color = NA, fill = NA)))


summary(impact, "report")
