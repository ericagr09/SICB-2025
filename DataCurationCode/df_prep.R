

## Load packages ----
  pacman::p_load(tidyverse, lubridate, here)

## Load data ----

  # nestling encounter data: one row per encounter (some nestlings have multiple encounters)
    d_enc <- read.delim(here::here("RawData", "encounters.txt"))

  # weather data from ithaca airport (one row per hour)
    # provided by Northeast regional climate center
    d_weather <- read.delim(here::here("RawData", "ithaca_airport_weather.txt"))
    
## Wrangle data ----
    
    # the weather data needs to be cleaned up a bit
        d_weather$date <- as.Date(d_weather$date, format = "%m/%d/%y")
        d_weather$yday <- yday(d_weather$date)
        d_weather$temp_C <- (d_weather$temp_F - 32) * 5/9
        d_weather$year <- year(d_weather$date)
        d_weather$yr_day_hr <- paste(d_weather$year, d_weather$yday, d_weather$hour, sep = "_")
        
    # filter down to field season and years needed so it will run faster
        d_weather <- d_weather %>%
          filter(year > 2012, yday > 120, yday < 200) %>%
          select(date, hour, yday, precip_inch, temp_C, year, yr_day_hr)
        
## Add temperature data to encounters ----
     
      # There isn't anything tricky here, but you'll want to adust to whatever date ranges
        # and times you want included for different temperature metrics (e.g., 3 days prior,
        # daytime only, morning of capture, min/max/average, etc).
        
    for(i in 1:nrow(d_enc)){
      # make a subset of temperature data that matches desired range from encounter - average daytime temp 
        sub1 <- d_weather %>% filter(yday == d_enc$encounter_doy[i], year == d_enc$exp_year[i],
                                     hour >= 6, hour <= 20, is.na(temp_C) == FALSE)
        
      # make another subset for a different time period (today plus two prior days; daytime only)
        sub2 <- d_weather %>% filter(yday >= (d_enc$encounter_doy[i] - 2), yday <= d_enc$encounter_doy[i],
                                     year == d_enc$exp_year[i], hour >= 6, hour <= 20, is.na(temp_C) == FALSE)
        
      # use those subsets to assign avg/max/min/range of temperature to encounter as desired
        # note I'm adding a check to make sure there is temperature data available
          if(nrow(sub1) > 0){
            d_enc$avgC_capture_day[i] <- mean(sub1$temp_C)
            d_enc$maxC_capture_day[i] <- max(sub1$temp_C)
          }
            
          if(nrow(sub2) > 0){
            d_enc$avgC_3day[i] <- mean(sub1$temp_C)
          }
    }

## Filter data out to start initial visualizations
initial_vis <- d_enc %>%
  filter(
    !is.na(sex),
    !individual_treatment %in% c("Cort", "CORT", "cort", "Egg_CORT"),
    !nest_treatment %in% c("High", "Low", "CORT_XFoster"), 
    sex %in% c("Male", "Female"), 
    age == 12,
    stress_series_type %in% c("B_S_D", "B_S"))

write.csv(initial_vis, "IntermediateData/9-11-25_data.csv", row.names = FALSE)