#this is BmoreAQIApp
#Libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(zoo)
library(data.table)
library(ggmap)
library(ggplot2)
library(viridis)
library(ggiraph)
#library(ggplotlyExtra)
library(htmlwidgets)
library(plotly)
library(ggiraph)
library(tidycensus)
library(sf)
library(tigris)
library(tmap)
library(tmaptools)
library(Amelia)
library(spdep)  # For spatial analysis
library(spatialreg)  # For spatial regression
library(data.table)
library(lubridate)
library(forecast)

# Memory management function
gc()  # Force garbage collection at startup

# Set memory limits
options(shiny.maxRequestSize = 50*1024^2)  # Reduce max upload size to 50MB
options(shiny.fullstacktrace = FALSE)  # Reduce stack trace size
options(shiny.error = function() {})  # Suppress error messages in production
options(dplyr.summarise.inform = FALSE)  # Reduce dplyr messages

# Spatial Analysis Functions
gist_pollutant <- "aqi_pm25"
k_neighbors <- 3

# Function to calculate Getis-Ord Gi* statistic for a single day
calc_gi_for_day <- function(day_data) {
  day_data <- day_data[!is.na(day_data[[gist_pollutant]]), ]
  n_sites <- nrow(day_data)
  k <- min(k_neighbors, n_sites - 1)
  if (k < 1) {
    return(tibble(
      site = day_data$site,
      date = day_data$timestamp_day,
      gi_stat = NA_real_,
      social_vuln_category = day_data$social_vuln_category
    ))
  }
  coords <- data.frame(longitude = day_data$longitude, latitude = day_data$latitude)
  nb <- knn2nb(knearneigh(coords, k = k))
  nbw <- nb2listw(nb, style = "W")
  gi_stat <- as.numeric(localG(day_data[[gist_pollutant]], nbw))
  tibble(
    site = day_data$site,
    date = day_data$timestamp_day,
    gi_stat = gi_stat,
    social_vuln_category = day_data$social_vuln_category
  )
}

# Function to calculate spatial lag model for a single day
calc_slm_for_day <- function(day_data) {
  day_data <- day_data %>%
    filter(!is.na(aqi_pm25), !is.na(longitude), !is.na(latitude))
  if (nrow(day_data) < 4) {
    return(tibble(
      date = unique(day_data$timestamp_day),
      rho = NA_real_,
      rho_p = NA_real_,
      coef_social_vuln = NA_real_,
      coef_social_vuln_p = NA_real_,
      social_vuln_category = unique(day_data$social_vuln_category)
    ))
  }
  coords <- day_data[, c("longitude", "latitude")]
  nb <- knn2nb(knearneigh(coords, k = min(3, nrow(day_data)-1)))
  nbw <- nb2listw(nb, style = "W")
  model <- lagsarlm(aqi_pm25 ~ social_vuln_score, data = day_data, listw = nbw)
  tibble(
    date = unique(day_data$timestamp_day),
    rho = model$rho,
    rho_p = summary(model)$LR1.p.value,
    coef_social_vuln = coef(model)["social_vuln_score"],
    coef_social_vuln_p = summary(model)$Coef["social_vuln_score", "Pr(>|z|)"],
    social_vuln_category = unique(day_data$social_vuln_category)
  )
}

#Functions

# Function to calculate AQI from PM2.5
calculate_aqi_pm25 <- function(pm25) {
  if (is.na(pm25)) return(NA)
  if (pm25 <= 9.0) {
    return(50 * pm25 / 9.0)  # Good
  } else if (pm25 <= 35.4) {
    return(50 + (100 - 50) * (pm25 - 9.1) / (35.4 - 9.1))  # Moderate
  } else if (pm25 <= 55.4) {
    return(100 + (150 - 100) * (pm25 - 35.5) / (55.4 - 35.5))  # Unhealthy for Sensitive Groups
  } else if (pm25 <= 125.4) {
    return(150 + (200 - 150) * (pm25 - 55.5) / (125.4 - 55.5))  # Unhealthy
  } else if (pm25 <= 225.4) {
    return(200 + (300 - 200) * (pm25 - 125.5) / (225.4 - 125.5))  # Very Unhealthy
  } else if (pm25 <= 500.4) {
    return(300 + (500 - 300) * (pm25 - 225.5) / (500.4 - 225.5))  # Hazardous
  } else {
    return(NA)  # Above AQI range
  }
}

# Function to calculate AQI from PM10
calculate_aqi_pm10 <- function(pm10) {
  # Return NA if the input is NA or NaN
  if (is.na(pm10) || is.nan(pm10)) {
    return(NA)
  }
  if (pm10 <= 54) {
    return(50 * pm10 / 54)  # Good
  } else if (pm10 <= 154) {
    return(50 + (100 - 50) * (pm10 - 55) / (154 - 55))  # Moderate
  } else if (pm10 <= 254) {
    return(100 + (150 - 100) * (pm10 - 155) / (254 - 155))  # Unhealthy for Sensitive Groups
  } else if (pm10 <= 354) {
    return(150 + (200 - 150) * (pm10 - 255) / (354 - 255))  # Unhealthy
  } else if (pm10 <= 424) {
    return(200 + (300 - 200) * (pm10 - 355) / (424 - 355))  # Very Unhealthy
  } else {
    return(301)  # Hazardous
  }
}


# Function to calculate AQI from Ozone 
calculate_aqi_o3 <- function(o3) {
  if (is.na(o3)) {
    return(NA)  # Return NA if o3 is NA
  } else if (o3 <= 0.054) {
    return(50 * o3 / 0.054)  # Good
  } else if (o3 <= 0.070) {
    return(50 + (100 - 50) * (o3 - 0.055) / (0.070 - 0.055))  # Moderate
  } else if (o3 <= 0.085) {
    return(100 + (150 - 100) * (o3 - 0.071) / (0.085 - 0.071))  # Unhealthy for Sensitive Groups
  } else if (o3 <= 0.105) {
    return(150 + (200 - 150) * (o3 - 0.086) / (0.105 - 0.086))  # Unhealthy
  } else if (o3 <= 0.200) {
    return(200 + (300 - 200) * (o3 - 0.106) / (0.200 - 0.106))  # Very Unhealthy
  } else {
    return(301)  # Hazardous
  }
}



#AQI

# Define the corrected color palette
aqi_colors <- c(
  "Good" = "#66c2a5",  # Green
  "Moderate" = "#ffd972",  # Yellow
  "Unhealthy for Sensitive Groups" = "#fc8d62",  # Orange
  "Unhealthy" = "#d90429",  # Red
  "Very Unhealthy" = "#a01a7d",#Purple
  "Hazardous" = "#640d14"# Maroon
)

# Convert the category column to a factor with the desired order
aqi_categories <- data.frame(
  category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
  ymin = c(0, 50, 100, 150, 200, 300),  # Corresponding lower bounds
  ymax = c(50, 100, 150, 200, 300, Inf)  # Corresponding upper bounds
)

aqi_categories <- aqi_categories %>%
  mutate(category = factor(category, levels = names(aqi_colors)))

aqi_colors <- c(
  "Good" = "#66c2a5",  # Green
  "Moderate" = "#ffd972",  # Yellow
  "Unhealthy for Sensitive Groups" = "#fc8d62",  # Orange
  "Unhealthy" = "#d90429",  # Red
  "Very Unhealthy" = "#a01a7d",#Purple
  "Hazardous" = "#640d14"# Maroon
)

# Convert the category column to a factor with the desired order
aqi_categories <- data.frame(
  category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
  ymin = c(0, 50, 100, 150, 200, 300),  # Corresponding lower bounds
  ymax = c(50, 100, 150, 200, 300, Inf)  # Corresponding upper bounds
)

aqi_categories <- aqi_categories %>%
  mutate(category = factor(category, levels = names(aqi_colors)))

# Function to classify AQI category based on pollutant value
classify_aqi <- function(value, pollutant) {
  # Define the AQI breakpoints for each pollutant
  aqi_categories <- data.frame(
    category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
    ymin = c(0, 50, 100, 150, 200, 300),  # Corresponding lower bounds
    ymax = c(50, 100, 150, 200, 300, Inf)  # Upper bounds
  )
  
  # Define pollutant-specific AQI breakpoints
  breakpoints <- list(
    pm25 = c(0, 9.0, 35.4, 55.4, 125.4, 225.4, Inf),
    pm10 = c(0, 54, 154, 254, 354, 424, Inf),
    ozone = c(0, 0.054, 0.070, 0.085, 0.105, 0.200, Inf)
  )
  
  # Get the appropriate breakpoints for the given pollutant
  pollutant_breakpoints <- breakpoints[[pollutant]]
  
  # Determine the AQI category based on the value and the pollutant's breakpoints
  for (i in 1:length(pollutant_breakpoints) - 1) {
    if (value >= pollutant_breakpoints[i] && value < pollutant_breakpoints[i + 1]) {
      return(aqi_categories$category[i])
    }
  }
  
  # If no category is found, return NA
  return(NA)
}


calculate_aqi_pm25 <- function(pm25) {
  if (is.na(pm25)) return(NA)
  if (pm25 <= 9.0) {
    return(50 * pm25 / 9.0)  # Good
  } else if (pm25 <= 35.4) {
    return(50 + (100 - 50) * (pm25 - 9.1) / (35.4 - 9.1))  # Moderate
  } else if (pm25 <= 55.4) {
    return(100 + (150 - 100) * (pm25 - 35.5) / (55.4 - 35.5))  # Unhealthy for Sensitive Groups
  } else if (pm25 <= 125.4) {
    return(150 + (200 - 150) * (pm25 - 55.5) / (125.4 - 55.5))  # Unhealthy
  } else if (pm25 <= 225.4) {
    return(200 + (300 - 200) * (pm25 - 125.5) / (225.4 - 125.5))  # Very Unhealthy
  } else if (pm25 <= 500.4) {
    return(300 + (500 - 300) * (pm25 - 225.5) / (500.4 - 225.5))  # Hazardous
  } else {
    return(NA)  # Above AQI range
  }
}

calculate_aqi_pm10 <- function(pm10) {
  # Return NA if the input is NA or NaN
  if (is.na(pm10) || is.nan(pm10)) {
    return(NA)
  }
  if (pm10 <= 54) {
    return(50 * pm10 / 54)  # Good
  } else if (pm10 <= 154) {
    return(50 + (100 - 50) * (pm10 - 55) / (154 - 55))  # Moderate
  } else if (pm10 <= 254) {
    return(100 + (150 - 100) * (pm10 - 155) / (254 - 155))  # Unhealthy for Sensitive Groups
  } else if (pm10 <= 354) {
    return(150 + (200 - 150) * (pm10 - 255) / (354 - 255))  # Unhealthy
  } else if (pm10 <= 424) {
    return(200 + (300 - 200) * (pm10 - 355) / (424 - 355))  # Very Unhealthy
  } else if (pm10 <= 604) {
    return(300 + (500 - 300) * (pm10 - 425) / (604 - 425))  # Hazardous
  } else {
    return(NA)  # Above AQI range
  }
}

calculate_aqi_o3 <- function(o3) {
  if (is.na(o3)) {
    return(NA)  # Return NA if o3 is NA
  } else if (o3 <= 0.054) {
    return(50 * o3 / 0.054)  # Good
  } else if (o3 <= 0.070) {
    return(50 + (100 - 50) * (o3 - 0.055) / (0.070 - 0.055))  # Moderate
  } else if (o3 <= 0.085) {
    return(100 + (150 - 100) * (o3 - 0.071) / (0.085 - 0.071))  # Unhealthy for Sensitive Groups
  } else if (o3 <= 0.105) {
    return(150 + (200 - 150) * (o3 - 0.086) / (0.105 - 0.086))  # Unhealthy
  } else if (o3 <= 0.200) {
    return(200 + (300 - 200) * (o3 - 0.106) / (0.200 - 0.106))  # Very Unhealthy
  } else if (o3 <= 0.604) {
    return(300 + (500 - 300) * (o3 - 0.201) / (0.604 - 0.201))  # Hazardous
  } else {
    return(NA)  # Above AQI range
  }
}



#Site Info


site_names = c(
  "ALotMatter", "Ark", "BayBrook", "Bilingual", "FederalHillPrep", "Filbert",
  "ToolBank", "ConventionCenter", "Stillmeadow", "StrengthToLove", "Waterfront",
  "BUGS", "FortMcHenry", "WestCovingtonPark"
)

#Site Information
site_info <- data.frame(
  site = c(
    "ALotMatter", "Ark", "BayBrook", "Bilingual", "FederalHillPrep", "Filbert",
    "ToolBank", "ConventionCenter", "Stillmeadow", "StrengthToLove", "Waterfront",
    "BUGS", "FortMcHenry", "WestCovingtonPark"
  ),
  site_name = c(
    "A Lot Matter", "Ark Church", "Bay Brook Elementary/Middle School", "Bilingual Christian Church", "Federal Hill Preparatory School", "Filbert Street Garden",
    "Baltimore Community ToolBank", "Convention Center", "Stillmeadow Community Fellowship", "Strength to Love II Farm", "Waterfront Partnership Garage",
    "Baltimore Urban Gardening with Students (BUGS)", "Fort McHenry Research Wetland", "West Covington Park"
  ),
  social_vuln_score = c(
    81.58, 88.13, 90.14, 73.29, 4.82, 90.14,
    49.05, 43.66, 49.92, 62.09, 42.3,
    9.55, 5.27, 8.74
  ),
  social_vuln_category = c(
    "High", "High", "High", "High", "Low", "High",
    "Moderate", "Moderate", "Moderate", "Moderate", "Moderate",
    "Low", "Low", "Low"
  ),
  longitude = c(
    -76.64776, -76.63861, -76.597796, -76.645, -76.6111, -76.591884,
    -76.6312967, -76.616697, -76.6994, -76.6471, -76.60467,
    -76.5967, -76.5843, -76.6155135
  ),
  latitude = c(
    39.29743, 39.26333, 39.22637, 39.253333, 39.2775, 39.224681,
    39.27796748, 39.28509, 39.2811, 39.30381, 39.28584,
    39.2817, 39.26417, 39.2619618
  )
)

# Make sure the category is a factor with the desired order
site_info$social_vuln_category <- factor(
  site_info$social_vuln_category,
  levels = c("High", "Moderate", "Low")
)

# Order the data frame
site_info <- site_info[order(site_info$social_vuln_category), ]



#Clean
library(data.table)
library(lubridate)
library(googledrive)

## Use data from Google Drive folder
## Get Service Account from Google Cloud Console
# Path to service account JSON key file
#service_account_key <- "key.json"
# Authenticate with Google Drive 
#drive_auth(path = service_account_key)
# Google Drive folder ID 
#folder_id <- "1G2Sug5Wazqn3AGz8W7Hyq0LR-Za3WeDWnEcIL46JQEZ4IYZ62flURge365CDaMpxAYZoYjbT"
# Create a temporary directory for downloading files
#temp_dir <- tempdir()
#dir.create(temp_dir, showWarnings = FALSE)
# List files in the Google Drive folder
#file_list_drive <- drive_ls(path = as_id(folder_id))

## With Zipped Folder
zip_file <- "data_files.zip"

# Define the directory where files will be extracted
data_dir <- "unzipped_data_files"

# If the directory doesn't exist, unzip the contents
if (!dir.exists(data_dir)) {
  unzip(zip_file, exdir = data_dir)
}
# Initialize list to store site data
all_sites_data <- list()

for (site in site_names) {
  tryCatch({
    file_list <- list.files(path = data_dir, 
                            pattern = paste0("^", site), 
                            full.names = TRUE,
                            ignore.case = TRUE)
    
    if (length(file_list) == 0) next
    
    site_data_list <- lapply(file_list, function(file) {
      tryCatch({
        data <- fread(file)
        data[, site := site]
        
        if (ncol(data) == 17) {
          setnames(data, c("timestamp", "arduino_id", "program", "bme_temp_c", "bme_pressure_hpa", 
                           "bme_humidity_percent", "bme_voc_ppm", "bme_altitude_m", "k30_co2_ppm", 
                           "ozone_ppb", "sensor1_pm1_ugm3", "sensor1_pm2.5_ugm3", "sensor1_pm10_ugm3", 
                           "sensor2_pm1_ugm3", "sensor2_pm2.5_ugm3", "sensor2_pm10_ugm3", "site"))
          return(data)
        } else {
          return(NULL)
        }
      }, error = function(e) {
        warning(paste("Error in file:", file))
        return(NULL)
      })
    })
    
    site_data_list <- site_data_list[!sapply(site_data_list, is.null)]
    if (length(site_data_list) > 0) {
      all_sites_data[[site]] <- rbindlist(site_data_list, fill = TRUE)
    }
    
  }, error = function(e) {
    warning(paste("Error processing site:", site, ":", e$message))
  })
}

if (length(all_sites_data) == 0) stop("No valid site data found.")

# Combine data
chunk_size <- 5
site_chunks <- split(names(all_sites_data), ceiling(seq_along(all_sites_data) / chunk_size))
merged_data <- rbindlist(lapply(site_chunks, function(chunk) {
  rbindlist(all_sites_data[chunk], fill = TRUE)
}), fill = TRUE)

rm(all_sites_data); gc()

# Clean and convert timestamp
merged_data[merged_data == -7999] <- NA
merged_data[, timestamp := as.character(timestamp)]
merged_data[, timestamp := parse_date_time(timestamp, orders = "Y/m/d H:M:S", tz = "America/New_York")]

# Adjust years: set years < 2025 or > 2025 to 2025
merged_data[, timestamp := if_else(
  year(timestamp) < 2025 | year(timestamp) > 2025,
  update(timestamp, year = 2025),
  timestamp
)]

# Filter out timestamps in the far future
merged_data <- merged_data[!is.na(timestamp) & timestamp <= Sys.time() + days(1)]
merged_data[, timestamp_hour := floor_date(timestamp, unit = "hour")]

# Convert Ozone to ppm
merged_data$ozone_ppb[is.nan(merged_data$ozone_ppb)] <- NA
merged_data$ozone_ppm <- merged_data$ozone_ppb / 1000

# Merge site info
merged_data <- merge(
  merged_data,
  site_info[, c("site", "longitude", "latitude", "social_vuln_score", "social_vuln_category")],
  by = "site",
  all.x = TRUE
)

# Reorder columns
setcolorder(merged_data, c(
  "timestamp", "site", "longitude", "latitude", "social_vuln_score", "social_vuln_category",
  "arduino_id", "program", "bme_temp_c", "bme_pressure_hpa", "bme_humidity_percent",
  "bme_voc_ppm", "bme_altitude_m", "k30_co2_ppm", "ozone_ppb", "ozone_ppm",
  "sensor1_pm1_ugm3", "sensor1_pm2.5_ugm3", "sensor1_pm10_ugm3",
  "sensor2_pm1_ugm3", "sensor2_pm2.5_ugm3", "sensor2_pm10_ugm3"
))

#Hourly

# Compute hourly averages grouped by site and timestamp_hour
hourly_avg <- merged_data[, .(
  sensor1_pm1_ugm3_avg = mean(sensor1_pm1_ugm3, na.rm = TRUE),
  sensor2_pm1_ugm3_avg = mean(sensor2_pm1_ugm3, na.rm = TRUE),
  sensor1_pm2.5_ugm3_avg = mean(sensor1_pm2.5_ugm3, na.rm = TRUE),
  sensor2_pm2.5_ugm3_avg = mean(sensor2_pm2.5_ugm3, na.rm = TRUE),
  sensor1_pm10_ugm3_avg  = mean(sensor1_pm10_ugm3, na.rm = TRUE),
  sensor2_pm10_ugm3_avg  = mean(sensor2_pm10_ugm3, na.rm = TRUE),
  pm1_ugm3_avg = mean(c(sensor1_pm1_ugm3, sensor2_pm1_ugm3), na.rm = TRUE),
  pm2.5_ugm3_avg = mean(c(sensor1_pm2.5_ugm3, sensor2_pm2.5_ugm3), na.rm = TRUE),
  pm10_ugm3_avg = mean(c(sensor1_pm10_ugm3, sensor2_pm10_ugm3), na.rm = TRUE),
  bme_temp_c_avg  = mean(bme_temp_c, na.rm = TRUE),
  bme_humidity_percent_avg  = mean(bme_humidity_percent, na.rm = TRUE),
  ozone_ppm_avg  = mean(ozone_ppm, na.rm = TRUE),
  k30_co2_ppm_avg  = mean(k30_co2_ppm, na.rm = TRUE)
), by = .(site, timestamp_hour)]

# Add site information (coordinates and social vulnerability) to hourly data
hourly_avg <- merge(hourly_avg, 
                    site_info[, c("site", "longitude", "latitude", "social_vuln_score", "social_vuln_category")], 
                    by = "site", 
                    all.x = TRUE)

# Reorder columns in hourly_avg
setcolorder(hourly_avg, c("timestamp_hour", "site", "longitude", "latitude",
                          "social_vuln_score", "social_vuln_category",
                          "sensor1_pm1_ugm3_avg", "sensor2_pm1_ugm3_avg", 
                          "pm1_ugm3_avg",
                          "sensor1_pm2.5_ugm3_avg", "sensor2_pm2.5_ugm3_avg", 
                          "pm2.5_ugm3_avg",
                          "sensor1_pm10_ugm3_avg", "sensor2_pm10_ugm3_avg",
                          "pm10_ugm3_avg",
                          "bme_temp_c_avg", "bme_humidity_percent_avg",
                          "ozone_ppm_avg", "k30_co2_ppm_avg"))

hourly_avg$ozone_ppm_avg[is.nan(hourly_avg$ozone_ppm_avg)]<-NA



#Daily


merged_data[, timestamp_day := floor_date(timestamp, unit = "day")]

# Compute daily averages
daily_avg <- merged_data[, .(
  sensor1_pm1_ugm3_avg = mean(sensor1_pm1_ugm3, na.rm = TRUE),
  sensor2_pm1_ugm3_avg = mean(sensor2_pm1_ugm3, na.rm = TRUE),
  sensor1_pm2.5_ugm3_avg = mean(sensor1_pm2.5_ugm3, na.rm = TRUE),
  sensor2_pm2.5_ugm3_avg = mean(sensor2_pm2.5_ugm3, na.rm = TRUE),
  sensor1_pm10_ugm3_avg  = mean(sensor1_pm10_ugm3, na.rm = TRUE),
  sensor2_pm10_ugm3_avg  = mean(sensor2_pm10_ugm3, na.rm = TRUE),
  pm1_ugm3_avg = mean(c(sensor1_pm1_ugm3, sensor2_pm1_ugm3), na.rm = TRUE),
  pm2.5_ugm3_avg = mean(c(sensor1_pm2.5_ugm3, sensor2_pm2.5_ugm3), na.rm = TRUE),
  pm10_ugm3_avg = mean(c(sensor1_pm10_ugm3, sensor2_pm10_ugm3), na.rm = TRUE),
  bme_temp_c_avg  = mean(bme_temp_c, na.rm = TRUE),
  bme_humidity_percent_avg  = mean(bme_humidity_percent, na.rm = TRUE),
  ozone_ppm_avg  = mean(ozone_ppm, na.rm = TRUE),
  k30_co2_ppm_avg  = mean(k30_co2_ppm, na.rm = TRUE)
), by = .(site, timestamp_day)]

# Add longitude and latitude to daily data
daily_avg <- merge(daily_avg, 
                   site_info[, c("site", "longitude", "latitude")], 
                   by = "site", 
                   all.x = TRUE)

# Add social vulnerability info to daily data
daily_avg <- merge(daily_avg, 
                   site_info[, c("site", "social_vuln_score", "social_vuln_category")], 
                   by = "site", 
                   all.x = TRUE)

# Reorder columns in daily_avg
setcolorder(daily_avg, c("timestamp_day", "site", "longitude", "latitude",
                         "social_vuln_score", "social_vuln_category",
                         "sensor1_pm1_ugm3_avg", "sensor2_pm1_ugm3_avg", 
                         "pm1_ugm3_avg",
                         "sensor1_pm2.5_ugm3_avg", "sensor2_pm2.5_ugm3_avg", 
                         "pm2.5_ugm3_avg",
                         "sensor1_pm10_ugm3_avg", "sensor2_pm10_ugm3_avg",
                         "pm10_ugm3_avg",
                         "bme_temp_c_avg", "bme_humidity_percent_avg",
                         "ozone_ppm_avg", "k30_co2_ppm_avg"))

daily_avg$ozone_ppm_avg[is.nan(daily_avg$ozone_ppm_avg)]<-NA

# Check for NaN values in PM10 measurements
daily_avg$pm10_ugm3_avg[is.nan(daily_avg$pm10_ugm3_avg)] <- NA

daily_avg[, `:=`(
  aqi_pm25 = sapply(pm2.5_ugm3_avg, calculate_aqi_pm25),
  aqi_pm10 = sapply(pm10_ugm3_avg, calculate_aqi_pm10)
)]

###Shiny APP

library(data.table)
library(dplyr)
library(DT)
library(ggiraph)
library(ggmap)
library(ggplot2)
library(htmlwidgets)
library(leaflet)
library(lubridate)
library(plotly)
library(sf)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(tmaptools)
library(viridis)
library(zoo)

# AQI Setup -------------------------------------------------------------------

# Define AQI colors
aqi_colors <- c(
  Good = "#66c2a5",
  Moderate = "#ffd972",
  `Unhealthy for Sensitive Groups` = "#fc8d62",
  Unhealthy = "#d90429",
  `Very Unhealthy` = "#a01a7d",
  Hazardous = "#640d14"
)

# Define AQI categories
aqi_categories <- data.frame(
  category = names(aqi_colors),
  ymin = c(0, 50, 100, 150, 200, 300),
  ymax = c(50, 100, 150, 200, 300, Inf)
)

# Calculate AQI for PM2.5
calculate_aqi_pm25 <- function(x) {
  if (is.na(x)) return(NA)
  if (x <= 9.0) return(50 * x / 9.0)
  if (x <= 35.4) return(50 + (100 - 50) * (x - 9.1) / (35.4 - 9.1))
  if (x <= 55.4) return(100 + (150 - 100) * (x - 35.5) / (55.4 - 35.5))
  if (x <= 125.4) return(150 + (200 - 150) * (x - 55.5) / (125.4 - 55.5))
  if (x <= 225.4) return(200 + (300 - 200) * (x - 125.5) / (225.4 - 125.5))
  if (x <= 500.4) return(300 + (500 - 300) * (x - 225.5) / (500.4 - 225.5))
  NA
}

# Calculate AQI for PM10
calculate_aqi_pm10 <- function(x) {
  if (is.na(x)) return(NA)
  if (x <= 54) return(50 * x / 54)
  if (x <= 154) return(50 + (100 - 50) * (x - 55) / (154 - 55))
  if (x <= 254) return(100 + (150 - 100) * (x - 155) / (254 - 155))
  if (x <= 354) return(150 + (200 - 150) * (x - 255) / (354 - 255))
  if (x <= 424) return(200 + (300 - 200) * (x - 355) / (424 - 355))
  301
}

# Calculate AQI for Ozone
calculate_aqi_o3 <- function(x) {
  if (is.na(x)) return(NA)
  if (x <= 0.054) return(50 * x / 0.054)
  if (x <= 0.070) return(50 + (100 - 50) * (x - 0.055) / (0.070 - 0.055))
  if (x <= 0.085) return(100 + (150 - 100) * (x - 0.071) / (0.085 - 0.071))
  if (x <= 0.105) return(150 + (200 - 150) * (x - 0.086) / (0.105 - 0.086))
  if (x <= 0.200) return(200 + (300 - 200) * (x - 0.106) / (0.200 - 0.106))
  301
}

# Get AQI category
get_aqi_category <- function(x) {
  if (is.na(x)) return(NA)
  for (i in seq_len(nrow(aqi_categories))) {
    if (x >= aqi_categories$ymin[i] && x <= aqi_categories$ymax[i]) {
      return(aqi_categories$category[i])
    }
  }
  NA
}

# UI Definition ---------------------------------------------------------------

ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center; margin-left: 10px;",
    img(
      src = "https://serc.si.edu/sites/default/files/website-gen/si_logo-primary-white.webp",
      height = "15px",
      style = "margin-right: 10px;"
    ),
    div("Breathe Baltimore", style = "font-weight: bold;")
  ),
  id = "main_navbar",
  windowTitle = "Breathe Baltimore",
  inverse = TRUE,
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .navbar {
          background-color: #008000 !important;
          border-color: #008000;
          position: fixed;
          width: 100%;
          top: 0;
          z-index: 1100;
        }
        body { padding-top: 70px; }
        .navbar-brand, .navbar-nav > li > a {
          color: white !important;
          font-weight: bold;
        }
        .navbar-nav > li > a:hover {
          color: #90EE90 !important;
        }
        .transparent-pane {
          background-color: rgba(255, 255, 255, 0.6);
          padding: 10px;
          border-radius: 5px;
          box-shadow: 0 0 10px rgba(0,0,0,0.1);
          z-index: 1000;
        }
        .aqi-legend-pane {
          background-color: transparent;
          padding: 10px;
          border-radius: 5px;
          z-index: 1000;
        }
        .close-btn {
          float: right;
          color: #aaa;
          font-size: 26px;
          font-weight: bold;
          cursor: pointer;
        }
        .close-btn:hover { color: red; }
        .aqi-badge {
          display: inline-block;
          padding: 4px 12px;
          margin-top: 5px;
          border-radius: 12px;
          font-size: 14px;
          font-weight: bold;
          color: white;
        }
        .table > thead > tr > th,
        .table > tbody > tr > td {
          text-align: center;
        }
        #map {
          height: calc(100vh - 70px) !important;
        }
        #about-page {
          background-image: url('https://serc.si.edu/sites/default/files/pictures/CitizenScience/Breathe-Baltimore/logo-image-incinerator-photo-taken-by-veronica-lucchese-ejji-staff-cropped.jpg?704');
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          min-height: calc(100vh - 70px);
          color: white;
          padding: 50px;
          text-shadow: 1px 1px 2px #000;
          box-sizing: border-box;
        }
        .leaflet-popup-content-wrapper {
          background-color: rgba(255, 255, 255, 0.95);
          border-radius: 8px;
          box-shadow: 0 3px 14px rgba(0,0,0,0.2);
          padding: 10px;
        }
        .leaflet-popup-content {
          font-family: 'Arial', sans-serif;
          font-size: 14px;
          color: #333;
          margin: 5px 10px;
        }
        .leaflet-popup-content .sensor-name {
          font-size: 16px;
          font-weight: bold;
          color: #008000;
          margin-bottom: 8px;
          text-align: center;
        }
        .leaflet-popup-content .indicator {
          margin: 4px 0;
        }
        .leaflet-popup-content .indicator-label {
          font-weight: bold;
        }
        .leaflet-popup-tip {
          background-color: rgba(255, 255, 255, 0.95);
        }
        .aqi-legend-container {
          display: flex;
          flex-direction: column;
          align-items: center;
        }
        .aqi-legend-box {
         background-color: white;
           border-radius: 8px;
          padding: 10px 15px;
          box-shadow: 0 0 6px rgba(0,0,0,0.2);
          font-weight: bold;
        }

        .aqi-labels {
          font-weight: bold;
      }
        .aqi-tooltip {
         font-weight: bold;
        }
        .aqi-color-row {
          display: flex;
          flex-direction: row;
        }
        .aqi-color {
          width: 40px;
          height: 20px;
          margin: 0 2px;
          border-radius: 3px;
          cursor: pointer;
          position: relative;
        }
        .aqi-tooltip {
          visibility: hidden;
          background-color: rgba(0, 0, 0, 0.8);
          color: white;
          text-align: center;
          border-radius: 4px;
          padding: 5px 10px;
          position: absolute;
          z-index: 1000;
          bottom: 25px;
          left: 50%;
          transform: translateX(-50%);
          white-space: nowrap;
          font-size: 12px;
        }
        .aqi-color:hover .aqi-tooltip {
          visibility: visible;
        }
        .aqi-labels {
          display: flex;
          flex-direction: row;
          margin-top: 5px;
          font-size: 12px;
        }
        .aqi-label {
          width: 44px;
          text-align: center;
          flex-shrink: 0;
        }
        #forecastPanel {
          z-index: 1000;
        }
      ")),
      tags$script(HTML("
        $(document).on('click', '.aqi-color', function() {
          $('.aqi-tooltip').not($(this).find('.aqi-tooltip')).css('visibility', 'hidden');
          $(this).find('.aqi-tooltip').css('visibility', function(i, visibility) {
            return visibility === 'visible' ? 'hidden' : 'visible';
          });
        });
      "))
    )
  ),
  tabPanel(
    title = "Dashboard",
    div(
      style = "position: relative; height: calc(100vh - 70px);",
      leafletOutput("map"),
      absolutePanel(
        top = 10,
        right = 10,
        class = "transparent-pane",
        width = 350,
        selectInput(
          inputId = "selected_site",
          label = h4("Select Sensor Site:", style = "font-weight: bold; margin-top: 0;"),
          choices = NULL
        ),
        radioButtons(
          inputId = "selected_pollutant",
          label = "Air Quality Indicator",
          choices = c(
            PM2.5 = "pm2.5_ugm3_avg",
            PM10 = "pm10_ugm3_avg",
            Ozone = "ozone_ppm_avg"
          ),
          selected = "pm2.5_ugm3_avg",
          inline = TRUE
        )
      ),
      jqui_resizable(
        jqui_draggable(
          absolutePanel(
            id = "timeseriesPanel",
            top = 70,
            left = 20,
            width = 370,
            class = "transparent-pane",
            div(
              span(
                class = "close-btn",
                onclick = "$('#timeseriesPanel').hide();",
                "×"
              ),
              tags$div(
                style = "text-align: center;",
                uiOutput("graphTitle")
              ),
              sliderInput(
                inputId = "time_range",
                label = "Select Time Range:",
                min = min(hourly_avg$timestamp_hour, na.rm = TRUE),
                max = max(hourly_avg$timestamp_hour, na.rm = TRUE),
                value = c(min(hourly_avg$timestamp_hour, na.rm = TRUE), max(hourly_avg$timestamp_hour, na.rm = TRUE)),
                timeFormat = "%b %d %Y %H:%M",
                step = 3600,
                ticks = FALSE,
                width = "100%"
              ),
              uiOutput("aqiCategoryLabel_top")
            ),
            textOutput("selectedSensorLabel"),
            plotlyOutput("pm25Graph", height = "250px")
          )
        )
      ),
      jqui_resizable(
        jqui_draggable(
          absolutePanel(
            id = "summaryPanel",
            top = 70,
            right = 20,
            width = 420,
            class = "transparent-pane",
            div(
              span(
                class = "close-btn",
                onclick = "$('#summaryPanel').hide();",
                "×"
              ),
              tags$div(
                style = "text-align: center;",
                h4("Current AQI Summary", style = "font-weight: bold; margin-top: 0;")
              ),
              uiOutput("aqiCategoryLabel_summary")
            ),
            tags$div(
              style = "overflow-x: auto; height: 300px; overflow-y: scroll;",
              tableOutput("sensorTable")
            )
          )
        )
      ),
      jqui_resizable(
        jqui_draggable(
          absolutePanel(
            id = "forecastPanel",
            top = 350,
            left = 20,
            width = 600,
            class = "transparent-pane",
            div(
              span(
                class = "close-btn",
                onclick = "$('#forecastPanel').hide();",
                "×"
              ),
              tags$div(
                style = "text-align: center;",
                h4("7-Day Air Quality Forecast", style = "font-weight: bold; margin-top: 0;")
              ),
              plotlyOutput("forecastPlot", height = "400px")
            )
          )
        )
      ),
      absolutePanel(
        bottom = 10,
        left = 10,
        class = "aqi-legend-pane",
        width = 350,
        #h5("AQI Legend"),
        uiOutput("aqiLegend")
      )
    )
  ),
  navbarMenu(
    title = "Data",
    tabPanel(
      "Daily Average Data",
      sidebarLayout(
        sidebarPanel(
          selectInput("daily_download_site", "Select Site to Download:",
                      choices = c("All", unique(daily_avg$site))),
          downloadButton("download_daily_data", "Download Daily Data")
        ),
        mainPanel(DTOutput("dailyDataTable"))
      )
    ),
    tabPanel(
      "Hourly Average Data",
      sidebarLayout(
        sidebarPanel(
          selectInput("hourly_download_site", "Select Site to Download:",
                      choices = c("All", unique(hourly_avg$site))),
          downloadButton("download_hourly_data", "Download Hourly Data")
        ),
        mainPanel(DTOutput("hourlyDataTable"))
      )
    ),
    tabPanel(
      title = "Spatial Analysis",
      fluidRow(
        column(12,
          h3("Spatial Analysis of Air Quality and Social Vulnerability"),
          p("This section provides spatial analysis of air quality patterns and their relationship with social vulnerability."),
          p("Social vulnerability, as defined by FEMA, refers to the susceptibility of a community to the impacts of hazards, including factors such as socioeconomic status, household composition, minority status, language, housing type, and transportation access.")
        )
      ),
      fluidRow(
        column(12,
          h4("Spatial Clusters Map"),
          uiOutput("cluster_map_time_slider"),
          leafletOutput("cluster_map"),
          p("This map shows the spatial distribution of air quality clusters, with red indicating hotspots and blue indicating coldspots."),
          p("Hotspots (red) represent areas where air quality measurements are significantly higher than the surrounding areas, indicating potential pollution concentration zones. Coldspots (blue) represent areas where air quality measurements are significantly lower than the surrounding areas, indicating relatively cleaner air zones. These clusters are identified using spatial statistical analysis to determine statistically significant patterns in the data."),
          br(),
          h4("Cumulative Hotspot/Coldspot Days by Site"),
          DT::dataTableOutput("site_cumulative_table"),
          htmlOutput("site_cumulative_summary")
        )
      )
    )
  ),
  tabPanel(
    title = "About",
    tags$div(
      id = "about-page",
      h4("About the Breathe Baltimore Project"),
      p("This project monitors real-time air quality in Baltimore using community-deployed sensors."),
      p(
        "Visit the full project description on ",
        tags$a(
          href = "https://github.com/Smithsonian/Breathe-Baltimore/blob/main/README.md",
          target = "_blank",
          "GitHub"
        ),
        "."
      )
    )
  )
)
# Server Definition -----------------------------------------------------------

server <- function(input, output, session) {
  # Memory management
  gc()  # Force garbage collection at server start
  
  # Initialize reactive values for data
  rv <- reactiveValues(
    daily_avg = NULL,
    hourly_avg = NULL,
    spatial_results = NULL
  )
  
  # Load data only when needed
  observe({
    # Load daily data
    if (is.null(rv$daily_avg)) {
      rv$daily_avg <- daily_avg
      gc()
    }
    
    # Load hourly data
    if (is.null(rv$hourly_avg)) {
      rv$hourly_avg <- hourly_avg
      gc()
    }
  })
  
  # Update site selection dropdown
  observe({
    req(rv$hourly_avg)
    updateSelectInput(
      session = session,
      inputId = "selected_site",
      choices = unique(rv$hourly_avg$site)
    )
  })

  # Spatial Analysis Server Code with lazy loading
  spatial_results <- reactive({
    req(rv$daily_avg)
    
    # Process only the last 30 days of data to reduce memory usage
    recent_data <- rv$daily_avg %>%
      filter(timestamp_day >= max(timestamp_day) - days(30))
    
    # Process data in smaller chunks
    chunk_size <- 5  # Process 5 days at a time
    dates <- unique(recent_data$timestamp_day)
    date_chunks <- split(dates, ceiling(seq_along(dates)/chunk_size))
    
    gi_results <- list()
    slm_results <- list()
    
    for(chunk in date_chunks) {
      chunk_data <- recent_data %>% filter(timestamp_day %in% chunk)
      
      # Gi* results
      gi_chunk <- chunk_data %>%
        group_by(timestamp_day) %>%
        group_split() %>%
        map_dfr(calc_gi_for_day) %>%
        mutate(
          cluster_type = dplyr::case_when(
            gi_stat > 1.96 ~ "Hotspot",
            gi_stat < -1.96 ~ "Coldspot",
            TRUE ~ "Not Significant"
          )
        )
      gi_results <- c(gi_results, list(gi_chunk))
      
      # SLM results
      slm_chunk <- chunk_data %>%
        group_by(timestamp_day) %>%
        group_split() %>%
        map_dfr(calc_slm_for_day)
      slm_results <- c(slm_results, list(slm_chunk))
      
      # Force garbage collection after each chunk
      gc()
    }
    
    list(
      gi = bind_rows(gi_results),
      slm = bind_rows(slm_results)
    )
  })

  # Add time slider for date selection
  output$cluster_map_time_slider <- renderUI({
    req(spatial_results())
    gi_df <- spatial_results()$gi
    
    # Get unique dates with significant clusters
    significant_dates <- gi_df %>%
      filter(cluster_type %in% c("Hotspot", "Coldspot")) %>%
      pull(date) %>%
      as.Date() %>%
      unique() %>%
      sort()
    
    if(length(significant_dates) == 0) {
      return(div("No significant clusters found in the selected time period."))
    }
    
    sliderInput(
      inputId = "selected_gi_date_slider",
      label = "Select Date:",
      min = min(significant_dates),
      max = max(significant_dates),
      value = max(significant_dates),
      timeFormat = "%Y-%m-%d",
      step = 1,
      animate = TRUE
    )
  })

  # Render cluster map with optimized data handling
  output$cluster_map <- renderLeaflet({
    req(spatial_results(), input$selected_gi_date_slider)
    
    # Get the selected date
    selected_date <- as.Date(input$selected_gi_date_slider)
    
    # Get Gi* results for the selected date
    selected_gi <- spatial_results()$gi %>%
      filter(as.Date(date) == selected_date)
    
    if(nrow(selected_gi) == 0) {
      return(leaflet() %>% addTiles() %>% 
        addControl("No data available for selected date", position = "topright"))
    }
    
    # Join with site_info to ensure all sensors are present
    map_data <- site_info %>%
      left_join(selected_gi, by = "site") %>%
      mutate(
        cluster_type = ifelse(is.na(cluster_type), "Not Significant", cluster_type)
      )
    
    # Add cumulative counts
    cum_counts <- cumulative_cluster_counts() %>%
      filter(site %in% map_data$site)
    
    map_data <- map_data %>%
      left_join(cum_counts, by = "site") %>%
      mutate(
        Hotspot = ifelse(is.na(Hotspot), 0, Hotspot),
        Coldspot = ifelse(is.na(Coldspot), 0, Coldspot)
      )
    
    cluster_colors <- c(
      "Hotspot" = "red",
      "Coldspot" = "blue",
      "Not Significant" = "gray"
    )
    
    map_data$cluster_type <- trimws(as.character(map_data$cluster_type))
    map_data$cluster_type[is.na(map_data$cluster_type)] <- "Not Significant"
    map_data$cluster_type[!map_data$cluster_type %in% names(cluster_colors)] <- "Not Significant"
    
    map_data$marker_color <- cluster_colors[map_data$cluster_type]
    map_data$marker_color[is.na(map_data$marker_color)] <- "gray"
    
    # Create map with optimized marker creation
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        color = ~marker_color,
        fillColor = ~marker_color,
        stroke = TRUE,
        weight = 2,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>Site:</b> ", site_name, "<br>",
          "<b>Social Vulnerability Category:</b> ", social_vuln_category.x, "<br>",
          "<b>Cluster Type (", selected_date, "):</b> ", cluster_type, "<br>",
          "<b>Gi* Statistic:</b> ", ifelse(is.na(gi_stat), "NA", round(gi_stat, 3)), "<br>",
          "<b>Cumulative Hotspot Days:</b> ", Hotspot, "<br>",
          "<b>Cumulative Coldspot Days:</b> ", Coldspot
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = cluster_colors,
        labels = names(cluster_colors),
        title = "Cluster Type"
      )
  })

  # Optimize cumulative cluster counts calculation
  cumulative_cluster_counts <- reactive({
    gi_df <- spatial_results()$gi
    
    # Process in smaller chunks
    chunk_size <- 100
    dates <- unique(gi_df$date)
    date_chunks <- split(dates, ceiling(seq_along(dates)/chunk_size))
    
    results <- list()
    for(chunk in date_chunks) {
      chunk_results <- gi_df %>%
        filter(date %in% chunk, cluster_type %in% c("Hotspot", "Coldspot")) %>%
        group_by(site, cluster_type) %>%
        summarise(count = n(), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = cluster_type, values_from = count, values_fill = 0)
      results <- c(results, list(chunk_results))
      gc()
    }
    
    # Combine results
    bind_rows(results) %>%
      group_by(site) %>%
      summarise(
        Hotspot = sum(Hotspot, na.rm = TRUE),
        Coldspot = sum(Coldspot, na.rm = TRUE)
      )
  })

  # Update selected site on map marker click
  # Use a reactiveVal to store the previously selected site
  selected_site_val <- reactiveVal(NULL)
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    # Check if click is NULL or lacks coordinates
    if (is.null(click) || is.null(click$lat) || is.null(click$lng)) return()
    
    matched <- hourly_avg %>%
      filter(
        round(latitude, 5) == round(click$lat, 5),
        round(longitude, 5) == round(click$lng, 5)
      )
    
    # Only update if a match is found
    if (nrow(matched) > 0 && !is.null(matched$site[1])) {
      updateSelectInput(
        session = session,
        inputId = "selected_site",
        selected = matched$site[1]
      )
    }
  })
  
  
  
  # Render selected sensor label
  output$selectedSensorLabel <- renderText({
    paste("Sensor:", input$selected_site)
  })
  
  # Compute AQI info for selected site and pollutant
  selected_aqi_info <- reactive({
    req(input$selected_site)
    req(input$selected_pollutant)
    
    val <- hourly_avg %>%
      filter(site == input$selected_site) %>%
      summarise(v = mean(.data[[input$selected_pollutant]], na.rm = TRUE)) %>%
      pull(v)
    
    aqi <- switch(
      input$selected_pollutant,
      pm2.5_ugm3_avg = calculate_aqi_pm25(val),
      pm10_ugm3_avg = calculate_aqi_pm10(val),
      ozone_ppm_avg = calculate_aqi_o3(val),
      NA
    )
    
    category <- get_aqi_category(aqi)
    color <- aqi_colors[category]
    list(value = val, aqi = aqi, category = category, color = color)
  })
  
  # Render graph title based on selected pollutant
  output$graphTitle <- renderUI({
    pollutant_name <- switch(
      input$selected_pollutant,
      pm2.5_ugm3_avg = "PM2.5",
      pm10_ugm3_avg = "PM10",
      ozone_ppm_avg = "Ozone"
    )
    h4(paste("Hourly US EPA", pollutant_name, "AQI"), style = "font-weight: bold; margin-top: 0;")
  })
  
  # Render AQI badge for timeseries panel
  output$aqiCategoryLabel_top <- renderUI({
    info <- selected_aqi_info()
    div(
      class = "aqi-badge",
      style = paste0("background-color:", info$color),
      paste(info$category, "AQI:", round(info$aqi))
    )
  })
  
  # Render AQI badge for summary panel
  output$aqiCategoryLabel_summary <- renderUI({
    req(input$selected_pollutant, input$selected_site)
    info <- selected_aqi_info()
    div(
      class = "aqi-badge",
      style = paste0("background-color:", info$color),
      paste(info$category, "AQI:", round(info$aqi))
    )
  })
  
  # Render AQI legend
  output$aqiLegend <- renderUI({
    tagList(
      div(
        class = "aqi-legend-box",
        h4("AQI Legend", style = "font-weight: bold; margin-top: 0;"),
        p("2 Clicks on sensor site location markers",style = "font-weight:normal"),
        p("You can move, resize, and close visualizations",style = "font-weight:normal"),
        p("AQI categories color-coded based on this key",style = "font-weight:normal"),
        div(
          class = "aqi-legend-container",
          div(
            class = "aqi-color-row",
            lapply(seq_along(aqi_colors), function(i) {
              cat <- aqi_categories$category[i]
              col <- aqi_colors[cat]
              ymin <- aqi_categories$ymin[i]
              ymax <- aqi_categories$ymax[i]
              div(
                class = "aqi-color",
                style = paste0("background-color:", col),
                div(
                  class = "aqi-tooltip",
                  paste(cat, ":", ymin, "-", ifelse(is.infinite(ymax), "+", ymax))
                )
              )
            })
          ),
          div(
            class = "aqi-labels",
            lapply(seq_along(aqi_categories$category), function(i) {
              ymin <- aqi_categories$ymin[i]
              ymax <- aqi_categories$ymax[i]
              div(
                class = "aqi-label",
                paste(ymin, "-", ifelse(is.infinite(ymax), "+", ymax))
              )
            })
          )
        )
      )
    )
  })
  
  
  
  # Render map with dynamic popups
  output$map <- renderLeaflet({
    req(input$selected_pollutant)
    data <- hourly_avg %>%
      group_by(site) %>%
      summarise(
        value = mean(.data[[input$selected_pollutant]], na.rm = TRUE),
        pm25 = mean(pm2.5_ugm3_avg, na.rm = TRUE),
        pm10 = mean(pm10_ugm3_avg, na.rm = TRUE),
        ozone = mean(ozone_ppm_avg, na.rm = TRUE),
        lat = mean(latitude),
        lon = mean(longitude)
      ) %>%
      mutate(
        aqi = case_when(
          input$selected_pollutant == "pm2.5_ugm3_avg" ~ sapply(value, calculate_aqi_pm25),
          input$selected_pollutant == "pm10_ugm3_avg" ~ sapply(value, calculate_aqi_pm10),
          input$selected_pollutant == "ozone_ppm_avg" ~ sapply(value, calculate_aqi_o3)
        ),
        category = sapply(aqi, get_aqi_category),
        color = ifelse(is.na(category), "#808080", aqi_colors[category]),
        number = seq_len(n())
      )
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~ifelse(site == input$selected_site, 17.5, 12.5),
        color = ~color,
        stroke = TRUE,
        weight = ~ifelse(site == input$selected_site, 7.5, 2.5),
        fillOpacity = 0.8,
        popup = lapply(seq_len(nrow(data)), function(i) {
          site <- data$site[i]
          color <- data$color[i]
          category <- data$category[i]
          aqi_value <- round(data$aqi[i])
          value <- data$value[i]
          reference_timestamp_est <- max(hourly_avg$timestamp_hour, na.rm = TRUE)
          month <- format(reference_timestamp_est, "%B")
          day <- as.numeric(format(reference_timestamp_est, "%d"))
          add_ordinal_suffix <- function(day) {
            if (day %in% c(11, 12, 13)) {
              return(paste0(day, "th"))
            }
            suffix <- switch(
              as.character(day %% 10),
              "1" = "st",
              "2" = "nd",
              "3" = "rd",
              "th"
            )
            paste0(day, suffix)
          }
          day_with_suffix <- add_ordinal_suffix(day)
          year <- format(reference_timestamp_est, "%Y")
          time <- format(reference_timestamp_est, "%I:%M:%S %p")
          timezone <- format(reference_timestamp_est, "%Z")
          timestamp <- paste(month, day_with_suffix, year, time, timezone, sep = ", ")
          
          pollutant_name <- switch(
            input$selected_pollutant,
            pm2.5_ugm3_avg = "PM2.5",
            pm10_ugm3_avg = "PM10",
            ozone_ppm_avg = "Ozone"
          )
          unit <- switch(
            input$selected_pollutant,
            pm2.5_ugm3_avg = "µg/m³",
            pm10_ugm3_avg = "µg/m³",
            ozone_ppm_avg = "ppm"
          )
          
          formatted_value <- if (input$selected_pollutant == "ozone_ppm_avg") {
            sprintf("%.3f", value)
          } else {
            sprintf("%.1f", value)
          }
          
          site_data <- hourly_avg %>% filter(site == site)
          now_time <- with_tz(Sys.time(), tzone = "America/New_York")
          
          get_aqi_avg <- function(hours) {
            values <- site_data %>%
              filter(timestamp_hour >= now_time - hours(hours)) %>%
              pull(.data[[input$selected_pollutant]])
            if (length(values) == 0 || all(is.na(values))) return("")
            mean_aqi <- mean(
              sapply(
                values,
                switch(
                  input$selected_pollutant,
                  pm2.5_ugm3_avg = calculate_aqi_pm25,
                  pm10_ugm3_avg = calculate_aqi_pm10,
                  ozone_ppm_avg = calculate_aqi_o3
                )
              ),
              na.rm = TRUE
            )
            round(mean_aqi)
          }
          
          now_val <- get_aqi_avg(0)
          hr1_val <- get_aqi_avg(1)
          hr3_val <- get_aqi_avg(3)
          hr6_val <- get_aqi_avg(6)
          day_val <- get_aqi_avg(24)
          wk_val <- get_aqi_avg(24 * 7)
          
          risk <- switch(
            category,
            Good = "little or no risk",
            Moderate = "a moderate risk for some",
            `Unhealthy for Sensitive Groups` = "a risk for sensitive individuals",
            Unhealthy = "a risk for the general public",
            `Very Unhealthy` = "a serious health risk",
            Hazardous = "a health emergency",
            "N/A"
          )
          
          HTML(
            paste0(
              "<div style='background-color:", color, "; color: black; padding: 12px; border-radius: 8px; font-family: Arial;'>",
              "<b>On ", timestamp, "</b><br>",
              "<b>Hourly Average</b><br>",
              "<b>US EPA ", pollutant_name, " AQI</b><br>",
              "<span style='font-size: 48px; font-weight: bold;'>", aqi_value, "</span><br>",
              "<div>", pollutant_name, ": ", formatted_value, " ", unit, "</div>",
              "<div>", category, ": Air quality is ", risk, " with 24 hours of exposure.</div><br>",
              "<table style='width:100%; font-size:14px; text-align:center;'>",
              "<tr><th>Now</th><th>1 hr</th><th>3 hr</th><th>6 hr</th><th>1 Day</th><th>Week</th></tr>",
              "<tr>",
              "<td>", now_val, "</td>",
              "<td>", hr1_val, "</td>",
              "<td>", hr3_val, "</td>",
              "<td>", hr6_val, "</td>",
              "<td>", day_val, "</td>",
              "<td>", wk_val, "</td>",
              "</tr></table>",
              "<br><b>Sensor:</b> ", site,
              "</div>"
            )
          )
        }),
        popupOptions = popupOptions(
          closeOnClick = FALSE,
          autoClose = FALSE
        )
      )
    
  })
  
  # Render timeseries plot for selected pollutant and date
  output$pm25Graph <- renderPlotly({
    req(input$selected_site, input$selected_pollutant, input$time_range)
    
    # Filter data based on slider range
    data <- hourly_avg %>%
      filter(
        site == input$selected_site,
        timestamp_hour >= input$time_range[1],
        timestamp_hour <= input$time_range[2]
      )
    
    pollutant_name <- switch(
      input$selected_pollutant,
      pm2.5_ugm3_avg = "PM2.5 (µg/m³)",
      pm10_ugm3_avg = "PM10 (µg/m³)",
      ozone_ppm_avg = "Ozone (ppm)"
    )
    
    plot_ly(data) %>%
      add_lines(
        x = ~timestamp_hour,
        y = ~.data[[input$selected_pollutant]],
        name = "Trend Line",
        line = list(color = "darkorange")
      ) %>%
      add_markers(
        x = ~timestamp_hour,
        y = ~.data[[input$selected_pollutant]],
        name = "Data Points",
        marker = list(color = "blue", size = 5, symbol = "circle")
      ) %>%
      layout(
        xaxis = list(title = "Timestamp"),
        yaxis = list(title = pollutant_name),
        margin = list(t = 30),
        showlegend = TRUE
      )
  })
  
  # Render time series prediction plot
  output$forecastGraph <- renderUI({
    req(input$selected_site, input$selected_pollutant)
    
    # Prepare daily smoothed data
    data <- hourly_avg %>%
      filter(site == input$selected_site) %>%
      mutate(date = as.Date(timestamp_hour)) %>%
      group_by(date) %>%
      summarise(value = mean(.data[[input$selected_pollutant]], na.rm = TRUE)) %>%
      arrange(date)
    
    # Ensure sufficient data
    if (nrow(data) < 30) {
      return(div("Insufficient data for forecasting (minimum 30 days required)."))
    }
    
    # Create time series object
    ts_data <- ts(data$value, frequency = 1)
    
    # Fit ARIMA model
    fit <- auto.arima(ts_data)
    
    # Forecast 7 days ahead
    forecast_result <- forecast(fit, h = 7)
    
    # Prepare data for plotting
    historical_data <- data.frame(
      Date = data$date,
      Value = as.numeric(ts_data),
      Type = "Historical"
    )
    
    forecast_dates <- seq(max(data$date) + 1, by = "day", length.out = 7)
    forecast_data <- data.frame(
      Date = forecast_dates,
      Value = as.numeric(forecast_result$mean),
      Type = "Forecast"
    )
    
    # Confidence intervals
    ci_lower <- data.frame(
      Date = forecast_dates,
      Value = as.numeric(forecast_result$lower[, 2]), # 95% CI
      Type = "Lower CI"
    )
    ci_upper <- data.frame(
      Date = forecast_dates,
      Value = as.numeric(forecast_result$upper[, 2]), # 95% CI
      Type = "Upper CI"
    )
    
    # Combine data
    plot_data <- rbind(historical_data, forecast_data, ci_lower, ci_upper)
    
    pollutant_name <- switch(
      input$selected_pollutant,
      pm2.5_ugm3_avg = "PM2.5 (µg/m³)",
      pm10_ugm3_avg = "PM10 (µg/m³)",
      ozone_ppm_avg = "Ozone (ppm)"
    )
    
    # Create Plotly plot
    p <- plot_ly(plot_data) %>%
      add_lines(
        x = ~Date,
        y = ~Value,
        color = ~Type,
        colors = c("Historical" = "blue", "Forecast" = "red", "Lower CI" = "grey", "Upper CI" = "grey"),
        line = list(
          dash = ~ifelse(Type %in% c("Lower CI", "Upper CI"), "dash", "solid")
        )
      ) %>%
      layout(
        title = h4(paste("7-Day Forecast for", pollutant_name, "at", input$selected_site), style = "font-weight: bold; margin-top: 0;"),
        xaxis = list(title = "Date"),
        yaxis = list(title = pollutant_name),
        showlegend = TRUE
      )
    
    # Wrap in resizable, draggable div with close button
    tagList(
      jqui_resizable(
        jqui_draggable(
          div(
            id = "forecastGraphContainer",
            style = "position: relative; width: 600px; height: 400px; border: 1px solid #ccc; background: white;",
            div(
              style = "position: absolute; top: 5px; right: 5px;",
              actionButton("closeForecast", "Close", icon = icon("times"))
            ),
            plotlyOutput("forecastPlot")
          )
        )
      )
    )
  })
  
  # Render the actual Plotly plot for forecast
  output$forecastPlot <- renderPlotly({
    req(input$selected_site, input$selected_pollutant)
    
    data <- hourly_avg %>%
      filter(site == input$selected_site) %>%
      mutate(date = as.Date(timestamp_hour)) %>%
      group_by(date) %>%
      summarise(value = mean(.data[[input$selected_pollutant]], na.rm = TRUE)) %>%
      arrange(date)
    
    if (nrow(data) < 30) return(NULL)
    
    ts_data <- ts(data$value, frequency = 1)
    fit <- auto.arima(ts_data)
    forecast_result <- forecast(fit, h = 7)
    
    historical_data <- data.frame(
      Date = data$date,
      Value = as.numeric(ts_data),
      Type = "Historical"
    )
    
    forecast_dates <- seq(max(data$date) + 1, by = "day", length.out = 7)
    forecast_data <- data.frame(
      Date = forecast_dates,
      Value = as.numeric(forecast_result$mean),
      Type = "Forecast"
    )
    
    ci_lower <- data.frame(
      Date = forecast_dates,
      Value = as.numeric(forecast_result$lower[, 2]),
      Type = "Lower CI"
    )
    ci_upper <- data.frame(
      Date = forecast_dates,
      Value = as.numeric(forecast_result$upper[, 2]),
      Type = "Upper CI"
    )
    
    plot_data <- rbind(historical_data, forecast_data, ci_lower, ci_upper)
    
    pollutant_name <- switch(
      input$selected_pollutant,
      pm2.5_ugm3_avg = "PM2.5 (µg/m³)",
      pm10_ugm3_avg = "PM10 (µg/m³)",
      ozone_ppm_avg = "Ozone (ppm)"
    )
    
    plot_ly(plot_data) %>%
      add_lines(
        x = ~Date,
        y = ~Value,
        color = ~Type,
        colors = c("Historical" = "blue", "Forecast" = "red", "Lower CI" = "grey", "Upper CI" = "grey"),
        line = list(
          dash = ~ifelse(Type %in% c("Lower CI", "Upper CI"), "dash", "solid")
        )
      ) %>%
      layout(
        title = h4(paste("7-Day Forecast for", pollutant_name, "at", input$selected_site), style = "font-weight: bold; margin-top: 0;"),
        xaxis = list(title = "Date"),
        yaxis = list(title = pollutant_name),
        showlegend = TRUE
      )
  })
  
  # Close forecast graph
  observeEvent(input$closeForecast, {
    shinyjs::hide("forecastGraphContainer")
  })
  
  # Render timestamp
  output$timestamp <- renderText({
    paste(
      "Latest:",
      format(
        max(hourly_avg$timestamp_hour, na.rm = TRUE),
        "%b %d, %Y %I:%M %p"
      )
    )
  })
  
  # Render average AQI
  output$avgAQI <- renderText({
    round(mean(hourly_avg$pm2.5_ugm3_avg, na.rm = TRUE))
  })
  
  # Render sensor summary table
  output$sensorTable <- renderTable({
    hourly_avg %>%
      group_by(site) %>%
      summarise(
        `PM2.5 (µg/m³)` = round(mean(pm2.5_ugm3_avg, na.rm = TRUE), 1),
        `PM10 (µg/m³)` = round(mean(pm10_ugm3_avg, na.rm = TRUE), 1),
        `Ozone (ppm)` = round(mean(ozone_ppm_avg, na.rm = TRUE), 3),
        `Temp (°C)` = round(mean(bme_temp_c_avg, na.rm = TRUE), 1),
        `Humidity (%)` = round(mean(bme_humidity_percent_avg, na.rm = TRUE), 1)
      )
  })
  
  # Render data tables
  output$dailyDataTable <- renderDT({
    daily_avg
  })
  output$hourlyDataTable <- renderDT({
    hourly_avg
  })
  
  # Update download dropdowns after data loads
  observe({
    updateSelectInput(session, "daily_download_site", choices = c("All", unique(daily_avg$site)))
    updateSelectInput(session, "hourly_download_site", choices = c("All", unique(hourly_avg$site)))
  })
  
  # Reactive for filtered data
  filtered_daily_data <- reactive({
    if (input$daily_download_site == "All") {
      daily_avg
    } else {
      daily_avg %>% filter(site == input$daily_download_site)
    }
  })
  
  filtered_hourly_data <- reactive({
    if (input$hourly_download_site == "All") {
      hourly_avg
    } else {
      hourly_avg %>% filter(site == input$hourly_download_site)
    }
  })
  
  # Download Handlers
  output$download_daily_data <- downloadHandler(
    filename = function() {
      paste0("daily_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_daily_data(), file, row.names = FALSE)
    }
  )
  
  output$download_hourly_data <- downloadHandler(
    filename = function() {
      paste0("hourly_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_hourly_data(), file, row.names = FALSE)
    }
  )

  output$site_cumulative_table <- DT::renderDataTable({
    req(site_info)
    # Get cumulative counts after March 1, 2025
    cum_counts <- cumulative_cluster_counts()
    # Join with site info
    table_data <- site_info %>%
      left_join(cum_counts, by = "site") %>%
      mutate(
        Hotspot = ifelse(is.na(Hotspot), 0, Hotspot),
        Coldspot = ifelse(is.na(Coldspot), 0, Coldspot)
      ) %>%
      select(
        Site = site_name,
        `Social Vulnerability Category` = social_vuln_category,
        `Cumulative Hotspot Days` = Hotspot,
        `Cumulative Coldspot Days` = Coldspot
      )
    DT::datatable(table_data, rownames = FALSE, options = list(pageLength = 15))
  })

  output$site_cumulative_summary <- renderUI({
    req(site_info)
    cum_counts <- cumulative_cluster_counts()
    table_data <- site_info %>%
      left_join(cum_counts, by = "site") %>%
      mutate(
        Hotspot = ifelse(is.na(Hotspot), 0, Hotspot),
        Coldspot = ifelse(is.na(Coldspot), 0, Coldspot)
      )
    max_hotspot <- table_data %>% filter(Hotspot == max(Hotspot, na.rm = TRUE)) %>% slice(1)
    max_coldspot <- table_data %>% filter(Coldspot == max(Coldspot, na.rm = TRUE)) %>% slice(1)
    HTML(paste0(
      "<b>", max_hotspot$site_name, "</b> has the highest cumulative hotspot days (", max_hotspot$Hotspot, "), and <b>",
      max_coldspot$site_name, "</b> has the highest cumulative coldspot days (", max_coldspot$Coldspot, ")."
    ))
  })
}

# Run the Shiny App with memory limits
shinyApp(ui=ui, server=server)
