txt_to_sheets <- function(
    data_folder_id, target_folder_id, num_files = 5) {
  library(googledrive)
  library(googlesheets4)
  library(readr)
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(stringr)

  
  # List text files in the source folder
  txt_files <- drive_ls(path = as_id(data_folder_id), pattern = "(?i)\\.txt$")
  
  if (nrow(txt_files) < num_files) {
    stop("Not enough TXT files in the folder.")
  }
  
  # List spreadsheets in the target folder
  target_files <- drive_ls(path = as_id(target_folder_id), type = "spreadsheet")
  
  # Extract base names (without .txt) from text files
  txt_base_names <- str_remove(txt_files$name, "\\.txt$")
  
  # Extract names of existing spreadsheets in the target folder
  target_sheet_names <- target_files$name
  
  # Identify text files that don't have corresponding spreadsheets
  files_to_process <- txt_files[!(txt_base_names %in% target_sheet_names), ]
  
  # Limit to num_files
  files_to_process <- files_to_process[1:min(nrow(files_to_process), num_files), ]
  
  if (nrow(files_to_process) == 0) {
    message("No new files to process.")
    return(invisible(NULL))
  }
  
  for (i in seq_len(nrow(files_to_process))) {
    file_id <- files_to_process$id[i]
    file_name <- files_to_process$name[i]
    
    file_content <- drive_read_string(as_id(file_id))
    file_data <- read.csv(text = file_content, sep = ",", header = FALSE)
    setDT(file_data)
    
    setnames(file_data, c("timestamp", "arduino_id", "program", "bme_temp_c", "bme_pressure_hpa", 
                          "bme_humidity_percent", "bme_voc_ppm", "bme_altitude_m", "k30_co2_ppm", 
                          "ozone_ppb", "sensor1_pm1_ugm3", "sensor1_pm2.5_ugm3", "sensor1_pm10_ugm3", 
                          "sensor2_pm1_ugm3", "sensor2_pm2.5_ugm3", "sensor2_pm10_ugm3"))
    
    file_data[, `:=`(long = NA_real_, lat = NA_real_, neighborhood = NA_character_,
                     vulnerability_score = NA_real_, vulnerability_level = NA_character_,
                     land_use_code = NA_character_)]
    
    setcolorder(file_data, c("timestamp", "long", "lat", "neighborhood", 
                             "vulnerability_score", "vulnerability_level", 
                             "land_use_code", "arduino_id", "program", 
                             "bme_temp_c", "bme_pressure_hpa", "bme_humidity_percent", 
                             "bme_voc_ppm", "bme_altitude_m", "k30_co2_ppm", "ozone_ppb", 
                             "sensor1_pm1_ugm3", "sensor1_pm2.5_ugm3", "sensor1_pm10_ugm3",
                             "sensor2_pm1_ugm3", "sensor2_pm2.5_ugm3", "sensor2_pm10_ugm3"))
    
    file_data[file_data == -7999] <- NA
    file_data[, timestamp := as.character(timestamp)]
    
    convert_timestamp <- function(ts) {
      ts_parsed <- ymd_hms(ts, tz = "UTC")
      if (is.na(ts_parsed)) ts_parsed <- mdy_hm(ts, tz = "UTC")
      return(ts_parsed)
    }
    
    file_data[, timestamp := sapply(timestamp, convert_timestamp)]
    file_data <- file_data[!is.na(timestamp)]
    file_data[, timestamp := as.POSIXct(timestamp)]
    file_data <- file_data[!duplicated(file_data$timestamp)]
    
    file_data[, timestamp_hour := floor_date(timestamp, unit = "hour")]
    file_data$ozone_ppb[is.nan(file_data$ozone_ppb)] <- NA
    file_data$ozone_ppm <- file_data$ozone_ppb / 1000
    
    hourly_avg <- file_data[, .(
      sensor1_pm2.5_ugm3_avg = round(mean(sensor1_pm2.5_ugm3, na.rm = TRUE), 2),
      sensor2_pm2.5_ugm3_avg = round(mean(sensor2_pm2.5_ugm3, na.rm = TRUE), 2),
      sensor1_pm10_ugm3_avg  = round(mean(sensor1_pm10_ugm3, na.rm = TRUE), 2),
      sensor2_pm10_ugm3_avg  = round(mean(sensor2_pm10_ugm3, na.rm = TRUE), 2),
      bme_temp_c_avg         = round(mean(bme_temp_c, na.rm = TRUE), 2),
      bme_humidity_percent_avg = round(mean(bme_humidity_percent, na.rm = TRUE), 2),
      ozone_ppm_avg          = round(mean(ozone_ppm, na.rm = TRUE), 2),
      k30_co2_ppm_avg        = round(mean(k30_co2_ppm, na.rm = TRUE), 2)
    ), by = timestamp_hour]
    
    hourly_avg$date <- as.Date(hourly_avg$timestamp_hour)
    hourly_avg$time <- format(as.POSIXct(hourly_avg$timestamp_hour), "%H:%M:%S")
    hourly_avg <- select(hourly_avg, -timestamp_hour)
    
    file_data[, timestamp_day := floor_date(timestamp, unit = "day")]
    daily_avg <- file_data[, .(
      sensor1_pm2.5_ugm3_avg = round(mean(sensor1_pm2.5_ugm3, na.rm = TRUE), 2),
      sensor2_pm2.5_ugm3_avg = round(mean(sensor2_pm2.5_ugm3, na.rm = TRUE), 2),
      sensor1_pm10_ugm3_avg  = round(mean(sensor1_pm10_ugm3, na.rm = TRUE), 2),
      sensor2_pm10_ugm3_avg  = round(mean(sensor2_pm10_ugm3, na.rm = TRUE), 2),
      bme_temp_c_avg         = round(mean(bme_temp_c, na.rm = TRUE), 2),
      bme_humidity_percent_avg = round(mean(bme_humidity_percent, na.rm = TRUE), 2),
      ozone_ppm_avg          = round(mean(ozone_ppm, na.rm = TRUE), 2),
      k30_co2_ppm_avg        = round(mean(k30_co2_ppm, na.rm = TRUE), 2)
    ), by = timestamp_day]
    daily_avg$timestamp_day <- format(daily_avg$timestamp_day, "%Y-%m-%d")
    
    # Create sheet
    new_sheet <- drive_create(name = file_name, type = "spreadsheet", path = as_id(target_folder_id))
    sheet_url <- new_sheet$id
    drive_rename(sheet_url, name = file_name)
    
    sheet_names <- c("About AQI", "PM2.5", "PM10", "Ozone", "Carbon-Dioxide", "About the project")
    existing_sheets <- sheet_names(ss = sheet_url)
    
    if ("Sheet1" %in% existing_sheets && !("About AQI" %in% existing_sheets)) {
      sheet_rename(ss = sheet_url, sheet = "Sheet1", new_name = "About AQI")
      existing_sheets <- sheet_names(ss = sheet_url)
    }
    
    for (sheet in sheet_names) {
      if (!(sheet %in% existing_sheets)) {
        sheet_add(ss = sheet_url, sheet = sheet)
      }
    }
    
    # PM2.5
    pm25_data <- hourly_avg %>% transmute(
      Date = date,
      Time = time,
      `Sensor 1` = sensor1_pm2.5_ugm3_avg,
      `Sensor 2` = sensor2_pm2.5_ugm3_avg,
      `Sensor Average` = rowMeans(cbind(sensor1_pm2.5_ugm3_avg, sensor2_pm2.5_ugm3_avg), na.rm = TRUE)
    )
    range_write(data = pm25_data, ss = sheet_url, sheet = "PM2.5", range = "A1", col_names = TRUE)
    
    daily_summary_pm25 <- daily_avg %>%
      transmute(Day = timestamp_day, `Daily PM2.5 Average Concentration` = rowMeans(cbind(sensor1_pm2.5_ugm3_avg, sensor2_pm2.5_ugm3_avg), na.rm = TRUE))
    range_write(ss = sheet_url, data = daily_summary_pm25, sheet = "PM2.5", range = "G1", col_names = TRUE)
    
    # PM10
    pm10_data <- hourly_avg %>% transmute(
      Date = date,
      Time = time,
      `Sensor 1` = sensor1_pm10_ugm3_avg,
      `Sensor 2` = sensor2_pm10_ugm3_avg,
      `Sensor Average` = rowMeans(cbind(sensor1_pm10_ugm3_avg, sensor2_pm10_ugm3_avg), na.rm = TRUE)
    )
    range_write(data = pm10_data, ss = sheet_url, sheet = "PM10", range = "A1", col_names = TRUE)
    daily_summary_pm10 <- daily_avg %>%
      transmute(Day = timestamp_day, `Daily PM10 Average Concentration` = rowMeans(cbind(sensor1_pm10_ugm3_avg, sensor2_pm10_ugm3_avg), na.rm = TRUE))
    range_write(ss = sheet_url, data = daily_summary_pm10, sheet = "PM10", range = "G1", col_names = TRUE)
    
    # Ozone
    ozone_data <- hourly_avg %>% transmute(Date = date, Time = time, Ozone = ozone_ppm_avg)
    range_write(data = ozone_data, ss = sheet_url, sheet = "Ozone", range = "A1", col_names = TRUE)
    daily_ozone <- daily_avg %>% transmute(Day = timestamp_day, `Daily Average Ozone` = ozone_ppm_avg)
    range_write(ss = sheet_url, data = daily_ozone, sheet = "Ozone", range = "E1", col_names = TRUE)
    
    # CO2
    co2_data <- hourly_avg %>% transmute(Date = date, Time = time, `Carbon Dioxide` = k30_co2_ppm_avg)
    range_write(data = co2_data, ss = sheet_url, sheet = "Carbon-Dioxide", range = "A1", col_names = TRUE)
    daily_co2 <- daily_avg %>% transmute(Day = timestamp_day, `Daily Average Carbon Dioxide` = k30_co2_ppm_avg)
    range_write(ss = sheet_url, data = daily_co2, sheet = "Carbon-Dioxide", range = "E1", col_names = TRUE)
    
    # Copy over static content
    template_sheet <- "https://docs.google.com/spreadsheets/d/17Vbd4FFzBijvWdVH4z56M_iWWdRQbYU0LyU_niB65sI"
    sheet_write(read_sheet(template_sheet, sheet = "About AQI"), ss = sheet_url, sheet = "About AQI")
    sheet_write(read_sheet(template_sheet, sheet = "About the project"), ss = sheet_url, sheet = "About the project")
    
    message(paste("Processed:", file_name))
  }
}


drive_auth() # Authenticate drive
gs4_auth() # Access and modify sheets

#Process a certain number of text files
txt_to_sheets(
  data_folder_id = "1G2Sug5Wazqn3AGz8W7Hyq0LR-Za3WeDWnEcIL46JQEZ4IYZ62flURge365CDaMpxAYZoYjbT",
  target_folder_id = "1h1lnmR7NSCcO2Ci_8WmHX2wMSBsU1Zao",
  num_files = 4
)

# Process all text files not processed and transferred to the target folder
txt_files <- drive_ls(path = as_id(data_folder_id), pattern = "(?i)\\.txt$")
txt_to_sheets(
  data_folder_id = "1G2Sug5Wazqn3AGz8W7Hyq0LR-Za3WeDWnEcIL46JQEZ4IYZ62flURge365CDaMpxAYZoYjbT",
  target_folder_id = "1h1lnmR7NSCcO2Ci_8WmHX2wMSBsU1Zao",
  num_files = nrow(txt_files)
)