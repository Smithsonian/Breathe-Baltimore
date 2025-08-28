library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
library(ggiraph)
library(DT)

# Increase the maximum file upload size to 50MB
options(shiny.maxRequestSize = 50*1024^2)

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
  # Debug print to see what values are being passed
  print(paste("classify_aqi called with value:", value, "pollutant:", pollutant))
  
  # Check if value is NA, NaN, Inf, -Inf, or negative
  if (is.na(value) || is.nan(value) || is.infinite(value) || value < 0) {
    print(paste("Returning NA for value:", value))
    return(NA)
  }
  
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
  if (is.na(pm25) || is.nan(pm25)) {
    return(NA)
  } else if (pm25 <= 9.0) {
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
  if (is.na(pm10) || is.nan(pm10)) {
    return(NA)
  } else if (pm10 <= 54) {
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


# Server
server <- function(input, output) {
  
  # Reactive function to process and clean uploaded data
  aqi_data <- reactive({
    req(input$file)
    
    withProgress(message = 'Processing data...', value = 0, {
      raw_data <- fread(input$file$datapath)
      incProgress(0.2, detail = "Renaming columns...")
      
      setnames(raw_data, c("timestamp", "arduino_id", "program", "bme_temp_c", "bme_pressure_hpa", 
                           "bme_humidity_percent", "bme_voc_ppm", "bme_altitude_m", "k30_co2_ppm", 
                           "ozone_ppb", "sensor1_pm1_ugm3", "sensor1_pm2.5_ugm3", "sensor1_pm10_ugm3", 
                           "sensor2_pm1_ugm3", "sensor2_pm2.5_ugm3", "sensor2_pm10_ugm3"))
      
      raw_data[raw_data == -7999] <- NA
      raw_data[, timestamp := as.character(timestamp)]
      
      
      incProgress(0.2, detail = "Converting timestamps...")
      convert_timestamp <- function(ts) {
        ts_parsed <- ymd_hms(ts, tz = "UTC")
        if (is.na(ts_parsed)) {
          ts_parsed <- mdy_hm(ts, tz = "UTC")
        }
        return(ts_parsed)
      }
      raw_data[, timestamp := sapply(timestamp, convert_timestamp)]
      raw_data <- raw_data[!is.na(timestamp)]
      raw_data[, timestamp := as.POSIXct(timestamp)]
      raw_data <- raw_data[!duplicated(raw_data$timestamp)]
      raw_data[, timestamp_hour := floor_date(timestamp, unit = "hour")]
      raw_data[, timestamp_day := as.Date(timestamp, tz = "UTC")]
      
      # Handle ozone data conversion more carefully
      raw_data$ozone_ppb[is.nan(raw_data$ozone_ppb)] <- NA
      raw_data$ozone_ppb[is.infinite(raw_data$ozone_ppb)] <- NA
      raw_data$ozone_ppm <- raw_data$ozone_ppb / 1000
      raw_data$ozone_ppm[is.nan(raw_data$ozone_ppm)] <- NA
      raw_data$ozone_ppm[is.infinite(raw_data$ozone_ppm)] <- NA
      
      # Debug print ozone data summary
      print("Ozone data summary:")
      print(summary(raw_data$ozone_ppm))
      print("Number of NA ozone values:")
      print(sum(is.na(raw_data$ozone_ppm)))
      print("Number of infinite ozone values:")
      print(sum(is.infinite(raw_data$ozone_ppm)))
      
      incProgress(0.3, detail = "Calculating averages...")
      
      # Extract the file name and modify it
      #input_file_name <- tools::file_path_sans_ext(basename(input$file$name))  # Remove extension
      #output_file_name <- paste0(input_file_name, "_cleaned.csv")  # Add '_cleaned' to the filename
      
      # Define the output path for Dropbox
      #output_file_path <- file.path("~/Dropbox/Bmore_AQI/Data/sensor_data/Cleaned", output_file_name)
      
      # Save the cleaned dataset to the specified path
      #fwrite(raw_data, file = output_file_path)
      
      return(raw_data)
    })
  })
  
  hourly_avg <- reactive({
    req(aqi_data())
    
    withProgress(message = 'Calculating hourly averages...', value = 0, {
      avg_data <- aqi_data() %>%
        filter(!is.na(timestamp_hour)) %>%  # Ensure no missing timestamps
        group_by(timestamp_hour) %>%
        summarize(
          pm25_avg = ifelse(all(is.na(c(sensor1_pm2.5_ugm3, sensor2_pm2.5_ugm3))),
                            NA,
                            mean(c(sensor1_pm2.5_ugm3, sensor2_pm2.5_ugm3), na.rm = TRUE)),
          pm10_avg = ifelse(all(is.na(c(sensor1_pm10_ugm3, sensor2_pm10_ugm3))),
                            NA,
                            mean(c(sensor1_pm10_ugm3, sensor2_pm10_ugm3), na.rm = TRUE)),
          ozone_avg = mean(ozone_ppm, na.rm = TRUE),
          k30_ppm_avg = mean(k30_co2_ppm, na.rm = TRUE)
        )
      
      avg_data$aqi_pm25_avg <- sapply(avg_data$pm25_avg, function(x) {
        if (is.na(x)) NA else calculate_aqi_pm25(x)
      })
      avg_data$aqi_pm10_avg <- sapply(avg_data$pm10_avg, function(x) {
        if (is.na(x)) NA else calculate_aqi_pm10(x)
      })
      avg_data$aqi_ozone_avg <- sapply(avg_data$ozone_avg, function(x) {
        if (is.na(x)) NA else calculate_aqi_o3(x)
      })
      
  
      incProgress(1, detail = "Done!")
      
      return(avg_data)
    })
  })
  
  hourly_avg_table <- reactive({
    req(aqi_data())
    
    withProgress(message = 'Calculating hourly averages...', value = 0, {
      # Debug print to check input data
      print("Input data dimensions:")
      print(dim(aqi_data()))
      
      hourly_avg_table <- aqi_data() %>%
        group_by(timestamp_hour) %>%
        summarize(
          sensor1pm25_avg = mean(sensor1_pm2.5_ugm3, na.rm = TRUE),
          sensor2pm25_avg = mean(sensor2_pm2.5_ugm3, na.rm = TRUE),
          sensor1pm10_avg = mean(sensor1_pm10_ugm3, na.rm = TRUE),
          sensor2pm10_avg = mean(sensor2_pm10_ugm3, na.rm = TRUE),
          pm25_avg = mean(c(sensor1_pm2.5_ugm3, sensor2_pm2.5_ugm3), na.rm = TRUE),
          pm10_avg = mean(c(sensor1_pm10_ugm3, sensor2_pm10_ugm3), na.rm = TRUE),
          ozone_avg = mean(ozone_ppm, na.rm = TRUE),
          k30_ppm_avg = mean(k30_co2_ppm, na.rm = TRUE),
          .groups = 'drop'  # Drop grouping
        )
      
      # Debug print after initial aggregation
      print("After hourly aggregation dimensions:")
      print(dim(hourly_avg_table))
      
      # Replace NaN with NA for consistency
      hourly_avg_table <- hourly_avg_table %>%
        mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))
      
      # Calculate AQI values - handle NA values gracefully
      hourly_avg_table <- hourly_avg_table %>%
        mutate(
          aqi_pm25_avg = sapply(pm25_avg, function(x) if(!is.na(x)) calculate_aqi_pm25(x) else NA),
          aqi_pm10_avg = sapply(pm10_avg, function(x) if(!is.na(x)) calculate_aqi_pm10(x) else NA),
          aqi_ozone_avg = sapply(ozone_avg, function(x) if(!is.na(x)) calculate_aqi_o3(x) else NA)
        )
      
      # Add categories - handle NA values gracefully
      hourly_avg_table <- hourly_avg_table %>%
        mutate(
          pm25_category = sapply(pm25_avg, function(x) if(!is.na(x)) classify_aqi(x, "pm25") else "No Data"),
          pm10_category = sapply(pm10_avg, function(x) if(!is.na(x)) classify_aqi(x, "pm10") else "No Data"),
          ozone_category = sapply(ozone_avg, function(x) {
            if(is.na(x) || is.nan(x)) {
              return("No Data")
            } else {
              return(classify_aqi(x, "ozone"))
            }
          })
        )
      
      # Debug print final dimensions
      print("Final hourly table dimensions:")
      print(dim(hourly_avg_table))
      print("Sample of final data:")
      print(head(hourly_avg_table))
      
      return(hourly_avg_table)
    })
  })
  
  daily_avg_table <- reactive({
    req(aqi_data())
    
    withProgress(message = 'Calculating daily averages...', value = 0, {
      # Debug print to check input data
      print("Input data dimensions for daily:")
      print(dim(aqi_data()))
      
      daily_avg_table <- aqi_data() %>%
        group_by(timestamp_day) %>%
        summarize(
          sensor1pm25_avg = mean(sensor1_pm2.5_ugm3, na.rm = TRUE),
          sensor2pm25_avg = mean(sensor2_pm2.5_ugm3, na.rm = TRUE),
          sensor1pm10_avg = mean(sensor1_pm10_ugm3, na.rm = TRUE),
          sensor2pm10_avg = mean(sensor2_pm10_ugm3, na.rm = TRUE),
          pm25_avg = mean(c(sensor1_pm2.5_ugm3, sensor2_pm2.5_ugm3), na.rm = TRUE),
          pm10_avg = mean(c(sensor1_pm10_ugm3, sensor2_pm10_ugm3), na.rm = TRUE),
          ozone_avg = mean(ozone_ppm, na.rm = TRUE),
          k30_ppm_avg = mean(k30_co2_ppm, na.rm = TRUE),
          .groups = 'drop'  # Drop grouping
        )
      
      # Debug print after initial aggregation
      print("After daily aggregation dimensions:")
      print(dim(daily_avg_table))
      
      # Replace NaN with NA for consistency
      daily_avg_table <- daily_avg_table %>%
        mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))
      
      # Calculate AQI values - handle NA values gracefully
      daily_avg_table <- daily_avg_table %>%
        mutate(
          aqi_pm25_avg = sapply(pm25_avg, function(x) if(!is.na(x)) calculate_aqi_pm25(x) else NA),
          aqi_pm10_avg = sapply(pm10_avg, function(x) if(!is.na(x)) calculate_aqi_pm10(x) else NA),
          aqi_ozone_avg = sapply(ozone_avg, function(x) if(!is.na(x)) calculate_aqi_o3(x) else NA)
        )
      
      # Add categories - handle NA values gracefully
      daily_avg_table <- daily_avg_table %>%
        mutate(
          pm25_category = sapply(pm25_avg, function(x) if(!is.na(x)) classify_aqi(x, "pm25") else "No Data"),
          pm10_category = sapply(pm10_avg, function(x) if(!is.na(x)) classify_aqi(x, "pm10") else "No Data"),
          ozone_category = sapply(ozone_avg, function(x) {
            if(is.na(x) || is.nan(x)) {
              return("No Data")
            } else {
              return(classify_aqi(x, "ozone"))
            }
          })
        )
      
      # Debug print final dimensions
      print("Final daily table dimensions:")
      print(dim(daily_avg_table))
      print("Sample of final daily data:")
      print(head(daily_avg_table))
      
      return(daily_avg_table)
    })
  })
  
  
  
  output$pm25_table <- DT::renderDataTable({
    req(hourly_avg_table())
    
    # Debugging - print the structure of the data
    print("PM2.5 Table Data Structure:")
    print(str(hourly_avg_table()))
    
    tryCatch({
      data <- hourly_avg_table() %>%
        mutate(
          pm25_category = ifelse(is.na(pm25_category), "No Data", pm25_category)
        ) %>%
        select(timestamp_hour, sensor1pm25_avg, sensor2pm25_avg, pm25_avg, pm25_category) %>%
        mutate(
          timestamp_hour = format(timestamp_hour, "%m-%d-%Y %H:%M:%S"),
          sensor1pm25_avg = round(sensor1pm25_avg, 2),
          sensor2pm25_avg = round(sensor2pm25_avg, 2),
          pm25_avg = round(pm25_avg, 2)
        )
      
      # Debug print
      print("Processed PM2.5 table data:")
      print(head(data))
      
      DT::datatable(
        data,
        colnames = c("Hourly Timestamp", "Sensor 1 PM2.5 Average (µg/m³)", "Sensor 2 PM2.5 Average (µg/m³)",
                     "Overall PM2.5 Average (µg/m³)", "AQI Category"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
    }, error = function(e) {
      # Print error for debugging
      print(paste("Error in PM2.5 table rendering:", e$message))
      # Return an empty table with appropriate structure
      DT::datatable(
        data.frame(
          timestamp_hour = character(),
          sensor1pm25_avg = numeric(),
          sensor2pm25_avg = numeric(),
          pm25_avg = numeric(),
          pm25_category = character()
        ),
        colnames = c("Hourly Timestamp", "Sensor 1 PM2.5 Average (µg/m³)", "Sensor 2 PM2.5 Average (µg/m³)",
                     "Overall PM2.5 Average (µg/m³)", "AQI Category"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
    })
  })
  
  output$pm25_table_daily <- DT::renderDataTable({
    req(daily_avg_table())
    
    # Debugging - print the structure of the data
    print("PM2.5 Daily Table Data Structure:")
    print(str(daily_avg_table()))
    
    tryCatch({
      data <- daily_avg_table() %>%
        mutate(
          pm25_category = ifelse(is.na(pm25_category), "No Data", pm25_category)
        ) %>%
        select(timestamp_day, sensor1pm25_avg, sensor2pm25_avg, pm25_avg, pm25_category) %>%
        mutate(
          timestamp_day = format(timestamp_day, "%m-%d-%Y"),
          sensor1pm25_avg = round(sensor1pm25_avg, 2),
          sensor2pm25_avg = round(sensor2pm25_avg, 2),
          pm25_avg = round(pm25_avg, 2)
        )
      
      # Debug print
      print("Processed PM2.5 daily table data:")
      print(head(data))
      
      DT::datatable(
        data,
        colnames = c("Day", "Sensor 1 PM2.5 Average (µg/m³)", "Sensor 2 PM2.5 Average (µg/m³)",
                     "Overall PM2.5 Average (µg/m³)", "AQI Category"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
    }, error = function(e) {
      # Print error for debugging
      print(paste("Error in PM2.5 daily table rendering:", e$message))
      # Return an empty table with appropriate structure
      DT::datatable(
        data.frame(
          timestamp_day = character(),
          sensor1pm25_avg = numeric(),
          sensor2pm25_avg = numeric(),
          pm25_avg = numeric(),
          pm25_category = character()
        ),
        colnames = c("Day", "Sensor 1 PM2.5 Average (µg/m³)", "Sensor 2 PM2.5 Average (µg/m³)",
                     "Overall PM2.5 Average (µg/m³)", "AQI Category"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
    })
  })
  
  output$pm10_table <- DT::renderDataTable({
    req(hourly_avg_table())
    
    # Debugging step to inspect data structure
    print(head(hourly_avg_table()))
    
    hourly_avg_table() %>%
      mutate(
        #aqi_pm10_avg = ifelse(is.na(aqi_pm10_avg), 0, aqi_pm10_avg),
        #pm10_avg = ifelse(is.na(pm10_avg), 0, pm10_avg),
        pm10_category = ifelse(is.na(pm10_category), "Unknown", pm10_category)
      ) %>%
      filter(!is.na(timestamp_hour)) %>%
      select(timestamp_hour, sensor1pm10_avg, sensor2pm10_avg, pm10_avg, pm10_category) %>%
      mutate(
        timestamp_hour = format(timestamp_hour, "%Y-%m-%d %H:%M:%S"),
        pm10_avg = round(pm10_avg, 2),
        sensor1pm10_avg = round(sensor1pm10_avg, 2),
        sensor2pm10_avg = round(sensor2pm10_avg, 2)
      ) %>%
      DT::datatable(
        colnames = c("Hourly Timestamp","Sensor 1 PM10 Average (µg/m³)","Sensor 2 PM10 Average (µg/m³)",
                     "Overall PM10 Average (µg/m³) ", "AQI Category"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
  })
  
  output$pm10_table_daily <- DT::renderDataTable({
    req(daily_avg_table())
    
    # Debugging step to inspect data structure
    print(head(daily_avg_table()))
    
    daily_avg_table() %>%
      mutate(
        #aqi_pm10_avg = ifelse(is.na(aqi_pm10_avg), 0, aqi_pm10_avg),
        #pm10_avg = ifelse(is.na(pm10_avg), 0, pm10_avg),
        pm10_category = ifelse(is.na(pm10_category), "Unknown", pm10_category)
      ) %>%
      filter(!is.na(timestamp_day)) %>%
      select(timestamp_day, sensor1pm10_avg, sensor2pm10_avg, pm10_avg, pm10_category) %>%
      mutate(
        timestamp_day = format(timestamp_day, "%Y-%m-%d"),
        pm10_avg = round(pm10_avg, 2),
        sensor1pm10_avg = round(sensor1pm10_avg, 2),
        sensor2pm10_avg = round(sensor2pm10_avg, 2)
      ) %>%
      DT::datatable(
        colnames = c("Day", "Sensor 1 PM10 Average (µg/m³)","Sensor 2 PM10 Average (µg/m³)",
                     "Overall PM10 Average (µg/m³) ", "AQI Category"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
  })
  
  
  output$ozone_table <- DT::renderDataTable({
    req(hourly_avg_table())
    
    # Debugging step to inspect data structure
    print(head(hourly_avg_table()))
    
    hourly_avg_table() %>%
      mutate(
        #aqi_ozone_avg = ifelse(is.na(aqi_ozone_avg), 0, aqi_ozone_avg),
        #ozone_avg = ifelse(is.na(ozone_avg), 0, ozone_avg),
        ozone_category = ifelse(is.na(ozone_category), "Unknown", ozone_category)
      ) %>%
      filter(!is.na(timestamp_hour)) %>%
      select(timestamp_hour, ozone_avg, ozone_category) %>%
      mutate(
        ozone_avg = round(ozone_avg, 2),
        timestamp_hour = format(timestamp_hour, "%Y-%m-%d %H:%M:%S")
      ) %>%
      DT::datatable(
        colnames = c("Hourly Timestamp", "Ozone (ppm)", "AQI Category"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
  })
  
  output$ozone_table_daily <- DT::renderDataTable({
    req(daily_avg_table())
    
    # Debugging step to inspect data structure
    print(head(daily_avg_table()))
    
    daily_avg_table() %>%
      mutate(
        #aqi_ozone_avg = ifelse(is.na(aqi_ozone_avg), 0, aqi_ozone_avg),
        #ozone_avg = ifelse(is.na(ozone_avg), 0, ozone_avg),
        ozone_category = ifelse(is.na(ozone_category), "Unknown", ozone_category)
      ) %>%
      filter(!is.na(timestamp_day)) %>%
      select(timestamp_day, ozone_avg, ozone_category) %>%
      mutate(
        ozone_avg = round(ozone_avg, 2),
        timestamp_day = format(timestamp_day, "%Y-%m-%d")
      ) %>%
      DT::datatable(
        colnames = c("Daily", "Ozone (ppm)", "AQI Category"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
  })
  
  output$co2_table <- DT::renderDataTable({
    req(hourly_avg_table())
    
    # Debugging step to inspect data structure
    print(head(hourly_avg_table()))
    
    hourly_avg_table() %>%
      mutate(
        k30_ppm_avg = ifelse(is.na(k30_ppm_avg), 0, k30_ppm_avg)
      ) %>%
      filter(!is.na(timestamp_hour)) %>%
      select(timestamp_hour, k30_ppm_avg) %>%
      mutate(
        k30_ppm_avg = round(k30_ppm_avg, 2),
        timestamp_hour = format(timestamp_hour, "%Y-%m-%d %H:%M:%S")
      ) %>%
      DT::datatable(
        colnames = c("Hourly Timestamp", "CO₂ (ppm)"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
  })
  
    output$co2_table_daily <- DT::renderDataTable({
    req(daily_avg_table())
    
    # Debugging step to inspect data structure
    print(head(daily_avg_table()))
    
    daily_avg_table() %>%
      mutate(
        k30_ppm_avg = ifelse(is.na(k30_ppm_avg), 0, k30_ppm_avg)
      ) %>%
      filter(!is.na(timestamp_day)) %>%
      select(timestamp_day, k30_ppm_avg) %>%
      mutate(
        k30_ppm_avg = round(k30_ppm_avg, 2),
        timestamp_day = format(timestamp_day, "%Y-%m-%d")
      ) %>%
      DT::datatable(
        colnames = c("Day", "CO₂ (ppm)"),
        options = list(
          pageLength = 10,
          autoWidth = TRUE
        )
      )
  })
  
  
  # Plotting PM2.5 with average AQI values using points instead of lines
  output$interactive_pm25_plot <- renderGirafe({
    req(hourly_avg())
    
    pm25_plot <- ggplot() +
      geom_rect_interactive(data = aqi_categories, 
                            aes(xmin = min(hourly_avg()$timestamp_hour), 
                                xmax = max(hourly_avg()$timestamp_hour), 
                                ymin = ymin, 
                                ymax = ymax, 
                                fill = category), 
                            alpha = 0.55) +
      geom_point_interactive(data = hourly_avg(), 
                             aes(x = timestamp_hour, 
                                 y = aqi_pm25_avg, #aqi_pm25_avg pm25_avg
                                 color = "PM2.5 Average",
                                 tooltip = paste("Hour:", format(timestamp_hour, "%Y-%m-%d %H:%M"),
                                                 "\nAQI:", round(aqi_pm25_avg, 2))),
                             size = 2.5) +  # Adjust size as needed
      scale_color_manual(values = c("PM2.5 Average" = "black")) +
      scale_fill_manual(values = aqi_colors) +
      labs(x = "Timestamp", y = "AQI", color = "Sensor", fill = "AQI Categories") +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank()
      ) +
      guides(fill = guide_legend(override.aes = list(alpha = 0.55)))
    
    girafe(
      ggobj = pm25_plot,
      width_svg = 10,
      options = list(
        opts_tooltip(
          css = "background-color: white; color: black; border: 1px solid black; padding: 5px;"
        )
      )
    )
  })
  
  # Plotting PM10 with average values using points instead of lines
  output$interactive_pm10_plot <- renderGirafe({
    req(hourly_avg())
    
    pm10_plot <- ggplot() +
      geom_rect_interactive(data = aqi_categories, 
                            aes(xmin = min(hourly_avg()$timestamp_hour), 
                                xmax = max(hourly_avg()$timestamp_hour), 
                                ymin = ymin, 
                                ymax = ymax, 
                                fill = category), 
                            alpha = 0.55) +
      geom_point_interactive(data = hourly_avg(), 
                             aes(x = timestamp_hour, 
                                 y = aqi_pm10_avg, 
                                 color = "PM10 Average",
                                 tooltip = paste("Hour:", format(timestamp_hour, "%Y-%m-%d %H:%M"),
                                                 "\nAQI:", round(aqi_pm10_avg, 2))),
                             size = 2.5) +  # Adjust size as needed
      scale_color_manual(values = c("PM10 Average" = "black")) +
      scale_fill_manual(values = aqi_colors) +
      labs(x = "Timestamp", y = "AQI", color = "Sensor", fill = "AQI Categories") +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank()
      ) +
      guides(fill = guide_legend(override.aes = list(alpha = 0.55)))
    
    girafe(
      ggobj = pm10_plot,
      width_svg = 10,
      options = list(
        opts_tooltip(
          css = "background-color: white; color: black; border: 1px solid black; padding: 5px;"
        )
      )
    )
  })
  
  # Plotting CO2 data
  output$interactive_k30_plot <- renderGirafe({
    req(hourly_avg())
    
    k30_plot <- ggplot() +
      geom_point_interactive(data = hourly_avg(), 
                             aes(x = timestamp_hour, 
                                 y = k30_ppm_avg, 
                                 tooltip = paste("Hour:", format(timestamp_hour, "%Y-%m-%d %H:%M"),
                                                 "\nCO₂ (ppm):", round(k30_ppm_avg, 2))),
                             size = 2.5, 
                             color = "black") +  # CO₂ points in blue
      labs(x = "Timestamp", y = "CO₂ (ppm)") +
      theme_minimal() +
      theme(
        legend.position = "none",  # No legend for CO₂
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank()
      )
    
    girafe(
      ggobj = k30_plot,
      width_svg = 10,
      options = list(
        opts_tooltip(
          css = "background-color: white; color: black; border: 1px solid black; padding: 5px;"
        )
      )
    )
  })
  
  # Plotting Ozone data
  output$interactive_ozone_plot <- renderGirafe({
    req(hourly_avg())
    
    ozone_plot <- ggplot() +
      geom_rect_interactive(data = aqi_categories, 
                            aes(xmin = min(hourly_avg()$timestamp_hour), 
                                xmax = max(hourly_avg()$timestamp_hour), 
                                ymin = ymin, 
                                ymax = ymax, 
                                fill = category), 
                            alpha = 0.55) +
      geom_point_interactive(data = hourly_avg(), 
                             aes(x = timestamp_hour, 
                                 y = ozone_avg, 
                                 color = "Ozone Average",
                                 tooltip = paste("Hour:", format(timestamp_hour, "%Y-%m-%d %H:%M"),
                                                 "\nOzone (ppm):", round(ozone_avg, 4))),
                             size = 2.5) +  # Adjust size as needed
      scale_color_manual(values = c("Ozone Average" = "black")) +
      scale_fill_manual(values = aqi_colors) +
      labs(x = "Timestamp", y = "AQI", color = "Sensor", fill = "AQI Categories") +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank()
      ) +
      guides(fill = guide_legend(override.aes = list(alpha = 0.55)))
    
    girafe(
      ggobj = ozone_plot,
      width_svg = 10,
      options = list(
        opts_tooltip(
          css = "background-color: white; color: black; border: 1px solid black; padding: 5px;"
        )
      )
    )
  })
  
  unhealthy_days <- reactive({
    req(hourly_avg_table())
    
    data <- hourly_avg_table()
    
    unhealthy_summary <- data %>%
      mutate(
        date = as.Date(timestamp_hour),
        aqi_pm25_avg = ifelse(is.na(aqi_pm25_avg), 0, aqi_pm25_avg),
        aqi_pm10_avg = ifelse(is.na(aqi_pm10_avg), 0, aqi_pm10_avg),
        ozone_avg = ifelse(is.na(ozone_avg), 0, ozone_avg)
      ) %>%
      group_by(date) %>%
      summarize(
        pm25_unhealthy_hours = sum(aqi_pm25_avg > 100, na.rm = TRUE),
        pm10_unhealthy_hours = sum(aqi_pm10_avg > 100, na.rm = TRUE),
        ozone_unhealthy_hours = sum(ozone_avg > 100, na.rm = TRUE),
        .groups = 'drop'  # Ensure that grouping is dropped after summarize
      ) %>%
      mutate(
        pm25_unhealthy_days = ifelse(pm25_unhealthy_hours > 0, 1, 0),
        pm10_unhealthy_days = ifelse(pm10_unhealthy_hours > 0, 1, 0),
        ozone_unhealthy_days = ifelse(ozone_unhealthy_hours > 0, 1, 0)
      ) %>%
      summarize(
        total_pm25_unhealthy_days = sum(pm25_unhealthy_days, na.rm = TRUE),
        total_pm10_unhealthy_days = sum(pm10_unhealthy_days, na.rm = TRUE),
        total_ozone_unhealthy_days = sum(ozone_unhealthy_days, na.rm = TRUE),
        .groups = 'drop'
      )
    
    return(unhealthy_summary)
  })
  
  
  
  
  
  # Render PM2.5 Unhealthy Days
  output$pm25_unhealthy_days_output <- renderUI({
    req(unhealthy_days()) # Ensure the data is available
    HTML(paste("<b># of Days with Unhealthy Air Quality (AQI > 100):</b>", 
               unhealthy_days()$total_pm25_unhealthy_days, "day(s)"))
  })
  
  # Render PM10 Unhealthy Days
  output$pm10_unhealthy_days_output <- renderUI({
    req(unhealthy_days()) # Ensure the data is available
    HTML(paste("<b># of Days with Unhealthy Air Quality (AQI > 100):</b>", 
               unhealthy_days()$total_pm10_unhealthy_days, "day(s)"))
  })
  
  # Render Ozone Unhealthy Days
  output$ozone_unhealthy_days_output <- renderUI({
    req(unhealthy_days()) # Ensure the data is available
    HTML(paste("<b># of Days with Unhealthy Air Quality (AQI > 100):</b>", 
               unhealthy_days()$total_ozone_unhealthy_days, "day(s)"))
  })
  
  # Download Handler for PM2.5
  output$download_pm25 <- downloadHandler(
    filename = function() {
      paste("Hourly_PM2.5_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(hourly_avg_table())
      data <- hourly_avg_table() %>%
        select(timestamp_hour, sensor1pm25_avg, sensor2pm25_avg, pm25_avg, pm25_category) %>%
        na.omit()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_pm25_daily <- downloadHandler(
    filename = function() {
      paste("Daily_PM2.5_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(daily_avg_table())
      data_daily <- daily_avg_table() %>%
        select(timestamp_day, sensor1pm25_avg, sensor2pm25_avg, pm25_avg, pm25_category) %>%
        na.omit()
      write.csv(data_daily, file, row.names = FALSE)
    }
  )
  
  # Download Handler for PM10
  output$download_pm10 <- downloadHandler(
    filename = function() {
      paste("Hourly_PM10_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(hourly_avg_table())
      data <- hourly_avg_table() %>%
        select(timestamp_hour, sensor1pm10_avg, sensor2pm10_avg, pm10_avg, pm10_category) %>%
        na.omit()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_pm10_daily <- downloadHandler(
    filename = function() {
      paste("Daily_PM10_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(daily_avg_table())
      data_daily <- daily_avg_table() %>%
        select(timestamp_day, sensor1pm10_avg, sensor2pm10_avg, pm10_avg, pm10_category) %>%
        na.omit()
      write.csv(data_daily, file, row.names = FALSE)
    }
  )
  
  # Download Handler for Ozone
  output$download_ozone <- downloadHandler(
    filename = function() {
      paste("Hourly_Ozone_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(hourly_avg_table())
      data <- hourly_avg_table() %>%
        select(timestamp_hour, ozone_avg, ozone_category) %>%
        na.omit()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_ozone_daily <- downloadHandler(
    filename = function() {
      paste("Daily_Ozone_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(daily_avg_table())
      data_daily <- daily_avg_table() %>%
        select(timestamp_day, ozone_avg, pm10_category) %>%
        na.omit()
      write.csv(data_daily, file, row.names = FALSE)
    }
  )
  
  # Download Handler for CO₂
  output$download_co2 <- downloadHandler(
    filename = function() {
      paste("Hourly_CO2_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(hourly_avg_table())
      data <- hourly_avg_table() %>%
        select(timestamp_hour, k30_ppm_avg) %>%
        na.omit()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_co2_daily <- downloadHandler(
    filename = function() {
      paste("Daily_CO2_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(daily_avg_table())
      data_daily <- daily_avg_table() %>%
        select(timestamp_day, k30_ppm_avg) %>%
        na.omit()
      write.csv(data_daily, file, row.names = FALSE)
    }
  )

  output$aqi_table <- renderDT({
    
    aqi_colors <- c(
      "Good" = "rgba(102, 194, 165, 0.5)",  # Green with transparency
      "Moderate" = "rgba(255, 217, 114, 0.5)",  # Yellow with transparency
      "Unhealthy for Sensitive Groups" = "rgba(252, 141, 98, 0.5)",  # Orange with transparency
      "Unhealthy" = "rgba(217, 4, 41, 0.5)",  # Red with transparency
      "Very Unhealthy" = "rgba(160, 26, 125, 0.5)",  # Purple with transparency
      "Hazardous" = "rgba(100, 13, 20, 0.5)"  # Maroon with transparency
    )
    
    aqi_colors_scale <- c(
      "0 - 50" = "rgba(102, 194, 165, 0.5)",  # Green with transparency
      "51 - 100" = "rgba(255, 217, 114, 0.5)",  # Yellow with transparency
      "101 - 150" = "rgba(252, 141, 98, 0.5)",  # Orange with transparency
      "151 - 200" = "rgba(217, 4, 41, 0.5)",  # Red with transparency
      "201 - 300" = "rgba(160, 26, 125, 0.5)",  # Purple with transparency
      "301+" = "rgba(100, 13, 20, 0.5)"  # Maroon with transparency for Hazardous
    )
    
    # Data for the AQI table
    aqi_data <- data.frame(
      AQI_Scale = c("0 - 50", "51 - 100", "101 - 150", "151 - 200", "201 - 300", "301+"),
      AQI_Category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", 
                       "Unhealthy", "Very Unhealthy", "Hazardous"),
      Description = c("Air pollution poses little to no risk", 
                      "Potential health impacts for sensitive groups", 
                      "Sensitive groups may experience health effects. 100 is considered the threshold into unhealthy air.", 
                      "Everyone may experience health effects. Limit heavy outdoor activity.", 
                      "Everyone should avoid prolonged or heavy exertion outdoors", 
                      "Emergency conditions. Avoid all physical activity outdoors.")
    )
    
    datatable(aqi_data, escape = FALSE, options = list(
      pageLength = 6, 
      dom = 't',  # Only show the table, no pagination
      columnDefs = list(list(targets = 0, visible = FALSE))  # Hide the first column (row indices)
    )) %>%
      formatStyle(
        'AQI_Scale', 
        backgroundColor = styleEqual(
          c("0 - 50", "51 - 100", "101 - 150", "151 - 200", "201 - 300", "301+"), 
          aqi_colors_scale
        )
      ) %>%
      formatStyle(
        'AQI_Category', 
        backgroundColor = styleEqual(
          c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"), 
          aqi_colors
        )
      )
    

    
  })
  
}


