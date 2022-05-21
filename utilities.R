readAPI <- function(link){
  
  res <- httr::GET(link)
  
  df <- jsonlite::fromJSON(rawToChar(res$content))
  
  return(df)
  
}


toNumber <- function(string){
  
  if(grepl(x = string ,pattern =  "E", fixed = TRUE)){
    
    n <- as.numeric(gsub(pattern = ",",replacement = ".",string))
    
  }else{
    
    n <- as.numeric(string)
  }
  
  return(n)
  
}

readPDF <- function(filePDF){
  
  pdf_data <- strsplit(filePDF, "\n")[[1]]
  
  facility_name <- strsplit(pdf_data[3], ":")[[1]][2]
  facility_name <- trimws(facility_name,which = "left")
  
  facility_id <- strsplit(pdf_data[4], ":")[[1]][2]
  facility_id <- trimws(facility_id,which = "left")
  
  country <- strsplit(pdf_data[6], ":")[[1]][2]
  country <- gsub(pattern = "CONTINENT",replacement = "",country)
  country <- trimws(country,which = "both")
  
  continent <- strsplit(pdf_data[6], ":")[[1]][3]
  continent <- trimws(continent,which = "left")
  
  city <- strsplit(pdf_data[7], ":")[[1]][2]
  city <- trimws(city,which = "left")
  
  # EPRTRSectorCode <- strsplit(pdf_data[9], ":")[[1]][2]
  # EPRTRSectorCode <- gsub(pattern = "eprtrSectorName",replacement = "",EPRTRSectorCode)
  # sector_code <- as.numeric(trimws(EPRTRSectorCode,which = "both"))
  
  eprtrSectorName <- strsplit(pdf_data[9], ":")[[1]][3]
  sector_name <- trimws(eprtrSectorName,which = "left")
  
  MainActivityCode <- strsplit(pdf_data[11], ":")[[1]][2]
  main_activity_code <- trimws(MainActivityCode,which = "left")
  
  target_realase <- strsplit(pdf_data[14], ":")[[1]][2]
  target_realase <- gsub(pattern = "pollutant",replacement = "",target_realase)
  target_realase <- trimws(target_realase,which = "both")
  
  pollutant <- strsplit(pdf_data[14], ":")[[1]][3]
  pollutant <- trimws(pollutant,which = "left")
  
  # emissions <- strsplit(pdf_data[15], ":")[[1]][2]
  # emissions <- toNumber(trimws(emissions,which = "left"))
  
  # Date 
  
  day <- strsplit(pdf_data[17], ":")[[1]][2]
  day <- gsub(pattern = "MONTH",replacement = "",day)
  day <- toNumber(trimws(day,which = "both"))
  
  month <- strsplit(pdf_data[17], ":")[[1]][3]
  month <- gsub(pattern = "YEAR",replacement = "",month)
  month <- toNumber(trimws(month,which = "both"))
  
  year <- strsplit(pdf_data[17], ":")[[1]][4]
  year <- toNumber(trimws(year,which = "left"))
  
  # METEOROLOGICAL CONDITIONS
  
  max_wind_speed <- strsplit(pdf_data[23], ":")[[1]][2]
  max_wind_speed <- gsub(pattern = "min_wind_speed",replacement = "",max_wind_speed)
  max_wind_speed <- toNumber(trimws(max_wind_speed,which = "both"))
  
  min_wind_speed <- strsplit(pdf_data[23], ":")[[1]][3]
  min_wind_speed <- gsub(pattern = "avg_wind_speed",replacement = "",min_wind_speed)
  min_wind_speed <- toNumber(trimws(min_wind_speed,which = "both"))
  
  avg_wind_speed <- strsplit(pdf_data[23], ":")[[1]][4]
  avg_wind_speed <- toNumber(trimws(avg_wind_speed,which = "left"))
  
  
  max_temp <- strsplit(pdf_data[28], ":")[[1]][2]
  max_temp <- gsub(pattern = "min_temp",replacement = "",max_temp)
  max_temp <- toNumber(trimws(max_temp,which = "both"))
  
  min_temp <- strsplit(pdf_data[28], ":")[[1]][3]
  min_temp <- gsub(pattern = "avg_temp",replacement = "",min_temp)
  min_temp <- toNumber(trimws(min_temp,which = "both"))
  
  avg_temp <- strsplit(pdf_data[28], ":")[[1]][4]
  avg_temp <- toNumber(trimws(avg_temp,which = "left"))
  
  
  fog_days <- strsplit(pdf_data[33], ":")[[1]][2]
  fog_days <- toNumber(trimws(fog_days,which = "left"))
  
  reporter_name <- strsplit(pdf_data[38], ":")[[1]][2]
  reporter_name <- trimws(reporter_name,which = "left")
  
  city_id <- gsub(pattern = "CITY_ID",replacement = "",pdf_data[43])
  city_id <- trimws(city_id,which = "left")
  
  
  df <- data.frame(facility_name = facility_name, facility_id = facility_id, country = country, continent = continent, city = city,
                  sector_name = sector_name, main_activity_code = main_activity_code,
                   target_realase = target_realase, pollutant = pollutant, year = year, month = month, day = day,
                   max_wind_speed = max_wind_speed, min_wind_speed = min_wind_speed, avg_wind_speed = avg_wind_speed, max_temp = max_temp,
                   min_temp = min_temp, avg_temp = avg_temp, fog_days = fog_days, reporter_name = reporter_name, city_id = city_id  )
  
  return(df)
}
