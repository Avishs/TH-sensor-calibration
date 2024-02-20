# libraries and path

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
# ========= o
script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_path)


# Merge all the data for one sensor

# open each folder and read in and merge all the files

sensor_data <- data.frame()

setwd("./sensor_data_2/")

for (folder in list.dirs()){
  print(folder)
  if (str_detect(folder, "([S][e])\\w+")){
    
    df <- 
      folder %>% 
      list.files(recursive = FALSE, full.names = TRUE) %>% 
      lapply(read_csv, col_types = cols(.default = "c")) %>% 
      bind_rows() %>% 
      filter(Date!="Date") %>% 
      select_if(~!all(is.na(.))) %>% 
      mutate(Date = iconv(Date, "UTF-8", "UTF-8",sub='')) %>% 
      mutate(Hour = iconv(Hour, "UTF-8", "UTF-8",sub='')) %>% 
      mutate(Minute = iconv(Minute, "UTF-8", "UTF-8",sub='')) %>% 
      mutate(DateTime = dmy_hms(paste0(Date," ", Hour, ":", Minute,":","00"), quiet = TRUE)) %>% 
      select(-c("Date", "Hour", "Minute")) %>% 
      filter(DateTime >= as_date("2024-01-02")) %>%
      mutate(Air_Temp = str_replace_all(Air_Temp, "[^0-9.-]", "")) %>% 
      mutate(Humidity = str_replace_all(Humidity, "[^0-9.-]", "")) %>% 
      mutate(Surface_Temp = str_replace_all(Surface_Temp, "[^0-9.-]", "")) 
    
    df$sensor <- str_replace(strsplit(folder, "/")[[1]][2]," ", "_")
    
    sensor_data <- sensor_data %>% rbind(df)
    
  }
}

# read weather station file
setwd('..')
WeatherS <- read_excel("./weather_station_data/Jan_Feb_2024.xlsx")

# hourly averages

sensor_data_hourly <- sensor_data %>%
  #mutate(hourtime = cut(DateTime, breaks='hour')) %>%
  group_by(sensor, hour(DateTime)) %>%
  summarize(Air_Temp = mean(Humidity, na.rm = TRUE))