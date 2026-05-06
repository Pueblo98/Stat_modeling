unzip("~/Downloads/flights_sample_3m.csv.zip", exdir = "~/Downloads/")   

flights_raw <- read.csv("~/Downloads/flights_sample_3m.csv")                                                                                                                                                  
names(flights_raw)                                                                                                                                                

flights <- flights_raw |>                                                                                                                                                                                     
  select(FL_DATE, AIRLINE, ORIGIN, ORIGIN_CITY, DEST, DEST_CITY,
         DEP_DELAY, ARR_DELAY, CANCELLED, CANCELLATION_CODE,                                                                                                                                                  
         DELAY_DUE_WEATHER, DELAY_DUE_CARRIER, DELAY_DUE_NAS,
         AIR_TIME, DISTANCE)                                                                                                                                                                                  

rm(flights_raw)                                                                                                                                                                                               
gc()            

saveRDS(flights, "~/Documents/smproject/flights_clean.rds")                                                                                                                                                   
dim(flights)
head(flights)

library(ggplot2)                                                                                                                                                                                              
library(dplyr)

# What airports appear most?
flights |> count(ORIGIN, sort = TRUE) |> head(10)                                                                                                                                                             

# Weather delay distribution (non-zero only)                                                                                                                                                                  
flights |>                                                                                                                                                                                                    
  filter(DELAY_DUE_WEATHER > 0) |>                                                                                                                                                                            
  ggplot(aes(x = DELAY_DUE_WEATHER)) +
  geom_histogram(bins = 50) +                                                                                                                                                                                 
  xlim(0, 300) +
  labs(title = "Distribution of weather delays (minutes)",                                                                                                                                                    
       x = "Weather delay (min)", y = "Count")                                                                                                                                                                

# Cancellation rate by month                                                                                                                                                                                  
flights |>      
  mutate(month = substr(FL_DATE, 1, 7)) |>                                                                                                                                                                    
  group_by(month) |>
  summarise(cancel_rate = mean(CANCELLED)) |>                                                                                                                                                                 
  ggplot(aes(x = month, y = cancel_rate, group = 1)) +
  geom_line() +                                                                                                                                                                                               
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Monthly cancellation rate 2019-2023") 

flights |> count(ORIGIN, sort = TRUE) |> head(10)   

range(flights$FL_DATE)

flights_5 <- flights |>                                                                                                                                                                                       
  filter(ORIGIN %in% c("ATL", "ORD", "DEN", "LAX", "MCO"))                                                                                                                                                    

dim(flights_5)                                                                                                                                                                                                
saveRDS(flights_5, "~/Documents/smproject/flights_5airports.rds")


install.packages("worldmet")                                                                                                                                                                                  
library(worldmet)                                                                                                                                                                                             

# Known NOAA station codes for each airport                                                                                                                                                                   
station_codes <- c(
  ATL = "722190-13874",
  ORD = "725300-94846",
  DEN = "722060-03017",                                                                                                                                                                                       
  LAX = "722950-23174",
  MCO = "722050-12815"                                                                                                                                                                                        
)               

#---not ran-----
# Combine and aggregate to daily averages                                                                                                                                                                     
library(dplyr)
weather_daily <- bind_rows(weather_list) |>                                                                                                                                                                   
  mutate(date = as.Date(date)) |>                                                                                                                                                                             
  group_by(airport, date) |>
  summarise(                                                                                                                                                                                                  
    temp_mean = mean(air_temp, na.rm = TRUE),                                                                                                                                                                 
    temp_max  = max(air_temp, na.rm = TRUE),
    precip    = sum(precip, na.rm = TRUE),                                                                                                                                                                    
    wind_speed = mean(ws, na.rm = TRUE),                                                                                                                                                                      
    .groups = "drop"
  )                                                                                                                                                                                                           

saveRDS(weather_daily, "~/Documents/smproject/weather_daily.rds")                                                                                                                                             
head(weather_daily)





#------
all_stations <- import_ghcn_stations()
head(all_stations)                                                                                                                                                                                            
names(all_stations)

all_stations |> filter(grepl("ATLANTA|HARTSFIELD", name, ignore.case = TRUE))                                                                                                                                 
all_stations |> filter(grepl("O'HARE|OHARE", name, ignore.case = TRUE))                                                                                                                                       
all_stations |> filter(grepl("DENVER", name, ignore.case = TRUE))                                                                                                                                             
all_stations |> filter(grepl("LOS ANGELES", name, ignore.case = TRUE))                                                                                                                                        
all_stations |> filter(grepl("ORLANDO", name, ignore.case = TRUE)) 

station_ids <- c(                                                                                                                                                                                             
  ATL = "USW00013874",  # Atlanta Hartsfield
  ORD = "USW00094846",  # Chicago O'Hare                                                                                                                                                                      
  DEN = "USW00003017",  # Denver International
  LAX = "USW00023174",  # Los Angeles International                                                                                                                                                           
  MCO = "USW00012815"   # Orlando International                                                                                                                                                               
)

install.packages("purrr")                                                                                                                                                                                     
install.packages("worldmet")    `                                                                                                                                                                        
library(dplyr)                                                                                                                                                                                                

library(readr)                                                                                                                                                                                                

# Direct NOAA ISD download function                                                                                                                                                                           
get_noaa_weather <- function(station_id, year) {
  url <- paste0("https://www.ncei.noaa.gov/data/global-hourly/access/",                                                                                                                                       
                year, "/", station_id, ".csv")                                                                                                                                                                
  read_csv(url, show_col_types = FALSE) |>                                                                                                                                                                    
    mutate(year = year)                                                                                                                                                                                       
}                                                                                                                                                                                                             

# Test ATL 2019 first                                                                                                                                                                                         
atl_2019 <- get_noaa_weather("72219013874", 2019)
head(atl_2019)                                                                                                                                                                                                
names(atl_2019)

parse_temp <- function(tmp) {                                                                                                                                                                                 
  val <- as.numeric(substr(tmp, 1, 5)) / 10
  ifelse(abs(val) > 80, NA, val)  # remove physically impossible values                                                                                                                                       
}                                                                                                                                                                                                             

atl_daily <- atl_2019 |>                                                                                                                                                                                      
  mutate(                                                                                                                                                                                                     
    date = as.Date(DATE),                                                                                                                                                                                     
    temp = parse_temp(TMP)
  ) |>                                                                                                                                                                                                        
  filter(!is.na(temp)) |>
  group_by(date) |>
  summarise(
    temp_mean = mean(temp, na.rm = TRUE),
    temp_max  = max(temp, na.rm = TRUE),                                                                                                                                                                      
    temp_min  = min(temp, na.rm = TRUE),
    .groups = "drop"                                                                                                                                                                                          
  )             

head(atl_daily) 



station_noaa <- c(                                                                                                                                                                                            
  ATL = "72219013874",
  ORD = "72530094846",                                                                                                                                                                                        
  DEN = "72206003017",
  LAX = "72295023174",                                                                                                                                                                                        
  MCO = "72205012815"
)                                                                                                                                                                                                             

parse_temp <- function(tmp) {                                                                                                                                                                                 
  val <- as.numeric(substr(tmp, 1, 5)) / 10
  ifelse(abs(val) > 80, NA, val)
}                                                                                                                                                                                                             

weather_all <- list()                                                                                                                                                                                         

for (airport in names(station_noaa)) {
  for (yr in 2019:2023) {
    message("Downloading ", airport, " - ", yr)                                                                                                                                                               
    tryCatch({
      raw <- get_noaa_weather(station_noaa[airport], yr)                                                                                                                                                      
      daily <- raw |>                                                                                                                                                                                         
        mutate(date = as.Date(DATE), temp = parse_temp(TMP)) |>
        filter(!is.na(temp)) |>                                                                                                                                                                               
        group_by(date) |>
        summarise(temp_mean = mean(temp, na.rm = TRUE),                                                                                                                                                       
                  temp_max  = max(temp, na.rm = TRUE),
                  temp_min  = min(temp, na.rm = TRUE),                                                                                                                                                        
                  .groups = "drop") |>
        mutate(airport = airport)                                                                                                                                                                             
      weather_all[[paste(airport, yr)]] <- daily
    }, error = function(e) message("Failed: ", airport, yr, " - ", e$message))                                                                                                                                
  }                                                                                                                                                                                                           
}                                                                                                                                                                                                             

weather_daily <- bind_rows(weather_all)
saveRDS(weather_daily, "~/Documents/smproject/weather_daily.rds")
dim(weather_daily)


# Add date column to flights                                                                                                                                                                                  
flights_5 <- flights_5 |>
  mutate(date = as.Date(FL_DATE))                                                                                                                                                                             

# Merge weather onto flights by airport + date                                                                                                                                                                
flights_weather <- flights_5 |>
  left_join(weather_daily, by = c("ORIGIN" = "airport", "date" = "date"))                                                                                                                                     

dim(flights_weather)
summary(flights_weather$temp_mean) 


flights_weather |>                                                                                                                                                                                            
  group_by(ORIGIN) |>                                                                                                                                                                                         
  summarise(na_temp = sum(is.na(temp_mean)), total = n())



all_stations |>
  filter(grepl("DENVER INTL", name, ignore.case = TRUE))

get_noaa_weather("72565003017", 2019) |> head()   

den_weather <- list()                                                                                                                                                                                         
for (yr in 2019:2023) {
  message("Downloading DEN - ", yr)                                                                                                                                                                           
  tryCatch({                                                                                                                                                                                                  
    raw <- get_noaa_weather("72565003017", yr)
    daily <- raw |>                                                                                                                                                                                           
      mutate(date = as.Date(DATE), temp = parse_temp(TMP)) |>
      filter(!is.na(temp)) |>
      group_by(date) |>
      summarise(temp_mean = mean(temp, na.rm = TRUE),
                temp_max  = max(temp, na.rm = TRUE),
                temp_min  = min(temp, na.rm = TRUE),
                .groups = "drop") |>                                                                                                                                                                          
      mutate(airport = "DEN")
    den_weather[[paste("DEN", yr)]] <- daily                                                                                                                                                                  
  }, error = function(e) message("Failed: DEN ", yr, " - ", e$message))
}                                                                                                                                                                                                             

den_daily <- bind_rows(den_weather)                                                                                                                                                                           

# Add to existing weather and re-merge                                                                                                                                                                        
weather_daily <- bind_rows(weather_daily, den_daily)
saveRDS(weather_daily, "~/Documents/smproject/weather_daily.rds")                                                                                                                                             

# Re-merge with flights                                                                                                                                                                                       
flights_weather <- flights_5 |>                                                                                                                                                                               
  mutate(date = as.Date(FL_DATE)) |>                                                                                                                                                                          
  left_join(weather_daily, by = c("ORIGIN" = "airport", "date" = "date"))

flights_weather |>
  group_by(ORIGIN) |>                                                                                                                                                                                         
  summarise(na_temp = sum(is.na(temp_mean)), total = n())

weather_daily <- weather_daily |> distinct(airport, date, .keep_all = TRUE)                                                                                                                                   

flights_weather <- flights_5 |>
  mutate(date = as.Date(FL_DATE)) |>                                                                                                                                                                          
  left_join(weather_daily, by = c("ORIGIN" = "airport", "date" = "date"))

saveRDS(flights_weather, "~/Documents/smproject/flights_weather.rds")
dim(flights_weather) 
library(ggplot2)
ggplot(flights_weather, aes(x = temp_mean, y = DEP_DELAY)) +                                                                                                                                                  
  geom_smooth() +                                                                                                                                                                                             
  facet_wrap(~ORIGIN) +
  labs(title = "Temperature vs departure delay by airport",                                                                                                                                                   
       x = "Mean daily temperature (°C)", y = "Departure delay (min)")

install.packages("mgcv")                                                                                                                                                                                      
library(mgcv)   
library(dplyr) 
# Filter out COVID period and cancelled flights                                                                                                                                                               
flights_model <- flights_weather |>
  filter(CANCELLED == 0,                                                                                                                                                                                      
         !(FL_DATE >= "2020-03-01" & FL_DATE <= "2021-06-01")) |>                                                                                                                                             
  mutate(month = as.integer(substr(FL_DATE, 6, 7)),                                                                                                                                                           
         covid = as.integer(FL_DATE >= "2020-03-01"))                                                                                                                                                         

gam_fit <- gam(DEP_DELAY ~ s(temp_mean) + s(month, bs = "cc") + ORIGIN,                                                                                                                                       
               data = flights_model,
               method = "REML")                                                                                                                                                                               

summary(gam_fit)
