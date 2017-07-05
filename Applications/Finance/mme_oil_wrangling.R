library(readr)
library(dplyr)
library(lubridate)
library(purrr)

setwd("/Users/opardo/Downloads/")

get_last_obs <- function(slice) {
  sunday_date <- ymd(slice$date[7])
  last_value <- rev(slice$mme[!is.na(slice$mme)])[1]
  return(
    data_frame(date = ymd(sunday_date), mme = last_value)
  )
}

dates <- data_frame(
  date = unlist(seq(dmy('04-07-2016'), dmy('02-07-2017'), by = "day"))
)

sunday_dates <- dates %>% 
  mutate(weekday = weekdays(date)) %>% 
  filter(weekday == "Sunday")

df <- read_csv("mme.csv") %>% 
  mutate(date = dmy(date)) %>% 
  right_join(dates, by = "date") %>% 
  mutate(
    weekday = weekdays(date),
    week = unlist(lapply(1:52, function(x) rep(x,7)))
  ) %>% 
  slice_rows("week") %>% 
  by_slice(get_last_obs, .collate = "rows") %>% 
  mutate(date = unlist(sunday_dates$date)) %>% 
  select(-week)
  
library(clipr)
write_clip(df)

