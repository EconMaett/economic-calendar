library(tidyverse)
library(rvest)

# Single day ----
my_date <- today()
my_date
page_link <- paste0("https://www.forexfactory.com/calendar?day=", my_date)
page_link

table <- read_html("https://www.forexfactory.com/calendar") |> 
  html_elements(".calendar__table") |> 
  html_table()

table <- table[[1]]
# You want
# Date, Currency, Impact, Detail, Actual, Forecast, Previous
# Note: Currency serves as country code too here
table

table |> 
  setNames(as.character(1:11)) |> 
  select(-as.character(c(3, 5, 7, 11))) |> 
  setNames(c("date", "time", "currency", "event", "actual", "forecast", "previous")) |> 
  filter(!(date == currency)) |> 
  mutate(time = ifelse(time == "All Day", "", time)) |> 
  mutate(date = paste(date, year(today()))) |> 
  mutate(date = str_sub(date, 5, 15)) |> 
  mutate(date = as_date(date, format = "%b %d %Y")) |> 
  mutate(date_time = paste0(date, " ", time)) |> 
  mutate(date_time = str_trim(date_time)) |>
  mutate(date_time = ymd_hm(date_time)) |> 
  select(date_time, currency, event, actual, forecast, previous) |> 
  mutate(across(.cols = c(actual, forecast, previous), .fns = ~ parse_number(.x))) |> 
  mutate(change = actual - previous, surprise = actual - forecast)

# Apply to all dates ----
date_start <- today()
date_end <- today() %m+% months(3)

date_range <- seq(as.Date(date_start), as.Date(date_end), by = "days")

# Remove Saturdays and Sundays
date_range <- date_range[!(wday(date_range, label = TRUE, locale = "en") %in% c("Sat", "Sun"))]

months_rng <- tolower(month(date_range, label = TRUE, abbr = TRUE, locale = "en"))
days_rng <- day(date_range)
years_rng <- year(date_range)
date_range <- paste0(months_rng, days_rng, ".", years_rng)
date_range

page_links <- paste0("https://www.forexfactory.com/calendar?day=", date_range)
page_links

tables <- purrr::map(
  .x = page_links, 
  .f = ~ read_html(.x) |> 
    html_elements(".calendar__table") |> 
    html_table() |> 
    pluck(1)
)

clean_tbl <- function(x) {
  x <- x |> 
    setNames(as.character(1:11)) |> 
    select(-as.character(c(3, 5, 7, 11))) |> 
    setNames(c("date", "time", "currency", "event", "actual", "forecast", "previous")) |> 
    filter(!(date == currency)) |> 
    mutate(time = ifelse(time == "All Day", "", time)) |> 
    mutate(date = paste(date, year(today()))) |> 
    mutate(date = str_sub(date, 5, 15)) |> 
    mutate(date = as_date(date, format = "%b %d %Y")) |> 
    mutate(date_time = paste0(date, " ", time)) |> 
    mutate(date_time = str_trim(date_time)) |>
    mutate(date_time = ymd_hm(date_time)) |> 
    select(date_time, currency, event, actual, forecast, previous) |> 
    mutate(across(.cols = c(actual, forecast, previous), .fns = ~ parse_number(.x))) |> 
    mutate(change = actual - previous, surprise = actual - forecast)
  
  return(x)
}

tables_clean <- purrr::map(
  .x = tables,
  .f = ~ clean_tbl(.x)
)

table_unified <- purrr::map_dfr(
  .x = tables_clean,
  .f = ~ bind_rows(.x)
)

table_unified <- table_unified |> 
  distinct()

table_unified <- table_unified |> 
  fill(date_time, .direction = "down")

table_unified

write_csv(x = table_unified, file = "data/economic-calendar.csv")
