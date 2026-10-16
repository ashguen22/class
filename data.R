end_date <- Sys.Date() - 5
start_date <- seq(end_date, length.out = 2, by = "-5 years")[2]

weather_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=58.3019&longitude=-134.4197",
  "&start_date=", format(start_date, "%Y-%m-%d"),
  "&end_date=", format(end_date, "%Y-%m-%d"),
  "&hourly=temperature_2m,precipitation,wind_speed_10m,snowfall",
  "&timezone=America%2FJuneau&format=csv"
)

weather <- read.csv(weather_url, skip = 2, comment.char = "#")
