# declare dependencies
if (!exists("setup_sourced")) source(here::here("setup.R"))

#-----------------------------------------------------------

# load confirmed cases
confirmed <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_covid19_confirmed_global.csv")) 
death <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_covid19_deaths_global.csv"))
recovered <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_covid19_recovered_global.csv"))

#-----------------------------------------------------------

# Transform to time series
drop_columns <- c("Country/Region", "Lat", "Long")
cc <- confirmed %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "count") %>%
  mutate(date = mdy(date)) 

# time series
ts(cc[,3], start = 2020, end = 2021, frequency = 52) %>%
  autoplot() + 
  ggtitle("Time plot: prediction of confirmed counts of coronavirus up to 2021") +
  ylab("Counts") + xlab("Time")

# seasonality
diff(ts(cc[,3], start = 2020, end = 2021, frequency = 52)) %>%
  autoplot() + 
  ggtitle("Seasonality plot: prediction of confirmed counts until 2021") +
  ylab("Counts") + xlab("Time")

# subseries plots
ggsubseriesplot(diff(ts(cc[,3], start = 2020, end = 2022, frequency = 52))) +
                  ggtitle("Seasonality plot: prediction of confirmed counts of coronavirus up to 2021") +
                  ylab("Counts") + xlab("Time")

ts(cc[,3], start = 2020, end = 2021, frequency = 52) %>%
  diff() %>%
  ggtsdisplay() +
  ggtitle("Seasonality plot: prediction of confirmed counts") +
  ylab("Counts") + xlab("Time")


ts(cc[,3], start = 2020, end = 2021, frequency = 52) %>%
  diff() %>%
  ggtsdisplay() 
  
#-----------------------------------------------------------

# Forecast for confirmed cases until 2021
fit1 <- snaive(ts(cc[,3], start = 2020, end = 2022, frequency = 52))
print(summary(fit1))
checkresiduals(fit1)
autoplot(fit1)


fit2 <- ets(ts(cc[,3], start = 2020, end = 2022, frequency = 12))
print(summary(fit2))
checkresiduals(fit2)
autoplot(fit2)

fit3 <- auto.arima((ts(cc[,3], start = 2020, end = 2022, frequency = 12)), d =1,
                   D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit3))
checkresiduals(fit3)
autoplot(fit3)

forecast(fit3, h = 36) %>%
  autoplot() +
  ggtitle("Forecast of confirmed cases up to 2025") +
  labs(subtitle = "Date source: CSSEGISandData/COVID-19",
          caption = "@nabavinoushin") +
  ylab("Counts") + xlab("Time")

ggsave("covid19_bc_prediction.png")


forecast(fit3, h = 36) %>%
  print(summary())
  