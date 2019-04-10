library(forecast)
library(timetk)
library(urca)
library(readxl)
library(tidyverse)
library(lubridate)

dat <-
  read_excel(path="2019 BA Case Competition Data.xlsx", sheet=3, na="NULL")

daily_appts <-
  dat %>%
  mutate(
    appt_date = as_date(AppointmentDTS)
  ) %>%
  group_by(appt_date) %>%
  summarize(n_appts = n())

ts_daily_appts <-
  tk_ts(
    daily_appts,
    select = n_appts,
    frequency = 7
  )

ur.kpss(ts_daily_appts) %>%
  summary() # Data fail test for non-stationarity => stationary

ggtsdisplay(ts_daily_appts)

daily_model1 <-
  Arima(
    ts_daily_appts,
    order = c(0,0,1),
    seasonal = c(1,0,0)
  )

ggtsdisplay(residuals(daily_model1))

daily_model2 <-
  Arima(
    ts_daily_appts,
    order = c(0,0,2),
    seasonal = c(1,0,0)
  )

ggtsdisplay(residuals(daily_model2))

daily_model3 <-
  Arima(
    ts_daily_appts,
    order = c(0,0,1),
    seasonal = c(1,1,0)
  )

ggtsdisplay(residuals(daily_model3))

daily_model4 <-
  Arima(
    ts_daily_appts,
    order = c(1,0,1),
    seasonal = c(1,1,0)
  )

ggtsdisplay(residuals(daily_model4))

summary(daily_model4)

# daily_snaive <-
#   snaive(ts_daily_appts,7)
# 
# summary(daily_snaive)
# 
# forecast(daily_snaive, h=7*4) %>%
#   autoplot()

checkresiduals(daily_model4)

horizon_length <- 91

daily_model4 %>%
  forecast(h=horizon_length) %>%
  autoplot()

fore_out <-
  daily_model4 %>%
  forecast(h=horizon_length)

forecasted <-
  tibble(
    appt_date = seq.Date(from = max(daily_appts$appt_date)+lubridate::days(1), by=1, length.out = horizon_length),
    n_appts = round(fore_out$mean),
    forecasted = T
  )

write_csv(forecasted, "forecasted_appts.csv")
