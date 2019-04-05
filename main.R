library(tidyverse)
library(readxl)
library(lubridate)
library(forecast)
library(timetk)

dat <-
  read_excel(path="2019 BA Case Competition Data.xlsx", sheet=3, na="NULL")

summary(dat)

# What do the number of appointments look like over time -----
appointments <-
  dat %>%
  group_by(
    appointment_date = as_date(AppointmentDTS)
  ) %>%
  summarize(
    num_appt = n(),
    NoShows = sum(NoShowFLG),
    NoShowRate = sum(NoShowFLG)/n()
  ) %>%
  filter(num_appt > 10)

ggplot(appointments, aes(appointment_date, NoShows)) +
  geom_col(fill="steelblue")

ggplot(appointments, aes(appointment_date, num_appt)) +
  geom_col(color="steelblue") +
  geom_smooth(se=F, color="tomato") +
  geom_col(aes(appointment_date, NoShows), color="pink") +
  geom_smooth(aes(appointment_date, NoShows),se=F, color="tomato") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Date",
    y = "Appointments"
  )

ggsave("DateEffect_NoShows.png")

# appointments_ts <- 
#   tk_ts(appointments, select=appointments) 
# 
# autoplot(appointments_ts)
# 
# checkresiduals(appointments_ts)

# Appointment distribution by customer ---------------------

dat %>%
  group_by(PatientID) %>%
  summarize(appointments = n()) %>%
  arrange(desc(appointments)) %>%
  mutate_at(.vars = vars(PatientID), .funs = funs(reorder(.,appointments))) %>% 
  ggplot(aes(appointments)) +
  geom_bar()

# Appointment lag vs cancellation rate -------------

patients <-
  read_csv("patient_data.csv")

ggplot(patients, aes(AvgApptLag, HistNoShowRate)) +
  geom_point()
  
# Appointment lag only affects younger generation 

patients %>% 
  filter(Age < 20) %>%
  ggplot(aes(AvgApptLag, HistNoShowRate)) +
  geom_point()

# Appointment weekday effect
# Can no-show and cancellations coexist
dat %>%
  mutate(test = NoShowFLG * CancelledLateFLG) %>%
  filter(test > 0) # No


dat %>%
  group_by(AppointmentWeekdayNBR) %>%
  summarize(
    NoShowCount = sum(NoShowFLG),
    CancelledLateCount = sum(CancelledLateFLG),
    Appointments = n(),
    NoShowCancelRate = (sum(NoShowFLG) + sum(CancelledLateFLG))/n()
  ) %>% 
  ggplot(aes(AppointmentWeekdayNBR, NoShowCancelRate)) +
    geom_col(fill="steelblue", alpha=0.95) +
    scale_x_continuous(breaks = 1:7, labels = as.character(1:7)) +
    labs(x = "Weekday", y = "No Show/Cancel Rate") +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())

# time of day effect
rate_timeOfDay <-
  dat %>%
  group_by(AppointmentHourNBR) %>%
  summarize(
    NoShowCancelRate = (sum(NoShowFLG) + sum(CancelledLateFLG))/n(),
    Appointments = n()
  )

ggplot(rate_timeOfDay, aes(AppointmentHourNBR, NoShowCancelRate, alpha=Appointments)) +
  geom_col(fill="steelblue") +
  geom_hline(yintercept = median(rate_timeOfDay$NoShowCancelRate), color = "red") +
  annotate(geom="text", x=7, y=.265, label="Median", color="red") +
  # scale_x_continuous(breaks = 1:7, labels = as.character(1:7)) +
  labs(x = "Hour", y = "No Show/Cancel Rate") +
  scale_x_continuous(
    breaks = 7:19,
    labels = 7:19
  ) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())

ggsave("HourEffect.png")

# age effect
dat %>%
  filter(AgeNBR < 100) %>%
  mutate(
    AgeBracket = factor(case_when(
      AgeNBR >= 0 & AgeNBR <= 10 ~ "0 - 10",
      AgeNBR >= 11 & AgeNBR <= 20 ~ "11 - 20",
      AgeNBR >= 21 & AgeNBR <= 30 ~ "21 - 30",
      AgeNBR >= 31 & AgeNBR <= 40 ~ "31 - 40",
      AgeNBR >= 41 & AgeNBR <= 50 ~ "41 - 50",
      AgeNBR >= 51 & AgeNBR <= 60 ~ "51 - 60",
      AgeNBR >= 61 & AgeNBR <= 70 ~ "61 - 70",
      AgeNBR >= 71 & AgeNBR <= 80 ~ "71 - 80",
      AgeNBR >= 81 & AgeNBR <= 90 ~ "81 - 90",
      AgeNBR >= 91 & AgeNBR <= 99 ~ "91 - 99",
    ), ordered=F)
  ) %>%
  group_by(AgeNBR, AgeBracket) %>%
  summarize(
    NoShowCancelRate = (sum(NoShowFLG) + sum(CancelledLateFLG))/n(),
    Appointments = n()
  ) %>% 
  ggplot(aes(AgeNBR, NoShowCancelRate, fill=AgeBracket, alpha=Appointments)) +
  geom_col() +
  labs(x = "Age", y = "No Show/Cancel Rate") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())

dat %>%
  filter(AgeNBR < 100) %>%
  mutate(
    AgeBracket = factor(case_when(
      AgeNBR >= 18 & AgeNBR <= 23 ~ "18 - 23",
      T ~ "Other"
    ), ordered=F)
  ) %>%
  group_by(AgeNBR, AgeBracket) %>%
  summarize(
    NoShowCancelRate = (sum(NoShowFLG) + sum(CancelledLateFLG))/n(),
    Appointments = n()
  ) %>% 
  ggplot(aes(AgeNBR, NoShowCancelRate, fill=AgeBracket, alpha=Appointments)) +
  geom_col() +
  labs(x = "Age", y = "No Show/Cancel Rate") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())

ggsave("AgeEffect_NoShowCancel.png")

dat %>%
  filter(AgeNBR < 100) %>%
  mutate(
    AgeBracket = factor(case_when(
      AgeNBR >= 18 & AgeNBR <= 29 ~ "18 - 29",
      T ~ "Other"
    ), ordered=F)
  ) %>%
  group_by(AgeNBR, AgeBracket) %>%
  summarize(
    NoShowRate = sum(NoShowFLG)/n(),
    Appointments = n()
  ) %>% 
  ggplot(aes(AgeNBR, NoShowRate, fill=AgeBracket, alpha=Appointments)) +
  geom_col() +
  labs(x = "Age", y = "No Show Rate") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())

ggsave("AgeEffect_NoShow.png")

# Past noshow predict noshows -------------------------------------

# get current noshow rate
dat %>%
  group_by(PatientID) %>%
  summarize(
    HistNoShowRate = median(NoshowRate24NBR),
    NoShowRate = sum(NoShowFLG)/n(),
    LogNoShowRate = log(NoShowRate),
    LogHistNoShowRate = log(HistNoShowRate),
    Appointments = n(),
    AlwaysShowsUp = case_when(
      HistNoShowRate == 0 & NoShowRate == 0 ~ T,
      T ~ F
    )
  ) %>%
  filter(Appointments > 1, !AlwaysShowsUp) %>%
  ggplot(aes(LogHistNoShowRate, LogNoShowRate)) +
  geom_point(alpha=0.5, color="steelblue") +
  geom_smooth(se=F, method="lm", color="tomato", size=1.2, alpha=0.8) +
  theme_minimal()

ggsave("HistoricEffect.png")

# Apartment living ---------------------------------------

dat %>%
  mutate_at(
    .vars = vars(LivesInApartmentFLG), 
    .funs = funs(factor(.,ordered=T))
  ) %>%
  group_by(PatientID) %>%
  summarize(
    Apartment = LivesInApartmentFLG[1],
    HistNoShowRate = NoshowRate24NBR[1],
    NoShowRate = sum(NoShowFLG)/n(),
  ) %>%
  gather(HistNoShowRate, NoShowRate, key = "metric", value = "value") %>%
  ggplot(aes(Apartment, value, color = metric)) +
  geom_point(alpha=0.2, position="jitter") +
  geom_boxplot(alpha=0, color="black") +
  facet_wrap(~ metric)

ggsave("ApartmentLiving_all.png")

dat %>%
  mutate_at(
    .vars = vars(LivesInApartmentFLG), 
    .funs = funs(factor(.,ordered=T))
  ) %>%
  group_by(PatientID) %>%
  summarize(
    Apartment = LivesInApartmentFLG[1],
    HistNoShowRate = NoshowRate24NBR[1],
    NoShowRate = sum(NoShowFLG)/n(),
  ) %>%
  filter(!(HistNoShowRate==0&NoShowRate==0)) %>%
  gather(HistNoShowRate, NoShowRate, key = "metric", value = "value") %>%
  ggplot(aes(Apartment, value, color = metric)) +
  geom_point(alpha=0.2, position="jitter") +
  geom_boxplot(alpha=0, color="black") +
  facet_wrap(~ metric)

ggsave("ApartmentLiving_SomeNoShows.png") # Filtered out people who never had a no-show

# Single effect --------------

dat %>%
  mutate_at(
    .vars = vars(LivesInApartmentFLG, SingleFLG), 
    .funs = funs(factor(.,ordered=T))
  ) %>%
  group_by(PatientID) %>%
  summarize(
    Apartment = LivesInApartmentFLG[1],
    Single = SingleFLG[1],
    HistNoShowRate = NoshowRate24NBR[1],
    NoShowRate = sum(NoShowFLG)/n(),
  ) %>%
  filter(!(HistNoShowRate==0&NoShowRate==0)) %>%
  gather(HistNoShowRate, NoShowRate, key = "metric", value = "value") %>%
  ggplot(aes(Single, value, color = metric)) +
  geom_point(alpha=0.2, position="jitter") +
  geom_boxplot(alpha=0, color="black") +
  facet_wrap(~ metric)

ggsave("SingleEffect.png")

dat %>%
  mutate_at(
    .vars = vars(LivesInApartmentFLG, SingleFLG), 
    .funs = funs(factor(.,ordered=T))
  ) %>%
  group_by(PatientID) %>%
  summarize(
    Apartment = LivesInApartmentFLG[1],
    Single = SingleFLG[1],
    HistNoShowRate = NoshowRate24NBR[1],
    NoShowRate = sum(NoShowFLG)/n(),
  ) %>%
  filter(!(HistNoShowRate==0&NoShowRate==0)) %>%
  mutate(
    ApartmentAndSingle = case_when(
      Apartment=="1" & Single=="1" ~ "1",
      T ~ "0"
    )
  ) %>%
  gather(HistNoShowRate, NoShowRate, key = "metric", value = "value") %>%
  ggplot(aes(ApartmentAndSingle, value, color = metric)) +
  geom_point(alpha=0.2, position="jitter") +
  geom_boxplot(alpha=0, color="black") +
  facet_wrap(~ metric)

ggsave("ApartmentAndSingle_effect.png")
