# declare dependencies
if (!exists("setup_sourced")) source(here::here("scripts", "setup.R"))

#-----------------------------------------------------------

# load confirmed cases
confirmed <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_covid19_confirmed_global.csv")) 

drop_columns <- c("Country/Region", "Lat", "Long")
cc1 <- confirmed %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "count") %>%
  mutate(date = mdy(date)) %>%
  filter(`Province/State` == "British Columbia") %>%
  ggplot(aes(x = date, y = count)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_bw() + labs(title="Coronavirus confirmed cases in BC", 
                    subtitle = "Date source: CSSEGISandData/COVID-19",
                    caption = "@nabavinoushin",
                    x="Time", y = "Case counts") 

ggsave("confirmed_cases.png")


# load deaths 
death <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_covid19_deaths_global.csv"))

drop_columns <- c("Country/Region", "Lat", "Long")

dc1 <- death %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "count") %>%
  mutate(date = mdy(date)) %>%
  filter(`Province/State` == "British Columbia") %>%
  ggplot(aes(x = date, y = count, fill = factor(count))) + 
  geom_bar(stat = "identity", fill="darkred") +
  theme_bw() + labs(title="Coronavirus deaths in BC", 
                    subtitle = "Date source: CSSEGISandData/COVID-19",
                    caption = "@nabavinoushin",
                    x="Time", y = "Case counts") +   
  scale_y_continuous(limits=c(0, 1), breaks = 1)


ggsave("death_cases.png")

# load recovered cases 
recovered <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_covid19_recovered_global.csv"))

rc1 <- recovered %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "count") %>%
  mutate(date = mdy(date)) %>%
  filter(`Province/State` == "British Columbia") %>%
  ggplot(aes(x = date, y = count)) + 
  geom_bar(stat = "identity", fill="green") +
  theme_bw() + labs(title="Coronavirus recovered cases in BC", 
                    subtitle = "Date source: CSSEGISandData/COVID-19",
                    caption = "@nabavinoushin",
                    x="Time", y = "Case counts")

ggsave("recovered_cases.png")


# grid design
library(gridExtra)
grid.arrange(cc1, dc1, rc1, nrow = 3)
ggsave("panel_bc_cases.png")
#-----------------------------------------------------------

# Combine three sources of data into one for easier visualization

cc <- confirmed %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "confirmed_counts") %>%
  mutate(date = mdy(date)) %>%
  filter(`Province/State` == "British Columbia")

dc <- death %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "death_counts") %>%
  mutate(date = mdy(date)) %>%
  filter(`Province/State` == "British Columbia")

rc<- recovered %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "recovered_counts") %>%
  mutate(date = mdy(date)) %>%
  filter(`Province/State` == "British Columbia") 

combined <- cbind(cc, dc, rc, by = "date") 

combined_cleanup <- combined_cleanup %>%
  mutate(death_counts = case_when(death_counts == "1" ~ "Yes",
                                  TRUE ~ "No"))

#-----------------------------------------------------------

# visualize combined data
combined_cleanup %>%
  ggplot(aes(x = date, y = confirmed_counts, shape = factor(death_counts))) + 
  geom_point(aes(size =recovered_counts, color= factor(death_counts)), 
             shape = 21, alpha = 0.7) +
  scale_colour_manual(values = c("#00AFBB", "#FC4E07")) +
  labs(title="COVID-19 observations in BC until March 16, 2020", 
                    subtitle = "Date source: CSSEGISandData/COVID-19",
                    caption = "@nabavinoushin",
                    x="Time", y = "Confirmed cases",
       color = "Death", size = "Recovered")
ggsave("covid19_bc_recovered_death_cases.png")

combined_cleanup %>%
  ggplot(aes(x = date, y = confirmed_counts), color = factor(death_counts)) + 
  geom_point(aes(color= factor(death_counts))) +
  scale_color_manual(values = c("grey3", "magenta1")) +
  labs(title="COVID-19 observations in BC until March 16, 2020", 
       subtitle = "Date source: CSSEGISandData/COVID-19",
       caption = "@nabavinoushin",
       x="Time", y = "Total number of confirmed cases",
       color = "1 case died?") 
ggsave("covid19_bc_death_cases.png")


combined_cleanup %>%
  ggplot(aes(x = date, y = confirmed_counts), color = recovered_counts) + 
  geom_point(aes(color= factor(recovered_counts))) +
  labs(title="COVID-19 observations in BC until March 16, 2020", 
       subtitle = "Date source: CSSEGISandData/COVID-19",
       caption = "@nabavinoushin",
       x="Time", y = "Total number of confirmed cases",
       color = "Cases recovered") 

ggsave("covid19_bc_recovered_cases.png")

  