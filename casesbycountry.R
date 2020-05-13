##---------------------------------------------------------------
##                        Load packages                         -
##---------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(zoo)
library(countrycode)
library(wbstats)
library(scales)


##---------------------------------------------------------------
##                          Import data                         -
##---------------------------------------------------------------


# Time series for global cases
confirmed_global = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
deaths_global = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
recovered_global = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')


# Country population data
population <- wb(country = "countries_only", indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018) %>%
  select(iso3c, value) %>%
  rename(pop = value)


##---------------------------------------------------------------
##                        Data cleaning                         -
##---------------------------------------------------------------

# Drop columns and convert to long format
confirmed_global <- confirmed_global %>%
  select(-Lat, -Long) %>%
  gather(date, confirmed, -`Province/State`, -`Country/Region`)

deaths_global <- deaths_global %>%
  select(-Lat, -Long) %>%
  gather(date, deaths, -`Province/State`, -`Country/Region`)

recovered_global <- recovered_global %>%
  select(-Lat, -Long) %>%
  gather(date, recovered, -`Province/State`, -`Country/Region`)

# Set date format
confirmed_global$date <- as.Date(confirmed_global$date, format = "%m/%d/%y")
deaths_global$date <- as.Date(deaths_global$date, format = "%m/%d/%y")
recovered_global$date <- as.Date(recovered_global$date, format = "%m/%d/%y")

# Merge global datasets
all_cases <- full_join(confirmed_global, deaths_global) %>%
  full_join(recovered_global) %>%
  filter(!is.na(confirmed)) %>% # Drop rows with missing data
  mutate(`Country/Region` = case_when(`Country/Region` == "United Kingdom" ~ "UK",
                                      `Country/Region` == "Korea, South" ~ "South Korea",
                                      TRUE ~ `Country/Region`))

as.Date(max(all_cases$date))

# Calculate active cases for global dataset
all_cases <- all_cases %>%
  mutate(active = confirmed - deaths - recovered)

# Aggregate countries and merge population data
country_cases <- all_cases %>%
  group_by(`Country/Region`, date) %>%
  summarise_at(vars(confirmed, deaths, recovered, active), sum) %>%
  rename(country = `Country/Region`) %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c", 
                             custom_match = c("Kosovo" = "XKX", "Eswatini" = "SWZ"), 
                             nomatch = NULL)) %>%
  left_join(population, by = "iso3c")

# Calculate new cases per day and 5 day average, and cases per million population
country_cases <- country_cases %>%
  group_by(country) %>%
  mutate_at(vars(confirmed, deaths, recovered, active), 
            funs(new = c(NA, diff(.)))) %>%
  mutate_at(vars(confirmed_new, deaths_new, recovered_new), 
            funs(replace(., which(. < 0), 0))) %>%
  mutate_at(vars(confirmed_new, deaths_new, recovered_new),
            funs(replace(., is.na(.), 0))) %>%
  mutate_at(vars(confirmed_new, deaths_new, recovered_new), 
            funs(avg = rollapply(., width = 5, FUN = mean, na.rm = TRUE, align = "right", partial = TRUE))) %>%
  mutate(active_pc = active / (pop/1E6))


##----------------------------------------------------------------
##                              Plot                             -
##----------------------------------------------------------------

# Select countries with most confirmed at latest date
countries_to_label <- country_cases %>%
  ungroup() %>%
  filter(date == max(date)) %>%
  top_n(n = 20, wt = confirmed) %>%
  select(country) %>%
  as.matrix() %>% c()

# Manually add some other countries
countries_to_label <- c("China", 
                        "Italy",
                        "Spain",
                        "Japan", 
                        "Brazil", 
                        "Netherlands", 
                        "France", 
                        "Sweden",
                        "South Africa", 
                        "Philippines", 
                        "Poland",
                        "Turkey",
                        "Ecuador",
                        countries_to_label) %>% unique()

# Make the animated plot
p <- country_cases %>%
  filter(confirmed >= 10) %>%
  mutate(label = if_else(country %in% countries_to_label, "yes", "no")) %>%
  ggplot(aes(x = active, y = confirmed_new_avg, group = country, label = country)) +
  geom_line(color = "grey") +
  geom_point(aes(fill = active_pc), color = "black", size = 2.5, shape = 21) +
  geom_label(data = . %>% filter(country %in% countries_to_label), 
             aes(label = country), hjust = 1.1, vjust = -0.1) +
  scale_fill_distiller(name = "Active cases per 1M population (2018)", trans = "log10", palette = "YlOrRd", direction = 1,
                       limits = c(0.1, 1E4), breaks = c(0.1, 1, 1E1, 1E2, 1E3, 1E4), labels = label_number_si(),
                       guide = guide_colorbar(direction = "horizontal",
                                              title.position = "top",
                                              label.position = "bottom",
                                              # title.hjust = 1,
                                              barwidth = unit(200, "pt"),
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              draw.ulim = FALSE, draw.llim = FALSE)) +
  scale_x_continuous(name = "Active cases", trans = "log10", limits = c(10, NA), labels = comma, breaks = c(1E1, 1E2, 1E3, 1E4, 1E5, 1E6)) +
  scale_y_continuous(name = "New cases (5 day average)", trans = "log10", limits = c(10, NA), labels = comma, breaks = c(1E1, 1E2, 1E3, 1E4, 1E5)) +
  transition_reveal(date) +
  theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(5.5, 40, 5.5, 5.5),
        # Legend
        legend.position = c(.10, .85),
        legend.justification = c("left", "top"),
        legend.margin = margin(6, 11, 6, 11)) +
  annotation_logticks() +
  labs(title = "COVID-19: New cases vs. Active cases", 
       subtitle = "Date: {frame_along}", 
       caption = "Data: Johns Hopkins CSSE and World Bank")

# Create GIF
animate(p, 
        fps = 15, duration = 30,
        renderer = gifski_renderer("casesbycountry.gif"), 
        type = "cairo",
        end_pause = 60,
        width = 1200, height = 800, res = 100)
