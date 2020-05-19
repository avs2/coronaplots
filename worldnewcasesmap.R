##---------------------------------------------------------------
##                        Load packages                         -
##---------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(zoo)
library(countrycode)
library(wbstats)
library(scales)
library(rnaturalearth)
library(transformr)

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

# World map
worldmap <- ne_countries(scale = "medium", returnclass = "sf")


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

max(all_cases$date)

# Calculate active cases
all_cases <- all_cases %>%
  mutate(active = confirmed - deaths - recovered)

# Add population data
country_cases <- all_cases %>%
  group_by(`Country/Region`, date) %>%
  summarise_at(vars(confirmed, deaths, recovered, active), sum) %>%
  rename(country = `Country/Region`) %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c", 
                             custom_match = c("Kosovo" = "XKX", "Eswatini" = "SWZ"), 
                             nomatch = NULL)) %>%
  left_join(population, by = "iso3c")

# Calculate new cases per day and weekly sum, adn per million population
country_cases <- country_cases %>%
  ungroup() %>%
  group_by(country) %>%
  mutate_at(vars(confirmed, deaths, recovered, active), 
            funs(new = c(NA, diff(.)))) %>%
  mutate_at(vars(confirmed_new, deaths_new, recovered_new), 
            funs(replace(., which(. < 0), 0))) %>%
  mutate_at(vars(confirmed_new, deaths_new, recovered_new),
            funs(replace(., is.na(.), 0))) %>%
  mutate_at(vars(confirmed_new, deaths_new, recovered_new), 
            funs(week = rollapply(., width = 7, FUN = sum, na.rm = TRUE, align = "right", partial = TRUE))) %>%
  mutate_at(vars(confirmed, deaths, recovered, active, confirmed_new, deaths_new, recovered_new, confirmed_new_week, deaths_new_week, recovered_new_week),
            funs(pc = . / (pop/1E6)))


##----------------------------------------------------------------
##                              Plot                             -
##----------------------------------------------------------------

# New confirmed cases in past 7 days
p <- worldmap %>%
  left_join(country_cases, by = c("iso_a3" = "iso3c")) %>%
  filter(date >= as.Date("2020-02-01")) %>%
  ggplot() +
  geom_sf(aes(fill = confirmed_new_week_pc), color = "grey80", size = 0.2) +
  geom_sf(data = . %>% group_by(iso_a3) %>% summarise(), colour = "black", size = 0.2, fill = "transparent") +
  scale_fill_distiller(
    name = "New Cases per Million Population", trans = "log10", direction = 1, palette = "YlOrRd", na.value = "white", 
    limits = c(0.01, NA), 
    breaks = c(1E-2, 1E-1, 1E0, 1E1, 1E2, 1E3, 1E4, 1E5), 
    labels = c("0.01", "0.1", "1", "10", "100", "1K", "10K", "100K"),
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      barwidth = unit(200, "pt"),
      frame.colour = "black", 
      ticks.colour = "black",
      draw.ulim = FALSE, draw.llim = FALSE)
  ) + 
  # transition_time(time = date) +
  transition_manual(date) +
  labs(
    title = "New Confirmed COVID-19 Cases per Million Population in Past 7 Days", 
    # subtitle = "Date: {frame_time}",
    subtitle = "Date: {current_frame}",
    caption = "Data: Johns Hopkins CSSE"
  ) +
  theme(
    # No axes
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(),
    # Legend
    legend.position = c(.55, .15),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 11, 6, 11),
    # Titles
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1, margin = margin())
  )

anim_params <- list(
  fps = 3,
  end_pause_seconds = 4,
  nframes = length(unique(p$data$date))
)

# Warning: rendering can take a very long time!
animate(
  p,
  duration = anim_params$nframes/anim_params$fps + anim_params$end_pause_seconds,
  end_pause = anim_params$fps * anim_params$end_pause_seconds,
  type = "cairo",
  renderer = gifski_renderer("worldnewcountrycases.gif"),
  width = 1200, height = 800, res = 100
)