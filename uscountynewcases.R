##---------------------------------------------------------------
##                        Load packages                         -
##---------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(zoo)
library(urbnmapr)
library(rmapshaper)
library(scales)
library(transformr)


##---------------------------------------------------------------
##                          Import data                         -
##---------------------------------------------------------------

# Time series for US cases by county
confirmed_us <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
deaths_us <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

# US map data
county_map <- get_urbn_map(map = "counties", sf = TRUE) %>%
  mutate(county_fips = as.numeric(county_fips)) %>%
  ms_simplify(keep = 0.01)


##---------------------------------------------------------------
##                        Data cleaning                         -
##---------------------------------------------------------------

# Drop unneeded columns and convert to long format
confirmed_us <- confirmed_us %>%
  select(-UID, -iso2, -iso3, -code3, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
  rename(county = Admin2) %>%
  gather(date, confirmed, -county, -Province_State, -FIPS)

deaths_us <- deaths_us %>%
  select(-UID, -iso2, -iso3, -code3, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
  rename(county = Admin2) %>%
  gather(date, deaths, -county, -Province_State, -FIPS, -Population)

# Set date format
confirmed_us$date <- as.Date(confirmed_us$date, format = "%m/%d/%y")
deaths_us$date <- as.Date(deaths_us$date, format = "%m/%d/%y")

# Merge US datasets
county_cases <- deaths_us %>%
  select(-county, -Province_State) %>%
  full_join(confirmed_us, by = c("FIPS", "date"))

# Calculate new cases per day and weekly sum
county_cases <- county_cases %>%
  ungroup() %>%
  group_by(county, Province_State) %>%
  arrange(date) %>%
  mutate_at(vars(confirmed, deaths), 
            funs(new = c(NA, diff(.)))) %>%
  mutate_at(vars(confirmed_new, deaths_new), 
            funs(replace(., which(. < 0), 0))) %>%
  mutate_at(vars(confirmed_new, deaths_new),
            funs(replace(., is.na(.), 0))) %>%
  mutate_at(vars(confirmed_new, deaths_new), 
            funs(week = rollapply(., width = 7, FUN = sum, na.rm = TRUE, align = "right", partial = TRUE)))


##----------------------------------------------------------------
##                              Plot                             -
##----------------------------------------------------------------

# New confirmed cases in past 7 days
p <- county_map %>%
  left_join(county_cases, by = c("county_fips" = "FIPS")) %>%
  filter(date >= as.Date("2020-02-29")) %>%
  ggplot() +
  geom_sf(aes(fill = confirmed_new_week), color = "grey80", size = 0.2) +
  geom_sf(data = . %>% group_by(state_name) %>% summarise(), colour = "black", size = 0.2, fill = "transparent") +
  scale_fill_distiller(
    name = "Number of Cases", trans = "log10", direction = 1, palette = "YlOrRd", na.value = "white", 
    limits = c(1, NA), breaks = c(1E0, 1E1, 1E2, 1E3, 1E4, 1E5), labels = label_number_si(),
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      barwidth = unit(150, "pt"),
      frame.colour = "black", 
      ticks.colour = "black",
      draw.ulim = FALSE, draw.llim = FALSE)
  ) + 
  transition_time(time = date) +
  labs(
    title = "Number of New Confirmed COVID-19 Cases in Past 7 Days by County", 
    subtitle = "Date: {frame_time}",
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
  fps = 5,
  animation_time = 30,
  end_pause_seconds = 4
)

# Warning: rendering can take a very long time!
animate(
  p,
  fps = anim_params$fps, 
  duration = anim_params$animation_time + anim_params$end_pause_seconds,
  end_pause = anim_params$fps*anim_params$end_pause_seconds,
  type = "cairo",
  renderer = gifski_renderer("uscountycases.gif"),
  width = 1200, height = 800, res = 100
)
