# installing the dplyr package
install.packages(dplyr)

# loading the dplyr package
library(dplyr)

# loading other required libraries
library(tidyr)
library(ggplot2)
library(lubridate)

# finding the required dataset
data(package = "dplyr")

# loading the storms dataset
data(storms)
# storms contains hurricane and tropical storm tracks, including time 
# information and storm  characteristics such as category, wind, pressure and
# status

# DATA MANAGEMENT
# Viewing the dataset
View(storms)

# looking at the dimensions of the dataset
dim(storms)
# the dataset has 19537 rows and 13 columns

# looking at the column types and structure of the dataset
str(storms)

# looking at the top rows of the dataset
head(storms)

# summary statistics for each column of the dataset
summary(storms)

glimpse(storms)

# DATA CLEANING
# looking for missing values in the dataset
colSums(is.na(storms))

# why the category column has NAs
# The category column uses a scale which only applies to hurricanes 
# It doesn't exist as a concept for any other storm type 
# So every observation where status is not hurricane will have NA for category

# why the tropicalstorm_force_diameter and hurricane_force_diameter columns
# have NAs
# the measurements of these diameters were only added from 2004 onwards
# data on these diameters before 2004 does not exist because it was not recorded

# handling NA values in the category column
storms %>% 
  filter(is.na(category)) %>%
  count(status)

# creating category label column to distinguish between hurricane and
# non-hurricane categories
# Treat NA category as non-hurricane
storms <- storms %>%
  mutate(category_label = if_else(is.na(category), "Non-hurricane",
                                  paste("Category", category)))

storms_clean <- storms %>%
  select(-tropicalstorm_force_diameter, -hurricane_force_diameter)

storms_clean
colSums(is.na(storms_clean))

# Creating a proper datetime column
storms_clean <- storms_clean %>%
  mutate(datetime = make_datetime(year, month, day, hour))


# Classify storms by era for trend analysis
storms_clean <- storms_clean %>%
  mutate(era = case_when(
    year < 1990 ~ "1975–1989",
    year < 2005 ~ "1990–2004",
    TRUE        ~ "2005–present"
  ))

storms_clean

View(storms_clean)

# UNCOVERING TRENDS
# deriving storm level summaries
storm_summary <- storms_clean %>%
  group_by(name, year) %>%
  summarise(
    peak_wind      = max(wind, na.rm = TRUE),
    min_pressure   = min(pressure, na.rm = TRUE),
    duration_hrs   = n() * 6,        # observations are 6hrs apart
    .groups = "drop"
  )

storm_summary

# looking at how many storms there are per year
storms_clean %>%
  distinct(name, year) %>%
  count(year) %>%
  arrange(year)


# barchart showing storm frequency per year
storms_clean %>%
  distinct(name, year) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_smooth(method = "lm", colour = "firebrick", se = FALSE) +
  labs(title = "Storm Frequency by Year ",
       x = "Year", y = "Number of Storms") +
  theme_minimal()
# The trend line slopes upward from left to right indicating that 
# storm frequency has increased over the period 1975 to the present


# looking at how many storms reached hurricane status per year
storms_clean %>%
  filter(status == "hurricane") %>%
  distinct(name, year) %>%
  count(year)

# looking at how peak wind speed changed over decades
storm_summary %>%
  group_by(era = case_when(
    year < 1990 ~ "1975–1989",
    year < 2005 ~ "1990–2004",
    TRUE        ~ "2005–present"
  )) %>%
  summarise(
    mean_peak_wind = mean(peak_wind),
    mean_min_pressure = mean(min_pressure, na.rm = TRUE)
  )

# Which months have most storms
storms_clean %>%
  distinct(name, year, month) %>%
  count(month, name = "storm_count") %>%
  arrange(desc(storm_count))
# month 9 (september) has the most storms while month 4 (April) has the 
# least number of storms

# STATISTICAL TESTS
# looking at whether wind speed is significantly different across storm 
#categories
kruskal.test(wind ~ category, data = storms_clean %>% filter(!is.na(category)))
# chi-squared = 4080.6, df = 4
# p-value < 2.2e-16 which is less than 0.001
# this shows that wind speed is significantly different across storm categories

# Correlation between wind speed and pressure
cor(storms_clean$wind, storms_clean$pressure, use = "complete.obs")
# correlation between wind speed and pressure is negative (-0.92777)
# this indicates that there is a strong negative relationship between wind speed 
# and wind pressure

# scatter plot of wind versus pressure
ggplot(storms_clean, aes(x = pressure, y = wind, colour = status)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Wind Speed vs. Atmospheric Pressure",
       subtitle = "Stronger storms have lower pressure",
       x = "Pressure (mbar)", y = "Wind Speed (knots)",
       colour = "Status") +
  theme_minimal()

# from the scatter plot, there is a strong negative relationship between 
# atmospheric pressure and wind speed across all storm types
# this indicates that as pressure decreases, wind speed increases

# Hurricanes cluster at the lowest pressures (below 900 mbar) and 
# highest wind speeds (above 150 knots)

# tropical depressions and tropical waves cluster at high pressures and 
# moderate wind speeds 

# Tropical and extratropical storms occupy intermediate positions, with 
# extratropical storms showing wider pressure variability for a given wind speed
# compared to tropical systems

# Trend lines indicate that the rate at which wind speed increases per unit drop 
# in pressure is consistent across storm types

# heatmap showing storm frequency by month and year
storms_clean %>%
  distinct(name, year, month) %>%
  count(year, month) %>%
  ggplot(aes(x = month, y = year, fill = n)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb) +
  labs(title = "Storm frequency by month and year",
       x = "Month", y = "Year", fill = "Storms") +
  theme_minimal()

# From the heatmap, storm activity is heavily concentrated in the months of 
# August to October
# There is near-zero storm occurrence from January through May across all years 

# Storm frequency has increased over time 
# From the mid-1990s onward, there is darker shading and more active months 
# The 1980s have low storm frequency

# The post-2000 period has more storm activity, with 2005 and 2020 being the
# years with the highest storm frequency

# Map of storm paths coloured by wind speed
ggplot(storms_clean, aes(x = long, y = lat, group = paste(name, year),
                   colour = wind)) +
  geom_path(alpha = 0.4, linewidth = 0.3) +
  scale_colour_gradient(low = "yellow", high = "darkred") +
  coord_quickmap(xlim = c(-110, -10), ylim = c(5, 55)) +
  labs(title = "Map of storm paths coloured by wind speed",
       x = "Longitude", y = "Latitude",
       colour = "Wind (knots)") +
  theme_minimal()

# from the map, the highest wind speeds cluster between 15°N and 30°N latitude
# Wind speeds decrease as tracks recurve into higher latitudes

# Most storms originate from the 10°N and 20°N latitude (below 60 knots)
# Most storm observations fall in the lower wind speed range

# boxplot showing storm duration by peak category
# looking at whether stronger storms last longer
storms_clean %>%
  group_by(name, year) %>%
  summarise(duration_hrs  = n() * 6,
            peak_category = max(as.numeric(category), na.rm = TRUE),
            .groups = "drop") %>%
  filter(is.finite(peak_category)) %>%
  ggplot(aes(x = factor(peak_category), y = duration_hrs,
             fill = factor(peak_category))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("1" = "#FED976", "2" = "#FEB24C",
                               "3" = "#FD8D3C", "4" = "#F03B20",
                               "5" = "#BD0026")) +
  labs(title = "Storm Duration by Peak Hurricane Category",
       x = "Peak Category", y = "Duration (hours)") +
  theme_minimal() +
  theme(legend.position = "none")

# from the boxplot, stronger storms last longer on average
# category 4 storms last the longest
# category 1 storms have the most outliers


