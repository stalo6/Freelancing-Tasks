# LOADING THE REQUIRED LIBRARIES
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)

# IMPORTING THE DATASET
water_quality <- read_csv("assignment1_water_quality_May_Sep2026.csv.xls")

# INSPECTING THE DATASET

View(water_quality)

glimpse(water_quality)
# the dataset has 73 obseravations and 10 variables

# the variables which are of the character data type are Sample_ID, Site, Habitat,
#  Sample_Date, Season

# the variables which are of the double data type are pH, DO_mgL, Turbidity_NTU,
# Conductivity_uScm, Temperature_C

# EXAMINING THE Habitat VARIABLE CATEGORIES
unique(water_quality$Habitat)

# The Habitat variable has inconsistent capitalization and spacing for 
# the same category. For instance: Grassland and grassland, RIVERINE and Riverine

# IDENTIFYING INVALID WATER QUALITY MEASUREMENTS
summary(water_quality)

invalid_records <- water_quality %>% 
  filter(pH < 0 | pH > 14 | DO_mgL < 0)
invalid_records

#  There is a pH value of -1 and a DO_mgL value of -2.
#  Water quality metrics have biological and physical limits 
#  pH must be between 0 and 14, and Dissolved Oxygen (DO_mgL) cannot be negative



# STANDARDISE Habitat NAMES
# Convert Habitat names to lowercase and remove trailing and leading spaces
water_quality <- water_quality %>%
mutate(Habitat = str_trim(str_to_lower(Habitat))) %>%
  
# CORRECTING VARIABLES IMPORTED USING AN INAPPROPRIATE DATA TYPE
  mutate(
    # Clean text : trim whitespace, standardise case
    across(c(Site, Habitat, Season), ~ str_squish(str_to_title(.x))),
    
    # Dates
    Sample_Date = dmy(Sample_Date),
    
    # Categorical variables
    Site    = factor(Site),
    Habitat = factor(Habitat),
    Season  = factor(Season, levels = c("Dry", "Wet"))
  ) %>%
  
# removing exact duplicate observations
  distinct()

# looking at the cleaned dataset
View(water_quality)
glimpse(water_quality)

# IDENTIFYING MISSING VALUES 
colSums(is.na(water_quality))

# IDENTIFYING DUPLICATED VALUES
sum(duplicated(water_quality))

# DEAL APPROPRIATELY WITH INAPPROPRIATE / MISSING VALUES
# Coerce known impossible values to NA so they are not factored into future means
water_quality <- water_quality %>%
  mutate(
    pH = ifelse(pH < 0 | pH > 14, NA, pH),
    DO_mgL = ifelse(DO_mgL < 0, NA, DO_mgL)
    ) %>%
# drop rows where essential metrics are completely missing
  drop_na(pH, DO_mgL)

# CREATE Water_Quality_Class VARIABLE 
water_quality <- water_quality %>%
mutate(
  Water_Quality_Class = case_when(
    pH >= 6.5 & pH <= 8.5 & DO_mgL >= 5 ~ "Good",
    TRUE ~ "Needs attention"
  )
)

# looking at the final dataset
View(water_quality)
glimpse(water_quality)
