library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

# IMPORTING THE DATASET
Water_Quality <- read_csv("assignment1_water_quality_Jan2026.csv.xls")
Water_Quality


# INSPECTING THE DATASET
View(Water_Quality)

glimpse(Water_Quality)

# The dataset has 73 rows and 10 columns
# The principal variable types are of five numeric measurements of 
# water quality: pH, DO_mgL, Turbidity_NTU, Conductivity_uScm, Temperature_C,
# three categorical variables denoting location and season: Site, Habitat, Season
# and one date variable, and one unique sample identifier: Sample_ID

# IDENTIFYING DATA QUALITY ISSUES
summary(Water_Quality)

# 1. Missing values in the dataset

  # checking for missing values in the dataset
    colSums(is.na(Water_Quality))
    
# There is 1 missing value in the DO_mgL column and 2 missing values in the 
# Turbidity_NTU column

# This requires attention because missing data reduces statistical power 
# and causes standard R functions like mean() to return NA errors by default
# If not handled, it will cause regression models to drop entire rows, hence
# biased estimates

# 2. Inconsistent Categorical Values
    
  # looking at the unique values in the Habitat column of the dataset
    unique(Water_Quality$Habitat)
  # looking at the unique values in the Season column of the dataset
    unique(Water_Quality$Season)
    
# The Habitat column contains inconsistent capitalization and spacing for the same categories
# For instance, it contains Woodland and woodland; Grassland and GRASSLAND, 
# Riverine and riverine
    
# This requires attention because R treats these variations as distinct categories. 
# This will make summary statistics, group comparisons like ANOVA,
# and data visualizations inaccurate because the same habitat type is 
# split into multiple artificial groups
    
# 3. Outliers
    
    # plotting histograms of numerical variables in the dataset
    Water_Quality %>%
      # Select only the numeric columns
      select(where(is.numeric)) %>%
      
      # Pivot the data into a long format
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
      
      ggplot(aes(x = Value)) +
      geom_histogram(bins = 15, fill = "steelblue", color = "black") +
      
      facet_wrap(~ Variable, scales = "free") +
      
      theme_minimal() +
      labs(title = "Distributions of Numeric Water Quality Variables",
           x = "Measurement Value", 
           y = "Frequency")

# There are outliers in the numeric columns. 
# In the Conductivity_uScm  column there is a value -100 but conductivity 
# cannot be negative
    
# In the Temperature_C column there is a value 65°C which is very hot 
# for natural water
    
# This requires attention because extreme outliers parametric statistics like 
# the mean and variance. Including these values will skew the data distribution 
# and lead to false conclusions or highly inaccurate predictive models.
    

# ADDITIONAL CHECK BEFORE ANALYSIS
    
# Assesing multicollinearity via a correlation matrix
    
numeric_data <- Water_Quality %>%
  select(where(is.numeric))
correlation_matrix <- cor(numeric_data, use = "complete.obs")
round(correlation_matrix, 2)

# Justification
# highly correlated independent variables provide redundant information
# which inflates variance and destabilizes coefficient estimates in 
# regression models.

# The multicollinearity check confirmed all numeric variables are independent 
# and can be kept for further analysis.


# CLEANING THE DATASET

 Water_Quality_Class <- Water_Quality %>%
   
   # Fix categories
   mutate(
     Site = as.factor(Site),
     Habitat = str_to_title(str_trim(Habitat)),
     Habitat = as.factor(Habitat),
     Season = as.factor(Season)
   ) %>%
   
   # Remove outliers
   filter(
     Conductivity_uScm >= 0,  
     Temperature_C <= 40      
   ) %>%
   
   # Remove duplicates
   distinct() %>%
   
   # Remove missing data
   drop_na()

 # inspecting the cleaned dataset
 glimpse(Water_Quality_Class)
 View(Water_Quality_Class)
 
 
 # CREATING THE pH_Status VARIABLE
 Water_Quality_Class <- Water_Quality_Class %>%
   mutate(
     pH_Status = case_when(
       pH < 6.5 ~ "Acidic",
       pH > 7.5 ~ "Alkaline",
       TRUE ~ "Neutral"
     ),
     # Convert it to a factor
     pH_Status = as.factor(pH_Status)
   )

# purpose of the pH_Status variable
# pH_Status converts continuous pH measurements into three categorical states: 
# Acidic, Neutral, and Alkaline. This is useful because aquatic ecosystems often
# respond categorically to pH thresholds rather than linearly. 
# Grouping the data this way allows for easier comparative analysis
 
 