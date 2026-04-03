library(readr)
library(readxl)
library(dplyr)
library(tidyr)

# When a package is loaded, R shows a "masked" warning like "dplyr::filter()
# masks stats::filter()" if a function in the new package shares a name with
# one already loaded. This means dplyr's version of that function takes
# precedence; to use the base-R version explicitly, prefix it: stats::filter().

# a masked message is a heads-up from R telling you that two different packages have
# a function with the same name

# R does not know which one you want to use so it uses the function you loaded most recently
# by default and masks (hides) the older one

# for instance base R has a function called fiter() and dplyr also has a function called 
# filter()

find("filter")

obesity <- read.csv("C:\\Users\\stalo\\Documents\\obesity.xlsx - Obesity_data.csv")
obesity

glimpse(obesity)

# glimpse(obesity) shows the dataset has 2111 rows, 16 columns, each column name for instance: No.,
# Gender, Age, Height and the data type of observations in each column like int, chr, dbl

dim(obesity)

# dim(obesity) returns c(rows,columns) ; in our case the output is [1] 2111   16 indicating
# our dataset has 2,111 observations and 17 variables before any cleaning.

# VARIABLE EXPLANATION FROM THE MASTER SHEET

# Height contains the height of individuals in our dataset
# Weight contains the weight of individuals in our dataset

# Missing values summary

# count of missing values per column
missing_count <- colSums(is.na(obesity))
missing_count

# percentage of missing values per column
missing_pct <- round(colSums(is.na(obesity)) / nrow(obesity) * 100, 2)
missing_pct

# Combine into a readable summary table
missing_summary <- data.frame(
  variable        = names(missing_count),
  n_missing       = as.integer(missing_count),
  pct_missing     = missing_pct,
  row.names       = NULL
) %>%
  arrange(desc(n_missing))

print(missing_summary)

# there are no missing values in any column of our dataset

# creating obesity_ready

obesity_ready <- obesity %>%
  select(Gender, Age, Height,
         Weight, family_history,
         FAVC, FCVC, NCP, CH2O,
         FAF, TUE, MTRANS, NObeyesdad) %>%
  
  filter(Age >= 18) %>%
  
  filter(!is.na(Height), !is.na(Weight)) %>%
  
  mutate(BMI = Weight / Height^2) %>%
  
  mutate(
    BMI_class = case_when(
      BMI < 18.5              ~ "Underweight",
      BMI >= 18.5 & BMI < 25  ~ "Normal",
      BMI >= 25.0 & BMI < 30  ~ "Overweight",
      BMI >= 30               ~ "Obese",
      TRUE                    ~ NA_character_   # incase of any unexpected NAs
    )
  )

# Quick check of the cleaned dataset
glimpse(obesity_ready)
dim(obesity_ready)

# summary table
summary_table <- obesity_ready %>%
  group_by(Gender, BMI_class) %>%
  summarise(
    n = n(),
    mean_BMI = mean(BMI),
    .groups  = "drop"             
  ) %>%
  arrange(desc(n))        

# Preview
print(summary_table)

# Export
write.csv(
  summary_table,
  file      = "summary_table_Phyllis Kiburi.csv",
  row.names = FALSE
)

message("Export complete: summary_table_Phyllis Kiburi.csv saved to working directory.")
  

