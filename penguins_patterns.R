library(readr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2)

# loading the dataset
penguins <- read_csv("C:\\Users\\stalo\\Downloads\\Penguins_data.csv",
                     show_col_types = FALSE)
penguins

# viewing the dataset
View(penguins)

# determining how many missing values there are in the dataset
sum(is.na(penguins))

# Showing number of missing values in each column
colSums(is.na(penguins))

# Creating a new dataset with the missing values removed
penguins_cleaned <- penguins %>%
  drop_na(Culmen_Length, Culmen_Depth, Flipper_Length, Body_Mass, Sex)

# confirming there are no missing valiues in the cleaned dataset
colSums(is.na(penguins_cleaned))

#  Plotting a scatter-plot of Culmen depth_mm against Culmen length
ggplot(penguins_cleaned, aes(x = Culmen_Length, y = Culmen_Depth, 
                             color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_manual(values = c("Adelie" = "darkorange", 
                                "Chinstrap" = "forestgreen", 
                                "Gentoo" = "darkred")) +
  theme_classic() +
  labs(
    x = "Culmen Length",
    y = "Culmen Depth"
  )

#Relationship between the variables and the differences between species 
#from the plot
# culmen length is beak length
# culmen depth is beak thickness

# Relationship between the variables from the plot.
# From the plot, the is a positive linear relationship between culmen depth
# and culmen depth for all the penguin species.
# This implies that as a penguin's beak gets longer, it gets deeper.

# Differences between species from the plot.
# Gentoo have long and less deep beaks.
# Chinstrap have long and deep beaks.
# Adelie have short and deep beaks.

