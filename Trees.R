# LOADING THE REQUIRED LIBRARIES
library(ggplot2)
library(dplyr)


# LOADING THE DATASET
data(trees)
trees

# INSPECTING THE DATASET
View(trees)
glimpse(trees)
summary(trees)

# The trees dataset provides measurements of the diameter, 
# height and volume of timber in 31 felled black cherry trees

# The dataset has 31 observations and 3 variables
# Girth represents Tree diameter in inches and is numeric
# Height represents Height in ft and is numeric
# Volume represents Volume of timber in cubic ft and is numeric

# looking for missing values in the dataset
colSums(is.na(trees))

# PLOTTING THE SCATTERPLOTS AND CALCULATING THE PEARSON CORRELATIONS

# scatterplot of Volume against Girth
ggplot(trees, aes(x =  Girth, y = Volume)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Girth and Volume for black cherry trees",
       x = "Girth (tree diameter in inches)",
       y = "Volume (cubic ft)") +
  theme_minimal()

# The scatterplot of Volume against Girth displays a tight, strong, 
# positive linear pattern 
# As tree trunk girth increases, timber volume increases consistently

# The data points follow the fitted linear regression line closely across 
# the entire range


# scatterplot of Volume against Height
ggplot(trees, aes(x =  Height, y = Volume)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Height and Volume for black cherry trees",
       x = "Height (ft)",
       y = "Volume (cubic ft)") +
  theme_minimal()

# The scatterplot shows a moderate, positive linear trend
# but the data points exhibit significantly more scatter(variation) around 
# the trend line compared to girth While taller trees generally 
# have greater volume
# height alone shows a much looser relationship with volume

# Pearson correlation between Girth and Volume
cor_girth_vol <- cor(trees$Girth, trees$Volume)
cat("Pearson Correlation (Girth & Volume):", cor_girth_vol, "\n")

# pearson correlation between Girth and Volume = 0.9671

# Correlation's direction
# The correlation coefficient of 0.9671 is positive 
# This means that trees with larger trunk diameters (Girth) systematically 
# possess larger timber volumes

# Correlation's strength
# A coefficient of 0.967 indicates a very strong linear relationship
# confirming that girth is an outstanding single predictor of timber volume

# Pearson correlation between Height and Volume
cor_height_vol <- cor(trees$Height, trees$Volume)
cat("Pearson Correlation (Height & Volume):", cor_height_vol, "\n")

# pearson correlation between Height and Volume = 0.5982

# What correlation cannot tell you

# Correlation does not imply causation
# Correlation proves association, not cause and effect
# A high correlation does not mean increasing girth mechanically causes volume 
# rather, both grow together as the tree matures

# Correlation ignores Confounding Variables
# Correlation measures the relationship between two variables in isolation 
# and does not account for the joint effect of additional variables 
# such as tree Height


# FITTING AND COMPARING REGRESSION MODELS

# Simple Linear Regression (Volume predicted by Girth)
simple_model <- lm(Volume ~ Girth, data = trees)
summary(simple_model)

# The response variable is Volume 
# The explanatory variable is Girth 

# Multiple Regression (Volume predicted by Girth and Height)
multi_model <- lm(Volume ~ Girth + Height, data = trees)
summary(multi_model)

# The response variable is Volume
# The explanatory variables are Girth and Height

# Compare Adjusted R-squared
cat("Simple Model Adj R-squared:", summary(simple_model)$adj.r.squared, "\n")
cat("Multiple Model Adj R-squared:", summary(multi_model)$adj.r.squared, "\n")

# Model Diagnostics for Multiple Regression
par(mfrow = c(2, 2))
plot(multi_model)
par(mfrow = c(1, 1))
