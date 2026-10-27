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

# slope estimate
# Girth = 5.0659; This means that for every 1 inch increase in tree girth
# the estimated timber volume increases by 5.0659 cubic feet
# This effect is statistically significant with a p-value of < 2e-16

# Multiple Regression (Volume predicted by Girth and Height)
multi_model <- lm(Volume ~ Girth + Height, data = trees)
summary(multi_model)

# The response variable is Volume
# The explanatory variables are Girth and Height

# slope estimates
# Girth = 4.7082;Holding tree height constant, for every 1 inch increase 
# in girth, timber volume increases by 4.7082 cubic feet

# Height = 0.3393;Holding tree girth constant, for every 1 foot increase 
# in tree height, timber volume increases by an estimated 0.34 cubic feet

# Compare Adjusted R-squared
cat("Simple Model Adj R-squared:", summary(simple_model)$adj.r.squared, "\n")
cat("Multiple Model Adj R-squared:", summary(multi_model)$adj.r.squared, "\n")

# Simple Model Adjusted R-squared = 0.9330895 implying 93.31 percent of 
# variation explained

# Multiple Model Adjusted R-squared = 0.9442322 implying 94.42 percent of 
# variation explained

# The Multiple Regression Model is superior
# The adjusted R-squared increases from 0.9330895 to 0.9442322 

# Because adjusted R-squared penalizes adding unnecessary variables 
# this increase confirms that adding Height adds real predictive power beyond
# Girth alone

# Additionally, the Residual Standard Error drops from 4.252 to 3.882, 
# showing reduced prediction error

# Model Diagnostics for Multiple Regression
par(mfrow = c(2, 2))
plot(multi_model)
par(mfrow = c(1, 1))

# Linearity (Residuals vs Fitted)
# The red smoother curve displays a slight U-shaped curve
# Residuals are slightly positive at small and large fitted values 
# and negative in the middle
# This slight non-linear curvature suggests volume naturally scales 
# geometrically rather than purely linearly

# Residual Normality (Q-Q Residuals)
# The data points closely adhere to the dashed theoretical quantile line, 
# with only slight deviations at the upper tail 
# This indicates that the normality assumption of residuals is reasonably 
# well met

# Residual Spread / Homoscedasticity (Scale-Location) 
# The spread of standardized residuals increases slightly as fitted values 
# increase
# There is minor heteroscedasticity (variance increases slightly with larger trees), 
# but it is not severe enough to violate model assumptions critically

# Influential Observations (Residuals vs Leverage)
# Observation 31 has noticeably higher leverage and residual magnitude than 
# the rest of the sample
# However, it remains within Cook's distance contour lines 
# (dashed lines are not crossed), meaning it does not distort the overall 
# stability of the regression fit

# Summary of evidence
# the scatter plots, pearson correlation, regression results and diagnostics
# demonstrate that tree size measurements explain almost all variation 
# in timber volume

# Girth acts as the primary driver of timber volume, 
# while Height offers significant secondary predictive value

# Limitation
# The sample consists of only 31 observations of a single species (black cherry trees)
# As a result, this model cannot be generalized to other tree species with 
# different structure or growth rates

# Why the results show association rather than proof of causation
# the regression model establishes statistical association, not causal proof
# High girth and height do not cause volume directly; rather, both parameters 
# reflect the biological accumulation of tree stem biomass as a 
# plant matures over time