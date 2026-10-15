# LOADING THE REQUIRED LIBRARIES
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

# LOADING THE DATASET
data(ToothGrowth)
ToothGrowth

#INSPECTING THE DATASET
View(ToothGrowth)
glimpse(ToothGrowth)
summary(ToothGrowth)

# The dataset has three variables, len which represents Tooth length, supp which
# represents Supplement type and dose which represents Dose in milligrams per day

# CHANGING THE dose VARIABLE TO A FACTOR
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# len is a continuous variable
# supp is a categorical variable
# dose is a categorical variable

# looking for missing values in the dataset
colSums(is.na(ToothGrowth))

# QUESTION ONE
# Do guinea pigs receiving doses of 0.5 and 2 mg/day differ in mean 
# tooth-growth measurement?

# The response variable in this question is len (tooth-growth measurement)
# len is continuous

# The explanatory variable in this question is dose
# dose is categorical

# stating the hypotheses

# Null hypothesis
# There is no difference in mean tooth-growth measurement between 
# guinea pigs receiving a 0.5 mg/day dose and those receiving a 2 mg/day dose

# Alternative hypothesis
# There is a difference in mean tooth-growth measurement between 
# guinea pigs receiving a 0.5 mg/day dose and those receiving a 2 mg/day dose

# Statistical Test
# An independent two-sample t-test
# This test is suitable because we are comparing the means of a 
# continuous response variable between exactly two independent categorical groups

# Subset data for only 0.5 and 2.0 mg/day doses
q1_data <- subset(ToothGrowth, dose %in% c(0.5, 2.0))
q1_data$dose <- droplevels(q1_data$dose)

# Descriptive summaries
tapply(q1_data$len, q1_data$dose, summary)
tapply(q1_data$len, q1_data$dose, sd)

# Run the independent t-test
t.test(len ~ dose, data = q1_data)

# Boxplot comparing tooth length by dose
ggplot(ToothGrowth, aes(x = dose, y = len, fill = dose)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 8) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Tooth Growth Distribution Across Vitamin C Doses",
       x = "Dose (mg/day)", y = "Tooth Length (len)")

# the two-sample Welch t-test comparing tooth length (len) between guinea pigs
# receiving 0.5 mg/day dose  and 2.0 mg/day dose of vitamin C yielded a 
# statistically significant result with p-value = 4.398e-14 < 0.001
# because the p-value is far below the standard significance threshold (alpha = 0.05), 
# we reject the null hypothesis that there is no difference 
# in mean tooth growth between the two dosage groups

# The difference is not just statistically significant; it is 
# exceptionally large in magnitude 
# increasing the vitamin C dose from 0.5 to 2.0 mg/day results in 
# an average tooth-growth jump of over 15 units (from a mean of 10.605 to 26.100)

# In relation to the research question, this confirms a massive dose-response 
# relationship. 
# Vitamin C is a critical catalyst for odontoblast activity 
# (the cells responsible for tooth formation in guinea pigs). 
# The box plot clearly demonstrates that increasing the dosage scales 
# tooth growth upward consistently, rather than resulting in random or negligible biological variation.


# QUESTION TWO
# Does mean tooth-growth measurement differ among the three dose levels?
# The response variable in this question is len(tooth-growth measurement)
# len is continuous

# The explanatory variable in this question is dose
# dose is categorical

# stating the hypotheses

# Null hypothesis
# Mean tooth-growth is the same across all three dose levels

# Alternative hypothesis
# At least one dose level has a different mean tooth growth from the others

# Statistical Test
# A One-way ANOVA (Analysis of Variance)
# This test is appropriate because we are comparing the means of a 
# continuous response variable across three independent categorical groups

# Descriptive summaries for all three groups
tapply(ToothGrowth$len, ToothGrowth$dose, summary)
tapply(ToothGrowth$len, ToothGrowth$dose, sd)

# Run the One-way ANOVA
q2_test <- aov(len ~ dose, data = ToothGrowth)
summary(q2_test)

# Check assumptions

# Normality of residuals
shapiro.test(residuals(q2_test))

# Homogeneity of variances (Equal variances)
bartlett.test(len ~ dose, data = ToothGrowth)

# A One-way ANOVA comparing mean tooth length (len) across the three vitamin C 
# dose levels (0.5, 1.0, and 2.0 mg/day) yields a highly significant F-statistic 
# with a p-value = 9.53e-16 < 0.001 
# Because the p-value is far below the standard significance threshold (alpha = 0.05), 
# we reject the null hypothesis that the mean tooth-growth measurement is 
# equal across all three dose levels

# From the box plot:
# Moving from left to right across the 0.5, 1.0, and 2.0 mg/day groups, 
# there is a clear, step-wise upward shift in both the median lines and the interquartile ranges.   
# The median tooth length sits near 10.5 for the 0.5 mg group.   
# It rises to approximately 19–21 for the 1.0 mg group.   
# It climbs further to roughly 27 for the 2.0 mg group. 

# The dispersion of data points within each group is relatively balanced, 
# and aside from a single upper outlier in the 0.5 mg group, 
# the distributions conform well to expected patterns without erratic skewness

# The ANOVA result, backed by the boxplot, proves that the relationship 
# is not binary (it's not just "low vs. high dose"). 
# Instead, every incremental increase in vitamin C dosage produces a proportional, 
# cumulative increase in tooth growth.

# In guinea pigs, vitamin C acts as an essential dietary requirement for 
# collagen synthesis and odontoblast development. 
# The progression visible in the box plot demonstrates a robust, 
# graded dose-response curve—higher systemic concentrations of vitamin C 
# systematically enhance dental tissue elongation in a predictable, 
# highly significant manner

# QUESTION THREE
# Is dose level associated with whether tooth growth is relatively high or low?

# The response variable is tooth growth category(high, low)
# The explanatory variable is dose

# tooth growth category is categorical
# dose is categorical

# stating the hypotheses

# Null hypothesis
# Dose level and tooth-growth category (high/low) are independent; 
# there is no association between them

# Alternative hypothesis
# There is an association between dose level and tooth-growth category

# Statistical Test
# A Chi-square test of independence. 
# This is suitable because we are testing for a relationship between two 
# categorical variables, evaluating if the observed frequencies in the 
# dose/growth categories deviate significantly from expected frequencies

# Create the categorical response variable based on the overall median
median_len <- median(ToothGrowth$len)
ToothGrowth$growth_cat <- ifelse(ToothGrowth$len >= median_len, "High", "Low")
ToothGrowth$growth_cat <- as.factor(ToothGrowth$growth_cat)

# Generate the contingency table (descriptive summary)
counts <- table(ToothGrowth$dose, ToothGrowth$growth_cat)
print(counts)

# Run the Chi-square test
q3_test <- chisq.test(counts)
print(q3_test)

# Inspect expected counts to ensure assumptions are met (>5)
print(q3_test$expected)

# Stacked Bar Chart of Dose against Growth Category
ggplot(ToothGrowth, aes(x = dose, fill = growth_cat)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Proportion of High vs. Low Tooth Growth by Dose",
       x = "Dose (mg/day)", y = "Proportion", fill = "Growth Category")

# A Chi-square test of independence examining the relationship between 
# vitamin C dose levels (0.5, 1.0, and 2.0 mg/day) and 
# tooth growth category (High vs. Low, split at the overall median) 
# yields a highly significant result p-value = 9.214e-08 < 0.001

# Because the p-value falls below the standard significance threshold 
# (alpha = 0.05), 
# we reject the null hypothesis that dose level and relative tooth-growth 
# category are independent

# From the stacked bar chart
# Across the three dose columns, the balance of Low versus High growth shifts 
# almost completely   
# At 0.5 mg/day: The bar is mostly dominated by the Low growth category 
# At 1.0 mg/day: The proportion splits right down the middle at roughly 50% Low 
# and 50% High growth.   
# At 2.0 mg/day: The trend reverses entirely, with the bar being mostly 
# dominated by the High growth category

# The chi-square test and the visual graphic together prove that categorizing 
# tooth growth into relative thresholds reveals a powerful, 
# predictable dependency on dietary intake

# Shifting from a low dose (0.5 mg/day) to a high dose (2.0 mg/day) does not 
# merely shift a continuous average; it guarantees whether an individual
# guinea pig will fall above or below the population's median tooth-growth
# benchmark. 
# This reinforces the biological reality that vitamin C is a vital 
# threshold-dependent nutrient for odontoblast development and 
# dental elongation in guinea pigs.