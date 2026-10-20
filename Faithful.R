# LOADING THE REQUIRED LIBRARIES
library(dplyr)
library(stringr)
library(ggplot2)

# LOADING THE faithful DATASET
data("faithful")
faithful

# INSPECTING THE faithful DATASET
glimpse(faithful)
View(faithful)
summary(faithful)

# The faithful dataset has two variables; eruptions which is numeric and
# represents eruption time in minutes; waiting is numeric and represents the
# Waiting time to next eruption

# LOOKING FOR MISSING VALUES IN THE DATASET
colSums(is.na(faithful))

# PREPARING THE DATA FOR ANALYSIS

faithful <- faithful %>%
  mutate(
    # creating eruption-duration groups (Short, Medium, Long)
    eruption_duration = factor(cut(
      eruptions,
      breaks = c(-Inf, 2.5, 3.5, Inf),
      labels = c("Short", "Medium", "Long"),
      right = FALSE
    )),
    
    # 2. Create waiting_time variable (High waiting time, Low waiting time)
    waiting_time = factor(
      if_else(waiting >= median(waiting), "High", "Low")
    )
  )

glimpse(faithful)
summary(faithful)


# QUESTION ONE
# Do short and long eruptions differ significantly in their mean waiting time 
# to the next eruption? (t-test)

# The variables in this question are eruption_duration and waiting
# waiting (waiting time to next eruption) is the response variable and is continuous
# eruption_duration is the explanatory variable and is categorical

# Stating the hypotheses

# Null Hypothesis
# There is no difference in mean waiting time to the next eruption 
# between short and long eruptions

# Alternative Hypothesis
# There is a difference in mean waiting time to the next eruption 
# between short and long eruptions

# Statistical Test
# Independent Two-Sample Welch t-test
# Justification: We are comparing the population means of a continuous response 
# variable (waiting) across exactly two independent categorical groups; Short, Long 
# t-test is used because it does not assume equal variances between the two groups

# Subsetting the data for Short and Long duration groups only
q1_data <- subset(faithful, eruption_duration %in% c("Short", "Long"))
q1_data$eruption_duration <- droplevels(q1_data$eruption_duration)

# descriptive summaries
tapply(q1_data$waiting, q1_data$eruption_duration, summary)
tapply(q1_data$waiting, q1_data$eruption_duration, sd)

# carrying out the t-test
t.test(waiting ~ eruption_duration, data = q1_data)


# Assumption Checking: Normality within groups
shapiro.test(q1_data$waiting[q1_data$eruption_duration == "Short"])
shapiro.test(q1_data$waiting[q1_data$eruption_duration == "Long"])

# boxplot for waiting time comparison for lond and short eruptions
ggplot(q1_data, aes(x = eruption_duration, y = waiting, fill = eruption_duration)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.8) +
  scale_fill_manual(values = c("Short" = "#3498db", "Long" = "#e74c3c")) +
  theme_minimal() +
  labs(
    title = "Waiting time comparison for Short and Long eruptions",
    x = "Eruption duration group",
    y = "Waiting time to next Eruption (mins)",
    fill = "Group"
  )

# Statistical Output

# The t-test yields a statistic t = -34.716; df = 184.63 with an extremely 
# small p-value < 2.2e-16

# The mean waiting time for the Short group is 54.28 minutes, compared to
# 80.33 minutes for the Long group

# Difference in Means: Long eruptions require an average of 26.05 
# additional minutes of waiting time compared to short eruptions

# 95% Confidence Interval: -27.53, -24.57 minutes
# We are 95% confident that the true population mean difference in waiting time 
# between Short and Long eruptions lies between 24.57 and 27.53 minutes

# Decision: Because p-value < 0.05, we reject the null hypothesis

# Assumption Checking (Normality Within Groups)

# Shapiro-Wilk Test Results
# Short Eruptions: W = 0.98017, p-value = 0.1749
# Long Eruptions: W = 0.99396, p-value = 0.7194   
# Assumption Assessment: For both duration groups, the p-values are greater than 
# the standard alpha = 0.05 threshold thus, we fail to reject the null 
# hypothesis of normality for both groups 
# The data in both the Short and Long eruption categories follow a normal 
# distribution, perfectly satisfying the normality assumption 
# required for the t-test

# From the boxplot

# There is complete distribution separation
# The interquartile ranges of the Short and Long groups show zero vertical 
# overlap 
# The middle 50% for Short sits between approximately 50 and 60 minutes,
# for Long it sits entirely between 78 and 85 minutes

# Symmetry and  Dispersion 
# The boxes are symmetric around their medians sitting around 54 and 80 minutes
# visually aligning with the Shapiro-Wilk normality test results

# Equal Variances
# Both boxes display similar height/spread confirming 
# homoscedasticity (comparable variance between groups) 
# The few red outlier points at the top and bottom whiskers of the Long group 
# do not destabilize the test due to the overall large sample size and 
# tight clustering


# Practical Finding: The evidence strongly demonstrates that eruption duration 
# directly corresponds to waiting time. Long eruptions require significantly 
# longer recovery times before the next eruption occurs averaging over 26 minutes 
# longer in waiting time compared to short eruptions


# QUESTION TWO
# Does mean waiting time to the next eruption differ among 
# short, medium and long eruptions? (ANOVA)

# The variables in this question are eruption_duration and waiting
# waiting (waiting time to next eruption) is the response variable and is continuous
# eruption_duration is the explanatory variable and is categorical

# Stating the hypotheses

# Null hypothesis
# Mean waiting time to the next eruption is equal across 
# short, medium, and long eruption groups

# Alternative hypothesis
# At least one eruption-duration group has a mean waiting time that 
# differs from the others

# Statistical Test
# One-Way ANOVA
# Justification: We are comparing the continuous mean response across 
# three independent categorical levels
# ANOVA tests whether the variance between the group means is significantly 
# larger than the variance within the groups

# descriptive summaries for all three groups
tapply(faithful$waiting, faithful$eruption_duration, summary)
tapply(faithful$waiting, faithful$eruption_duration, sd)

# carrying out one-way ANOVA
q2_test <- aov(waiting ~ eruption_duration, data = faithful)
summary(q2_test)

# Assumption Checking
# normality of residuals
shapiro.test(residuals(q2_test))


# homogeneity of variances
bartlett.test(waiting ~ eruption_duration, data = faithful)

# boxplot for distribution of waiting times across all eruption duration groups
ggplot(faithful, aes(x = eruption_duration, y = waiting, fill = eruption_duration)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.8) +
  scale_fill_manual(values = c("Short" = "#3498db", "Medium" = "#f39c12", "Long" = "#e74c3c")) +
  theme_minimal() +
  labs(
    title = "Distribution of waiting times across all eruption duration groups",
    x = "Eruption duration droup",
    y = "Waiting time to next eruption (mins)",
    fill = "Duration group"
  )

# Statistical Output

# The One-way ANOVA yields an F-statistic F(2, 269) = 577.4 with a p-value 
# p < 2e-16
# Short Group: mean = 54.28 minutes; median = 54 minutes
# Medium Group: mean = 66.17 minutes; median = 65.50 minutes
# Long Group: mean = 80.33 minutes; median = 80 minutes
# Decision: Because p < 0.05, we reject the null hypothesis
# There is highly statistically significant evidence that mean
# waiting time differs among the short, medium, and long 
# eruption duration categories

# Assumption Checks

# Normality of residuals (Shapiro-Wilk Test)
# Test output: W = 0.99406, p-value = 0.3646
# Insight: Because p > 0.05, we fail to reject the null hypothesis of normality
# The residuals are normally distributed, fully satisfying the normality assumption for ANOVA

# Homogeneity of Variances (Bartlett's Test)
# Test output: Bartlett's K-squared = 5.9046, df = 2, p-value = 0.05222
# Insight: Since p > 0.05, we assume equal variances 
# across groups (homoscedasticity), validating the ANOVA model structure

# From the boxplot

# The boxplot clearly shows a step-wise increase in waiting times 
# as eruption duration increases from Short to Medium to Long


# Practical Finding: 
# Mean waiting time varies significantly across all three eruption lengths

# longer eruptions require progressively longer waiting times until 
# the next eruption


# QUESTION THREE
# Is eruption-duration group associated with whether the waiting time
# to the next eruption is relatively high or low? (Chi-square test)

# The variables in this question are eruption_duration and waiting_time
# waiting_time is the response variable and is categorical
# eruption-duration is the explanatory variable and is categorical

# Stating the hypotheses

# Null hypothesis
# Eruption-duration and waiting-time(High,Low) are independent; 
# there is no association between them

# Alternative hypothesis
# Eruption-duration and waiting-time are associated (not independent)

# Statistical Test
# Chi-Square Test of Independence
# Justification: Both the explanatory and response variables are categorical
# The test evaluates whether observed frequency distributions deviate 
# significantly from what would be expected under independence

# generating contingency table
counts <- table(faithful$eruption_duration, faithful$waiting_time)
print(counts)

# Execute Chi-Square Test
q3_test <- chisq.test(counts)
print(q3_test)

# Inspect Expected Counts to verify assumption
print("Expected Counts:")
print(q3_test$expected)

# stacked bar chart of proportion of high and low waiting times by eruption group
ggplot(faithful, aes(x = eruption_duration, fill = waiting_time)) +
  geom_bar(position = "fill", color = "black", alpha = 0.85) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Low" = "#2ecc71", "High" = "#9b59b6")) +
  theme_minimal() +
  labs(
    title = "Proportion of high and low waiting times by eruption group",
    x = "Eruption duration group",
    y = "Percentage",
    fill = "Waiting category"
  )

# Statistical Output

# The Chi-square test yields X-squared = 161.69; df = 2; p-value (p < 2.2e-16)
# Decision: Because p < 0.05 we reject the null hypothesis 
# There is strong statistical evidence of a high dependency 
# between eruption duration length and waiting time category

# Contingency table breakdown 

# Short Group: 0 High versus 92 Low waiting times
# 100% of short eruptions resulted in a low recovery time
# Long Group: 136 High versus 32 Low waiting times
# 81% of long eruptions led to a high recovery time 
# Medium Group: 2 High versus 10 Low waiting times 
# 83.3% of medium eruptions led to a low waiting time

# Assumption Check

# Expected Frequencies Output:Short: 46.68 (High), 45.32 (Low); 
# Medium: 6.09 (High), 5.91 (Low); Long: 85.24 (High), 82.76 (Low)  

# The fundamental assumption for the Pearson Chi-Square test is that 
# all expected cell frequencies must be at least 5 
# Since the smallest expected cell count in the model is 5.91, 
# the minimum threshold is met across all six cells meaning simulated p-values 
# are not required

# From the stacked bar chart
# The Short column is entirely green (100% Low waiting time)
# The Medium column remains mostly green ( around 83% Low waiting time)
# The Long column is mostly purple ( around 81% High waiting time)
# If the two variables were independent, all three bars would look 
# nearly identical, displaying roughly 50/50 splits matching the 
# overall population median 
# The massive contrast between the Short and Long bars proves non-independence

# Practical Finding: Eruption duration is strongly associated with whether 
# waiting time is above or below the median
# Short eruptions almost guaranteed a below-median (low) wait time, 
# whereas long eruptions overwhelmingly led to above-median (high) wait times
