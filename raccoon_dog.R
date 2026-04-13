# installing and loading the ecoteach Package
install.packages("ecoteach")
library(ecoteach)

# loading other required libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# finding the required dataset
data(package = "ecoteach")

# loading the raccoondog_environment dataset
data(raccoondog_environment)
raccoondog_environment
# the dataset captures seasonal wildlife activity under human and evironmental
# influence for multiple species and sites

# we will mostly base our analysis on the raccoon dogs because
# the research was designed to understand their activity patterns and 
# habitat associations

# DATA MANAGEMENT 

# looking at the dimensions of the dataset
dim(raccoondog_environment)
# there are 142 rows and 22 columns

# looking at the column types and structure of the dataset
str(raccoondog_environment)

# viewing the dataset
View(raccoondog_environment)

# looking at the top rows of the dataset
head(raccoondog_environment)

# summary statistics for each column of the dataset
summary(raccoondog_environment)


# DATA CLEANING AND VALIDATION

# looking for missing values in the dataset
colSums(is.na(raccoondog_environment))
# there are no missing values in all columns of the dataset

# looking at factor levels
# looking at the seasons in the dataset
levels(raccoondog_environment$Season)

# looking at the vegetation in the dataset
levels(raccoondog_environment$Vegetation)

# Create a binary presence/absence variable
raccoondog_environment <- raccoondog_environment %>%
  mutate(presence = if_else(raccoon_dog > 0, 1, 0))

# Subset to detections only
detections_only <- subset(raccoondog_environment, presence == 1)
detections_only


# UNCOVERING TRENDS

# Counting detections per season
raccoondog_environment %>%
  group_by(Season) %>%
  summarise(
    total_detections = sum(raccoon_dog),
    mean_detections  = mean(raccoon_dog),
    sites_with_presence = sum(presence)
  )

# Boxplot of detections by season
ggplot(raccoondog_environment, aes(x = Season, y = raccoon_dog, fill = Season)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Autumn" = "lightblue", "Winter" = "lightgreen")) +
  labs(title = "Raccoon Dog Detections by Season",
       x = "Season",
       y = "Detections") +
  theme_minimal() +
  theme(legend.position = "none")
# most detections are made during autumn

# looking at the detection rate in each vegetation type
raccoondog_environment %>%
  group_by(Vegetation) %>%
  summarise(detection_rate = mean(presence))

# barplot of the vegetation type by raccoon dog detection rate
raccoondog_environment %>%
  group_by(Vegetation) %>%
  summarise(detection_rate = mean(presence)) %>%
  ggplot(aes(x = reorder(Vegetation, detection_rate),
             y = detection_rate, fill = detection_rate)) +
  scale_fill_gradient(low = "lightyellow", high = "darkgreen") +
  geom_col() +
  coord_flip() +
  labs(
    title = "Presence Rate by Vegetation Type",
    x = "Vegetation Type", y = "Proportion of Sites with Detections",
    fill = "Rate"
  ) +
  theme_minimal()


# How do detections vary across seasons?
season_summary <- raccoondog_environment %>%
  group_by(Season) %>%
  summarise(mean_detections = mean(raccoon_dog))

print(season_summary)

# looking at whether distance to agricultural land differs between presence and 
#absence

tapply(raccoondog_environment$dist_agricultural,
       raccoondog_environment$presence,
       mean)

# Correlation between raccoon dog counts and environmental predictors
cor(raccoondog_environment$raccoon_dog,
    raccoondog_environment$dist_agricultural,
    use = "complete.obs")
# the correlation is -0.1420732 so there is a negative relationship between
# raccoon dog counts and environmental predictors

# STATISTICAL TESTS

# looking at whether detections are significantly different between seasons
kruskal.test(raccoon_dog ~ Season, data = raccoondog_environment)
# the p-value = 0.1729 is greater than 0.05 indicating that detections are
# not significantly different across the different seasons

# Logistic regression: what predicts presence?
model <- glm(presence ~ dist_agricultural + Season + Vegetation,
             data = raccoondog_environment,
             family = binomial)
summary(model)

# the significant predictor is dist_agricultural which is the distance to
# agricultural land with p = 0.0207
# this indicates that raccoon dogs are more likely to be detected closer to 
# agricultural land hence distance to agricultural land is the 
# dominant predictor of raccoon dog presence

# scatter of plot of raccoon dog detections versus distance to agricultural land
# with trend lines fitted per season
ggplot(raccoondog_environment,
       aes(x = dist_agricultural, y = raccoon_dog, colour = Season)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title    = "Detections versus Distance to Agricultural Land",
    x = "Distance to Agricultural Land (m)",
    y = "Number of Detections",
    colour = "Season"
  ) +
  theme_minimal()

# from the scatter plot, the trend lines slope downwards indicating that
# detections decrease as distance from agricultural land increases

# heatmap of mean raccoon dog detection by season and vegetation type
raccoondog_environment %>%
  group_by(Season, Vegetation) %>%
  summarise(mean_detections = mean(raccoon_dog), .groups = "drop") %>%
  ggplot(aes(x = Season, y = Vegetation, fill = mean_detections)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(mean_detections, 1)),
            colour = "black", size = 3.5) +
  scale_fill_gradient(low = "lightyellow", high = "firebrick") +
  labs(
    title = "Detections by Season and Vegetation Type",
    x = "Season",
    y = "Vegetation Type",
    fill = "Mean\nDetections"
  ) +
  theme_minimal()


# bar plot of mean detections per species by season for all species
raccoondog_environment %>%
  select(Season, asian_badger:tolai_hare) %>%
  group_by(Season) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(-Season,
               names_to  = "species",
               values_to = "mean_detections") %>%
  ggplot(aes(x = reorder(species, mean_detections),
             y = mean_detections,
             fill = Season)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Mean Detections per Species by Season",
       x = "Species", y = "Mean Detections") +
  theme_minimal()
# detections are more frequent in autumn compared to winter for the raccoon dog,
# asian badger, roe deer, siberian weasel and hog badger while detection are
# more frequent in the winter compared to autumn for the rock squirrel,
#tolai hare, red squirrel, striped squirrel, leopard cat and wild boar
