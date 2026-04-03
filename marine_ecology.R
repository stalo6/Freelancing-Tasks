library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

# loading the datasets
shallow_sea <- read_excel("C:\\Users\\stalo\\Documents\\Marine Ecology_dataset.xlsx",sheet = "Shallow sea")
deep_sea <- read_excel("C:\\Users\\stalo\\Documents\\Marine Ecology_dataset.xlsx", sheet = "Deep sea")

View(shallow_sea)
View(deep_sea)

# merging the shallow_sea and deep_sea datasets
merged_data <- bind_rows(shallow_sea, deep_sea)
View(merged_data)

glimpse(merged_data)

# looking for missing values in the data set
colSums(is.na(merged_data))

# converting character variables that should be categorical to factors
factors_vars <- c("Category", "Habitat", "Functional Group", "Zone")
merged_data[factors_vars] <- lapply(merged_data[factors_vars], as.factor)

# looking at the updated dataset variables
glimpse(merged_data)

# H1 - Species richness(individuals per habitat) are higher in the shallow seas
# than in deep ocean

# species count (shallow sea VS deep ocean)
species_summary <- merged_data %>%
  filter(Habitat %in% c("Shallow seas", "Deep ocean")) %>%
  count(Habitat)

species_summary

# visualizing Species richness(individuals per habitat) total count in the
# shallow seas VS deep ocean
ggplot(species_summary, aes(Habitat, n, fill = Habitat)) +
  geom_col(width = 0.4) +
  labs(title = "Species Richness by Habitat",
       x = "Zone",
       y = "Number of Species")

# update the dataset
merged_data <- merged_data %>%
  mutate(Habitat = case_match(Habitat,
                              "Shallow Seas" ~ "Shallow seas",
                              "Deep ocean (>500 m)" ~ "Deep ocean",
                              "Deep ocean (~400 m)" ~ "Deep ocean",
                              .default = Habitat))
View(merged_data)


# shannon diversity index
# creating abundance table
species_abundance <- table(merged_data$Species, merged_data$Habitat)

# calculate the true proportions
species_prop <- prop.table(species_abundance, margin = 2)

# shannon index
shannon_index <- -colSums(species_prop * log(species_prop), na.rm = TRUE)
shannon_index

shannon_df <- data.frame(
  Habitat = names(shannon_index),
  shannon_index = as.numeric(shannon_index)
) %>%
  filter(Habitat %in% c("Shallow seas", "Deep ocean"))

# barplot showing the shannon index for shallow seas and deep ocean
ggplot(shannon_df, aes(x = reorder(Habitat, -shannon_index),
                       y = shannon_index, fill = Habitat)) +
  geom_col(show.legend = FALSE, width = 0.4) +
  theme_minimal() +
  labs(title = "Shannon Diversity Index: Shallow VS Deep",
       x = "Habitat",
       y = "Shannon Index")
  theme(axis.text = element_text(size = 11))
  

# H2- Active foraging behaviours are proportionally more frequent in the shallow
# seas than the deep ocean

# creating the active foraging data
foraging <- matrix(c(19,15,17,20), nrow = 2, byrow = TRUE)
rownames(foraging) <- c("Shallow Sea", "Deep Sea")
colnames(foraging) <- c("Active", "Not Active")

foraging

# chisquare test to test whether active foraging behaviours differed between
# shallow seas and the deep ocean
chisq.test(foraging)

# p-value = 0.5492 which is greater than 0.05 indicating was no significant
# difference in active foraging behaviours between the shallow seas and the 
# deep ocean

# H3- Functional group composition(grazers VS predators VS detritivores) differs
# markedly between the shallow seas than the deep ocean


# defining the functional groups
predator_groups <- c("Predator", "Apex Predator", "Micro-Predator", "predator",
                     "Filter Feeder / Predator")

grazer_groups <- c("Herbivore", "Herbivore / Primary Consumer",
                   "Primary consumer (symbiotic mutualist)",
                   "Primary consumer (bacteria farmer)","Herbivore/Detritivore",
                   "Primary consumer (symbiotic mutualist)",
                   "Primary consumer (filter feeder & symbiotic)")

detritivore_groups <- c("Detritivore", "Herbivore/Detritivore")

functional_data <- merged_data %>%
  mutate(Broad_group = case_when(
    `Functional Group` %in% predator_groups ~ "Predator",
    `Functional Group` %in% grazer_groups ~ "Grazer",
    `Functional Group` %in% detritivore_groups ~ "Detritivore",
    TRUE ~ "Other"
  )) %>%
  filter(Broad_group != "Other")

functional_data%>%
  filter(Habitat %in% c("Shallow seas", "Deep ocean")) %>%
  count(Habitat,`Functional Group`)


