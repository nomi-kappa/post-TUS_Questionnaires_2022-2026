##Clean workspace
rm(list = ls())

# Load packages #######
library(data.table) 
library(zoo)                 # methods for totally ordered indexed observations
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

# 
ONday_posttus_data_oldQ <- read.csv("ONday_combined_data_oldQ.csv");View(ONday_posttus_data_oldQ)
str(ONday_posttus_data_oldQ)


## add symptom values labels
value <- 1:4
value_label <- as.factor(c('absent', 'mild', 'moderate', 'severe'))
vlabels <- data.table(value, value_label)

relation = 1:5
rel_label <- as.factor(c('unrelated', 'unlikely', 'possible', 'probable', 'definite'))
rlabels <- data.table(relation, rel_label)


# Change values for symptoms columns
symptom <- c("Headache.value", "Neck.pain.value", "Tooth.pain.value", "Head.value", "Itchiness.value",
                      "Hearing.changes.value", "Speech.value", "Vision.value", "Twitching.value", "Balance.value",
                      "Hand.value", "Numbness.value", "Tightness.value", "Unusual.feelings.value", "Anxious.value",
                      "Sleepiness.value", "Attentional.difficulties.value", "Forgetful.value", "Nausea.value",
                      "Dizziness.value", "Other.value")

# Change values for relation columns
relation <- c("Headache.relation", "Neck.pain.relation", "Tooth.pain.relation", "Head.relation", "Itchiness.relation",
                      "Hearing.changes.relation", "Speech.relation", "Vision.relation", "Twitching.relation", "Balance.relation",
                      "Hand.relation", "Numbness.relation", "Tightness.relation", "Unusual.feelings.relation", "Anxious.relation",
                      "Sleepiness.relation", "Attentional.difficulties.relation", "Forgetful.relation", "Nausea.relation",
                      "Dizziness.relation", "Other.relation")
# Change values in symptom columns
for (col in symptom) {
  ONday_posttus_data_oldQ[[col]] <- factor(ONday_posttus_data_oldQ[[col]], levels = vlabels$value, labels = vlabels$value_label)
}

# Change values in relation columns
for (col in relation) {
  ONday_posttus_data_oldQ[[col]] <- factor(ONday_posttus_data_oldQ[[col]], levels = rlabels$relation, labels = rlabels$rel_label)
}



          # Plotting


# Gather symptom columns into long format
ONday_posttus_data_long <- pivot_longer(ONday_posttus_data_oldQ, cols = matches(".*\\.value$"), names_to = "Symptom", values_to = "Intensity");View(ONday_posttus_data_long)



# For Symptom
symptom_counts <- table(ONday_posttus_data_long$Intensity, ONday_posttus_data_long$Symptom, ONday_posttus_data_long$session)
symptom_counts_df <- as.data.frame(symptom_counts)
names(symptom_counts_df) <- c("Intensity", "Symptom", "Session", "Count")

symptom_plot <- ggplot(symptom_counts_df, aes(x = Symptom, y = Count, fill = Session)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Symptom Intensity Count", x = "Symptom", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  scale_fill_brewer(palette = "Set1") # Use a color palette for sessions

# Display the plots
print(relation_plot)
print(symptom_plot)


            # Plotting

# Define the colors for different intensities
severity_color <- c("absent" = "lightblue", "mild" = "yellow", "moderate" = "orange",  "severe" = "red")

# Create the plot
relation_plot <- ggplot(ONday_posttus_data_long, aes(x = Symptom, fill = Intensity)) +
  geom_bar() +
  scale_fill_manual(values = severity_colors) +
  labs(title = "Symptom Intensity Count", x = "Symptom", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  facet_wrap(~session)  # Facet by session

# Display the plot
print(symptom_plot)























# For Relation

# Select only the columns ending with .relation
relation_columns <- grep("\\.relation$", names(ONday_posttus_data_oldQ), value = TRUE)

# Gather relation columns into long format
relation_long <- ONday_posttus_data_oldQ %>%
  select(session, studyID, timestamp, todayDate, Date.of.visit, stimulation_type, all_of(relation_columns)) %>%
  pivot_longer(cols = matches(".*\\.relation$"),
               names_to = "Symptom",
               values_to = "Relation")

# Check the structure of the resulting data frame
str(relation_long)



# First plot

relation_counts <- table(relation_long$Relation, relation_long$Symptom, relation_long$session)
relation_counts_df <- as.data.frame(relation_counts)
names(relation_counts_df) <- c("Relation", "Symptom", "Session", "Count")

relation_plot <- ggplot(relation_counts_df, aes(x = Symptom, y = Count, fill = Session)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relation Intensity Count", x = "Symptom", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  scale_fill_brewer(palette = "Set1") # Use a color palette for sessions

# Display the plot
print(relation_plot)





unique(relation_long$Relation)# not sure what grey colour is in this case

# Second plot

# Define the colors for different intensities
severity_colors <- c("unrelated" = "lightblue", "unlikely" = "yellow", "possible" = "orange",  "probable" =  "brown", "definite" = "red")

# Create the plot
relation_plot <- ggplot(relation_long, aes(x = Symptom, fill = Relation)) +
  geom_bar() +
  scale_fill_manual(values = severity_colors) +
  labs(title = "Relation Intensity Count", x = "Sympton_relation", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  facet_wrap(~session)  # Facet by session


# Display the plot
print(relation_plot)
