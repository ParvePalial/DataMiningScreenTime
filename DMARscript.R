# ============================================================================
# Student Screen Time, Addiction & Health Effects - Comprehensive Analysis
# ============================================================================

# Install and load required packages
required_packages <- c("readxl", "tidyverse", "ggplot2", "plotly", "corrplot", 
                       "caret", "randomForest", "e1071", "rpart", "rpart.plot",
                       "gridExtra", "reshape2", "scales", "viridis", "GGally")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ============================================================================
# 1. DATA LOADING AND PREPROCESSING
# ============================================================================

# Load data (adjust file path as needed)
data <- read_excel("screen_time_data.xlsx")

# Display structure
str(data)
summary(data)

# Clean column names for easier reference
colnames(data) <- c(
  "Name", "Age", "Gender", "Year_of_Study", "Daily_Hours", "Usage_Time",
  "Primary_App", "Gaming_Type", "Social_Media_Hours", "YouTube_Hours",
  "Messaging_Hours", "Music_Hours", "Gaming_Hours", "Affects_Studies",
  "Affects_Sleep", "Want_Reduce", "Q1_Anxious", "Q2_Check_Morning",
  "Q3_Use_While_Working", "Q4_Difficult_Limit", "Q5_Hard_Focus",
  "Q6_Attention_Reduced", "Q7_Endless_Scroll", "Q8_Mentally_Drained",
  "Q9_Eye_Strain", "Q10_Neck_Pain", "Q11_Reduced_Physical",
  "Q12_Use_Before_Sleep", "Q13_Sleep_Later", "Q14_Sleep_Quality",
  "Q15_Distracted_Study", "Q16_Use_During_Class", "Q17_Missed_Deadlines",
  "Open_Response_1", "Open_Response_2"
)

# ============================================================================
# 2. FEATURE ENGINEERING
# ============================================================================

# Create composite scores
data <- data %>%
  mutate(
    # Addiction Score (Q1-Q4): Mean of addiction-related questions
    Addiction_Score = rowMeans(select(., Q1_Anxious:Q4_Difficult_Limit), na.rm = TRUE),
    
    # Brain Rot Score (Q5-Q8): Cognitive effects
    Brain_Rot_Score = rowMeans(select(., Q5_Hard_Focus:Q8_Mentally_Drained), na.rm = TRUE),
    
    # Physical Health Score (Q9-Q11)
    Physical_Health_Score = rowMeans(select(., Q9_Eye_Strain:Q11_Reduced_Physical), na.rm = TRUE),
    
    # Sleep Impact Score (Q12-Q14)
    Sleep_Impact_Score = rowMeans(select(., Q12_Use_Before_Sleep:Q14_Sleep_Quality), na.rm = TRUE),
    
    # Productivity Score (Q15-Q17)
    Productivity_Score = rowMeans(select(., Q15_Distracted_Study:Q17_Missed_Deadlines), na.rm = TRUE),
    
    # Overall Impact Score
    Overall_Impact_Score = rowMeans(select(., Addiction_Score:Productivity_Score), na.rm = TRUE),
    
    # Convert Daily_Hours to numeric category
    Daily_Hours_Numeric = case_when(
      Daily_Hours == "Less than 2 hours" ~ 1,
      Daily_Hours == "2-4 hours" ~ 2.5,
      Daily_Hours == "4-6 hours" ~ 5,
      Daily_Hours == "6-8 hours" ~ 7,
      Daily_Hours == "More than 8 hours" ~ 9,
      TRUE ~ NA_real_
    ),
    
    # Risk Category based on Overall Impact Score
    Risk_Category = case_when(
      Overall_Impact_Score < 2.5 ~ "Low Risk",
      Overall_Impact_Score < 3.5 ~ "Moderate Risk",
      Overall_Impact_Score >= 3.5 ~ "High Risk",
      TRUE ~ "Unknown"
    )
  )

# ============================================================================
# 3. EXPLORATORY DATA ANALYSIS - VISUALIZATIONS
# ============================================================================

# Set theme
theme_set(theme_minimal() + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                  plot.subtitle = element_text(hjust = 0.5, size = 10)))

# --- Chart 1: Age Distribution ---
p1 <- ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "#3498db", color = "white", alpha = 0.8) +
  labs(title = "Age Distribution of Participants",
       x = "Age", y = "Frequency") +
  theme_minimal()

# --- Chart 2: Gender Distribution ---
p2 <- ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("#e74c3c", "#3498db", "#95a5a6")) +
  labs(title = "Gender Distribution",
       x = "Gender", y = "Count") +
  theme_minimal()

# --- Chart 3: Daily Screen Time Distribution ---
p3 <- ggplot(data, aes(x = Daily_Hours, fill = Daily_Hours)) +
  geom_bar(alpha = 0.8) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Daily Screen Time Distribution",
       x = "Hours per Day", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Chart 4: Primary App Usage ---
p4 <- ggplot(data, aes(x = reorder(Primary_App, Primary_App, function(x) -length(x)))) +
  geom_bar(fill = "#9b59b6", alpha = 0.8) +
  coord_flip() +
  labs(title = "Primary App Usage Categories",
       x = "App Category", y = "Count") +
  theme_minimal()

# --- Chart 5: Usage Time Preference ---
p5 <- ggplot(data, aes(x = Usage_Time, fill = Usage_Time)) +
  geom_bar(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "When Do Students Use Phones Most?",
       x = "Time of Day", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Chart 6: Composite Scores Distribution ---
scores_data <- data %>%
  select(Addiction_Score, Brain_Rot_Score, Physical_Health_Score, 
         Sleep_Impact_Score, Productivity_Score) %>%
  pivot_longer(everything(), names_to = "Score_Type", values_to = "Score")

p6 <- ggplot(scores_data, aes(x = Score_Type, y = Score, fill = Score_Type)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(title = "Distribution of Impact Scores",
       subtitle = "1 = Low Impact, 5 = High Impact",
       x = "", y = "Score") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Chart 7: Risk Category Distribution ---
p7 <- ggplot(data, aes(x = Risk_Category, fill = Risk_Category)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("Low Risk" = "#2ecc71", 
                               "Moderate Risk" = "#f39c12",
                               "High Risk" = "#e74c3c")) +
  labs(title = "Risk Category Distribution",
       x = "Risk Level", y = "Count") +
  theme_minimal()

# --- Chart 8: Screen Time vs Addiction Score ---
p8 <- ggplot(data, aes(x = Daily_Hours_Numeric, y = Addiction_Score, color = Risk_Category)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  scale_color_manual(values = c("Low Risk" = "#2ecc71", 
                                "Moderate Risk" = "#f39c12",
                                "High Risk" = "#e74c3c")) +
  labs(title = "Screen Time vs Addiction Score",
       x = "Daily Hours", y = "Addiction Score",
       color = "Risk Category") +
  theme_minimal()

# --- Chart 9: Correlation Heatmap ---
correlation_data <- data %>%
  select(Addiction_Score:Productivity_Score, Daily_Hours_Numeric) %>%
  na.omit()

cor_matrix <- cor(correlation_data)

p9 <- ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#3498db", high = "#e74c3c", mid = "white",
                       midpoint = 0, limit = c(-1, 1)) +
  labs(title = "Correlation Heatmap of Impact Scores",
       x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Chart 10: Sleep Impact by Year of Study ---
p10 <- ggplot(data, aes(x = Year_of_Study, y = Sleep_Impact_Score, fill = Year_of_Study)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Sleep Impact Score by Year of Study",
       x = "Year of Study", y = "Sleep Impact Score") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Chart 11: Study Impact Analysis ---
study_impact <- data %>%
  count(Affects_Studies, Daily_Hours) %>%
  group_by(Daily_Hours) %>%
  mutate(percentage = n / sum(n) * 100)

p11 <- ggplot(study_impact, aes(x = Daily_Hours, y = percentage, fill = Affects_Studies)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(title = "Does Screen Time Affect Studies?",
       x = "Daily Screen Time", y = "Percentage (%)",
       fill = "Affects Studies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Chart 12: Desire to Reduce Screen Time ---
p12 <- ggplot(data, aes(x = Want_Reduce, fill = Risk_Category)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Low Risk" = "#2ecc71", 
                               "Moderate Risk" = "#f39c12",
                               "High Risk" = "#e74c3c")) +
  labs(title = "Desire to Reduce Screen Time by Risk Level",
       x = "Want to Reduce?", y = "Count",
       fill = "Risk Category") +
  theme_minimal()

# Display all plots
grid.arrange(p1, p2, p3, p4, ncol = 2)
grid.arrange(p5, p6, p7, p8, ncol = 2)
grid.arrange(p9, p10, p11, p12, ncol = 2)

# ============================================================================
# 4. DATA MINING - CLASSIFICATION ALGORITHMS
# ============================================================================

# Prepare data for classification
classification_data <- data %>%
  select(Age, Gender, Year_of_Study, Daily_Hours_Numeric, 
         Addiction_Score:Productivity_Score, Risk_Category) %>%
  na.omit() %>%
  mutate(
    Gender = as.factor(Gender),
    Year_of_Study = as.factor(Year_of_Study),
    Risk_Category = as.factor(Risk_Category)
  )

# Split data into training and testing sets (70-30 split)
set.seed(123)
trainIndex <- createDataPartition(classification_data$Risk_Category, p = 0.7, list = FALSE)
train_data <- classification_data[trainIndex, ]
test_data <- classification_data[-trainIndex, ]

# --- Algorithm 1: Decision Tree ---
dt_model <- rpart(Risk_Category ~ Age + Gender + Daily_Hours_Numeric + 
                    Addiction_Score + Brain_Rot_Score + Physical_Health_Score +
                    Sleep_Impact_Score + Productivity_Score,
                  data = train_data,
                  method = "class")

# Plot decision tree
rpart.plot(dt_model, main = "Decision Tree for Risk Classification",
           extra = 104, box.palette = "RdYlGn")

# Predictions
dt_predictions <- predict(dt_model, test_data, type = "class")
dt_confusion <- confusionMatrix(dt_predictions, test_data$Risk_Category)
print("Decision Tree Results:")
print(dt_confusion)

# --- Algorithm 2: Random Forest ---
rf_model <- randomForest(Risk_Category ~ Age + Gender + Daily_Hours_Numeric + 
                           Addiction_Score + Brain_Rot_Score + Physical_Health_Score +
                           Sleep_Impact_Score + Productivity_Score,
                         data = train_data,
                         ntree = 500,
                         importance = TRUE)

# Variable importance plot
varImpPlot(rf_model, main = "Variable Importance - Random Forest")

# Predictions
rf_predictions <- predict(rf_model, test_data)
rf_confusion <- confusionMatrix(rf_predictions, test_data$Risk_Category)
print("Random Forest Results:")
print(rf_confusion)

# --- Algorithm 3: Support Vector Machine (SVM) ---
svm_model <- svm(Risk_Category ~ Age + Gender + Daily_Hours_Numeric + 
                   Addiction_Score + Brain_Rot_Score + Physical_Health_Score +
                   Sleep_Impact_Score + Productivity_Score,
                 data = train_data,
                 kernel = "radial")

# Predictions
svm_predictions <- predict(svm_model, test_data)
svm_confusion <- confusionMatrix(svm_predictions, test_data$Risk_Category)
print("SVM Results:")
print(svm_confusion)

# --- Comparison of Models ---
model_comparison <- data.frame(
  Model = c("Decision Tree", "Random Forest", "SVM"),
  Accuracy = c(dt_confusion$overall['Accuracy'],
               rf_confusion$overall['Accuracy'],
               svm_confusion$overall['Accuracy']),
  Kappa = c(dt_confusion$overall['Kappa'],
            rf_confusion$overall['Kappa'],
            svm_confusion$overall['Kappa'])
)

print("Model Comparison:")
print(model_comparison)

# Plot model comparison
ggplot(model_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) +
  scale_fill_viridis_d() +
  labs(title = "Model Performance Comparison",
       y = "Accuracy") +
  theme_minimal() +
  ylim(0, 1)

# ============================================================================
# 5. CLUSTERING ANALYSIS (K-MEANS)
# ============================================================================

# Prepare data for clustering
cluster_data <- data %>%
  select(Addiction_Score:Productivity_Score, Daily_Hours_Numeric) %>%
  na.omit() %>%
  scale()

# Determine optimal number of clusters (Elbow Method)
wss <- sapply(1:10, function(k) {
  kmeans(cluster_data, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal K")

# Perform k-means clustering (k=3)
set.seed(123)
kmeans_result <- kmeans(cluster_data, centers = 3, nstart = 25)

# Add cluster assignments to original data
data$Cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters
ggplot(data, aes(x = Daily_Hours_Numeric, y = Overall_Impact_Score, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_viridis_d() +
  labs(title = "K-Means Clustering: Screen Time vs Overall Impact",
       x = "Daily Hours", y = "Overall Impact Score") +
  theme_minimal()

# ============================================================================
# 6. STATISTICAL TESTS
# ============================================================================

# ANOVA: Does screen time significantly affect different impact scores?
anova_addiction <- aov(Addiction_Score ~ Daily_Hours, data = data)
print("ANOVA: Daily Hours vs Addiction Score")
print(summary(anova_addiction))

# Chi-square test: Gender vs Risk Category
chi_test <- chisq.test(table(data$Gender, data$Risk_Category))
print("Chi-Square Test: Gender vs Risk Category")
print(chi_test)

# Correlation test
cor_test <- cor.test(data$Daily_Hours_Numeric, data$Overall_Impact_Score)
print("Correlation Test: Daily Hours vs Overall Impact")
print(cor_test)

# ============================================================================
# 7. SUMMARY STATISTICS
# ============================================================================

# Overall summary
summary_stats <- data %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Mean_Daily_Hours = mean(Daily_Hours_Numeric, na.rm = TRUE),
    Mean_Addiction_Score = mean(Addiction_Score, na.rm = TRUE),
    Mean_Brain_Rot_Score = mean(Brain_Rot_Score, na.rm = TRUE),
    Mean_Physical_Health_Score = mean(Physical_Health_Score, na.rm = TRUE),
    Mean_Sleep_Impact_Score = mean(Sleep_Impact_Score, na.rm = TRUE),
    Mean_Productivity_Score = mean(Productivity_Score, na.rm = TRUE),
    Pct_High_Risk = sum(Risk_Category == "High Risk", na.rm = TRUE) / n() * 100
  )

print("Summary Statistics:")
print(summary_stats)

# ============================================================================
# 8. EXPORT RESULTS
# ============================================================================

# Save processed data
write.csv(data, "processed_screen_time_data.csv", row.names = FALSE)

# Save model comparison
write.csv(model_comparison, "model_comparison_results.csv", row.names = FALSE)

# Save all plots
pdf("screen_time_analysis_plots.pdf", width = 12, height = 8)
grid.arrange(p1, p2, p3, p4, ncol = 2)
grid.arrange(p5, p6, p7, p8, ncol = 2)
grid.arrange(p9, p10, p11, p12, ncol = 2)
dev.off()

print("Analysis complete! All results have been saved.")
print("Check your working directory for:")
print("- processed_screen_time_data.csv")
print("- model_comparison_results.csv")
print("- screen_time_analysis_plots.pdf")