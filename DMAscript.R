
# ============================================================================
# Student Screen Time, Addiction & Health Effects - Comprehensive Analysis
# Fixed + Enhanced Version
# ============================================================================

# --- 0. SETUP ----------------------------------------------------------------

# Set working directory
setwd("~/Desktop/Code/AI/Data Mining")

# Verify file exists
if (!file.exists("screen_time_data.xlsx")) {
  stop("Error: screen_time_data.xlsx not found in ~/Desktop/Code/AI/Data Mining")
}

# --- 1. INSTALL & LOAD REQUIRED PACKAGES -------------------------------------

required_packages <- c(
  "readxl", "tidyverse", "ggplot2", "plotly", "corrplot", 
  "caret", "randomForest", "e1071", "rpart", "rpart.plot",
  "gridExtra", "reshape2", "scales", "viridis", "GGally"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}


# ============================================================================
# 2. DATA CLEANING & FEATURE ENGINEERING (MATCHED TO YOUR DATA)
# ============================================================================

cat("\nCleaning and transforming data...\n")

# --- 1. Rename columns exactly as per your Excel file headers ---
colnames(data) <- c(
  "Name",
  "Age",
  "Gender",
  "Year_of_Study",
  "Daily_Hours",                 # Avg Hours Per Day
  "Usage_Time",                  # Mostly Used Time
  "Primary_App",                 # Primary Usage
  "Gaming_Type",                 # If Gaming - Type
  "Affects_Studies",
  "Affects_Sleep",
  "Want_Reduce",                 # Want to Reduce Screen Time
  "Q1_Anxious",                  # Anxious if Away (1–5)
  "Q2_Check_Morning",            # Check After Waking (1–5)
  "Q3_Use_While_Working",        # Use Phone When Should Work (1–5)
  "Q4_Difficult_Limit",          # Difficult to Limit (1–5)
  "Q5_Hard_Focus",               # Long Hours Reduce Focus (1–5)
  "Q6_Attention_Reduced",        # Attention Span Reduced (1–5)
  "Q7_Endless_Scroll",           # Endless Scrolling (1–5)
  "Q8_Eye_Strain",               # Eye Strain (1–5)
  "Q9_Neck_Pain",                # Neck/Back Pain (1–5)
  "Q10_Reduced_Physical",        # Reduced Physical Activity (1–5)
  "Q11_Use_Bed",                 # Use Phone in Bed (1–5)
  "Q12_Sleep_Later",             # Sleep Later Because of Phone (1–5)
  "Q13_Distracted",              # Often Distracted (1–5)
  "Open_Response_1",             # Positive/Negative Effects (optional)
  "Open_Response_2",             # Strategies Tried (optional)
  "Open_Response_3"              # Feedback (optional)
)

# --- 2. Convert all Likert-scale (1–5) or Yes/No responses to numeric safely ---
data <- data %>%
  mutate(
    across(matches("^Q[0-9]+_"), ~ as.numeric(as.factor(.)), .names = "{.col}")
  )

# --- 3. Create composite impact scores ---
data <- data %>%
  mutate(
    # 1. Addiction-related behavior
    Addiction_Score = rowMeans(select(., Q1_Anxious:Q4_Difficult_Limit), na.rm = TRUE),

    # 2. Cognitive / Focus-related effects
    Brain_Rot_Score = rowMeans(select(., Q5_Hard_Focus:Q7_Endless_Scroll), na.rm = TRUE),

    # 3. Physical health impact
    Physical_Health_Score = rowMeans(select(., Q8_Eye_Strain:Q10_Reduced_Physical), na.rm = TRUE),

    # 4. Sleep-related impact
    Sleep_Impact_Score = rowMeans(select(., Q11_Use_Bed:Q12_Sleep_Later), na.rm = TRUE),

    # 5. Productivity / distraction level
    Productivity_Score = Q13_Distracted,

    # 6. Overall impact average
    Overall_Impact_Score = rowMeans(
      select(., Addiction_Score, Brain_Rot_Score, Physical_Health_Score,
             Sleep_Impact_Score, Productivity_Score),
      na.rm = TRUE
    ),

    # 7. Convert screen time category to numeric midpoint
    Daily_Hours_Numeric = case_when(
      Daily_Hours == "Less than 2 hours" ~ 1,
      Daily_Hours == "2-4 hours" ~ 2.5,
      Daily_Hours == "4-6 hours" ~ 5,
      Daily_Hours == "6-8 hours" ~ 7,
      Daily_Hours == "More than 8 hours" ~ 9,
      TRUE ~ NA_real_
    ),

    # 8. Risk Category classification
    Risk_Category = case_when(
      Overall_Impact_Score < 2.5 ~ "Low Risk",
      Overall_Impact_Score < 3.5 ~ "Moderate Risk",
      Overall_Impact_Score >= 3.5 ~ "High Risk",
      TRUE ~ "Unknown"
    )
  )

cat("\n✅ Feature engineering complete!\n")
cat("Generated columns:\n")
cat("- Addiction_Score\n- Brain_Rot_Score\n- Physical_Health_Score\n- Sleep_Impact_Score\n- Productivity_Score\n- Overall_Impact_Score\n- Daily_Hours_Numeric\n- Risk_Category\n")


# # ============================================================================
# # 2. DATA CLEANING & FEATURE ENGINEERING (FIXED)
# # ============================================================================

# cat("\nCleaning and transforming data...\n")

# # Check available columns before selecting
# cat("\nAvailable columns:\n")
# print(colnames(data))

# # --- Convert all question columns (Q1–Q17 or any 'Q##_') to numeric safely ---
# # This automatically handles missing or renamed columns like Q17_Missed_Deadline
# data <- data %>%
#   mutate(
#     across(
#       matches("^Q[0-9]+_"), 
#       ~ as.numeric(as.factor(.)),
#       .names = "{.col}"
#     )
#   )

# # --- Create composite impact scores ---
# data <- data %>%
#   mutate(
#     Addiction_Score = rowMeans(select(., matches("^Q[1-4]_")), na.rm = TRUE),
#     Brain_Rot_Score = rowMeans(select(., matches("^Q[5-8]_")), na.rm = TRUE),
#     Physical_Health_Score = rowMeans(select(., matches("^Q(9|10|11)_")), na.rm = TRUE),
#     Sleep_Impact_Score = rowMeans(select(., matches("^Q(12|13|14)_")), na.rm = TRUE),
#     Productivity_Score = rowMeans(select(., matches("^Q(15|16|17)_")), na.rm = TRUE),

#     # Overall average impact
#     Overall_Impact_Score = rowMeans(
#       select(., Addiction_Score, Brain_Rot_Score, Physical_Health_Score,
#              Sleep_Impact_Score, Productivity_Score),
#       na.rm = TRUE
#     ),

#     # Convert categorical hours to numeric midpoint
#     Daily_Hours_Numeric = case_when(
#       Daily_Hours == "Less than 2 hours" ~ 1,
#       Daily_Hours == "2-4 hours" ~ 2.5,
#       Daily_Hours == "4-6 hours" ~ 5,
#       Daily_Hours == "6-8 hours" ~ 7,
#       Daily_Hours == "More than 8 hours" ~ 9,
#       TRUE ~ NA_real_
#     ),

#     # Risk category based on overall impact
#     Risk_Category = case_when(
#       Overall_Impact_Score < 2.5 ~ "Low Risk",
#       Overall_Impact_Score < 3.5 ~ "Moderate Risk",
#       Overall_Impact_Score >= 3.5 ~ "High Risk",
#       TRUE ~ "Unknown"
#     )
#   )

# cat("\nFeature engineering complete.\n")

# # --- 3. FEATURE ENGINEERING ---------------------------------------------------

# cat("\nCreating composite scores...\n")

# data <- data %>%
#   mutate(
#     Addiction_Score = rowMeans(select(., Q1_Anxious:Q4_Difficult_Limit), na.rm = TRUE),
#     Brain_Rot_Score = rowMeans(select(., Q5_Hard_Focus:Q8_Mentally_Drained), na.rm = TRUE),
#     Physical_Health_Score = rowMeans(select(., Q9_Eye_Strain:Q11_Reduced_Physical), na.rm = TRUE),
#     Sleep_Impact_Score = rowMeans(select(., Q12_Use_Before_Sleep:Q14_Sleep_Quality), na.rm = TRUE),
#     Productivity_Score = rowMeans(select(., Q15_Distracted_Study:Q17_Missed_Deadlines), na.rm = TRUE),
#     Overall_Impact_Score = rowMeans(select(., Addiction_Score:Productivity_Score), na.rm = TRUE),
#     Daily_Hours_Numeric = case_when(
#       Daily_Hours == "Less than 2 hours" ~ 1,
#       Daily_Hours == "2-4 hours" ~ 2.5,
#       Daily_Hours == "4-6 hours" ~ 5,
#       Daily_Hours == "6-8 hours" ~ 7,
#       Daily_Hours == "More than 8 hours" ~ 9,
#       TRUE ~ NA_real_
#     ),
#     Risk_Category = case_when(
#       Overall_Impact_Score < 2.5 ~ "Low Risk",
#       Overall_Impact_Score < 3.5 ~ "Moderate Risk",
#       Overall_Impact_Score >= 3.5 ~ "High Risk",
#       TRUE ~ "Unknown"
#     )
#   )

# # --- 4. VISUALIZATION SETTINGS -----------------------------------------------

# theme_set(theme_minimal() +
#             theme(
#               plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
#               plot.subtitle = element_text(hjust = 0.5, size = 10)
#             ))

# # --- 5. EXPLORATORY DATA ANALYSIS --------------------------------------------

# cat("\nGenerating visualizations...\n")

# p1 <- ggplot(data, aes(x = Age)) +
#   geom_histogram(binwidth = 1, fill = "#3498db", color = "white", alpha = 0.8) +
#   labs(title = "Age Distribution of Participants")

# p2 <- ggplot(data, aes(x = Gender, fill = Gender)) +
#   geom_bar(alpha = 0.8) +
#   scale_fill_manual(values = c("#e74c3c", "#3498db", "#95a5a6")) +
#   labs(title = "Gender Distribution")

# p3 <- ggplot(data, aes(x = Daily_Hours, fill = Daily_Hours)) +
#   geom_bar(alpha = 0.8) +
#   coord_flip() +
#   scale_fill_viridis_d() +
#   labs(title = "Daily Screen Time Distribution") +
#   theme(legend.position = "none")

# p4 <- ggplot(data, aes(x = Primary_App)) +
#   geom_bar(fill = "#9b59b6", alpha = 0.8) +
#   coord_flip() +
#   labs(title = "Primary App Usage Categories")

# # --- Composite Scores Distribution ---
# scores_data <- data %>%
#   select(Addiction_Score, Brain_Rot_Score, Physical_Health_Score, 
#          Sleep_Impact_Score, Productivity_Score) %>%
#   pivot_longer(everything(), names_to = "Score_Type", values_to = "Score")

# p6 <- ggplot(scores_data, aes(x = Score_Type, y = Score, fill = Score_Type)) +
#   geom_boxplot(alpha = 0.8) +
#   scale_fill_viridis_d() +
#   coord_flip() +
#   labs(title = "Distribution of Impact Scores",
#        subtitle = "1 = Low Impact, 5 = High Impact") +
#   theme(legend.position = "none")

# # Display plots
# grid.arrange(p1, p2, p3, p4, p6, ncol = 2)

# # --- 6. DATA MINING (CLASSIFICATION) -----------------------------------------

# cat("\nRunning classification algorithms...\n")

# classification_data <- data %>%
#   select(Age, Gender, Year_of_Study, Daily_Hours_Numeric, 
#          Addiction_Score:Productivity_Score, Risk_Category) %>%
#   na.omit() %>%
#   mutate(
#     Gender = as.factor(Gender),
#     Year_of_Study = as.factor(Year_of_Study),
#     Risk_Category = as.factor(Risk_Category)
#   )

# set.seed(123)
# trainIndex <- createDataPartition(classification_data$Risk_Category, p = 0.7, list = FALSE)
# train_data <- classification_data[trainIndex, ]
# test_data <- classification_data[-trainIndex, ]

# # --- Decision Tree
# dt_model <- rpart(Risk_Category ~ ., data = train_data, method = "class")
# rpart.plot(dt_model, main = "Decision Tree for Risk Classification", extra = 104)
# dt_predictions <- predict(dt_model, test_data, type = "class")
# dt_confusion <- confusionMatrix(dt_predictions, test_data$Risk_Category)

# # --- Random Forest
# rf_model <- randomForest(Risk_Category ~ ., data = train_data, ntree = 500, importance = TRUE)
# rf_predictions <- predict(rf_model, test_data)
# rf_confusion <- confusionMatrix(rf_predictions, test_data$Risk_Category)

# # --- SVM
# svm_model <- svm(Risk_Category ~ ., data = train_data, kernel = "radial")
# svm_predictions <- predict(svm_model, test_data)
# svm_confusion <- confusionMatrix(svm_predictions, test_data$Risk_Category)

# # --- Model Comparison
# model_comparison <- data.frame(
#   Model = c("Decision Tree", "Random Forest", "SVM"),
#   Accuracy = c(dt_confusion$overall['Accuracy'],
#                rf_confusion$overall['Accuracy'],
#                svm_confusion$overall['Accuracy'])
# )

# ggplot(model_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
#   geom_bar(stat = "identity", alpha = 0.8) +
#   geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) +
#   scale_fill_viridis_d() +
#   labs(title = "Model Performance Comparison") +
#   theme_minimal() +
#   ylim(0, 1)

# # --- 7. CLUSTERING (K-MEANS) -------------------------------------------------

# cat("\nPerforming K-Means clustering...\n")

# cluster_data <- data %>%
#   select(Addiction_Score:Productivity_Score, Daily_Hours_Numeric) %>%
#   na.omit() %>%
#   scale()

# set.seed(123)
# kmeans_result <- kmeans(cluster_data, centers = 3, nstart = 25)
# data$Cluster <- as.factor(kmeans_result$cluster)

# ggplot(data, aes(x = Daily_Hours_Numeric, y = Overall_Impact_Score, color = Cluster)) +
#   geom_point(size = 3, alpha = 0.7) +
#   scale_color_viridis_d() +
#   labs(title = "K-Means Clustering: Screen Time vs Overall Impact")

# # --- 8. STATISTICAL TESTS -----------------------------------------------------

# cat("\nPerforming statistical tests...\n")

# anova_addiction <- aov(Addiction_Score ~ Daily_Hours, data = data)
# cat("\nANOVA: Daily Hours vs Addiction Score\n")
# print(summary(anova_addiction))

# chi_test <- chisq.test(table(data$Gender, data$Risk_Category))
# cat("\nChi-Square Test: Gender vs Risk Category\n")
# print(chi_test)

# cor_test <- cor.test(data$Daily_Hours_Numeric, data$Overall_Impact_Score)
# cat("\nCorrelation Test: Daily Hours vs Overall Impact\n")
# print(cor_test)

# # --- 9. SUMMARY STATISTICS ----------------------------------------------------

# summary_stats <- data %>%
#   summarise(
#     Mean_Age = mean(Age, na.rm = TRUE),
#     Mean_Daily_Hours = mean(Daily_Hours_Numeric, na.rm = TRUE),
#     Mean_Addiction_Score = mean(Addiction_Score, na.rm = TRUE),
#     Mean_Sleep_Impact_Score = mean(Sleep_Impact_Score, na.rm = TRUE),
#     Pct_High_Risk = sum(Risk_Category == "High Risk", na.rm = TRUE) / n() * 100
#   )

# cat("\nSummary Statistics:\n")
# print(summary_stats)

# # --- 10. EXPORT RESULTS -------------------------------------------------------

# cat("\nExporting results...\n")

# write.csv(data, "processed_screen_time_data.csv", row.names = FALSE)
# write.csv(model_comparison, "model_comparison_results.csv", row.names = FALSE)

# pdf("screen_time_analysis_plots.pdf", width = 12, height = 8)
# grid.arrange(p1, p2, p3, p4, p6, ncol = 2)
# dev.off()

# cat("\n========================================\n")
# cat("Analysis complete! All results have been saved.\n")
# cat("Check your working directory for:\n")
# cat("- processed_screen_time_data.csv\n")
# cat("- model_comparison_results.csv\n")
# cat("- screen_time_analysis_plots.pdf\n")
# cat("========================================\n")
