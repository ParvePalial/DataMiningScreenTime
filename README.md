# 📱 Student Screen Time, Addiction & Health Effects  
### _A Comprehensive Data Mining Analysis using R_

---

## 🧩 Project Overview

This project explores the relationship between **student screen time**, **mobile phone addiction**, and **mental & physical health outcomes** using **data mining and statistical modeling** techniques in R.  

The analysis applies **data preprocessing**, **feature engineering**, **exploratory data analysis (EDA)**, **machine learning models**, and **statistical tests** to draw insights from survey-based data.

---

## 🎯 Objectives

- To examine the correlation between daily screen time and students’ psychological & physical well-being.  
- To identify high-risk behavioral patterns using classification algorithms.  
- To cluster students into risk segments based on overall digital impact.  
- To visualize findings and produce a research-ready analytical report.

---

## 🧠 Methodology

### **1. Data Collection**
Survey-based dataset `screen_time_data.xlsx` containing student responses:
- Demographics (Age, Gender, Year of Study)
- Screen time habits
- Usage type (Gaming, Social Media, YouTube, Messaging, etc.)
- 17 Likert-scale questions (Q1–Q17) covering:
  - **Addiction indicators**
  - **Cognitive / focus issues**
  - **Physical strain**
  - **Sleep impact**
  - **Productivity loss**

### **2. Data Preprocessing**
- Cleaned inconsistent column names.
- Converted all categorical survey responses (Yes/No/Likert) to numeric.
- Created composite indicators:
  - `Addiction_Score`
  - `Brain_Rot_Score`
  - `Physical_Health_Score`
  - `Sleep_Impact_Score`
  - `Productivity_Score`
  - `Overall_Impact_Score`

### **3. Feature Engineering**
Derived new features:
- `Daily_Hours_Numeric`: Numeric midpoint for each screen time category.
- `Risk_Category`: Classified as **Low**, **Moderate**, or **High Risk** based on the overall impact score.

---

## 📊 Exploratory Data Analysis (EDA)

### Visualizations:
- Age and Gender distribution histograms.  
- Screen time and app usage frequency charts.  
- Boxplots of composite health and addiction scores.  
- Correlation heatmap between digital impact metrics.  
- Sleep and study performance impact visualizations.  

All plots are exported to:
screen_time_analysis_plots.pdf

yaml
Copy code

---

## 🤖 Machine Learning Models

Three classification models predict **Risk Category**:

| Model | Description | Library |
|--------|--------------|----------|
| Decision Tree | Interpretable model using recursive splits | `rpart`, `rpart.plot` |
| Random Forest | Ensemble model for high accuracy | `randomForest` |
| Support Vector Machine (SVM) | Non-linear boundary classifier | `e1071` |

### Performance Comparison
Metrics (Accuracy, Kappa) are compared visually using `ggplot2`.

Results saved to:
model_comparison_results.csv

yaml
Copy code

---

## 🔍 Clustering Analysis

**K-Means Clustering** groups students by their screen time and impact scores.  
- Uses scaled numeric features.  
- Optimal cluster count found via **Elbow Method**.  
- Results visualized as clusters of risk profiles.

---

## 📈 Statistical Tests

| Test | Purpose | Method |
|------|----------|--------|
| **ANOVA** | Tests if mean addiction scores differ by daily screen time | `aov()` |
| **Chi-square** | Tests independence between gender and risk category | `chisq.test()` |
| **Correlation Test** | Quantifies relationship between screen time and impact score | `cor.test()` |

---

## 📦 Output Files

| File | Description |
|------|--------------|
| `processed_screen_time_data.csv` | Cleaned dataset with engineered features |
| `model_comparison_results.csv` | Accuracy and Kappa for all ML models |
| `screen_time_analysis_plots.pdf` | All generated visualizations |

---

## 🧰 Dependencies

Install the following R packages before running the script:

```r
install.packages(c(
  "readxl", "tidyverse", "ggplot2", "plotly", "corrplot", 
  "caret", "randomForest", "e1071", "rpart", "rpart.plot",
  "gridExtra", "reshape2", "scales", "viridis", "GGally"
))
```

---

## 🚀 How to Run

Option 1 — RStudio
Open screen_time_analysis.R in RStudio.

Set working directory:

```r
setwd("~/Desktop/Code/AI/Data Mining")
```
Click Source to run the entire script.

Option 2 — Terminal

``` bash
Rscript screen_time_analysis.R
```
✅ Output files will appear in the same folder.

--- 

## 🧮 Key Insights
Higher screen time strongly correlates with increased addiction and sleep disruption.

Students averaging >6 hours/day show significantly higher cognitive fatigue and productivity loss.

The Random Forest model achieved the highest classification accuracy.

K-Means clustering reveals 3 clear behavioral segments:

Cluster 1 – Low Risk

Cluster 2 – Moderate Habitual Users

Cluster 3 – High-Risk Overusers

--- 

## 🧾 References
Wickham, H. (2019). R for Data Science: Import, Tidy, Transform, Visualize, and Model Data.

R Documentation: tidyverse, caret, ggplot2

Sleep Foundation & WHO reports on digital screen exposure and health.

---

## 🧑‍💻 Author
Project by: Parve Palial , Yuvraj Singh
Institution: Dr BR Ambedkar National Institue of Technology Jalandhar
Date: 10/11/25
Contact: parve.palial@gmail.com