# Customer Status Prediction Using Multi-Source Data Integration

## Overview
This project builds an end-to-end **customer status prediction** pipeline in **R** by integrating multiple data sources, performing feature engineering, handling class imbalance with **SMOTE**, and evaluating classification performance using **ROC/AUC** and confusion matrix.

## Key Contributions
- **Multi-source data integration** to build a unified modeling dataset  
- **Feature engineering** (created **13 new features**) to improve predictive signal  
- **Class imbalance handling** using **SMOTE**  
- Trained and compared classical ML models (e.g., **Logistic Regression**, **Random Forest**)  
- Model evaluation using **ROC/AUC**, confusion matrix, and standard classification metrics  

## Tech Stack
- R / RStudio  
- tidyverse  
- caret  
- themis (SMOTE)  
- randomForest
- logisticRegression

## Repository Structure
- `src/` → R scripts (data prep, feature engineering, modeling, tuning)
- `reports/` → Final report (`.Rmd` / `.html`)
- `plots/` → exported plots and figures
- `tuned_models/` → saved tuned models (if included)
- `data/` → exported / processed datasets (if included)

## How to Run
1. Open the R scripts under `src/` in order (or run the final pipeline script if you have one).
2. Install required packages:
   ```r
   install.packages(c("tidyverse", "caret", "themis"))
3. Generate the report:
   - Open reports/Final_Report.Rmd
   - Knit to HTML/PDF

Results (Summary)
Best model: Random Forest
- ROC/AUC: 0.98
- Notes: SMOTE improved recall for minority class
- You can find detailed analysis and plots in reports/Final_Report.html.

Author
Yesim Arslan

