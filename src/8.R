library(dplyr)
library(knitr)

# LogReg metrik tablosu (cm_logreg varsa)
metrics_logreg <- data.frame(
  Class  = rownames(cm_logreg$byClass),
  Recall = cm_logreg$byClass[, "Sensitivity"],
  F1     = cm_logreg$byClass[, "F1"]
) %>%
  mutate(Model = "Logistic Regression")

# RF metrik tablosu (cm_rf varsa)
metrics_rf <- data.frame(
  Class  = rownames(cm_rf$byClass),
  Recall = cm_rf$byClass[, "Sensitivity"],
  F1     = cm_rf$byClass[, "F1"]
) %>%
  mutate(Model = "Random Forest")

compare_metrics <- bind_rows(metrics_logreg, metrics_rf) %>%
  select(Model, Class, Recall, F1) %>%
  arrange(Class, Model)

knitr::kable(compare_metrics, digits = 4,
             caption = "Validation Performance Comparison: Logistic Regression vs Random Forest")






summary_compare <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Accuracy = c(cm_logreg$overall["Accuracy"], cm_rf$overall["Accuracy"]),
  Kappa    = c(cm_logreg$overall["Kappa"],    cm_rf$overall["Kappa"])
)

knitr::kable(summary_compare, digits = 4,
             caption = "Overall Metrics on Validation Set")






library(ggplot2)

churn_compare <- compare_metrics %>%
  filter(grepl("Churn", Class)) %>%
  tidyr::pivot_longer(cols = c(Recall, F1), names_to = "Metric", values_to = "Value")

ggplot(churn_compare, aes(x = Metric, y = Value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(title = "Churn Performance Comparison on Validation",
       subtitle = "Higher Recall and F1 are better",
       x = NULL, y = "Score") +
  theme_minimal()





knitr::kable(cm_logreg$table, caption = "Confusion Matrix — Logistic Regression")
knitr::kable(cm_rf$table,     caption = "Confusion Matrix — Random Forest")