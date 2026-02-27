##Confusion Matrix

library(ggplot2)
library(dplyr)

# Confusion matrix tablosunu data.frame'e çevir
cm_df <- as.data.frame(cm_rf$table)

colnames(cm_df) <- c("Prediction", "Reference", "Count")

# Heatmap
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  labs(
    title = "Confusion Matrix – Random Forest (Validation Set)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme_minimal(base_size = 14)


library(ggplot2)
library(dplyr)

# Confusion matrix tablosunu data.frame'e çevir
cm_df <- as.data.frame(cm_rf$table)

colnames(cm_df) <- c("Prediction", "Reference", "Count")

# Heatmap
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  labs(
    title = "Confusion Matrix – Random Forest (Validation Set)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme_minimal(base_size = 14)





cm_df_lr <- as.data.frame(cm_logreg$table)
colnames(cm_df_lr) <- c("Prediction", "Reference", "Count")

ggplot(cm_df_lr, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#fff5f0", high = "#67000d") +
  labs(
    title = "Confusion Matrix – Logistic Regression (Validation Set)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme_minimal(base_size = 14)





library(ggplot2)
library(dplyr)

cm_df <- as.data.frame(cm_rf$table)
colnames(cm_df) <- c("Predicted", "Actual", "Count")

ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = Count), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  labs(
    title = "Confusion Matrix – Random Forest (Validation Set)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme_minimal(base_size = 14)



cm_df_lr <- as.data.frame(cm_logreg$table)
colnames(cm_df_lr) <- c("Predicted", "Actual", "Count")

ggplot(cm_df_lr, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = Count), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#fff5f0", high = "#67000d") +
  labs(
    title = "Confusion Matrix – Logistic Regression (Validation Set)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme_minimal(base_size = 14)