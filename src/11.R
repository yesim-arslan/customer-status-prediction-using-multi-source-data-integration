##confusion matrix + recall/f1

# Paketler
pkgs <- c("ggplot2","dplyr","tidyr","patchwork")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

# ----------------------------
# 1) Confusion matrix data
# ----------------------------
cm_df <- as.data.frame(cm_rf$table)
colnames(cm_df) <- c("Predicted", "Actual", "Count")

p_cm <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = Count), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  labs(title = "Confusion Matrix (RF)", x = "Actual", y = "Predicted") +
  theme_minimal(base_size = 14)

# ----------------------------
# 2) Recall & F1 data
# ----------------------------
metrics_rf <- data.frame(
  Class  = rownames(cm_rf$byClass),
  Recall = cm_rf$byClass[, "Sensitivity"],
  F1     = cm_rf$byClass[, "F1"]
) %>%
  mutate(Class = gsub("^Class: ", "", Class)) %>%
  pivot_longer(cols = c(Recall, F1), names_to = "Metric", values_to = "Score")

p_metrics <- ggplot(metrics_rf, aes(x = Class, y = Score, fill = Metric)) +
  geom_col(position = "dodge") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Class Metrics (RF)", x = NULL, y = "Score") +
  theme_minimal(base_size = 14)

# ----------------------------
# 3) Tek figürde birleştir
# ----------------------------
(p_cm | p_metrics) +
  plot_annotation(title = "Random Forest Evaluation (Validation Set)")