#MODELLING
##RANDOM FOREST

pkgs <- c("randomForest", "caret", "dplyr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)



set.seed(42)

rf_model <- randomForest(
  status ~ .,
  data = train_smote,
  ntree = 500,        # ağaç sayısı (stabil)
  mtry = floor(sqrt(ncol(train_smote) - 1)),  # klasik kural
  importance = TRUE
)

rf_model


varImpPlot(rf_model, n.var = 15, main = "Random Forest - Top 15 Feature Importance")








rf_val_pred <- predict(rf_model, newdata = val_processed)


cm_rf <- caret::confusionMatrix(
  data = rf_val_pred,
  reference = val_processed$status
)

cm_rf



metrics_rf <- data.frame(
  Class  = rownames(cm_rf$byClass),
  Recall = cm_rf$byClass[, "Sensitivity"],
  F1     = cm_rf$byClass[, "F1"]
)

metrics_rf