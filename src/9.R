# ----------------------------
# 0) Paketler
# ----------------------------
pkgs <- c("pROC", "knitr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

val_processed$status <- as.factor(val_processed$status)

# ----------------------------
# 1) Macro ROC fonksiyonu (curve + AUC)
# ----------------------------
macro_roc_curve <- function(truth_factor, prob_matrix, grid_points = 1001) {
  prob_matrix <- as.data.frame(prob_matrix)
  classes <- colnames(prob_matrix)
  if (is.null(classes)) stop("prob_matrix must have class-named columns.")
  if (!is.factor(truth_factor)) truth_factor <- as.factor(truth_factor)
  
  # Ortak FPR grid'i (0..1)
  fpr_grid <- seq(0, 1, length.out = grid_points)
  
  # Her class için one-vs-rest ROC -> (FPR, TPR) noktaları
  tpr_mat <- sapply(classes, function(cls) {
    y_true <- as.integer(truth_factor == cls)
    
    roc_obj <- pROC::roc(
      response = y_true,
      predictor = prob_matrix[[cls]],
      quiet = TRUE,
      direction = "<"
    )
    
    # pROC coords: TPR = sensitivity, FPR = 1 - specificity
    coords_df <- pROC::coords(
      roc_obj,
      x = "all",
      ret = c("specificity", "sensitivity"),
      transpose = FALSE
    )
    
    fpr <- 1 - coords_df$specificity
    tpr <- coords_df$sensitivity
    
    # Sıralama + benzersizleştirme (interp için)
    ord <- order(fpr, tpr)
    fpr <- fpr[ord]; tpr <- tpr[ord]
    
    # Aynı FPR tekrarlarını temizle
    keep <- !duplicated(fpr)
    fpr <- fpr[keep]; tpr <- tpr[keep]
    
    # Sınır noktalarını garanti et (0,0) ve (1,1)
    if (fpr[1] > 0) { fpr <- c(0, fpr); tpr <- c(0, tpr) }
    if (fpr[length(fpr)] < 1) { fpr <- c(fpr, 1); tpr <- c(tpr, 1) }
    
    # Grid üzerinde lineer interpolasyon
    approx(x = fpr, y = tpr, xout = fpr_grid, ties = "ordered")$y
  })
  
  # Macro TPR: sınıf ortalaması
  tpr_macro <- rowMeans(tpr_mat)
  
  # Trapezoid ile macro AUC (FPR ekseninde)
  auc_macro <- sum(diff(fpr_grid) * (head(tpr_macro, -1) + tail(tpr_macro, -1)) / 2)
  
  list(
    fpr = fpr_grid,
    tpr = tpr_macro,
    auc_macro = auc_macro
  )
}

# ----------------------------
# 2) LogReg probs -> Macro ROC
# ----------------------------
prob_logreg_vp <- predict(logreg_model, newdata = val_processed, type = "probs") |> as.data.frame()
macro_logreg <- macro_roc_curve(val_processed$status, prob_logreg_vp)

# ----------------------------
# 3) RF probs -> Macro ROC
# ----------------------------
prob_rf_vp <- predict(rf_model, newdata = val_processed, type = "prob") |> as.data.frame()
macro_rf <- macro_roc_curve(val_processed$status, prob_rf_vp)

# ----------------------------
# 4) Macro ROC grafiği (2 model, renkli)
# ----------------------------
plot(macro_logreg$fpr, macro_logreg$tpr, type = "l",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     main = "Macro-average ROC (Validation: val_processed)",
     lwd = 3, col = "#1f77b4")

lines(macro_rf$fpr, macro_rf$tpr, lwd = 3, col = "#d62728")

abline(a = 0, b = 1, lty = 2, col = "gray60")

legend("bottomright",
       legend = c(
         paste0("LogReg macro AUC = ", round(macro_logreg$auc_macro, 3)),
         paste0("RF macro AUC = ", round(macro_rf$auc_macro, 3))
       ),
       col = c("#1f77b4", "#d62728"),
       lwd = 3, bty = "n", cex = 0.9)

# ----------------------------
# 5) Sunumluk tablo (macro AUC)
# ----------------------------
auc_macro_tbl <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  AUC_Macro = c(macro_logreg$auc_macro, macro_rf$auc_macro)
)

knitr::kable(auc_macro_tbl, digits = 4,
             caption = "Macro-averaged ROC-AUC (Validation: val_processed)")