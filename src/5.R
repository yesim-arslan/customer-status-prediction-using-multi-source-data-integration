# Gerekli paket (tibble varsa rownames düzeni için)
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")

out_dir <- "data_exports"
dir.create(out_dir, showWarnings = FALSE)

save_csv_safe <- function(x, name, out_dir) {
  if (!exists(name, envir = .GlobalEnv)) stop(paste0("Obj yok: ", name))
  df <- get(name, envir = .GlobalEnv)
  
  # tibble / data.frame değilse dene (bazen recipe output tibble olur zaten)
  df <- as.data.frame(df)
  
  # Rowname’ler varsa kolona çevir (CSV’de kaybolmasın)
  if (!is.null(rownames(df)) && any(rownames(df) != seq_len(nrow(df)))) {
    df <- tibble::rownames_to_column(df, var = "row_id")
  }
  
  path <- file.path(out_dir, paste0(name, ".csv"))
  
  # Excel uyumu için UTF-8 + BOM
  write.csv(df, file = path, row.names = FALSE, fileEncoding = "UTF-8")
  message("Saved: ", normalizePath(path))
}

save_csv_safe(train_set, "train_set", out_dir)
save_csv_safe(val_set, "val_set", out_dir)
save_csv_safe(train_smote, "train_smote", out_dir)
save_csv_safe(val_processed, "val_processed", out_dir)