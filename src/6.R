#MODELLING

##LOGISTIC REGRESSION

# --- 1) Paketler ---
pkgs <- c("readr","dplyr","janitor")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

# --- 2) Dosyaları oku ---
train_smote <- readr::read_csv("train_smote.csv", show_col_types = FALSE) %>% janitor::clean_names()
val_processed <- readr::read_csv("val_processed.csv", show_col_types = FALSE) %>% janitor::clean_names()

# --- 3) Hedef değişken kontrol (status var mı?) ---
if (!("status" %in% names(train_smote))) stop("train_smote içinde 'status' yok. Kolon adlarını kontrol et.")
if (!("status" %in% names(val_processed))) stop("val_processed içinde 'status' yok. Kolon adlarını kontrol et.")

# status faktör olsun (çok sınıflı)
train_smote <- train_smote %>% mutate(status = as.factor(status))
val_processed <- val_processed %>% mutate(status = as.factor(status))

# --- 4) Seviye (level) uyumu: validation'da train’de olmayan class var mı? ---
cat("\nTrain levels:\n"); print(levels(train_smote$status))
cat("\nVal levels:\n");   print(levels(val_processed$status))

extra_levels <- setdiff(levels(val_processed$status), levels(train_smote$status))
if (length(extra_levels) > 0) {
  warning(paste("Validation'da train'de olmayan status level var:", paste(extra_levels, collapse=", ")))
}

# --- 5) Kolon seti aynı mı? (model input için kritik) ---
x_train <- setdiff(names(train_smote), "status")
x_val   <- setdiff(names(val_processed), "status")

missing_in_val <- setdiff(x_train, x_val)
missing_in_tr  <- setdiff(x_val, x_train)

cat("\nModel kolon sayısı | train:", length(x_train), " val:", length(x_val), "\n")
if (length(missing_in_val) > 0) {
  cat("\nVAL'da eksik kolonlar (train'de var):\n"); print(missing_in_val)
}
if (length(missing_in_tr) > 0) {
  cat("\nTRAIN'de eksik kolonlar (val'da var):\n"); print(missing_in_tr)
}

# Eğer kolonlar aynıysa TRUE döner:
cat("\nKolonlar birebir aynı mı? -> ", setequal(x_train, x_val), "\n")

# --- 6) Hızlı dağılım kontrolü ---
cat("\nTrain SMOTE status dağılımı:\n")
print(prop.table(table(train_smote$status)))

cat("\nValidation status dağılımı:\n")
print(prop.table(table(val_processed$status)))




pkgs <- c("nnet","caret","dplyr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)




set.seed(42)

logreg_model <- nnet::multinom(
  status ~ .,
  data = train_smote,
  MaxNWts = 10000,
  trace = FALSE
)





library(dplyr)

# Silinecek olası identifier kolonlar (hangisi varsa siler)
id_cols <- c("customer_id", "row_id", "cus_id", "customerid", "id")

train_smote <- train_smote %>% select(-any_of(id_cols))
val_processed <- val_processed %>% select(-any_of(id_cols))

# status faktör kalsın
train_smote <- train_smote %>% mutate(status = as.factor(status))
val_processed <- val_processed %>% mutate(status = as.factor(status))

# Kolonlar hala birebir aynı mı kontrol
x_train <- setdiff(names(train_smote), "status")
x_val   <- setdiff(names(val_processed), "status")
cat("Kolonlar birebir aynı mı? -> ", setequal(x_train, x_val), "\n")

# customer_id gerçekten gitti mi?
cat("train_smote içinde customer_id var mı? -> ", "customer_id" %in% names(train_smote), "\n")
cat("val_processed içinde customer_id var mı? -> ", "customer_id" %in% names(val_processed), "\n")





library(nnet)
library(caret)

set.seed(42)

logreg_model <- nnet::multinom(
  status ~ .,
  data = train_smote,
  MaxNWts = 10000,
  trace = FALSE
)

val_pred <- predict(logreg_model, newdata = val_processed)

cm_logreg <- caret::confusionMatrix(
  data = val_pred,
  reference = val_processed$status
)

cm_logreg