# SMOTE


library(recipes)
library(themis)
library(dplyr)

set.seed(42)

rec <- recipe(status ~ ., data = train_set) %>%
  # 1) ID'yi çıkar (model için anlamsız)
  update_role(customer_id, new_role = "id") %>%
  # 2) Yeni categorical levels'ı handle et (validation'da yeni değerler olabilir)
  step_novel(all_nominal_predictors(), new_level = "unseen") %>%
  # 3) Kategorikleri dummy'ye çevir
  step_dummy(all_nominal_predictors()) %>%
  # 4) Zero variance sütunları kaldır (dummy'den sonra!)
  #    (örn: year, region_unseen, vertical_unseen - hiç kullanılmamış dummy'ler)
  step_zv(all_predictors()) %>%
  # 5) Numeric'leri ölçekle
  step_normalize(all_numeric_predictors()) %>%
  # 6) SMOTE'yi EN SON uygula (artık her şey numeric)
  step_smote(status)

# Train tarafını SMOTE ile üret
train_smote <- prep(rec) %>%
  bake(new_data = NULL)

# Validation set'i ASLA SMOTE etme.
# Ama aynı dönüşümleri uygulamalısın (dummy + normalize) -> bake
val_processed <- prep(rec, retain = TRUE) %>%
  bake(new_data = val_set)

# Kontroller
prop.table(table(train_set$status))
prop.table(table(train_smote$status))





library(tidyverse)

smote_dist <- train_smote %>%
  count(status) %>%
  mutate(percent = n / sum(n))


ggplot(smote_dist, aes(x = status, y = percent, fill = status)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Class Distribution After SMOTE (Training Set)",
    x = "Customer Status",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
