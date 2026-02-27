#train/validation split

library(tidyverse)
library(caret)

set.seed(42)  # reproducibility

# Target
target_var <- "status"

# Create stratified split index
train_index <- createDataPartition(
  train_fe[[target_var]],
  p = 0.8,
  list = FALSE
)

# Split
train_set <- train_fe[train_index, ]
val_set   <- train_fe[-train_index, ]

# Kontrol: boyutlar
dim(train_set)
dim(val_set)

# Kontrol: status dağılımı
prop.table(table(train_fe$status))
prop.table(table(train_set$status))
prop.table(table(val_set$status))