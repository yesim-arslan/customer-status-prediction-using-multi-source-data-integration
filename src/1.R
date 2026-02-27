pkgs <- c("tidyverse", "janitor", "stringr", "readr")
to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(pkgs, library, character.only = TRUE))

####

sat_tr <- read_csv("customer_satisfaction_scores_train.csv")
demo_tr <- read_csv("customer_demographics_train.csv")
mrr_tr <- read_csv("customer_monthly_recurring_revenue_train.csv")
rev_tr <- read_csv("customer_revenue_history_train.csv")
ticket_tr <- read_csv("support_ticket_activity_train.csv")
news_tr <- read_csv("newsletter_engagement_train.csv")
bug_tr <- read_csv("product_bug_reports_train.csv")
reg_tr <- read_csv("customer_region_and_industry_train.csv")
stat_tr <- read_csv("customer_status_level_train.csv")

sat_te <- read_csv("customer_satisfaction_scores_test.csv")
demo_te <- read_csv("customer_demographics_test.csv")
mrr_te <- read_csv("customer_monthly_recurring_revenue_test.csv")
rev_te <- read_csv("customer_revenue_history_test.csv")
ticket_te <- read_csv("support_ticket_activity_test.csv")
news_te <- read_csv("newsletter_engagement_test.csv")
bug_te <- read_csv("product_bug_reports_test.csv")
reg_te <- read_csv("customer_region_and_industry_test.csv")
stat_te <- read_csv("customer_status_level_test.csv")


glimpse(sat_tr)
glimpse(stat_tr)

#######

train_dfs <- list(
  sat_tr = sat_tr,
  demo_tr = demo_tr,
  ticket_tr = ticket_tr,
  news_tr = news_tr,
  bug_tr = bug_tr,
  reg_tr = reg_tr,
  mrr_tr = mrr_tr,
  rev_tr = rev_tr,
  stat_tr = stat_tr
)

test_dfs <- list(
  sat_te = sat_te,
  demo_te = demo_te,
  ticket_te = ticket_te,
  news_te = news_te,
  bug_te = bug_te,
  reg_te = reg_te,
  mrr_te = mrr_te,
  rev_te = rev_te,
  stat_te = stat_te
)

standardize_customer_id <- function(df) {
  # First, clean all column names
  df <- df %>% janitor::clean_names()

  # Then rename CUS ID to customer_id if needed
  if ("cus_id" %in% names(df)) {
    df <- df %>% rename(customer_id = cus_id)
  }

  return(df)
}

for (name in names(train_dfs)) {
  train_dfs[[name]] <- standardize_customer_id(train_dfs[[name]])
}

for (name in names(test_dfs)) {
  test_dfs[[name]] <- standardize_customer_id(test_dfs[[name]])
}

list2env(train_dfs, envir = .GlobalEnv)
list2env(test_dfs, envir = .GlobalEnv)

# Clean MRR and Revenue columns (remove $ and convert to numeric)
clean_numeric_column <- function(x) {
  if (is.character(x)) {
    x <- gsub("[$,]", "", x) # Remove $ and commas
    x <- as.numeric(x)
  }
  return(x)
}

# Apply to MRR datasets
if ("mrr" %in% names(mrr_tr)) {
  mrr_tr$mrr_num <- clean_numeric_column(mrr_tr$mrr)
  mrr_tr <- mrr_tr %>% select(customer_id, mrr_num)
}

if ("mrr" %in% names(mrr_te)) {
  mrr_te$mrr_num <- clean_numeric_column(mrr_te$mrr)
  mrr_te <- mrr_te %>% select(customer_id, mrr_num)
}

# Apply to Revenue datasets
if ("total_revenue" %in% names(rev_tr)) {
  rev_tr$total_revenue_num <- clean_numeric_column(rev_tr$total_revenue)
  rev_tr <- rev_tr %>% select(customer_id, total_revenue_num)
}

if ("total_revenue" %in% names(rev_te)) {
  rev_te$total_revenue_num <- clean_numeric_column(rev_te$total_revenue)
  rev_te <- rev_te %>% select(customer_id, total_revenue_num)
}

names(sat_tr)
names(demo_tr)

#######

prep_satisfaction <- function(df) {
  df2 <- df %>%
    janitor::clean_names() # kolon adlarını güvenli şekilde temizler

  # Beklenen kolonlar (clean_names sonrası)
  needed <- c(
    "customer_id", "year", "quarter",
    "how_likely_are_you_to_recommend_insider_to_a_friend_or_colleague",
    "how_would_you_rate_the_value_you_gain_from_our_company",
    "please_rate_the_overall_quality_of_our_products",
    "please_rate_the_usability_of_the_panel"
  )

  missing <- setdiff(needed, names(df2))
  if (length(missing) > 0) {
    message("Eksik görünen kolon(lar): ", paste(missing, collapse = ", "))
    message(
      "Mevcut kolonlar içinden 'recommend' geçenler: ",
      paste(grep("recommend", names(df2), value = TRUE), collapse = " | ")
    )
    stop("Satisfaction kolon isimleri beklediğimiz formatta değil. Yukarıdaki çıktıyı bana at.")
  }

  df2 %>%
    select(all_of(needed)) %>%
    mutate(across(-customer_id, as.numeric)) %>%
    group_by(customer_id) %>%
    summarise(
      year = mean(year, na.rm = TRUE),
      quarter = mean(quarter, na.rm = TRUE),
      how_likely_are_you_to_recommend_insider_to_a_friend_or_colleague =
        mean(how_likely_are_you_to_recommend_insider_to_a_friend_or_colleague, na.rm = TRUE),
      how_would_you_rate_the_value_you_gain_from_our_company =
        mean(how_would_you_rate_the_value_you_gain_from_our_company, na.rm = TRUE),
      please_rate_the_overall_quality_of_our_products =
        mean(please_rate_the_overall_quality_of_our_products, na.rm = TRUE),
      please_rate_the_usability_of_the_panel =
        mean(please_rate_the_usability_of_the_panel, na.rm = TRUE),
      .groups = "drop"
    )
}

# yeniden üret
sat_tr_clean <- prep_satisfaction(sat_tr)
sat_te_clean <- prep_satisfaction(sat_te)

# hızlı kontrol
glimpse(sat_tr_clean)

###########

# 1) Train final'i üret (birleştirme sırası önemli değil ama stabil olsun diye sabit tutuyoruz)
final_train_rebuilt <- sat_tr_clean %>%
  left_join(demo_tr %>% select(customer_id, customer_age_months), by = "customer_id") %>%
  left_join(mrr_tr, by = "customer_id") %>%
  left_join(rev_tr, by = "customer_id") %>%
  left_join(ticket_tr %>% select(customer_id, help_ticket_count, help_ticket_lead_time_hours), by = "customer_id") %>%
  left_join(news_tr %>% select(customer_id, company_newsletter_interaction_count), by = "customer_id") %>%
  left_join(bug_tr %>% select(customer_id, product_bug_task_count), by = "customer_id") %>%
  left_join(reg_tr %>% select(customer_id, region, vertical, subvertical), by = "customer_id") %>%
  left_join(stat_tr %>% select(customer_id, status, customer_level), by = "customer_id")

# 2) Hızlı kalite kontrolleri
cat("Rebuilt train rows:", nrow(final_train_rebuilt), "\n")
cat("Rebuilt train cols:", ncol(final_train_rebuilt), "\n")
cat("NA count (total):", sum(is.na(final_train_rebuilt)), "\n")

# Eğer customer_id bazında tekrar var mı?
cat("Duplicate customer_id:", sum(duplicated(final_train_rebuilt$customer_id)), "\n")

# Target dağılımı
print(table(final_train_rebuilt$status, useNA = "ifany"))


###########

write_csv(
  final_train_rebuilt,
  "finalTrainDataset.csv"
)

##########
final_test_rebuilt <- sat_te_clean %>%
  left_join(demo_te %>% select(customer_id, customer_age_months), by = "customer_id") %>%
  left_join(mrr_te, by = "customer_id") %>%
  left_join(rev_te, by = "customer_id") %>%
  left_join(ticket_te %>% select(customer_id, help_ticket_count, help_ticket_lead_time_hours), by = "customer_id") %>%
  left_join(news_te %>% select(customer_id, company_newsletter_interaction_count), by = "customer_id") %>%
  left_join(bug_te %>% select(customer_id, product_bug_task_count), by = "customer_id") %>%
  left_join(reg_te %>% select(customer_id, region, vertical, subvertical), by = "customer_id") %>%
  left_join(stat_te %>% select(customer_id, status, customer_level), by = "customer_id")

# Hızlı kontroller
cat("Final TEST rows:", nrow(final_test_rebuilt), "\n")
cat("Final TEST cols:", ncol(final_test_rebuilt), "\n")
cat("NA count (total):", sum(is.na(final_test_rebuilt)), "\n")
cat("Duplicate customer_id:", sum(duplicated(final_test_rebuilt$customer_id)), "\n")
##########
write_csv(
  final_test_rebuilt,
  "finalTestDataset.csv"
)
###########
round_numeric_cols <- function(df, digits = 2) {
  df %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(., digits)
      )
    )
}

# Train ve Test'e uygula
final_train_rebuilt <- round_numeric_cols(final_train_rebuilt, digits = 2)
final_test_rebuilt <- round_numeric_cols(final_test_rebuilt, digits = 2)

# Kontrol: örnek birkaç numeric kolon
summary(final_train_rebuilt)
summary(final_test_rebuilt)

write_csv(
  final_train_rebuilt,
  "finalTrainDataset2.csv"
)
write_csv(
  final_test_rebuilt,
  "finalTestDataset2.csv"
)

#############

missing_audit <- function(df, df_name = "data") {
  cat("\n============================\n")
  cat("MISSING VALUE AUDIT:", df_name, "\n")
  cat("============================\n")

  total_na <- sum(is.na(df))
  cat("Total NA (all cells):", total_na, "\n")

  na_by_col <- sapply(df, function(x) sum(is.na(x)))
  na_by_col <- sort(na_by_col, decreasing = TRUE)

  # Sadece NA olan kolonları yazdır (yoksa da belirt)
  cols_with_na <- na_by_col[na_by_col > 0]

  if (length(cols_with_na) == 0) {
    cat("✅ No missing values found in any column.\n")
  } else {
    cat("❌ Columns with missing values:\n")
    print(cols_with_na)

    # NA oranı da göster
    na_rate <- round(100 * cols_with_na / nrow(df), 2)
    cat("\nNA rate (%) for those columns:\n")
    print(na_rate)
  }

  invisible(list(total_na = total_na, na_by_col = na_by_col))
}

# Kontrol edilecek datasetler (yuvarlama yaptığın final'lar)
audit_train <- missing_audit(final_train_rebuilt, "final_train_rebuilt")
audit_test <- missing_audit(final_test_rebuilt, "final_test_rebuilt")

########
# missing value handling

final_test_rebuilt <- final_test_rebuilt %>%
  select(-status)

final_train_rebuilt <- final_train_rebuilt %>%
  mutate(
    vertical = ifelse(is.na(vertical), "Unknown", vertical),
    subvertical = ifelse(is.na(subvertical), "Unknown", subvertical)
  )

final_test_rebuilt <- final_test_rebuilt %>%
  mutate(
    vertical = ifelse(is.na(vertical), "Unknown", vertical),
    subvertical = ifelse(is.na(subvertical), "Unknown", subvertical)
  )

final_train_rebuilt <- final_train_rebuilt %>%
  mutate(
    company_newsletter_interaction_count =
      ifelse(is.na(company_newsletter_interaction_count), 0,
        company_newsletter_interaction_count
      )
  )

final_test_rebuilt <- final_test_rebuilt %>%
  mutate(
    company_newsletter_interaction_count =
      ifelse(is.na(company_newsletter_interaction_count), 0,
        company_newsletter_interaction_count
      )
  )

#--
impute_group_median <- function(df, target_col, group_col) {
  # Grup bazlı median hesapla
  medians <- df %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      med = median(.data[[target_col]], na.rm = TRUE),
      .groups = "drop"
    )

  global_median <- median(df[[target_col]], na.rm = TRUE)

  df %>%
    left_join(medians, by = setNames(group_col, group_col)) %>%
    mutate(
      "{target_col}" := ifelse(
        is.na(.data[[target_col]]),
        ifelse(is.na(med), global_median, med),
        .data[[target_col]]
      )
    ) %>%
    select(-med)
}

num_cols_group <- c(
  "total_revenue_num",
  "mrr_num",
  "help_ticket_lead_time_hours",
  "help_ticket_count"
)

for (col in num_cols_group) {
  final_train_rebuilt <- impute_group_median(
    final_train_rebuilt,
    target_col = col,
    group_col = "customer_level"
  )
}

for (col in num_cols_group) {
  final_test_rebuilt <- impute_group_median(
    final_test_rebuilt,
    target_col = col,
    group_col = "customer_level"
  )
}


missing_audit(final_train_rebuilt, "final_train_after_imputation")
missing_audit(final_test_rebuilt, "final_test_after_imputation")


median_usability_train <- median(
  final_train_rebuilt$please_rate_the_usability_of_the_panel,
  na.rm = TRUE
)

final_train_rebuilt <- final_train_rebuilt %>%
  mutate(
    please_rate_the_usability_of_the_panel =
      ifelse(
        is.na(please_rate_the_usability_of_the_panel),
        median_usability_train,
        please_rate_the_usability_of_the_panel
      )
  )


final_test_rebuilt <- final_test_rebuilt %>%
  mutate(
    please_rate_the_usability_of_the_panel =
      ifelse(
        is.na(please_rate_the_usability_of_the_panel),
        median_usability_train,
        please_rate_the_usability_of_the_panel
      )
  )


missing_audit(final_train_rebuilt, "final_train_AFTER_ALL_IMPUTATION")
missing_audit(final_test_rebuilt, "final_test_AFTER_ALL_IMPUTATION")


write_csv(
  final_train_rebuilt,
  "finalTrainDataset3.csv"
)
write_csv(
  final_test_rebuilt,
  "finalTestDataset3.csv"
)

########
# EDA

library(tidyverse)
library(janitor)
library(skimr)
library(scales)
library(GGally)

train <- read_csv("finalTrainDataset3.csv") |> clean_names()
test <- read_csv("finalTestDataset3.csv") |> clean_names()

# Quick checks
dim(train)
dim(test)
glimpse(train)
skim(train)


### status distibution(percent) table
train |>
  count(status) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = reorder(status, -n), y = n)) +
  geom_col() +
  labs(title = "Status Distribution (Count)", x = "Status", y = "Count")

train |>
  count(status) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = reorder(status, -pct), y = pct)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  labs(title = "Status Distribution (Percent)", x = "Status", y = "Percent")


### revenue features (log1p)
num_cols <- train |>
  select(where(is.numeric)) |>
  names()

train |>
  pivot_longer(all_of(num_cols), names_to = "feature", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~feature, scales = "free", ncol = 3) +
  labs(title = "Numeric Feature Distributions")

# ##Revenue-like features için log1p
train |>
  pivot_longer(c(mrr_num, total_revenue_num), names_to = "feature", values_to = "value") |>
  ggplot(aes(x = log1p(value))) +
  geom_histogram(bins = 30) +
  facet_wrap(~feature, scales = "free") +
  labs(title = "Revenue Features (log1p)")


### lead time by status
train |>
  ggplot(aes(x = status, y = mrr_num)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_continuous(labels = comma) +
  labs(title = "MRR by Status", x = "Status", y = "MRR")

train |>
  ggplot(aes(x = status, y = help_ticket_lead_time_hours)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(title = "Lead Time by Status", x = "Status", y = "Hours")


### costumer level composition by status
train |>
  count(status, customer_level) |>
  group_by(status) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = status, y = pct, fill = customer_level)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  labs(title = "Customer Level Composition by Status", x = "Status", y = "Percent")


### Correlation Heatmap of numeric features
library(tidyverse)
library(janitor)

# Dataset'i oku
train <- read_csv("finalTrainDataset3.csv") |>
  clean_names()

# Sadece numeric kolonları seç
num_data <- train |>
  select(where(is.numeric))

# Zero variance sütunları kaldır (korelasyon hesaplanamaz)
# year gibi tüm değerleri aynı olan sütunlar
zero_var_cols <- sapply(num_data, function(x) sd(x, na.rm = TRUE) == 0 | is.na(sd(x, na.rm = TRUE)))
if (any(zero_var_cols)) {
  cat(
    "Removing zero variance columns for correlation:",
    paste(names(num_data)[zero_var_cols], collapse = ", "), "\n"
  )
  num_data <- num_data[, !zero_var_cols]
}

# Korelasyon matrisi
cor_mat <- cor(
  num_data,
  use = "pairwise.complete.obs"
)

# Long format (ggplot için)
cor_df <- as.data.frame(as.table(cor_mat)) |>
  rename(
    var_x = Var1,
    var_y = Var2,
    corr  = Freq
  )

# Heatmap
ggplot(cor_df, aes(x = var_x, y = var_y, fill = corr)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#4575b4",
    mid = "white",
    high = "#d73027",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(hjust = 1), # Simplified - no margin
    axis.title = element_blank()
  ) +
  labs(
    title = "Correlation Heatmap of Numeric Features",
    fill  = "Correlation"
  )

################
