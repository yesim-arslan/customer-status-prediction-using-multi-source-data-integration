#Feature Engineering

library(tidyverse)
library(janitor)

# 0) Load
train <- read_csv("finalTrainDataset3.csv") |> clean_names()
test  <- read_csv("finalTestDataset3.csv")  |> clean_names()

# 1) Revenue & financial features
add_fin_features <- function(df) {
  df |> 
    mutate(
      # log transforms (handles skewness, log1p works with 0 safely)
      log_mrr = log1p(mrr_num),
      log_total_revenue = log1p(total_revenue_num),
      
      # revenue per ticket (avoid division by zero)
      revenue_per_ticket = total_revenue_num / (help_ticket_count + 1)
    )
}

train_fe <- add_fin_features(train)
test_fe  <- add_fin_features(test)

# Quick sanity check
train_fe |> select(mrr_num, log_mrr, total_revenue_num, log_total_revenue, help_ticket_count, revenue_per_ticket) |> head()



# 2) Customer maturity & intensity features
add_maturity_features <- function(df) {
  df |>
    mutate(
      # normalize support activity by customer lifetime
      tickets_per_month = help_ticket_count / customer_age_months,
      
      # normalize revenue by customer lifetime
      mrr_per_month = total_revenue_num / customer_age_months
    )
}

train_fe <- add_maturity_features(train_fe)
test_fe  <- add_maturity_features(test_fe)

# Sanity check
train_fe |>
  select(
    customer_age_months,
    help_ticket_count,
    tickets_per_month,
    total_revenue_num,
    mrr_per_month
  ) |>
  head()

#3.Satisfaction Aggregation
library(dplyr)

add_satisfaction_features <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      avg_satisfaction = mean(
        c(
          how_likely_are_you_to_recommend_insider_to_a_friend_or_colleague,
          how_would_you_rate_the_value_you_gain_from_our_company,
          please_rate_the_overall_quality_of_our_products,
          please_rate_the_usability_of_the_panel
        ),
        na.rm = TRUE
      ),
      satisfaction_sd = sd(
        c(
          how_likely_are_you_to_recommend_insider_to_a_friend_or_colleague,
          how_would_you_rate_the_value_you_gain_from_our_company,
          please_rate_the_overall_quality_of_our_products,
          please_rate_the_usability_of_the_panel
        ),
        na.rm = TRUE
      )
    ) %>%
    ungroup()
}

# Apply
train_fe <- add_satisfaction_features(train_fe)
test_fe  <- add_satisfaction_features(test_fe)

# Check
train_fe %>%
  select(
    how_likely_are_you_to_recommend_insider_to_a_friend_or_colleague,
    how_would_you_rate_the_value_you_gain_from_our_company,
    please_rate_the_overall_quality_of_our_products,
    please_rate_the_usability_of_the_panel,
    avg_satisfaction,
    satisfaction_sd
  ) %>%
  head(10)



# 4) Support quality features
add_support_features <- function(df) {
  
  ticket_median <- median(df$help_ticket_count, na.rm = TRUE)
  
  df %>%
    mutate(
      # average resolution time per ticket
      lead_time_per_ticket = help_ticket_lead_time_hours / (help_ticket_count + 1),
      
      # high support burden flag
      high_support = if_else(
        help_ticket_count > ticket_median,
        1L,
        0L
      )
    )
}

# Apply
train_fe <- add_support_features(train_fe)
test_fe  <- add_support_features(test_fe)

# Check
train_fe %>%
  select(
    help_ticket_count,
    help_ticket_lead_time_hours,
    lead_time_per_ticket,
    high_support
  ) %>%
  head(10)



# 5) Engagement & product health features
add_engagement_features <- function(df) {
  df %>%
    mutate(
      # newsletter engagement flag
      engaged_newsletter = if_else(
        company_newsletter_interaction_count > 0,
        1L,
        0L
      ),
      
      # bug density normalized by customer lifetime
      bugs_per_month = product_bug_task_count / customer_age_months
    )
}

# Apply
train_fe <- add_engagement_features(train_fe)
test_fe  <- add_engagement_features(test_fe)

# Check
train_fe %>%
  select(
    company_newsletter_interaction_count,
    engaged_newsletter,
    product_bug_task_count,
    customer_age_months,
    bugs_per_month
  ) %>%
  head(10)



# 6) Risk & segmentation flags
add_risk_flags <- function(df) {
  
  mrr_median        <- median(df$mrr_num, na.rm = TRUE)
  ticket_median     <- median(df$help_ticket_count, na.rm = TRUE)
  age_median        <- median(df$customer_age_months, na.rm = TRUE)
  satisfaction_med  <- median(df$avg_satisfaction, na.rm = TRUE)
  
  df %>%
    mutate(
      # low value but high support pain
      low_value_high_pain = if_else(
        mrr_num < mrr_median & help_ticket_count > ticket_median,
        1L,
        0L
      ),
      
      # mature but unhappy customer
      mature_unhappy_customer = if_else(
        customer_age_months > age_median & avg_satisfaction < satisfaction_med,
        1L,
        0L
      )
    )
}

# Apply
train_fe <- add_risk_flags(train_fe)
test_fe  <- add_risk_flags(test_fe)

# Check
train_fe %>%
  select(
    mrr_num,
    help_ticket_count,
    customer_age_months,
    avg_satisfaction,
    low_value_high_pain,
    mature_unhappy_customer
  ) %>%
  head(10)