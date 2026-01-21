# Created: 2026-01-20
# Updated: 2026-01-20 - LinkedIn mobile, taller graphs, + 3-month rolling average version
# Purpose: Generate publication-ready faceted ggplot of search interest trends for experiential travel companies

# Load required libraries
library(tidyverse)
library(lubridate)
library(scales)

# Set working directory (adjust if needed)
# setwd("C:/Users/alexp/gd_alpapag/apclients/posts/experiential-travel")

# Read CSV files (UTF-16LE encoded with tab separator)
kindred_df <- read_tsv("kindred_Keyword Stats 2026-01-17 at 17_13_59.csv", locale = locale(encoding = "UTF-16LE"))
welcome_df <- read_tsv("welcome_Keyword Stats 2026-01-17 at 17_12_21.csv", locale = locale(encoding = "UTF-16LE"))
withlocals_df <- read_tsv("withlocalsKeyword Stats 2026-01-17 at 17_11_16.csv", locale = locale(encoding = "UTF-16LE"))

# Custom formatter for K notation (1000 -> 1K)
format_k <- function(x) {
  ifelse(x >= 1000, paste0(x / 1000, "K"), as.character(x))
}

# Function to transform and aggregate data
transform_data <- function(df, company_name) {
  df %>%
    # Remove empty rows
    filter(!is.na(Keyword) & Keyword != "") %>%
    # Pivot to long format
    pivot_longer(
      cols = starts_with("Searches:"),
      names_to = "date_str",
      values_to = "searches"
    ) %>%
    # Clean date column
    mutate(
      date_str = str_remove(date_str, "Searches: "),
      date = parse_date_time(date_str, "b Y"),
      company = company_name
    ) %>%
    # Aggregate by date (sum all keywords)
    group_by(company, date) %>%
    summarise(
      total_searches = sum(searches, na.rm = TRUE),
      .groups = "drop"
    )
}

# Transform each dataset
kindred_long <- transform_data(kindred_df, "Kindred")
welcome_long <- transform_data(welcome_df, "Welcome Pickups")
withlocals_long <- transform_data(withlocals_df, "Withlocals")

# Combine all datasets
combined_df <- bind_rows(kindred_long, welcome_long, withlocals_long)

# Set factor levels for consistent ordering
combined_df$company <- factor(
  combined_df$company,
  levels = c("Kindred", "Welcome Pickups", "Withlocals")
)

# Define a refined color palette
company_colors <- c(
  "Kindred" = "#2E86AB",
  "Welcome Pickups" = "#A23B72",
  "Withlocals" = "#F18F01"
)


# ============================================================
# VERSION 2: 3-Month Rolling Average
# ============================================================

library(zoo)



# Calculate 3-month rolling average by company (align right to keep Nov 2025)
combined_df_rolling <- combined_df %>%
  arrange(company, date) %>%
  group_by(company) %>%
  mutate(
    searches_3m_avg = rollmean(total_searches, k = 3, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  filter(!is.na(searches_3m_avg))

# Create rolling average plot with subtle linear trend
p_rolling <- ggplot(combined_df_rolling, aes(x = date, y = searches_3m_avg, color = company)) +
  # Regression line (trend) - very subtle
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7,
    linetype = "dashed",
    colour = "gray",
    alpha = 0.60
  ) +
  # Data line - thicker
  geom_line(linewidth = 1.3, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.7) +
  facet_wrap(~ company, ncol = 1, scales = "free_y") +
  scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = format_k,
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  scale_color_manual(values = company_colors) +
  labs(
    title = "Rising Search Interest in \nExperiential Travel Startups",
    #subtitle = "3 startups experiencing growing search demand",
    x = NULL,
    y = "Monthly Searches, Google US",
    caption = "Search demand volume is directional and based on brand-related searches. \nData source: Google Search data, US (3m rolling avg) Feb 2022-Nov 2025. \nhttps://github.com/papageorgiou/posts/tree/master/experiential-travel\n@alex_papageo"
  ) +
  theme_minimal(base_size = 9, base_family = "sans") +
  theme(
    # Title styling - ensure full visibility
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0,
      margin = margin(b = 1)
    ),
    plot.subtitle = element_text(
      size = 10,
      color = "gray40",
      hjust = 0,
      margin = margin(b = 6)
    ),
    plot.caption = element_text(
      size = 6,
      color = "gray50",
      hjust = 1,
      margin = margin(t = 3)
    ),
    # Facet styling - compact
    strip.text = element_text(
      face = "bold",
      size = 10,
      hjust = 0,
      margin = margin(b = 1, t = 2)
    ),
    strip.background = element_rect(fill = "gray95", color = NA),
    # Panel styling
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.3),
    panel.spacing = unit(0.4, "lines"),
    # Axis styling
    axis.text.x = element_text(
      size = 7,
      color = "gray30"
    ),
    axis.text.y = element_text(size = 7, color = "gray30"),
    axis.title.y = element_text(
      size = 8,
      color = "gray30",
      margin = margin(r = 3)
    ),
    # Legend - hide since facets already show company
    legend.position = "none",
    # Plot margins - tight
    plot.margin = margin(t = 8, r = 8, b = 5, l = 5)
  ) +   theme(
    plot.caption = element_text(hjust = 0, size=8, face="italic", margin = margin(t=10)),   # 0 = left, 0.5 = center, 1 = right
   plot.caption.position = "plot")


p_rolling

# Save rolling average version
ggsave(
  filename = "search_interest_linkedin_rolling.png",
  width = 3.9,
  height = 7,
  units = "in",
  #res = 300,
  bg = "white"
)
