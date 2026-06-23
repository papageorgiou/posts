# Created: 2026-06-18
# Purpose: Faceted Google Trends search-interest visualization for "experiential X"
#          activities. Two versions: raw monthly and 3-month rolling average.
# Data:    10 *.Explore.md webclips, each ending in a monthly value table
#          (May 2019 -> Jun 2026), normalized 0-100 to that term's own peak.

library(tidyverse)
library(lubridate)
library(zoo)
library(ggtext)

setwd("/Users/alexp/gd_alpapag/apclients/posts/experiential")

base_family <- "Helvetica Neue"

# ------------------------------------------------------------------
# 1. Parse the markdown tables
# ------------------------------------------------------------------

# Map filename -> clean activity label
activity_from_file <- function(path) {
  stem <- str_remove(basename(path), " - Explore\\.md$")
  if (stem == "experiential") return("Experiential")
  str_to_title(str_remove(stem, "^experiential "))
}

parse_explore <- function(path) {
  activity <- activity_from_file(path)
  lines <- read_lines(path)
  # Data rows look like:  | <date> | <int> |
  rows <- lines[str_detect(lines, "^\\|.*\\|\\s*[0-9]+\\s*\\|\\s*$")]
  rows <- rows[!str_detect(rows, "---")]      # drop separator
  rows <- rows[!str_detect(rows, "\\| *x *\\|")] # drop header

  tibble(raw = rows) %>%
    separate(raw, into = c("blank", "date_str", "value", "tail"),
             sep = "\\|", extra = "merge", fill = "right") %>%
    transmute(
      activity = activity,
      # strip Unicode directional / format marks (LRM, RLM, isolates, etc.)
      date_str = str_remove_all(date_str, "[\u200E\u200F\u202A-\u202E\u2066-\u2069]"),
      date_str = str_trim(date_str),
      date  = mdy(date_str),
      value = as.integer(str_trim(value))
    ) %>%
    filter(!is.na(date))
}

files <- list.files(pattern = " - Explore\\.md$")
combined <- map_dfr(files, parse_explore)

# Window: Jan 2024 -> May 2026 (drop incomplete Jun 2026)
combined <- combined %>%
  filter(date >= as.Date("2024-01-01"), date <= as.Date("2026-05-01"))

# Order facets: clean growers first, off-pattern next, bare term last
activity_levels <- c("Travel", "Design", "Education", "Marketing", "Retail",
                     "Music", "Wellness", "Entertainment", "Dating", "Experiential")
combined <- combined %>%
  mutate(activity = factor(activity, levels = activity_levels))

stopifnot(!any(is.na(combined$activity)))
message("Activities: ", n_distinct(combined$activity),
        " | rows: ", nrow(combined),
        " | range: ", min(combined$date), " to ", max(combined$date))

# 3-month right-aligned rolling average
combined_roll <- combined %>%
  arrange(activity, date) %>%
  group_by(activity) %>%
  mutate(value_roll = rollmean(value, k = 3, fill = NA, align = "right")) %>%
  ungroup() %>%
  filter(!is.na(value_roll))

# ------------------------------------------------------------------
# 2. Style (dataviz-linkedin: Warm Ledger palette + my_social_theme)
# ------------------------------------------------------------------

bg_plot   <- "#F6EFE8"
bg_figure <- "#FAF8F5"
gridlines <- "#E2D6CB"
text_axes <- "#2B2F33"
line_col  <- "#2B5FB8"   # warm-ledger blue
trend_col <- "#B83A2F"   # warm-ledger red

my_social_theme <- function(base_size = 13, base_family = "Helvetica Neue") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = bg_figure, colour = NA),
      panel.background = element_rect(fill = bg_plot, colour = NA),
      panel.grid.major = element_line(colour = gridlines, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing    = unit(0.7, "lines"),
      text   = element_text(colour = text_axes),
      axis.text  = element_text(colour = "gray35", size = rel(0.75)),
      axis.title = element_text(colour = "gray35", size = rel(0.8)),
      strip.text = element_text(face = "bold", size = rel(1.2), hjust = 0,
                                colour = text_axes,
                                margin = margin(b = 3, t = 5)),
      strip.background = element_rect(fill = "#EADfd3", colour = NA),
      plot.title    = element_textbox_simple(face = "bold", size = rel(1.55),
                                             colour = "#1c1f22", lineheight = 1.05,
                                             margin = margin(b = 4)),
      plot.subtitle = element_textbox_simple(size = rel(0.95), colour = "gray30",
                                             lineheight = 1.25,
                                             margin = margin(b = 10)),
      plot.caption  = element_textbox_simple(size = rel(0.62), colour = "gray45",
                                             halign = 0, hjust = 0,
                                             lineheight = 1.3,
                                             margin = margin(t = 12)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      legend.position = "none",
      plot.margin = margin(t = 14, r = 16, b = 10, l = 14)
    )
}

my_caption <- paste0(
  "Source: Google Trends, Worldwide, search interest (0-100, normalized to each term's own peak). ",
  "Jan 2024-May 2026, retrieved Jun 2026.<br>",
  "Data, code and analysis by @alex_papageo: github.com/papageorgiou/posts/tree/master/experiential"
)

title_txt <- "The rise of <span style='color:#B83A2F;'><i>experiential</i></span>"
sub_txt   <- paste0(
  "Monthly Google search interest is climbing across nearly every <b>experiential</b> category."
)

# ------------------------------------------------------------------
# 3. Plot builder
# ------------------------------------------------------------------

build_plot <- function(df, yvar, subtitle, smooth = FALSE) {
  p <- ggplot(df, aes(x = date, y = .data[[yvar]])) +
    geom_line(colour = line_col, linewidth = 1.4) +
    facet_wrap(~ activity, ncol = 2) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b '%y",
                 expand = expansion(mult = c(0.02, 0.03))) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100),
                       expand = expansion(mult = c(0.02, 0.08))) +
    labs(title = title_txt, subtitle = subtitle, caption = my_caption,
         x = NULL, y = "Search interest (peak = 100)") +
    my_social_theme(base_family = base_family)

  if (smooth) {
    p <- p + geom_point(colour = line_col, size = 0.7, alpha = 0.5)
  } else {
    p <- p +
      geom_point(colour = line_col, size = 0.8, alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.9,
                  linetype = "dashed", colour = trend_col)
  }
  p
}

p_raw <- build_plot(combined, "value",
                    paste0(sub_txt, " <span style='color:#B83A2F;'>Dashed red = linear trend.</span>"),
                    smooth = FALSE)

p_roll <- build_plot(combined_roll, "value_roll",
                     paste0(sub_txt, " <i>Lines = 3-month rolling average.</i>"),
                     smooth = TRUE)

# ------------------------------------------------------------------
# 4. Export (1080px wide, 150 dpi -> 7.2in; 5 rows -> ~11in tall)
# ------------------------------------------------------------------

ggsave("experiential_facets_raw.png", p_raw,
       width = 7.2, height = 10.8, units = "in", dpi = 150, bg = bg_figure)
ggsave("experiential_facets_rolling.png", p_roll,
       width = 7.2, height = 10.8, units = "in", dpi = 150, bg = bg_figure)

message("Done. Wrote experiential_facets_raw.png and experiential_facets_rolling.png")
