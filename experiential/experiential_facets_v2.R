# Created: 2026-06-18
# Purpose: Patchwork layout of "experiential X" search-interest trends.
#          Bare "Experiential" full-width on top, 3x3 grid of the 9 niches below.
#          No trendlines. Each facet a different colour. Three style variants.
# Data:    10 *.Explore.md webclips (monthly Google Trends, 0-100 per-term norm).

library(tidyverse)
library(lubridate)
library(zoo)
library(ggtext)
library(patchwork)

setwd("/Users/alexp/gd_alpapag/apclients/posts/experiential")
base_family <- "Helvetica Neue"

# ------------------------------------------------------------------
# 1. Parse + prep (same pipeline as experiential_trends.R)
# ------------------------------------------------------------------
activity_from_file <- function(path) {
  stem <- str_remove(basename(path), " - Explore\\.md$")
  if (stem == "experiential") return("Experiential")
  str_to_title(str_remove(stem, "^experiential "))
}

parse_explore <- function(path) {
  activity <- activity_from_file(path)
  lines <- read_lines(path)
  rows <- lines[str_detect(lines, "^\\|.*\\|\\s*[0-9]+\\s*\\|\\s*$")]
  rows <- rows[!str_detect(rows, "---")]
  rows <- rows[!str_detect(rows, "\\| *x *\\|")]
  tibble(raw = rows) %>%
    separate(raw, into = c("blank", "date_str", "value", "tail"),
             sep = "\\|", extra = "merge", fill = "right") %>%
    transmute(
      activity = activity,
      date_str = str_remove_all(date_str, "[\u200E\u200F\u202A-\u202E\u2066-\u2069]"),
      date_str = str_trim(date_str),
      date  = mdy(date_str),
      value = as.integer(str_trim(value))
    ) %>%
    filter(!is.na(date))
}

files <- list.files(pattern = " - Explore\\.md$")
combined <- map_dfr(files, parse_explore) %>%
  filter(date >= as.Date("2024-01-01"), date <= as.Date("2026-05-01")) %>%
  arrange(activity, date) %>%
  group_by(activity) %>%
  mutate(value_roll = rollmean(value, k = 3, fill = NA, align = "right")) %>%
  ungroup() %>%
  filter(!is.na(value_roll))

# Grid order: the five from the post first, then the rest. Bare term sits on top.
grid_order <- c("Education", "Marketing", "Retail", "Design", "Wellness",
                "Travel", "Music", "Entertainment", "Dating")

top_df  <- combined %>% filter(activity == "Experiential")
grid_df <- combined %>%
  filter(activity %in% grid_order) %>%
  mutate(activity   = factor(activity, levels = grid_order),
         facet_label = factor(paste("Experiential", as.character(activity)),
                              levels = paste("Experiential", grid_order)))

# ------------------------------------------------------------------
# 2. Style definitions (three variants)
# ------------------------------------------------------------------
# Each variant: figure bg, panel bg, grid, text, strip fill/colour, top colour,
# 9-colour facet palette, geom style ("line" or "area"), point flag.

styles <- list(
  warm = list(
    name = "warm",
    bg_fig = "#FAF8F5", bg_panel = "#F6EFE8", grid = "#E2D6CB",
    text = "#2B2F33", muted = "gray40", strip_fill = "#EADfd3",
    strip_col = "#2B2F33", top_col = "#1c1f22", accent = "#B83A2F",
    geom = "line", points = TRUE,
    pal = c("#2B5FB8","#B83A2F","#8F6A00","#13734A","#6B4FA3",
            "#1F7A7A","#C05A1A","#B44A7A","#4E5A63")
  ),
  newsprint = list(
    name = "newsprint",
    bg_fig = "#F7F9F8", bg_panel = "#EFF2F0", grid = "#D2D8D4",
    text = "#202427", muted = "gray40", strip_fill = "#E4E9E6",
    strip_col = "#202427", top_col = "#2C4E8F", accent = "#A8322A",
    geom = "area", points = FALSE,
    pal = c("#2C4E8F","#A8322A","#856000","#0F6A3F","#5B4A86",
            "#2C7D6C","#A6562B","#8E3D57","#586168")
  ),
  dark = list(
    name = "dark",
    bg_fig = "#14171C", bg_panel = "#1E232B", grid = "#2E343E",
    text = "#E6E8EC", muted = "#9AA3AE", strip_fill = "#272D37",
    strip_col = "#F0F2F5", top_col = "#4C8DF6", accent = "#FF7A66",
    geom = "line", points = TRUE,
    pal = c("#4C8DF6","#FF6B5E","#FFC857","#3DD68C","#B084F5",
            "#36D6D6","#FF9F43","#FF6FA5","#9AA7B4")
  )
)

theme_variant <- function(s, strip_size = 0.82) {
  theme_minimal(base_size = 12, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = s$bg_fig, colour = NA),
      panel.background = element_rect(fill = s$bg_panel, colour = NA),
      panel.grid.major = element_line(colour = s$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing    = unit(0.6, "lines"),
      text       = element_text(colour = s$text),
      axis.text  = element_text(colour = s$muted, size = rel(0.68)),
      axis.title = element_text(colour = s$muted, size = rel(0.72)),
      strip.text = element_text(face = "bold", size = rel(strip_size), hjust = 0,
                                colour = s$strip_col, margin = margin(b = 2, t = 3)),
      strip.background = element_rect(fill = s$strip_fill, colour = NA),
      legend.position = "none",
      plot.margin = margin(t = 4, r = 8, b = 2, l = 4)
    )
}

# ------------------------------------------------------------------
# 3. Plot builders
# ------------------------------------------------------------------
add_geom <- function(p, s, colour, fill = NULL) {
  if (s$geom == "area") {
    p <- p + geom_area(fill = colour, alpha = 0.22, colour = NA) +
      geom_line(colour = colour, linewidth = 0.8)
  } else {
    p <- p + geom_line(colour = colour, linewidth = 0.9)
    if (s$points) p <- p + geom_point(colour = colour, size = 0.6, alpha = 0.55)
  }
  p
}

build_top <- function(s) {
  p <- ggplot(top_df, aes(date, value_roll))
  p <- add_geom(p, s, s$top_col)
  p +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100),
                       expand = expansion(mult = c(0.02, 0.08))) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~ "Experiential (the umbrella term)") +
    theme_variant(s, strip_size = 0.95)
}

build_grid <- function(s) {
  ggplot(grid_df, aes(date, value_roll, colour = activity, fill = activity)) +
    {if (s$geom == "area")
       list(geom_area(alpha = 0.20, colour = NA),
            geom_line(linewidth = 0.75))
     else
       list(geom_line(linewidth = 0.8),
            if (s$points) geom_point(size = 0.5, alpha = 0.5) else NULL)} +
    facet_wrap(~ facet_label, ncol = 3) +
    scale_colour_manual(values = setNames(s$pal, grid_order)) +
    scale_fill_manual(values = setNames(s$pal, grid_order)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100),
                       expand = expansion(mult = c(0.02, 0.08))) +
    labs(x = NULL, y = "Search interest (peak = 100)") +
    theme_variant(s)
}

cap <- paste0(
  "Source: Google Trends, US, search interest (0-100, normalized to each term's own peak). ",
  "Jan 2024-May 2026, 3-month rolling avg, retrieved Jun 2026.<br>",
  "Note: a platform-wide step-up around Aug 2025 affects all terms and likely reflects a Google Trends sampling change.<br>",
  "Data, code and analysis by @alex_papageo: github.com/papageorgiou/posts/tree/master/experiential"
)

build_version <- function(s) {
  p_top  <- build_top(s)
  p_grid <- build_grid(s)
  (p_top / p_grid) +
    plot_layout(heights = c(1, 3)) +
    plot_annotation(
      title = "The new <span style='color:#B83A2F;'>'e-'</span> is for experiential",
      subtitle = "Google search interest is climbing across nearly every <b>experiential</b> category. Each panel is normalized to its own peak (=100).",
      caption = cap,
      theme = theme(
        plot.background = element_rect(fill = s$bg_fig, colour = NA),
        plot.title    = element_textbox_simple(family = base_family, face = "bold",
                          size = 21, colour = if (s$name=="dark") "#F2F4F7" else "#1c1f22",
                          margin = margin(t = 4, b = 4)),
        plot.subtitle = element_textbox_simple(family = base_family, size = 12.5,
                          colour = s$muted, lineheight = 1.25, margin = margin(b = 8)),
        plot.caption  = element_textbox_simple(family = base_family, size = 8,
                          colour = s$muted, halign = 0, hjust = 0, lineheight = 1.3,
                          margin = margin(t = 10)),
        plot.margin = margin(t = 12, r = 14, b = 10, l = 12)
      )
    )
}

# ------------------------------------------------------------------
# 4. Render the three variants
# ------------------------------------------------------------------
for (key in names(styles)) {
  s <- styles[[key]]
  # accent in title is fixed red; on dark, swap to its accent
  v <- build_version(s)
  fn <- paste0("experiential_v2_", key, ".png")
  ggsave(fn, v, width = 9, height = 11, units = "in", dpi = 150, bg = s$bg_fig)
  message("Wrote ", fn)
}
