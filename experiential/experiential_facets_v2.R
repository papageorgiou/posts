# Created: 2026-06-18 (rev 2026-06-19)
# Purpose: Patchwork layout of "experiential X" search-interest trends.
#          Bare "experiential" full-width on top, grid of the niches below.
#          No trendlines. Each facet a different colour. Three style variants,
#          all built on the Google Trends colour palette.
#          Facet titles: lowercase, large/bold, magnifying-glass prefix.
#          Layouts: 3x3 (all 9 niches), plus 3x2 and 2x3 (6-niche) variants.
# Data:    10 *.Explore.md webclips (monthly Google Trends, 0-100 per-term norm).
# Note:    rendered via ragg so the colour magnifying-glass emoji shows up.

library(tidyverse)
library(lubridate)
library(zoo)
library(ggtext)
library(patchwork)
library(ragg)

setwd("/Users/alexp/gd_alpapag/apclients/posts/experiential")
base_family <- "Helvetica Neue"
MAG <- "\U0001F50D"   # magnifying-glass icon

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

# Facet labels are lowercase with a magnifying-glass prefix: "🔍 experiential music".
facet_lab <- function(x) paste0(MAG, " experiential ", tolower(x))

top_df  <- combined %>% filter(activity == "Experiential")

make_grid_df <- function(niches) {
  combined %>%
    filter(activity %in% niches) %>%
    mutate(activity    = factor(activity, levels = niches),
           facet_label = factor(facet_lab(as.character(activity)),
                                levels = facet_lab(niches)))
}

# ------------------------------------------------------------------
# 2. Style definitions (three variants, all Google-palette based)
# ------------------------------------------------------------------
# Google brand colours: blue #4285F4, red #EA4335, yellow #FBBC04, green #34A853.
# Light variants use the bright Google categorical palette; the dark variant uses
# Google's official dark-mode tints (blue #8AB4F8, red #F28B82, etc.).

# Bright Google categorical palette (9 hues).
pal_google <- c("#4285F4", "#EA4335", "#FBBC04", "#34A853", "#A142F4",
                "#24C1E0", "#FF6D01", "#E52592", "#5F6368")
# Google dark-mode tints (9 hues).
pal_google_dark <- c("#8AB4F8", "#F28B82", "#FDD663", "#81C995", "#D7AEFB",
                     "#78D9EC", "#FCAD70", "#FF8BCB", "#9AA0A6")

GBLUE <- "#4285F4"; GRED <- "#EA4335"
GBLUE_D <- "#8AB4F8"; GRED_D <- "#F28B82"

styles <- list(
  warm = list(
    name = "warm",
    bg_fig = "#FAF8F5", bg_panel = "#F6EFE8", grid = "#E2D6CB",
    text = "#202124", muted = "#5F6368", strip_fill = "#EADfd3",
    strip_col = "#202124", top_col = GBLUE, accent = GRED,
    title_col = GRED,
    geom = "line", points = TRUE, pal = pal_google
  ),
  newsprint = list(
    name = "newsprint",
    bg_fig = "#FFFFFF", bg_panel = "#F8F9FA", grid = "#DADCE0",
    text = "#202124", muted = "#5F6368", strip_fill = "#F1F3F4",
    strip_col = "#202124", top_col = GBLUE, accent = GRED,
    title_col = GRED,
    geom = "area", points = FALSE, pal = pal_google
  ),
  dark = list(
    name = "dark",
    bg_fig = "#14171C", bg_panel = "#1E232B", grid = "#2E343E",
    text = "#E6E8EC", muted = "#9AA3AE", strip_fill = "#272D37",
    strip_col = "#F0F2F5", top_col = GBLUE_D, accent = GRED_D,
    title_col = GRED_D,
    geom = "line", points = TRUE, pal = pal_google_dark
  )
)

theme_variant <- function(s, strip_size = 1.05) {
  theme_minimal(base_size = 12, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = s$bg_fig, colour = NA),
      panel.background = element_rect(fill = s$bg_panel, colour = NA),
      panel.grid.major = element_line(colour = s$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      # vertical gridlines on (one per year break)
      panel.grid.major.x = element_line(colour = s$grid, linewidth = 0.3),
      panel.spacing    = unit(0.6, "lines"),
      text       = element_text(colour = s$text),
      axis.text  = element_text(colour = s$muted, size = rel(0.68)),
      axis.title = element_text(colour = s$muted, size = rel(0.72)),
      # larger + bolder facet titles
      strip.text = element_text(family = base_family, face = "bold",
                                size = rel(strip_size), hjust = 0,
                                colour = s$strip_col, margin = margin(b = 3, t = 4)),
      strip.background = element_rect(fill = s$strip_fill, colour = NA),
      legend.position = "none",
      plot.margin = margin(t = 4, r = 8, b = 2, l = 4)
    )
}

# ------------------------------------------------------------------
# 3. Plot builders
# ------------------------------------------------------------------
add_geom <- function(p, s, colour) {
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
    facet_wrap(~ paste0(MAG, " experiential — the umbrella term")) +
    theme_variant(s, strip_size = 1.25)
}

build_grid <- function(s, niches, ncol) {
  gdf <- make_grid_df(niches)
  ggplot(gdf, aes(date, value_roll, colour = activity, fill = activity)) +
    {if (s$geom == "area")
       list(geom_area(alpha = 0.20, colour = NA),
            geom_line(linewidth = 0.75))
     else
       list(geom_line(linewidth = 0.8),
            if (s$points) geom_point(size = 0.5, alpha = 0.5) else NULL)} +
    facet_wrap(~ facet_label, ncol = ncol) +
    scale_colour_manual(values = setNames(s$pal, grid_order)) +
    scale_fill_manual(values = setNames(s$pal, grid_order)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100),
                       expand = expansion(mult = c(0.02, 0.08))) +
    labs(x = NULL, y = "Search interest (peak = 100)") +
    theme_variant(s)
}

# Caption: faint grey source/note lines, then a bolder, accent-coloured byline +
# GitHub link so the credit is the most visible part of the footer.
cap_for <- function(s) {
  link_col <- if (s$name == "dark") s$strip_col else "#202124"
  paste0(
    "Source: Google Trends, US, search interest (0-100, normalized to each term's own peak). ",
    "Jan 2024-May 2026, 3-month rolling avg, retrieved Jun 2026.<br>",
    "Note: a platform-wide step-up around Aug 2025 affects all terms and likely reflects a Google Trends sampling change.<br>",
    "<span style='color:", link_col, ";'><b>Data, code & analysis by @alex_papageo &nbsp;·&nbsp; ",
    "<span style='color:", s$title_col, ";'>github.com/papageorgiou/posts</span></b></span>"
  )
}

build_version <- function(s, niches, ncol, grid_height) {
  p_top  <- build_top(s)
  p_grid <- build_grid(s, niches, ncol)
  (p_top / p_grid) +
    plot_layout(heights = c(1, grid_height)) +
    plot_annotation(
      title = paste0("The new <span style='color:", s$title_col,
                     ";'>'e-'</span> is for <span style='color:", s$title_col,
                     ";'><i>experiential</i></span>"),
      subtitle = "Google search interest is climbing across nearly every <b>experiential</b> category. Each panel is normalized to its own peak (=100).",
      caption = cap_for(s),
      theme = theme(
        plot.background = element_rect(fill = s$bg_fig, colour = NA),
        plot.title    = element_textbox_simple(family = base_family, face = "bold",
                          size = 23, colour = s$text,
                          margin = margin(t = 4, b = 4)),
        plot.subtitle = element_textbox_simple(family = base_family, size = 12.5,
                          colour = s$muted, lineheight = 1.25, margin = margin(b = 8)),
        plot.caption  = element_textbox_simple(family = base_family, size = 9.5,
                          colour = s$muted, halign = 0, hjust = 0, lineheight = 1.4,
                          margin = margin(t = 12)),
        plot.margin = margin(t = 12, r = 14, b = 10, l = 12)
      )
    )
}

# ------------------------------------------------------------------
# 4. Render
# ------------------------------------------------------------------
# Six-niche subset for the non-3x3 layouts (the five from the post + Travel).
niches6 <- c("Education", "Marketing", "Retail", "Design", "Wellness", "Travel")

# layout spec: niches, ncol, grid_height (relative to top), width, height
layouts <- list(
  list(suffix = "",      niches = grid_order, ncol = 3, gh = 3.0, w = 9,  h = 11),  # 3x3, all 9
  list(suffix = "_3x2",  niches = niches6,    ncol = 3, gh = 2.0, w = 9,  h = 8.2), # 3 cols x 2 rows
  list(suffix = "_2x3",  niches = niches6,    ncol = 2, gh = 2.6, w = 7,  h = 10)   # 2 cols x 3 rows
)

for (key in names(styles)) {
  s <- styles[[key]]
  for (L in layouts) {
    v  <- build_version(s, L$niches, L$ncol, L$gh)
    fn <- paste0("experiential_v2_", key, L$suffix, ".png")
    ggsave(fn, v, width = L$w, height = L$h, units = "in", dpi = 150,
           bg = s$bg_fig, device = ragg::agg_png)
    message("Wrote ", fn)
  }
}
