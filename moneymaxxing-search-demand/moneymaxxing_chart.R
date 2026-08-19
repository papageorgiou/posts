# Moneymaxxing search demand ---------------------------------------------
# LinkedIn chart: monthly Google searches for "moneymaxxing", Jan 2025 - Jul 2026
# Source data: Google Ads Keyword Planner export (UTF-16LE, tab separated)

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(scales)

# ---- 1. Read and reshape the Keyword Planner export ---------------------

read_gkp <- function(path) {
  lines <- readr::read_lines(path, locale = readr::locale(encoding = "UTF-16LE"))
  # line 1 is the export title/date-range banner; the header row follows
  raw <- readr::read_tsv(I(lines[-1]), quote = "", show_col_types = FALSE)
  names(raw) <- trimws(names(raw))
  raw |>
    filter(!is.na(Keyword)) |>
    select(Keyword, starts_with("Searches:")) |>
    mutate(across(starts_with("Searches:"), as.character)) |>
    pivot_longer(
      -Keyword,
      names_to = "month_label",
      values_to = "searches"
    ) |>
    mutate(
      # the export carries stray non-ASCII bytes on the last column - strip them
      keyword  = trimws(gsub("[^A-Za-z ]", "", Keyword)),
      month    = as.Date(
        paste0("01 ", gsub("Searches:\\s*", "", gsub("[^A-Za-z0-9 :]", "", month_label))),
        format = "%d %b %Y"
      ),
      searches = as.numeric(gsub("[^0-9]", "", searches))
    ) |>
    select(keyword, month, searches) |>
    arrange(month)
}

mm <- read_gkp("data/moneymax_Keyword_Stats_2026-08-18_at_22_53_52.csv") |>
  filter(keyword == "moneymaxxing", month >= as.Date("2025-01-01"))

# ---- 2. Palette ---------------------------------------------------------
# Warm Ledger base; money-green as the series colour, deep red for the peak.

bg_plot   <- "#F6EFE8"
bg_figure <- "#FAF8F5"
gridlines <- "#E2D6CB"
text_axes <- "#2B2F33"

green <- "#13734A"
red   <- "#B83A2F"
muted <- "#8A7F76"

my_font <- "Helvetica Neue"

# ---- 3. Key numbers for annotation --------------------------------------

first_pt <- mm |> slice_min(month, n = 1)
last_pt  <- mm |> slice_max(month, n = 1)
peak_pt  <- mm |> slice_max(searches, n = 1)

growth <- round(last_pt$searches / first_pt$searches, 1)

my_caption <- paste0(
  "Source: Google Ads Keyword Planner, monthly search volume for \"moneymaxxing\",\n",
  "Jan 2025-Jul 2026. Data retrieved August 2026.\n",
  "Keyword Planner reports volumes in rounded buckets, so month-to-month steps are approximate.\n",
  "Code and data: github.com/papageorgiou/posts/tree/main/moneymaxxing-search-demand"
)

# ---- 4. Chart -----------------------------------------------------------

p <- ggplot(mm, aes(month, searches)) +
  geom_area(fill = green, alpha = 0.10) +
  geom_line(colour = green, linewidth = 1.6) +
  geom_point(data = mm |> filter(month != peak_pt$month),
             colour = green, size = 2.6) +
  geom_point(data = peak_pt, colour = red, size = 4.2) +
  # first / last value labels
  geom_text(data = first_pt, aes(label = searches),
            vjust = 2.2, hjust = 0.2, size = 5, colour = text_axes,
            family = my_font, fontface = "bold") +
  geom_text(data = last_pt, aes(label = searches),
            vjust = -1.4, size = 5, colour = text_axes,
            family = my_font, fontface = "bold") +
  # peak callout
  annotate("text", x = peak_pt$month, y = peak_pt$searches + 26,
           label = paste0(peak_pt$searches, " in May 2026"),
           colour = red, family = my_font, fontface = "bold", size = 5) +
  scale_x_date(breaks = seq(min(mm$month), max(mm$month), by = "3 months"),
               date_labels = "%b\n%Y",
               expand = expansion(mult = c(0.06, 0.10))) +
  scale_y_continuous(limits = c(0, 320), breaks = seq(0, 300, 100),
                     labels = comma_format()) +
  labs(
    title = paste0("\"Moneymaxxing\" searches: ", growth, "x in 18 months"),
    subtitle = paste0(
      "Monthly Google searches, <span style='color:", green,
      ";'>**moneymaxxing**</span>.<br>",
      "Tiny numbers - but the direction has been one way for 18 months.<br>",
      "Every megatrend looked like this before it was one."
    ),
    x = NULL,
    y = "Monthly searches",
    caption = my_caption
  ) +
  theme_minimal(base_family = my_font, base_size = 15) +
  theme(
    plot.background   = element_rect(fill = bg_figure, colour = NA),
    panel.background  = element_rect(fill = bg_plot, colour = NA),
    panel.grid.major.y = element_line(colour = gridlines),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    # textbox so a long headline wraps instead of running off the canvas
    plot.title = element_textbox_simple(face = "bold", size = 21, colour = text_axes,
                                        lineheight = 1.15, margin = margin(b = 14)),
    plot.subtitle = element_markdown(size = 14.5, colour = muted, lineheight = 1.35,
                                     margin = margin(b = 18)),
    axis.text = element_text(colour = text_axes, size = 13),
    axis.title.y = element_text(colour = muted, size = 13,
                                margin = margin(r = 8)),
    plot.caption = element_text(hjust = 0, size = 9.5, colour = muted,
                                face = "italic", lineheight = 1.4,
                                margin = margin(t = 16)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(24, 26, 18, 22)
  )

ggsave("moneymaxxing_linkedin.png", p, width = 1080, height = 1080,
       units = "px", dpi = 150, bg = bg_figure)
