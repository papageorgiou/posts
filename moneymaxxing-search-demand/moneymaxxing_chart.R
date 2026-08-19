# Moneymaxxing search demand ---------------------------------------------
# LinkedIn chart: monthly Google searches for "moneymaxxing", Jan 2025 - Jul 2026
# Source data: Google search volume export (UTF-16LE, tab separated), US

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(scales)
library(magick)

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
# Warm Ledger base with a money-green series.

bg_plot   <- "#F6EFE8"
bg_figure <- "#FAF8F5"
gridlines <- "#E2D6CB"
text_axes <- "#2B2F33"

green <- "#13734A"
muted <- "#8A7F76"

my_font <- "Helvetica Neue"

# ---- 3. Key numbers -----------------------------------------------------

first_pt <- mm |> slice_min(month, n = 1)
last_pt  <- mm |> slice_max(month, n = 1)

growth <- round(last_pt$searches / first_pt$searches, 1)

# ---- 4. Photo variants --------------------------------------------------
# Four versions of the same chart, each with a different top-left photo.
# Studio shots on a clean white sweep get the background knocked out so the
# subject floats on the warm paper; photos with a tinted or textured
# background are placed as a plain rectangular inset instead.

variants <- list(
  list(file = "img/piggy_bank_pexels_9660.jpg",
       out = "moneymaxxing_linkedin.png",
       credit = "ClickerHappy / Pexels", knockout = TRUE, fuzz = 6),
  list(file = "img/coins_white_pexels_19693228.jpg",
       out = "moneymaxxing_linkedin_v2_coins.png",
       credit = "William Warby / Pexels", knockout = TRUE, fuzz = 22),
  list(file = "img/cash_envelope_pexels_534229.jpg",
       out = "moneymaxxing_linkedin_v3_envelope.png",
       credit = "Pixabay / Pexels", knockout = FALSE),
  list(file = "img/woman_dollars_pexels_7680634.jpg",
       out = "moneymaxxing_linkedin_v4_portrait.png",
       credit = "Karola G / Pexels", knockout = FALSE)
)

photo_grob <- function(v) {
  img <- image_read(v$file)
  if (isTRUE(v$knockout)) {
    img <- img |> image_transparent(color = "white", fuzz = v$fuzz) |> image_trim()
  } else {
    # crop to a 3:2 card so every framed inset keeps the same footprint
    img <- image_resize(img, "600x") |>
      image_crop("600x400", gravity = "center") |>
      image_border("#E2D6CB", "3x3")
  }
  grid::rasterGrob(img, interpolate = TRUE)
}

# ---- 5. Chart -----------------------------------------------------------

build_chart <- function(v) {

  my_caption <- paste0(
    "Source: Google data, monthly US search volume for \"moneymaxxing\", Jan 2025-Jul 2026.\n",
    "Data retrieved August 2026. Photo: ", v$credit, ".\n",
    "Code and data: github.com/papageorgiou/posts/tree/main/moneymaxxing-search-demand"
  )

  x0 <- min(mm$month)

  p <- ggplot(mm, aes(month, searches)) +
    geom_area(fill = green, alpha = 0.10) +
    geom_line(colour = green, linewidth = 1.6) +
    geom_point(colour = green, size = 2.6) +
    annotation_custom(
      photo_grob(v),
      xmin = x0, xmax = x0 + 210, ymin = 195, ymax = 315
    ) +
    geom_text(data = first_pt, aes(label = searches),
              vjust = 2.2, hjust = 0.2, size = 5, colour = text_axes,
              family = my_font, fontface = "bold") +
    geom_text(data = last_pt, aes(label = searches),
              vjust = -1.4, size = 5, colour = text_axes,
              family = my_font, fontface = "bold") +
    scale_x_date(breaks = seq(min(mm$month), max(mm$month), by = "3 months"),
                 date_labels = "%b\n%Y",
                 expand = expansion(mult = c(0.06, 0.10))) +
    scale_y_continuous(limits = c(0, 320), breaks = seq(0, 300, 100),
                       labels = comma_format()) +
    labs(
      title = paste0("\"Moneymaxxing\" searches: ", growth, "x in 18 months"),
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

  ggsave(v$out, p, width = 1080, height = 1080,
         units = "px", dpi = 150, bg = bg_figure)
}

invisible(lapply(variants, build_chart))
