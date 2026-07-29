# LinkedIn charts: AI job title search demand, US, Aug 2022 - Jul 2026.
# Source data: Google Ads Keyword Planner API. Pipeline: github.com/papageorgiou/ai-jobs
#
# Three standalone options, one per post angle. Each is designed to work alone
# in the feed, so each carries its own annotation rather than relying on caption
# text to explain it.

library(arrow)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggrepel)
library(scales)
library(tidyr)

# Run from this directory: Rscript charts.R

# --- Warm Ledger palette ------------------------------------------------------
bg_plot   <- "#F6EFE8"
bg_figure <- "#FAF8F5"
gridlines <- "#E2D6CB"
text_axes <- "#2B2F33"

pal <- c(blue = "#2B5FB8", red = "#B83A2F", yellow = "#8F6A00", green = "#13734A",
         purple = "#6B4FA3", teal = "#1F7A7A", orange = "#C05A1A", slate = "#4E5A63")

# Mobile-first: everything scales off a large base so text survives the feed.
theme_li <- function(base = 15) {
  theme_minimal(base_size = base) +
    theme(
      plot.background  = element_rect(fill = bg_figure, colour = NA),
      panel.background = element_rect(fill = bg_plot, colour = NA),
      panel.grid.major = element_line(colour = gridlines, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      text             = element_text(colour = text_axes),
      axis.text        = element_text(size = rel(0.85)),
      plot.title       = element_markdown(size = rel(1.6), face = "bold", lineheight = 1.2,
                                          margin = margin(b = 8)),
      plot.subtitle    = element_textbox_simple(size = rel(0.95), colour = "grey25",
                                                lineheight = 1.4, margin = margin(b = 18)),
      plot.caption     = element_text(size = rel(0.62), colour = "grey45", hjust = 0,
                                      lineheight = 1.3),
      legend.position  = "none",
      plot.margin      = margin(24, 26, 18, 24)
    )
}

CAP <- paste0("Source: Google Ads Keyword Planner, United States, Aug 2022 - Jul 2026.\n",
              "Retrieved 29 Jul 2026. Volumes are rounded buckets, not exact counts.\n",
              "Code and data: github.com/papageorgiou/posts")

rolling  <- read_parquet("data/rolling.parquet")
clusters <- read_parquet("data/clusters.parquet")
rolling  <- rolling |> left_join(select(clusters, keyword, intent), by = "keyword")

save_li <- function(p, name, w, h) {
  ggsave(name, p, width = w, height = h, dpi = 150)
  message("wrote ", name, " (", w * 150, "x", h * 150, ")")
}

# =============================================================================
# A. Prompt engineer peaked
# =============================================================================
kws <- c("prompt engineer", "context engineer", "ai engineer")
d <- rolling |> filter(keyword %in% kws)

peak <- d |> filter(keyword == "prompt engineer") |> slice_max(roll_avg, n = 1)
last <- d |> group_by(keyword) |> slice_max(month, n = 1) |> ungroup()

pA <- ggplot(d, aes(month, roll_avg, colour = keyword)) +
  geom_line(linewidth = 1.6) +
  geom_point(data = peak, size = 3.6) +
  # Annotation 1 (label): mark the peak explicitly, since the whole claim rests
  # on it. Anchored to the right of the peak so it cannot clip the left margin.
  annotate("richtext", x = peak$month + 75, y = 92000, hjust = 0, size = 4.3,
           label = paste0("**Peak ", label_comma()(peak$roll_avg), "/mo**<br>June 2023"),
           fill = NA, label.colour = NA, colour = pal[["red"]], lineheight = 1.3) +
  annotate("segment", x = peak$month + 65, xend = peak$month + 10,
           y = 88000, yend = peak$roll_avg * 1.15, colour = pal[["red"]], linewidth = 0.4) +
  # Annotation 2: the size of the fall is the point, so state it.
  annotate("richtext", x = as.Date("2025-10-01"), y = 62000, hjust = 0.5, size = 4.5,
           label = "**Down ~70%**", fill = NA, label.colour = NA, colour = pal[["red"]]) +
  annotate("richtext", x = as.Date("2024-10-01"), y = 700, hjust = 0.5, size = 4.2,
           label = "**96x in one year**", fill = NA, label.colour = NA,
           colour = pal[["green"]]) +
  geom_text_repel(data = last, aes(label = keyword), hjust = 0, nudge_x = 50,
                  direction = "y", size = 4.1, fontface = "bold", min.segment.length = 0,
                  segment.colour = "grey60", box.padding = 0.5, seed = 1) +
  scale_colour_manual(values = c(`prompt engineer` = pal[["red"]],
                                 `context engineer` = pal[["green"]],
                                 `ai engineer` = pal[["blue"]])) +
  scale_y_log10(labels = label_comma(), breaks = c(100, 1000, 10000, 100000)) +
  # Explicit breaks: the wide right expansion that makes room for the series
  # labels would otherwise draw a 2028 tick into empty space.
  scale_x_date(date_labels = "%Y", breaks = as.Date(paste0(2023:2026, "-01-01")),
               expand = expansion(mult = c(0.03, 0.42))) +
  labs(
    title = "The hottest AI job title<br>already peaked",
    subtitle = glue::glue(
      "US Google searches. <span style='color:{pal[['red']]}'>**Prompt engineer**</span> topped out in June 2023. ",
      "<span style='color:{pal[['green']]}'>**Context engineer**</span> barely existed a year ago. ",
      "<span style='color:{pal[['blue']]}'>**AI engineer**</span> never spiked, and just overtook both."),
    x = NULL, y = "Searches per month (log scale)", caption = CAP) +
  theme_li()
save_li(pA, "post-a-prompt-engineer-peaked.png", 7.2, 7.2)

# =============================================================================
# B. Tool intent vs career intent
# =============================================================================
split_ts <- rolling |> group_by(intent, month) |>
  summarise(searches = sum(searches), .groups = "drop")

ends <- split_ts |> group_by(intent) |> slice_max(month, n = 1) |> ungroup()

pB <- ggplot(split_ts, aes(month, searches, colour = intent)) +
  geom_line(linewidth = 1.7) +
  annotate("richtext", x = as.Date("2024-02-01"), y = 375000, hjust = 0, size = 4.6,
           label = paste0("**71% of all the volume**<br>is people looking for<br>",
                          "*software*, not a job"),
           fill = NA, label.colour = NA, colour = pal[["red"]], lineheight = 1.4) +
  annotate("richtext", x = as.Date("2024-02-01"), y = 45000, hjust = 0, size = 4.4,
           label = "**Actual career searches**<br>are the smaller line",
           fill = NA, label.colour = NA, colour = pal[["slate"]], lineheight = 1.4) +
  geom_text_repel(data = ends,
                  aes(label = c("career", "tool")[match(intent, c("career", "tool-risk"))]),
                  hjust = 0, nudge_x = 60, direction = "y", size = 4.6,
                  fontface = "bold", segment.colour = "grey60", seed = 2) +
  scale_colour_manual(values = c(career = pal[["slate"]], `tool-risk` = pal[["red"]])) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_date(date_labels = "%Y", expand = expansion(mult = c(0.03, 0.16))) +
  labs(
    title = "Most \"AI job title\" searches<br>are not about jobs",
    subtitle = glue::glue(
      "283 AI job titles, split by what the searcher actually wants: ",
      "<span style='color:{pal[['red']]}'>**an AI tool that does the job**</span> ",
      "(ai photo editor, ai lawyer) versus ",
      "<span style='color:{pal[['slate']]}'>**a career doing the job**</span> ",
      "(ai engineer, prompt engineer)."),
    x = NULL, y = "Searches per month", caption = CAP) +
  theme_li()
save_li(pB, "post-b-tool-not-job.png", 7.2, 7.2)

# =============================================================================
# C. Two-thirds of AI job titles have no demand
# =============================================================================
# A dot grid reads faster than a bar pair: the eye counts the coloured block
# against the grey field without needing the axis.
TOTAL <- 1052; WITH_VOL <- 390; NCOL <- 34

grid <- tibble(i = seq_len(TOTAL)) |>
  mutate(has_vol = i <= WITH_VOL,
         col = (i - 1) %% NCOL,
         row = (i - 1) %/% NCOL)

pC <- ggplot(grid, aes(col, -row, fill = has_vol)) +
  geom_tile(width = 0.82, height = 0.82) +
  annotate("richtext", x = NCOL / 2 - 0.5, y = 1.9, hjust = 0.5, size = 4.8,
           label = paste0("<span style='color:", pal[["blue"]],
                          "'>**", WITH_VOL, " titles**</span> that anyone actually searches for"),
           fill = NA, label.colour = NA, lineheight = 1.3) +
  annotate("richtext", x = NCOL / 2 - 0.5, y = -(TOTAL %/% NCOL) - 2.6, hjust = 0.5, size = 4.8,
           label = paste0("<span style='color:#9C8F84'>**", TOTAL - WITH_VOL,
                          " titles**</span> with no measurable search demand at all"),
           fill = NA, label.colour = NA, lineheight = 1.3) +
  scale_fill_manual(values = c(`TRUE` = pal[["blue"]], `FALSE` = "#DCD2C8")) +
  coord_equal(clip = "off") +
  labs(
    title = "Two-thirds of AI job titles<br>are invented vocabulary",
    subtitle = paste("Each square is one AI job title collected from published lists of",
                     "\"AI jobs of the future\". Coloured squares are the ones with any",
                     "US Google search volume over the last four years."),
    x = NULL, y = NULL, caption = CAP) +
  theme_li() +
  # panel.grid alone does not clear major, which theme_li() sets explicitly.
  theme(axis.text = element_blank(),
        panel.grid = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_rect(fill = bg_figure, colour = NA),
        plot.margin = margin(24, 26, 18, 24))
save_li(pC, "post-c-invented-vocabulary.png", 7.2, 9)

message("\ndone")
