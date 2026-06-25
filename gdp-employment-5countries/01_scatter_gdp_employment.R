# Chart 1: Scatter — GDP per capita vs Employment Rate (2015–2024)
# Insight: High-income economies cluster above 70% employment regardless of growth pace

library(tidyverse)
library(ggtext)
library(scales)

# ── Palette & theme ────────────────────────────────────────────────────────────
bg_plot   <- "#F6EFE8"
bg_figure <- "#FAF8F5"
gridlines <- "#E2D6CB"
text_axes <- "#2B2F33"

palette_warm <- c(
  USA     = "#2B5FB8",
  Germany = "#B83A2F",
  Japan   = "#8F6A00",
  Brazil  = "#13734A",
  Nigeria = "#6B4FA3"
)

my_social_theme <- function(base_size = 14, base_family = "Helvetica") {
  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      plot.background   = element_rect(fill = bg_figure, color = NA),
      panel.background  = element_rect(fill = bg_plot,   color = NA),
      panel.grid.major  = element_line(color = gridlines, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.ticks        = element_blank(),
      axis.text         = element_text(color = text_axes, size = rel(0.85)),
      axis.title        = element_text(color = text_axes, size = rel(0.9)),
      plot.title        = element_text(face = "bold", size = rel(1.4), color = text_axes,
                                       hjust = 0, margin = margin(b = 6)),
      plot.subtitle     = element_markdown(size = rel(0.95), color = "#5A5F65",
                                           margin = margin(b = 14)),
      plot.caption      = element_text(size = rel(0.72), color = "#888888",
                                       hjust = 0, margin = margin(t = 10)),
      plot.margin       = margin(20, 24, 16, 24),
      legend.position   = "none"
    )
}

# ── Fake data ──────────────────────────────────────────────────────────────────
set.seed(42)
countries <- c("USA", "Germany", "Japan", "Brazil", "Nigeria")

gdp_base   <- c(55000, 48000, 42000, 9000, 2200)
emp_base   <- c(71, 75, 74, 62, 55)
gdp_trend  <- c(1800, 1400, 800, 300, 150)
emp_trend  <- c(0.3, 0.4, 0.2, 0.5, 0.6)

df <- map_dfr(seq_along(countries), function(i) {
  tibble(
    country        = countries[i],
    year           = 2015:2024,
    gdp_per_capita = gdp_base[i] + gdp_trend[i] * (2015:2024 - 2015) +
                     rnorm(10, 0, gdp_base[i] * 0.015),
    emp_rate       = emp_base[i] + emp_trend[i] * (2015:2024 - 2015) +
                     rnorm(10, 0, 0.6)
  )
})

# Label only 2024 points
df_labels <- df |> filter(year == 2024)

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(gdp_per_capita, emp_rate, color = country)) +
  geom_path(aes(group = country), linewidth = 0.5, alpha = 0.35) +
  geom_point(aes(size = ifelse(year == 2024, 3.5, 1.8), alpha = ifelse(year == 2024, 1, 0.45))) +
  geom_text(
    data = df_labels,
    aes(label = country),
    hjust = -0.18, vjust = 0.4, size = 3.6, fontface = "bold"
  ) +
  # Delineate: global average employment benchmark
  geom_hline(yintercept = 68, linetype = "dashed", color = "#999999", linewidth = 0.6) +
  annotate("text", x = 6000, y = 68.8, label = "World avg ~68%",
           size = 3.2, color = "#888888", hjust = 0) +
  scale_color_manual(values = palette_warm) +
  scale_size_identity() +
  scale_alpha_identity() +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "k"),
                     expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     limits = c(50, 82)) +
  labs(
    title    = "Wealthier economies sustain higher\nemployment — but gaps are narrowing",
    subtitle = paste0(
      "<span style='color:#2B5FB8'>**USA**</span>  ",
      "<span style='color:#B83A2F'>**Germany**</span>  ",
      "<span style='color:#8F6A00'>**Japan**</span>  ",
      "<span style='color:#13734A'>**Brazil**</span>  ",
      "<span style='color:#6B4FA3'>**Nigeria**</span>"
    ),
    x       = "GDP per capita (USD)",
    y       = "Employment rate (%)",
    caption = "Data: Synthetic data for illustration · github.com/papageorgiou/posts"
  ) +
  my_social_theme()

ggsave(
  "01_scatter_gdp_employment.png",
  plot   = p,
  path   = "/Users/alexp/gd_alpapag/apclients/posts/gdp-employment-5countries",
  width  = 1080, height = 1080, units = "px", dpi = 150, bg = bg_figure
)

message("Chart 1 saved.")
