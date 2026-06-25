# Chart 2: Line — GDP per capita growth 2015–2024 by country
# Insight: USA and Germany doubled their GDP gap over Nigeria in a decade

library(tidyverse)
library(ggtext)
library(scales)
library(gghighlight)

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
      plot.background  = element_rect(fill = bg_figure, color = NA),
      panel.background = element_rect(fill = bg_plot,   color = NA),
      panel.grid.major = element_line(color = gridlines, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.ticks       = element_blank(),
      axis.text        = element_text(color = text_axes, size = rel(0.85)),
      axis.title       = element_text(color = text_axes, size = rel(0.9)),
      plot.title       = element_text(face = "bold", size = rel(1.4), color = text_axes,
                                      hjust = 0, margin = margin(b = 6)),
      plot.subtitle    = element_markdown(size = rel(0.95), color = "#5A5F65",
                                          margin = margin(b = 14)),
      plot.caption     = element_text(size = rel(0.72), color = "#888888",
                                      hjust = 0, margin = margin(t = 10)),
      plot.margin      = margin(20, 24, 16, 24),
      legend.position  = "none"
    )
}

# ── Fake data ──────────────────────────────────────────────────────────────────
set.seed(42)
countries  <- c("USA", "Germany", "Japan", "Brazil", "Nigeria")
gdp_base   <- c(55000, 48000, 42000, 9000, 2200)
gdp_trend  <- c(1800, 1400, 800, 300, 150)

df <- map_dfr(seq_along(countries), function(i) {
  tibble(
    country        = countries[i],
    year           = 2015:2024,
    gdp_per_capita = gdp_base[i] + gdp_trend[i] * (2015:2024 - 2015) +
                     rnorm(10, 0, gdp_base[i] * 0.015)
  )
})

# End-of-line labels
df_end <- df |> filter(year == 2024)

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(year, gdp_per_capita, color = country, group = country)) +
  geom_line(linewidth = 1.8, alpha = 0.9) +
  geom_point(data = df_end, size = 3.5) +
  geom_text(
    data = df_end,
    aes(label = paste0(country, "\n", label_dollar(scale = 1e-3, suffix = "k")(gdp_per_capita))),
    hjust   = -0.12,
    size    = 3.4,
    fontface = "bold",
    lineheight = 0.9
  ) +
  # Bracket annotation: highlight USA–Nigeria gap in 2024
  annotate("segment",
    x = 2024.05, xend = 2024.05,
    y = df_end$gdp_per_capita[df_end$country == "Nigeria"],
    yend = df_end$gdp_per_capita[df_end$country == "USA"],
    color = "#888888", linewidth = 0.6,
    arrow = arrow(ends = "both", length = unit(4, "pt"), type = "open")
  ) +
  annotate("text", x = 2024.3, y = mean(c(2200 + 150*9, 55000 + 1800*9)),
           label = "~$53k gap", size = 3, color = "#666666", hjust = 0) +
  scale_color_manual(values = palette_warm) +
  scale_x_continuous(breaks = seq(2015, 2024, 3),
                     expand = expansion(mult = c(0.02, 0.22))) +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "k"),
                     expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title    = "The GDP gap between rich and poor nations\nwidened through the decade",
    subtitle = paste0(
      "<span style='color:#2B5FB8'>**USA**</span>  ",
      "<span style='color:#B83A2F'>**Germany**</span>  ",
      "<span style='color:#8F6A00'>**Japan**</span>  ",
      "<span style='color:#13734A'>**Brazil**</span>  ",
      "<span style='color:#6B4FA3'>**Nigeria**</span>"
    ),
    x       = NULL,
    y       = "GDP per capita (USD)",
    caption = "Data: Synthetic data for illustration · github.com/papageorgiou/posts"
  ) +
  my_social_theme()

ggsave(
  "02_line_gdp_growth.png",
  plot   = p,
  path   = "/Users/alexp/gd_alpapag/apclients/posts/gdp-employment-5countries",
  width  = 1080, height = 1080, units = "px", dpi = 150, bg = bg_figure
)

message("Chart 2 saved.")
