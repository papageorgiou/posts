# Chart 3: Horizontal Bar — Employment rate by country, 2024
# Insight: Germany leads employment at 77%, while Nigeria trails the global average by 13 pts

library(tidyverse)
library(ggtext)
library(scales)

# ── Palette & theme ────────────────────────────────────────────────────────────
bg_plot   <- "#F6EFE8"
bg_figure <- "#FAF8F5"
gridlines <- "#E2D6CB"
text_axes <- "#2B2F33"

my_social_theme <- function(base_size = 14, base_family = "Helvetica") {
  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = bg_figure, color = NA),
      panel.background = element_rect(fill = bg_plot,   color = NA),
      panel.grid.major.x = element_line(color = gridlines, linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks       = element_blank(),
      axis.text        = element_text(color = text_axes, size = rel(0.9)),
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

# ── Data ───────────────────────────────────────────────────────────────────────
df <- tribble(
  ~country,   ~emp_rate,
  "Germany",  77.2,
  "Japan",    75.8,
  "USA",      73.7,
  "Brazil",   66.5,
  "Nigeria",  60.4
) |>
  mutate(
    country   = fct_reorder(country, emp_rate),
    highlight = country == "Germany",
    bar_color = if_else(highlight, "#2B5FB8", "#B0B8C4"),
    label     = paste0(emp_rate, "%")
  )

world_avg <- 68

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(emp_rate, country)) +
  geom_col(aes(fill = bar_color), width = 0.6) +
  geom_text(aes(label = label), hjust = -0.2, size = 4.2, fontface = "bold",
            color = text_axes) +
  # Delineate: world average
  geom_vline(xintercept = world_avg, linetype = "dashed",
             color = "#888888", linewidth = 0.7) +
  annotate("text", x = world_avg + 0.4, y = 0.45,
           label = paste0("World avg\n", world_avg, "%"),
           size = 3.2, color = "#888888", hjust = 0, vjust = 0) +
  scale_fill_identity() +
  scale_x_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 88),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Germany leads employment in 2024 —\nNigeria sits 13 points below the world average",
    subtitle = "Share of working-age population (15–64) in employment, 2024",
    x        = "Employment rate",
    y        = NULL,
    caption  = "Data: Synthetic data for illustration · github.com/papageorgiou/posts"
  ) +
  my_social_theme()

ggsave(
  "03_bar_employment_2024.png",
  plot   = p,
  path   = "/Users/alexp/gd_alpapag/apclients/posts/gdp-employment-5countries",
  width  = 1080, height = 1080, units = "px", dpi = 150, bg = bg_figure
)

message("Chart 3 saved.")
