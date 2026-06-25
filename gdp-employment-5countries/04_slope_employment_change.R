# Chart 4: Slope / Dumbbell — Employment rate change 2015 vs 2024
# Insight: Brazil and Nigeria made the biggest employment gains over the decade

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

# ── Data ───────────────────────────────────────────────────────────────────────
df <- tribble(
  ~country,   ~emp_2015, ~emp_2024,
  "USA",      70.8,      73.7,
  "Germany",  73.0,      77.2,
  "Japan",    73.3,      75.8,
  "Brazil",   61.5,      66.5,
  "Nigeria",  55.0,      60.4
) |>
  mutate(
    country = fct_reorder(country, emp_2024 - emp_2015),
    delta   = emp_2024 - emp_2015
  )

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(df) +
  # Connector segment
  geom_segment(
    aes(x = emp_2015, xend = emp_2024, y = country, yend = country,
        color = country),
    linewidth = 1.6, alpha = 0.4
  ) +
  # 2015 point (hollow)
  geom_point(aes(x = emp_2015, y = country, color = country),
             shape = 1, size = 5, stroke = 1.5) +
  # 2024 point (solid)
  geom_point(aes(x = emp_2024, y = country, color = country),
             shape = 19, size = 5) +
  # Left labels: 2015 values
  geom_text(aes(x = emp_2015, y = country,
                label = paste0(emp_2015, "%")),
            hjust = 1.35, size = 3.3, color = "#666666") +
  # Right labels: 2024 values + delta
  geom_text(aes(x = emp_2024, y = country,
                label = paste0(emp_2024, "%  (+", round(delta, 1), "pp)")),
            hjust = -0.15, size = 3.5, fontface = "bold",
            color = text_axes) +
  # Highlight: biggest mover annotated
  annotate("richtext",
    x = 64, y = "Nigeria",
    label = "**Biggest gain:**<br>+5.4 pp over 9 years",
    fill = "#FAF8F5", label.color = "#CCCCCC",
    size = 3.2, color = "#6B4FA3", hjust = 0, vjust = -0.4
  ) +
  scale_color_manual(values = palette_warm) +
  scale_x_continuous(
    labels = label_percent(scale = 1),
    limits = c(52, 84),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  labs(
    title    = "Emerging economies made the biggest\nemployment gains 2015–2024",
    subtitle = "Open circle = 2015  ·  Filled circle = 2024  ·  Change in pp shown",
    x        = "Employment rate (%)",
    y        = NULL,
    caption  = "Data: Synthetic data for illustration · github.com/papageorgiou/posts"
  ) +
  my_social_theme()

ggsave(
  "04_slope_employment_change.png",
  plot   = p,
  path   = "/Users/alexp/gd_alpapag/apclients/posts/gdp-employment-5countries",
  width  = 1080, height = 1080, units = "px", dpi = 150, bg = bg_figure
)

message("Chart 4 saved.")
