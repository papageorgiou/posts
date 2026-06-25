# Chart 5: Bubble — GDP per capita vs Employment Rate, sized by population (2024)
# Insight: Nigeria's huge population makes its low employment rate a global labour challenge

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
      plot.background  = element_rect(fill = bg_figure, color = NA),
      panel.background = element_rect(fill = bg_plot,   color = NA),
      panel.grid.major = element_line(color = gridlines, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.ticks       = element_blank(),
      axis.text        = element_text(color = text_axes, size = rel(0.85)),
      axis.title       = element_text(color = text_axes, size = rel(0.9)),
      plot.title       = element_text(face = "bold", size = rel(1.35), color = text_axes,
                                      hjust = 0, margin = margin(b = 6)),
      plot.subtitle    = element_markdown(size = rel(0.95), color = "#5A5F65",
                                          margin = margin(b = 14)),
      plot.caption     = element_text(size = rel(0.72), color = "#888888",
                                      hjust = 0, margin = margin(t = 10)),
      plot.margin      = margin(20, 24, 16, 24),
      legend.position  = "none"
    )
}

# ── Data (2024 snapshot) ───────────────────────────────────────────────────────
df <- tribble(
  ~country,   ~gdp_per_capita, ~emp_rate, ~population_m,
  "USA",      71200,           73.7,      335,
  "Germany",  61200,           77.2,       84,
  "Japan",    49200,           75.8,      123,
  "Brazil",   11700,           66.5,      215,
  "Nigeria",  3550,            60.4,      230
)

world_avg_emp <- 68
world_avg_gdp <- 13500

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(gdp_per_capita, emp_rate, size = population_m, color = country)) +
  # Quadrant shading: highlight bottom-left (low GDP, low employment)
  annotate("rect",
    xmin = 0,    xmax = world_avg_gdp,
    ymin = 50,   ymax = world_avg_emp,
    fill = "#E8D6C8", alpha = 0.5
  ) +
  annotate("text",
    x = 1200, y = 51.5, hjust = 0,
    label = "Low GDP · Low employment",
    size = 3, color = "#B08060", fontface = "italic"
  ) +
  # Reference lines
  geom_vline(xintercept = world_avg_gdp, linetype = "dashed",
             color = "#AAAAAA", linewidth = 0.5) +
  geom_hline(yintercept = world_avg_emp, linetype = "dashed",
             color = "#AAAAAA", linewidth = 0.5) +
  # Bubbles
  geom_point(alpha = 0.78) +
  # Country labels
  geom_text(
    aes(label = paste0(country, "\n", round(population_m), "M")),
    size = 3.3, fontface = "bold",
    vjust = -1.1, lineheight = 0.85
  ) +
  # Highlight callout on Nigeria
  annotate("richtext",
    x = 14000, y = 58,
    label = "**Nigeria** has 230M people<br>but only 60% employment —<br>a structural jobs deficit",
    fill = "#FAF8F5", label.color = "#CCCCCC",
    size = 3.1, color = "#6B4FA3", hjust = 0
  ) +
  scale_color_manual(values = palette_warm) +
  scale_size_continuous(range = c(8, 38)) +
  scale_x_continuous(
    labels = label_dollar(scale = 1e-3, suffix = "k"),
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(50, 82)
  ) +
  labs(
    title    = "Nigeria's 230M population makes its\nemployment gap a global challenge",
    subtitle = paste0(
      "Bubble size = population  ·  ",
      "<span style='color:#2B5FB8'>**USA**</span>  ",
      "<span style='color:#B83A2F'>**Germany**</span>  ",
      "<span style='color:#8F6A00'>**Japan**</span>  ",
      "<span style='color:#13734A'>**Brazil**</span>  ",
      "<span style='color:#6B4FA3'>**Nigeria**</span>"
    ),
    x       = "GDP per capita (USD, 2024)",
    y       = "Employment rate (%)",
    caption = "Data: Synthetic data for illustration · github.com/papageorgiou/posts"
  ) +
  my_social_theme() +
  theme(plot.subtitle = element_markdown(size = rel(0.88), color = "#5A5F65",
                                         margin = margin(b = 14)))

ggsave(
  "05_bubble_gdp_emp_pop.png",
  plot   = p,
  path   = "/Users/alexp/gd_alpapag/apclients/posts/gdp-employment-5countries",
  width  = 1080, height = 1080, units = "px", dpi = 150, bg = bg_figure
)

message("Chart 5 saved.")
