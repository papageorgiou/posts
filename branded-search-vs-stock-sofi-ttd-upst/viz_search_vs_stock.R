# What consumers want vs. what the market thinks — SoFi, The Trade Desk, Upstart
# Branded Google search vs. share price, indexed to 100 at a recent base month.
# The story is the GAP between the two and how it widens through 2026.
# Renders two versions: indexed from Jan 2024 and from Jan 2025.
# Style: dataviz-linkedin skill — Warm Ledger palette, my_social_theme().
# Run from this folder:  Rscript viz_search_vs_stock.R

suppressMessages({
  library(tidyverse)
  library(ggtext)
  library(ggthemes)
  library(ragg)
})

# ---- Warm Ledger palette (from the dataviz-linkedin skill) -------------------
bg_plot   <- "#F6EFE8"   # plot area fill
bg_figure <- "#FAF8F5"   # full figure background
gridlines <- "#E2D6CB"
text_axes <- "#2B2F33"
col_search <- "#B83A2F"  # red    — branded search (consumer demand)
col_price  <- "#1F7A7A"  # teal   — share price (the market)
col_gap    <- "#C05A1A"  # orange — the gap / 2026 highlight

my_font <- "Helvetica Neue"

# ---- Base theme from the skill ----------------------------------------------
my_social_theme <- function(strip_title_size = 1, base_size = 12,
                            base_family = my_font) {
  colors <- tibble::deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      line = element_line(colour = "black"),
      rect = element_rect(fill = colors["White"], linetype = 0, colour = NA),
      text = element_text(colour = colors["Dark Gray"]),
      axis.text  = element_text(),
      axis.ticks = element_blank(),
      axis.line  = element_blank(),
      legend.position = "none",
      panel.grid       = element_line(colour = NULL),
      panel.grid.major = element_line(colour = colors["Medium Gray"]),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0, size = rel(1.5),
                               colour = "gray50", face = "bold"),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      strip.background = element_rect(fill = "white", colour = "black", linewidth = 1),
      strip.text = element_text(size = rel(strip_title_size), face = "bold")
    )
}

# ---- Data --------------------------------------------------------------------
raw <- read_csv("data/monthly_aligned_3companies.csv", show_col_types = FALSE)

# Google Ads Keyword Planner occasionally reports a single-month spike that is a
# data artifact (e.g. TTD branded search = 10,490 in Nov 2024 vs ~600–2,500 in the
# surrounding months, then back to 590). A Hampel filter flags points more than
# t0 median-absolute-deviations from the local median (7-month window). To avoid
# reshaping ordinary month-to-month noise, a point is only replaced if it is ALSO
# at least `ratio`x above / below the local median — so only genuine spikes are
# touched. Applied to branded search only; prices are untouched.
hampel <- function(x, k = 3, t0 = 3, ratio = 2.5) {
  y <- x
  for (i in seq_along(x)) {
    win   <- x[max(1, i - k):min(length(x), i + k)]
    med   <- median(win)
    sigma <- 1.4826 * median(abs(win - med))
    is_outlier <- is.finite(sigma) && sigma > 0 && abs(x[i] - med) > t0 * sigma
    is_extreme <- med > 0 && (x[i] > ratio * med || x[i] < med / ratio)
    if (is_outlier && is_extreme) y[i] <- med
  }
  y
}

raw <- raw |>
  arrange(symbol, month) |>
  group_by(symbol) |>
  mutate(branded_vol = hampel(branded_vol)) |>
  ungroup()

# Per-company labels: logo + one-line descriptor (HTML rendered by ggtext).
meta <- tribble(
  ~symbol, ~order, ~descr,
  "SOFI", 1, "Digital one-stop neobank for young professionals — banking, loans and investing in one app",
  "TTD",  2, "The largest independent demand-side platform for programmatic advertising on the open internet",
  "UPST", 3, "AI lending marketplace that prices consumer loans beyond the traditional FICO score"
) |>
  mutate(label = sprintf(
    "<img src='assets/logos/%s.png' height='26'><br><span style='font-size:9.5pt;color:%s'>%s</span>",
    tolower(symbol), text_axes, descr
  ))

# ---- Chart builder -----------------------------------------------------------
# start_date : first month of the window (also the index base = 100)
# base_label : human label for that base month, e.g. "January 2024"
# sym_order  : company tickers top-to-bottom in the facet stack
make_chart <- function(start_date, base_label, out_file,
                       sym_order = c("SOFI", "TTD", "UPST")) {

  start_date     <- as.Date(start_date)
  highlight_2026 <- as.Date("2026-01-01")
  lvl            <- meta$label[match(sym_order, meta$symbol)]  # facet stack order

  # Long form (for the lines), indexed to 100 at the window's first month.
  long_df <- raw |>
    filter(month >= start_date) |>
    select(symbol, month, `Branded search` = branded_vol, `Share price` = stock_close) |>
    pivot_longer(c(`Branded search`, `Share price`),
                 names_to = "metric", values_to = "value") |>
    group_by(symbol, metric) |>
    arrange(month, .by_group = TRUE) |>
    mutate(index = value / first(value) * 100) |>
    ungroup() |>
    left_join(meta, by = "symbol") |>
    mutate(label = factor(label, levels = lvl))

  # Wide form (for the gap ribbon): one row per company-month.
  wide_df <- long_df |>
    select(symbol, label, month, metric, index) |>
    pivot_wider(names_from = metric, values_from = index) |>
    rename(search = `Branded search`, price = `Share price`) |>
    mutate(lo = pmin(search, price), hi = pmax(search, price))

  # Gap at the final month: dotted connector + label.
  end_df <- wide_df |>
    group_by(symbol, label) |>
    filter(month == max(month)) |>
    ungroup() |>
    mutate(
      mid     = (search + price) / 2,
      gap     = round(search - price),
      gaptext = sprintf("%+d pt\ngap", gap)
    )

  subtitle <- sprintf(
    paste0("<span style='color:%s'>**Branded Google search**</span> (consumer demand) vs ",
           "<span style='color:%s'>**share price**</span> (the market), indexed to 100 at %s. ",
           "<span style='color:%s'>**Shaded gap**</span> widens through 2026."),
    col_search, col_price, base_label, col_gap
  )

  my_caption <- paste0(
    "Source: Google Ads API (Keyword Planner) for branded search;\n",
    "Yahoo Finance for monthly share price (mean daily close). Window: ", base_label, " – May 2026.\n",
    "Indexed to 100 at the first month. Single-month search outliers smoothed (Hampel filter, search series only).\n",
    "Data, code & methodology: github.com/papageorgiou/posts/tree/master/branded-search-vs-stock-sofi-ttd-upst\n",
    "By @alex_papageo"
  )

  p <- ggplot(wide_df, aes(month)) +
    # gap area, full window (neutral) then the 2026 portion highlighted in orange
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = gridlines, alpha = 0.9) +
    geom_ribbon(data = filter(wide_df, month >= highlight_2026 - 31),
                aes(ymin = lo, ymax = hi), fill = col_gap, alpha = 0.28) +
    geom_vline(xintercept = highlight_2026, colour = col_gap,
               linetype = "dotted", linewidth = 0.5, alpha = 0.7) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "#B7AC9E", linewidth = 0.55) +
    # the two series
    geom_line(aes(y = search), colour = col_search, linewidth = 1.5) +
    geom_line(aes(y = price),  colour = col_price,  linewidth = 1.5) +
    # final-month gap: dotted connector + endpoints + label
    geom_segment(data = end_df,
                 aes(x = month, xend = month, y = price, yend = search),
                 linetype = "dotted", colour = col_gap, linewidth = 0.9,
                 arrow = arrow(ends = "both", length = unit(4, "pt"))) +
    geom_point(data = end_df, aes(y = search), colour = col_search, size = 2.3) +
    geom_point(data = end_df, aes(y = price),  colour = col_price,  size = 2.3) +
    geom_text(data = end_df, aes(y = mid, label = gaptext),
              hjust = -0.12, vjust = 0.5, size = 3.5, fontface = "bold",
              colour = col_gap, family = my_font, lineheight = 0.9) +
    facet_wrap(~ label, ncol = 1, scales = "free_y") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                 expand = expansion(mult = c(0.02, 0.16))) +
    scale_y_continuous(labels = scales::label_number()) +
    labs(
      title    = "When what consumers want pulls apart from what the market thinks",
      subtitle = subtitle,
      caption  = my_caption,
      x = NULL, y = sprintf("Indexed to 100 at %s", base_label)
    ) +
    my_social_theme(base_family = my_font) +
    theme(
      plot.background  = element_rect(fill = bg_figure, colour = NA),
      panel.background = element_rect(fill = bg_plot, colour = NA),
      panel.grid.major = element_line(colour = gridlines),
      panel.spacing.y  = unit(1.2, "lines"),
      plot.title    = element_textbox_simple(size = 19, face = "bold", colour = text_axes,
                                       lineheight = 1.12, margin = margin(b = 5)),
      plot.subtitle = element_textbox_simple(size = 12, colour = text_axes,
                                       lineheight = 1.25, margin = margin(b = 12)),
      plot.caption  = element_text(hjust = 0, size = 8.3, colour = "gray45",
                                   face = "italic", lineheight = 1.4,
                                   margin = margin(t = 12)),
      strip.background = element_rect(fill = bg_figure, colour = NA),
      strip.text = element_textbox_simple(
        halign = 0, hjust = 0, lineheight = 1.25,
        padding = margin(2, 2, 6, 0), margin = margin(t = 4, b = 2)
      ),
      axis.text  = element_text(colour = text_axes, size = 10.5),
      axis.title.y = element_text(colour = text_axes, size = 11, margin = margin(r = 6)),
      plot.margin = margin(16, 18, 12, 16)
    )

  agg_png(out_file, width = 1080, height = 1350, units = "px", res = 150,
          background = bg_figure)
  print(p)
  invisible(dev.off())
  cat("Saved", out_file, "\n")
}

make_chart("2024-01-01", "January 2024", "outputs/search_vs_stock_from2024.png")
make_chart("2025-01-01", "January 2025", "outputs/search_vs_stock_from2025.png",
           sym_order = c("TTD", "UPST", "SOFI"))  # SoFi moved to the bottom
