# Created: 2026-06-22
# Purpose: Plain base-R small-multiples of the "experiential X" search-interest
#          trends. Deliberately unstyled -- the way a statistician would have
#          knocked this out in R ~2008: black lines, default fonts, no colour,
#          no icons. Three layouts: 3x3 (9 niches), 3x2 and 2x3 (6 niches).
#          Title, subtitle and caption are kept from the styled version.

setwd("/Users/alexp/gd_alpapag/apclients/posts/experiential")
Sys.setlocale("LC_TIME", "C")   # parse English month names ("May 1, 2019")

# ------------------------------------------------------------------
# 1. Parse the markdown tables (same pipeline as the other scripts)
# ------------------------------------------------------------------
activity_from_file <- function(path) {
  stem <- sub(" - Explore\\.md$", "", basename(path))
  if (stem == "experiential") return("Experiential")
  niche <- sub("^experiential ", "", stem)
  paste0(toupper(substring(niche, 1, 1)), substring(niche, 2))
}

parse_explore <- function(path) {
  activity <- activity_from_file(path)
  lines <- readLines(path, warn = FALSE)
  rows  <- grep("^\\|.*\\|[[:space:]]*[0-9]+[[:space:]]*\\|[[:space:]]*$",
                lines, value = TRUE)
  rows  <- rows[!grepl("---", rows)]
  rows  <- rows[!grepl("\\| *x *\\|", rows)]
  parts <- strsplit(rows, "\\|")
  bidi <- "[\u200E\u200F\u202A-\u202E\u2066-\u2069]"
  date_str <- vapply(parts, function(p) gsub(bidi, "", trimws(p[2])),
                     character(1))
  value    <- vapply(parts, function(p) as.integer(trimws(p[3])), integer(1))
  out <- data.frame(activity = activity,
                    date  = as.Date(date_str, format = "%b %d, %Y"),
                    value = value, stringsAsFactors = FALSE)
  out[!is.na(out$date), ]
}

files    <- list.files(pattern = " - Explore\\.md$")
combined <- do.call(rbind, lapply(files, parse_explore))
combined <- combined[combined$date >= as.Date("2024-01-01") &
                     combined$date <= as.Date("2026-05-01"), ]
combined <- combined[order(combined$activity, combined$date), ]

# 3-month right-aligned rolling average (base R, no zoo)
roll3 <- function(x) {
  out <- rep(NA_real_, length(x))
  for (i in 3:length(x)) out[i] <- mean(x[(i - 2):i])
  out
}
combined$value_roll <- ave(combined$value, combined$activity, FUN = roll3)
combined <- combined[!is.na(combined$value_roll), ]

# ------------------------------------------------------------------
# 2. Labels / text
# ------------------------------------------------------------------
grid_order <- c("Education", "Marketing", "Retail", "Design", "Wellness",
                "Travel", "Dating", "Entertainment", "Music")
niches6 <- c("Education", "Marketing", "Retail", "Design", "Wellness", "Travel")

title_txt <- "The Rise of Experiential"
sub_txt   <- "Google search interest is climbing across nearly every experiential category."
# Caption split across short lines so it can be set at a larger size without
# running off the right edge of the narrower layouts.
cap1 <- "Source: Google Trends, Worldwide. Search interest 0-100, normalized to each term's own peak."
cap2 <- "Monthly data May 19-May 26. Focus window: Jan 24-May 26, 3-m rolling average, retrieved Jun 2026."
cap3 <- "Data, code & analysis: github.com/papageorgiou/posts"

xticks <- seq(as.Date("2024-01-01"), as.Date("2026-01-01"), by = "year")

# ------------------------------------------------------------------
# 3. Draw one layout
# ------------------------------------------------------------------
# title_cex / sub_cex / facet_cex let callers dial the headline sizes;
# the top outer margin and per-panel top margin grow with them so the
# bigger text never collides.
draw_panels <- function(niches, nr, nc, cap_cex = 0.85,
                        title_cex = 2.2, sub_cex = 1.2, facet_cex = 1.7,
                        yaxis_cex = 1.0, ylab_cex = 1.05, xaxis_cex = 1.05,
                        oma_top = 7, oma_bottom = 7, mar_top = 2.8,
                        title_line = 4.1, sub_line = 1.6) {
  op <- par(mfrow = c(nr, nc),
            oma = c(oma_bottom, 4.8, oma_top, 1.5), # outer: title top, caption bottom
            mar = c(2.6, 2.2, mar_top, 0.8),        # per-panel
            mgp = c(2, 0.6, 0), tcl = -0.3,
            cex.axis = 0.8, las = 1, xpd = NA)  # xpd=NA: titles may spill into gaps
  on.exit(par(op))

  for (a in niches) {
    d <- combined[combined$activity == a, ]
    plot(d$date, d$value_roll, type = "l", lwd = 1.5,
         ylim = c(0, 100), xlab = "", ylab = "", axes = FALSE)
    axis.Date(1, at = xticks, format = "%Y", cex.axis = xaxis_cex)
    axis(2, at = c(0, 50, 100), cex.axis = yaxis_cex)
    box()
    title(main = paste("experiential", tolower(a)), font.main = 2,
          cex.main = facet_cex)
  }

  # Outer annotations
  mtext(title_txt, side = 3, outer = TRUE, line = title_line, adj = 0,
        cex = title_cex, font = 2)
  mtext(sub_txt, side = 3, outer = TRUE, line = sub_line, adj = 0, cex = sub_cex)
  mtext("Relative Search interest (peak = 100)", side = 2, outer = TRUE, line = 1.8,
        cex = ylab_cex, las = 0)
  mtext(cap1, side = 1, outer = TRUE, line = 3.0, adj = 0, cex = cap_cex)
  mtext(cap2, side = 1, outer = TRUE, line = 4.2, adj = 0, cex = cap_cex)
  mtext(cap3, side = 1, outer = TRUE, line = 5.4, adj = 0, cex = cap_cex)
}

# ------------------------------------------------------------------
# 4. Render the three layouts
# ------------------------------------------------------------------
layouts <- list(
  # xl_facet: facet-title size for the XL variant (3x3 is narrowest and holds
  # the long "experiential entertainment" label, so it needs a touch smaller).
  # xl_facet sized to the widest label each layout must hold without clipping:
  # 3x3 holds "experiential entertainment" in narrow 3-col panels, so it stays
  # smallest; the 2-col 2x3 has the widest panels so it goes biggest.
  list(suffix = "3x3", niches = grid_order, nr = 3, nc = 3, w = 9,   h = 11,  xl_facet = 2.05, xl_sub = 1.5),
  list(suffix = "3x2", niches = niches6,    nr = 2, nc = 3, w = 9,   h = 8.2, xl_facet = 2.3,  xl_sub = 1.5),
  list(suffix = "2x3", niches = niches6,    nr = 3, nc = 2, w = 7,   h = 10,  xl_facet = 2.6,  xl_sub = 1.0)
)

# Narrow (w < 8) layouts get a slightly smaller caption so the source line fits.
std_cap <- function(w) if (w < 8) 0.78 else 0.9
xl_cap  <- function(w) if (w < 8) 0.82 else 1.1

for (L in layouts) {
  fn <- paste0("experiential_base_", L$suffix, ".png")
  png(fn, width = L$w * 150, height = L$h * 150, res = 150, bg = "white")
  draw_panels(L$niches, L$nr, L$nc, cap_cex = std_cap(L$w),
              sub_cex = if (L$w < 8) 1.0 else 1.2)
  dev.off()
  message("Wrote ", fn)
}

# XL-title variants: same layouts, bigger headline + facet titles for
# maximum legibility in a LinkedIn feed. Taller top margin to fit them.
for (L in layouts) {
  fn <- paste0("experiential_base_", L$suffix, "_xl.png")
  png(fn, width = L$w * 150, height = (L$h + 1.5) * 150, res = 150, bg = "white")
  draw_panels(L$niches, L$nr, L$nc, cap_cex = xl_cap(L$w),
              title_cex = 3.1, sub_cex = L$xl_sub, facet_cex = L$xl_facet,
              yaxis_cex = 1.3, ylab_cex = 1.6, xaxis_cex = 1.35,
              oma_top = 9, oma_bottom = 8.6, mar_top = 3.9,
              title_line = 5.4, sub_line = 2)
  dev.off()
  message("Wrote ", fn)
}
