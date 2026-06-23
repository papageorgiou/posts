# Recreate the favpng "colorful search" magnifying glass as a clean,
# transparent PNG. Google-colored ring (4 quarter arcs) + a blue handle
# leaning to the lower-right. Used as the inline icon in facet strip labels.

library(ggplot2)
library(grid)

g_red    <- "#EA4335"
g_yellow <- "#FBBC05"
g_green  <- "#34A853"
g_blue   <- "#4285F4"

r  <- 1            # ring radius (centreline)
lw <- 7            # ring/handle stroke width (pt)

# x is mirrored (-cos) so the lens leans to the RIGHT and the handle
# trails to the lower-left.
arc <- function(a0, a1, col, n = 100) {
  a <- seq(a0, a1, length.out = n)
  data.frame(x = -r * cos(a), y = r * sin(a), col = col,
             grp = paste0(col, a0))
}

# Seams at 3/12/9/6 o'clock; quarters centred on the diagonals.
ring <- rbind(
  arc(0,         pi/2,      g_red),     # top-right
  arc(pi/2,      pi,        g_yellow),  # top-left
  arc(pi,        3*pi/2,    g_green),   # bottom-left
  arc(3*pi/2,    2*pi,      g_blue)     # bottom-right
)

# Handle: radial blue bar from the ring out to the lower-right (~ -45 deg).
ang <- -pi/4
handle <- data.frame(
  x = -c(r * cos(ang), 1.95 * cos(ang)),
  y = c(r * sin(ang), 1.95 * sin(ang)),
  col = g_blue, grp = "handle"
)

p <- ggplot() +
  geom_path(data = ring, aes(x, y, group = grp, colour = col),
            linewidth = lw, lineend = "round") +
  geom_path(data = handle, aes(x, y, group = grp, colour = col),
            linewidth = lw, lineend = "round") +
  scale_colour_identity() +
  coord_equal(xlim = c(-2.05, 1.3), ylim = c(-2.05, 1.3),
              expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = NA, colour = NA),
        plot.margin = margin(2, 2, 2, 2))

ggsave("mag_glass.png", p, width = 1.2, height = 1.2, units = "in",
       dpi = 300, bg = "transparent")
message("Wrote mag_glass.png")
