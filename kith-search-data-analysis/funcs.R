
theme_alex <- function () {
  theme_minimal(base_size = 11.5) %+replace%
    theme(
      # add padding to the plot:
      #plot.margin = unit(rep(0.5, 4), "cm"),  #plot.margin = unit(c(1, 1, 1, 1), "lines")


      # remove the plot background and border
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),

      # make the legend and strip background transparent
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.position = "top",
      legend.title = element_blank(),

      # strip
      strip.background = element_rect(fill = "transparent", colour = NA, size = 1),#element_rect(fill = "white")  - BBC
      strip.text = element_text(size = 12, colour = "#757575", face = "plain"),
      #strip.background = element_rect(fill="white", colour="black", size=1), # trends linkedin


      # add light, dotted major grid lines only
      panel.grid.major.y = element_line(linetype = "dotted", colour = "gray50", size = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      plot.title=element_text(face = "bold", hjust = 0.02, margin = margin(b = 4)),

      # remove the axis tick marks and hide axis lines
      #axis.ticks.y  = element_blank(),
      axis.ticks = element_blank(),
      #axis.line.x  = element_line(color = "gray80", linetype="solid", size = 0.3),
      axis.line.y  = element_blank(),

      # modify the bottom margins of the title and subtitle
      # plot.title = element_text(size = 18, colour = "#757575", hjust = 0, margin = margin(b = 4)),
      # plot.subtitle = element_text(size = 12, colour = "#757575", hjust = 0, margin = margin(b = 10)),

      # change to Open Sans for axes titles, tick labels, legend title and legend key, and strip text
     # axis.title = element_blank(),
      axis.title = element_text( size = 11, colour = "#757575", face = "plain", hjust = 1),
      axis.text = element_text( size = 10, colour = "#757575", face = "plain"),
      legend.text = element_text(size = 10, colour = "#757575")
    )
}


#' plot by category
#'

#'
#' @return a ggplot graph
#' @export
#'
plot_by_category <- function(data, my_title="This is a good title" ,
                             my_subtitle="this is a subtitle", n_rows=3,
                             time_col="month", ts_col="roll_avg_round", colour_facet_group="Keyword",
                             si_accuracy=0.1, my_strip_title_size=1,
                             my_caption = NULL,
                             my_font = "Segoe UI")
{

  data %>%
    ggplot(aes(x= .data[[time_col]], y= .data[[ts_col]], colour= .data[[colour_facet_group]])) +
    geom_line(alpha = 0.9, size = 1.8) +
    geom_smooth(method = "lm", se = F, linetype=3,  alpha=0.3) +
    expand_limits(y = NULL) +
    facet_wrap(~.data[[colour_facet_group]], scales = "free_y", nrow = n_rows, labeller = label_wrap_gen(width=20)) +
    # scale_y_continuous(labels = scales::label_number_si(accuracy=si_accuracy))
    scale_y_continuous(labels  = scales::comma_format()) +
    labs(title = my_title, subtitle = my_subtitle, caption = my_caption)  +
    ylab("Search Interest by month - Google US \n") + xlab(label = NULL)    #+ scale_color_wsj() a
  #my_social_theme(strip_title_size = my_strip_title_size, base_family=my_font)


}

#'

#'
#' @return a ggplot graph
#' @export
#'

library(ggthemes)
my_social_theme <- function (strip_title_size, base_size = 12, base_family = "Roboto Condensed")  # "Roboto Condensed" sans "Gidole" # Segoe UI
{
  colors <- deframe(ggthemes::ggthemes_data[["fivethirtyeight"]]) #
  (theme_foundation(base_size = base_size, base_family = base_family) +
      theme(line = element_line(colour = "black"),
            rect = element_rect(fill = colors["White"],
                                linetype = 0, colour = NA),
            text = element_text(colour = colors["Dark Gray"]),
            #axis.title = element_blank(),
            axis.text = element_text(),
            axis.ticks = element_blank(), axis.line = element_blank(),
            legend.background = element_rect(), legend.position = "none",
            legend.direction = "horizontal", legend.box = "vertical",
            panel.grid = element_line(colour = NULL),
            panel.grid.major = element_line(colour = colors["Medium Gray"]),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0,  size = rel(1.5), colour="gray50",  face = "bold"),
            plot.margin = unit(c(1,  1, 1, 1), "lines"),
            #strip.background = element_rect(),
            strip.background = element_rect(fill="white", colour="black",size=1),
            strip.text = element_text(size=rel(strip_title_size), face = "bold")))
}
