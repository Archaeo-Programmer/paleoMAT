interpolate_time_plots <-
  function(fit,
           eliminated,
           agemin_rounded,
           agemax_rounded,
           std.err = TRUE) {
    if (std.err == TRUE) {
      plot_fit <- ggplot(data = fit,
                         aes(x = date,
                             y = value)) +
        geom_ribbon(
          data = fit,
          aes(
            x = date,
            ymin = value - 1.96 *  se.fit,
            ymax = value + 1.96 * se.fit,
            alpha = 0.2
          ),
          fill = "grey",
          colour = "dark grey",
          show.legend = F
        ) +
        geom_line(colour = "red", size = 1.0) +
        geom_point(data = eliminated, aes(x = date,
                                          y = value)) +
        xlab("Year BC/AD") +
        ylab("Temperature °C") +
        scale_x_continuous(
          breaks = seq(agemin_rounded, agemax_rounded, 500),
          minor_breaks = seq((agemin_rounded + 250), (agemax_rounded -
                                                        250), 500)
        ) +
        scale_y_continuous(breaks = seq((
          DescTools::RoundTo(
            min(fit$value - 1.96 * fit$se.fit),
            multiple = 1.0,
            FUN = floor
          )
        ), (
          DescTools::RoundTo(
            max(fit$value + 1.96 * fit$se.fit),
            multiple = 1.0,
            FUN = ceiling
          )
        ), 3.0)) +
        theme_bw() +
        theme(
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(10.0, 10.0, 10.0, 10.0),
          axis.text = element_text(
            size = 10,
            colour = "black",
            family = "Helvetica"
          ),
          axis.title.y = element_text(
            size = 12,
            family = "Helvetica",
            margin = margin(
              t = 10,
              r = 20,
              b = 10,
              l = 10
            )
          ),
          axis.title.x = element_text(
            size = 12,
            family = "Helvetica",
            margin = margin(
              t = 10,
              r = 10,
              b = 10,
              l = 10
            )
          ),
          panel.grid.major = element_line(colour = "light grey"),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10, family = "Helvetica"),
          legend.title = element_text(size = 11, family = "Helvetica")
        )

    } else {
      # Now, plot the results. Save as a ggplot object and will return with the rest of the data and models.
      plot_fit <- ggplot(data = fit,
                         aes(x = date,
                             y = value)) +
        geom_line(colour = "red", size = 1.0) +
        geom_point(data = eliminated, aes(x = date,
                                          y = value)) +
        xlab("Year BC/AD") +
        ylab("Temperature °C") +
        scale_x_continuous(
          breaks = seq(agemin_rounded, agemax_rounded, 500),
          minor_breaks = seq((agemin_rounded + 250), (agemax_rounded -
                                                        250), 500)
        ) +
        scale_y_continuous(breaks = seq((
          DescTools::RoundTo(min(fit$value), multiple = 1.0, FUN = floor)
        ), (
          DescTools::RoundTo(max(fit$value), multiple = 1.0, FUN = ceiling)
        ), 3.0)) +
        theme_bw() +
        theme(
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(10.0, 10.0, 10.0, 10.0),
          axis.text = element_text(
            size = 10,
            colour = "black",
            family = "Helvetica"
          ),
          axis.title.y = element_text(
            size = 12,
            family = "Helvetica",
            margin = margin(
              t = 10,
              r = 20,
              b = 10,
              l = 10
            )
          ),
          axis.title.x = element_text(
            size = 12,
            family = "Helvetica",
            margin = margin(
              t = 10,
              r = 10,
              b = 10,
              l = 10
            )
          ),
          panel.grid.major = element_line(colour = "light grey"),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10, family = "Helvetica"),
          legend.title = element_text(size = 11, family = "Helvetica")
        )
    }
  }
