plot_map_anomaly <-
  function(x, x.name, us.cities, animation = FALSE) {
    title.name <- x.name
    temperature <- x

    usa <- sf::st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
    usa <- sf::st_transform(usa, crs = 4326)

    cities2 <- us.cities %>%
      dplyr::select(name, long, lat) %>%
      dplyr::filter(
        name %in% c(
          "Farmington NM",
          "Santa Fe NM",
          "Albuquerque NM",
          "Flagstaff AZ",
          "Phoenix AZ"
        )
      ) %>%
      dplyr::add_row(name = "Mesa Verde CO",
                     long = -108.463050,
                     lat = 37.230299)

    cities <- cities2  %>%
      sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

    crs(temperature) <-
      "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
    temperature <- as.data.frame(temperature, xy = TRUE)
    temperature_transformed <- usmap::usmap_transform(temperature)

    anomaly_plots <- ggplot() +
      geom_raster(
        data = temperature_transformed,
        aes(x = x, y = y, fill = layer),
        alpha = 0.8,
        na.rm = TRUE
      ) +
      scale_fill_gradientn(colors = colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdBu")))(255),
                           limits = c(-8,8),
                           na.value = "transparent",
                           name = "Anomaly °C") +
      coord_equal() +
      theme_classic() +
      scale_x_continuous(breaks = seq(-113,-105.5, 1.0),
                         limits = c(-113,-105.5)) +
      scale_y_continuous(breaks = seq(33.5, 38.0, 0.5),
                         limits = c(33.5, 38.0)) +
      geom_sf(
        data = usa,
        color = "#2b2b2b",
        fill = "transparent",
        size = 0.5
      ) +
      geom_sf(
        data = cities,
        shape = 23,
        fill = "blue",
        size = 3
      ) +
      shadowtext::geom_shadowtext(
        data = cities2,
        aes(x = long, y = lat, label = name),
        nudge_y = 0.15,
        color = "white"
      ) +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle(title.name) +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(
          size = 12,
          colour = "black",
          family = "Helvetica"
        ),
        axis.title.y = element_text(
          size = 20,
          family = "Helvetica",
          margin = margin(
            t = 10,
            r = 10,
            b = 10,
            l = 10
          )
        ),
        axis.title.x = element_text(
          size = 20,
          family = "Helvetica",
          margin = margin(
            t = 10,
            r = 10,
            b = 10,
            l = 10
          )
        ),
        plot.title = element_text(
          hjust = 0.5,
          size = 24,
          family = "Helvetica"
        ),
        panel.grid.minor = element_line(colour = "light grey"),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 13, family = "Helvetica"),
        legend.title = element_text(size = 15, family = "Helvetica")
      )


    if (animation == TRUE) {
      fp <-
        file.path(
          "/Users/andrew/Dropbox/WSU/SKOPEII/Figures/animation_anomaly/animation_2",
          paste0(title.name, ".png")
        )

      ggsave(
        plot = anomaly_plots,
        filename = fp,
        width = 8.5,
        height = 5,
        dpi = 400,
        units = "in",
        device = "png"
      )
    }

    return(anomaly_plots)

  }
