source("data.R")

weather$time <- as.POSIXct(weather$time, format = "%Y-%m-%dT%H:%M", tz = "America/Juneau")

find_event_runs <- \(condition, label, minimum_hours) {
  event_hours <- which(!is.na(condition) & condition)
  if (length(event_hours) == 0) {
    return(data.frame(
      start = as.POSIXct(character(), tz = "America/Juneau"),
      end = as.POSIXct(character(), tz = "America/Juneau"),
      midpoint = as.POSIXct(character(), tz = "America/Juneau"),
      label = character(),
      severity = numeric()
    ))
  }

  starts_new_run <- c(
    TRUE,
    diff(event_hours) != 1 |
      as.numeric(diff(weather$time[event_hours]), units = "secs") != 3600
  )
  runs <- split(event_hours, cumsum(starts_new_run))
  runs <- Filter(\(run) length(run) >= minimum_hours, runs)
  if (length(runs) == 0) {
    return(data.frame(
      start = as.POSIXct(character(), tz = "America/Juneau"),
      end = as.POSIXct(character(), tz = "America/Juneau"),
      midpoint = as.POSIXct(character(), tz = "America/Juneau"),
      label = character(),
      severity = numeric()
    ))
  }

  do.call(rbind, lapply(runs, \(run) {
    start <- weather$time[min(run)]
    end <- weather$time[max(run)] + 3600
    data.frame(
      start = start,
      end = end,
      midpoint = start + as.numeric(difftime(end, start, units = "secs")) / 2,
      label = label,
      severity = sum(weather$snowfall..cm.[run], na.rm = TRUE)
    )
  }))
}

blizzard_conditions <- with(
  weather,
  snowfall..cm. > 0 & wind_speed_10m..km.h. >= 56.3
)
blizzard_events <- find_event_runs(blizzard_conditions, "Blizzard-like", 3)
heavy_snow_conditions <- weather$snowfall..cm. >= 2.5 & !blizzard_conditions
heavy_snow_events <- find_event_runs(heavy_snow_conditions, "Heavy snow", 2)
weather_events <- rbind(blizzard_events, heavy_snow_events)

if (nrow(weather_events) > 0) {
  weather_events$year <- format(weather_events$start, "%Y")
  label_events <- do.call(rbind, lapply(
    split(weather_events, interaction(weather_events$label, weather_events$year)),
    \(events) events[which.max(events$severity), ]
  ))
} else {
  label_events <- weather_events
}
label_events$event_type <- label_events$label
label_events$label <- paste0(label_events$label, "\n", format(label_events$start, "%Y"))
label_events$label_x <- label_events$midpoint +
  ifelse(seq_len(nrow(label_events)) %% 2 == 0, -30, 30) * 86400
label_events$label_y <- max(weather$temperature_2m...C., na.rm = TRUE) +
  ifelse(seq_len(nrow(label_events)) %% 2 == 0, 1, 6)

weather_theme <- ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#101A2B", color = NA),
    panel.background = ggplot2::element_rect(fill = "#101A2B", color = NA),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "#29384D"),
    text = ggplot2::element_text(color = "#E8F1FA"),
    axis.text = ggplot2::element_text(color = "#B7C8D9"),
    plot.title = ggplot2::element_text(face = "bold", size = 20)
  )

flag_stars <- data.frame(
  x = c(0.22, 0.37, 0.39, 0.24, 0.53, 0.66, 0.77, 0.84),
  y = c(0.28, 0.28, 0.51, 0.50, 0.56, 0.64, 0.74, 0.84)
)

title_plot <- ggplot2::ggplot() +
  ggplot2::annotate(
    "text",
    x = 0,
    y = 0.5,
    label = "Five Years of Juneau Weather",
    hjust = 0,
    color = "#E8F1FA",
    fontface = "bold",
    size = 7
  ) +
  ggplot2::annotate(
    "text",
    x = 0,
    y = 0.08,
    label = if (nrow(weather_events) > 0) {
      "Shaded bands mark events; labels identify each event type's strongest episode per year"
    } else {
      "No blizzard-like or sustained heavy-snow events detected"
    },
    hjust = 0,
    color = "#B7C8D9",
    size = 3
  ) +
  ggplot2::xlim(0, 1) +
  ggplot2::ylim(0, 1) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#101A2B", color = NA)
  )

alaska_flag <- ggplot2::ggplot() +
  ggplot2::annotate(
    "rect",
    xmin = 0,
    xmax = 1.9,
    ymin = 0,
    ymax = 1,
    fill = "#003087"
  ) +
  ggplot2::geom_point(
    data = flag_stars,
    ggplot2::aes(x = x, y = y),
    shape = 8,
    color = "#F4C542",
    size = 5,
    stroke = 1.2
  ) +
  ggplot2::coord_fixed(xlim = c(0, 1.9), ylim = c(0, 1), expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#101A2B", color = NA)
  )

temperature_plot <- ggplot2::ggplot(
  weather,
  ggplot2::aes(x = time, y = temperature_2m...C.)
) +
  ggplot2::geom_rect(
    data = weather_events,
    ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
    inherit.aes = FALSE,
    alpha = 0.12
  ) +
  ggplot2::geom_line(color = "#8BE9FD", linewidth = 0.5) +
  ggplot2::geom_label(
    data = label_events,
    ggplot2::aes(x = label_x, y = label_y, label = label, fill = event_type),
    inherit.aes = FALSE,
    color = "#101A2B",
    vjust = 1.15,
    size = 2.5,
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = c("Blizzard-like" = "#F4C542", "Heavy snow" = "#9B8AFB"),
    guide = "none"
  ) +
  ggplot2::coord_cartesian(
    ylim = c(
      min(weather$temperature_2m...C., na.rm = TRUE) - 1,
      max(weather$temperature_2m...C., na.rm = TRUE) + 10
    )
  ) +
  ggplot2::labs(
    y = "Temperature (°C)",
    x = NULL
  ) +
  weather_theme

precipitation_plot <- ggplot2::ggplot(
  weather,
  ggplot2::aes(x = time, y = precipitation..mm.)
) +
  ggplot2::geom_rect(
    data = weather_events,
    ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
    inherit.aes = FALSE,
    alpha = 0.12
  ) +
  ggplot2::geom_line(color = "#FF9F68", linewidth = 0.5) +
  ggplot2::scale_fill_manual(
    values = c("Blizzard-like" = "#F4C542", "Heavy snow" = "#9B8AFB"),
    guide = "none"
  ) +
  ggplot2::labs(
    y = "Hourly precipitation (mm/hour)",
    x = "Time"
  ) +
  weather_theme

weather_plot <- patchwork::wrap_plots(
  patchwork::wrap_plots(title_plot, alaska_flag, nrow = 1, widths = c(3, 1)),
  temperature_plot,
  precipitation_plot,
  ncol = 1,
  heights = c(0.35, 1, 1)
) +
  patchwork::plot_annotation(
    caption = "Source: Open-Meteo. Blizzard-like = snowfall with wind >= 56 km/h for 3+ hours (visibility is unavailable in the archive); heavy snow = snowfall >= 2.5 cm/hour for 2+ hours."
  )

ggplot2::ggsave(
  "sarasota-weather.png",
  plot = weather_plot,
  width = 10,
  height = 9,
  dpi = 300,
  bg = "#101A2B"
)
