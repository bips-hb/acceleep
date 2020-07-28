library(ggplot2)
library(dplyr)
library(acceleep)

files_overview <- get_overview_table() %>%
  distinct(model, placement)

# Resolution at which accelerometry is plotted (recommended is 1Hz b/c lots of points)
res <- 1

for (row in seq_len(nrow(files_overview))) {
  # browser()

  model <- files_overview %>%
    slice(row) %>%
    pull(model)

  placement <-  files_overview %>%
    slice(row) %>%
    pull(placement)

  full_data <- get_combined_data(model = model, placement = placement, res = res)

  # EE
  for (outcome in c("kJ", "MET", "Jrel")) {
    p_ee <- full_data %>%
      select("ID", "interval", .env$outcome) %>%
      distinct() %>%
      group_by(.data$ID) %>%
      mutate(index = seq_along(.data$ID)) %>%
      rename(outcome = .env$outcome) %>%
      ggplot(aes(x = index, y = outcome)) +
      facet_wrap(~ID) +
      geom_path() +
      labs(
        title = "Per-subject Energy Expenditure",
        y = outcome
      ) +
      theme_minimal()


    ggsave(
      plot = p_ee,
      filename = glue::glue("orig-data-ee-{outcome}.png"),
      path = here::here("output"),
      width = 20, height = 14
    )
  }


  # Accel
  p_accel <- full_data %>%
    # filter(ID == "021") %>%
    arrange(ID, interval, rowid) %>%
    group_by(ID) %>%
    mutate(index = seq_along(ID)) %>%
    tidyr::pivot_longer(cols = c("X", "Y", "Z"), names_to = "axis", values_to = "value") %>%
    ggplot(aes(x = index, y = value, color = axis)) +
    facet_wrap(~ID) +
    geom_path() +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = "Per-subject Accelerometry data",
      subtitle = glue::glue("{label_accel_models(model)} ({label_placement(placement)}) at {res}Hz"),
      x = "Time Index", y = "Measurement (not normalized)",
      color = "Axis"
    ) +
    theme_minimal() +
    theme(legend.position = "top")

    ggsave(
      plot = p_accel,
      filename = glue::glue("orig-data-accel-{model}-{placement}-{res}Hz.png"),
      path = here::here("output"),
      width = 20, height = 14
    )

}
