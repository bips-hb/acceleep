library(ggplot2)
library(dplyr)
library(acceleep)

files_overview <- get_overview_table() %>%
  distinct(model, placement)

# All subjects accelerometry + EE ----

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


# A single subject ----

files_overview <- get_overview_table() %>%
  filter(sid == "022")

files_overview %>%
  filter(model == "geneactiv", placement == "hip_right") %>%
  pull(file_clean) %>%
  readRDS() %>%
  arrange(interval) %>%
  mutate(index = seq_along(ID)) %>%
  group_by(interval) %>%
  mutate(index_within_interval = seq_along(interval)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = c("X", "Y", "Z"), names_to = "axis", values_to = "value") %>%
  ggplot(aes(x = index_within_interval, y = value, color = axis)) +
  facet_wrap(~interval) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Per-subject Accelerometry data",
    subtitle = glue::glue("{label_accel_models('geneactiv')} ({label_placement('hip_right')}) at {res}Hz"),
    x = "Time Index", y = "Measurement (not normalized)",
    color = "Axis"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# Experiment ----

p_sdee <- files_overview %>%
  filter(model == "geneactiv", placement == "hip_right") %>%
  pull(file_clean) %>%
  readRDS() %>%
  arrange(interval) %>%
  mutate(index = seq_along(ID)) %>%
  group_by(interval) %>%
  mutate(index_within_interval = seq_along(interval)) %>%
  ungroup() %>%
  group_by(ID, interval) %>%
  summarize(
    x_sd = sd(X), y_sd = sd(Y), z_sd = sd(Z),
    kJ = unique(kJ)
  ) %>%
  mutate(index = seq_along(interval)) %>%
  mutate(across(c(x_sd, y_sd, z_sd, kJ), ~as.numeric(scale(.x)))) %>%
  tidyr::pivot_longer(cols = c(ends_with("_sd"), "kJ")) %>%
  ggplot(aes(x = index, y = value, color = name)) +
  geom_path() +
  scale_color_manual(
    values = c(kJ = "red", x_sd = "royalblue1", y_sd = "royalblue2", z_sd = "royalblue3"),
    labels = c(kJ = "kJ", x_sd = "SD(X)", y_sd = "SD(Y)", z_sd = "SD(Z)")
  ) +
  labs(
    title = "Single-Subject Accelerometry Standard Deviation and EE (kJ)",
    subtitle = "Accelerometry reduced to standard deviation over a given interval\nAll values standardized",
    x = "Interval Index", y = "Standardized Value",
    caption = glue::glue("{label_accel_models('geneactiv')} ({label_placement('hip_right')})"),
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")


ggsave(
  plot = p_sdee,
  filename = glue::glue("subj-022-accel-sd-kj-standardized-geneactiv-hip-right.png"),
  path = here::here("output"),
  width = 13, height = 8
)


# With slight offset
p_sdee_offset <- files_overview %>%
  filter(model == "geneactiv", placement == "hip_right") %>%
  pull(file_clean) %>%
  readRDS() %>%
  arrange(interval) %>%
  ungroup() %>%
  group_by(ID, interval) %>%
  summarize(
    x_sd = sd(X), y_sd = sd(Y), z_sd = sd(Z),
    kJ = unique(kJ)
  ) %>%
  mutate(index = seq_along(interval)) %>%
  mutate(across(c(x_sd, y_sd, z_sd, kJ), ~as.numeric(scale(.x)))) %>%
  tidyr::pivot_longer(cols = c(ends_with("_sd"), "kJ")) %>%
  # mild offset
  mutate(
    index = ifelse(name == "kJ", index, index + 2)
  ) %>%
  ggplot(aes(x = index, y = value, color = name)) +
  geom_path() +
  scale_color_manual(
    values = c(kJ = "red", x_sd = "royalblue1", y_sd = "royalblue2", z_sd = "royalblue3"),
    labels = c(kJ = "kJ", x_sd = "SD(X)", y_sd = "SD(Y)", z_sd = "SD(Z)")
  ) +
  labs(
    title = "Single-Subject Accelerometry Standard Deviation and EE (kJ)",
    subtitle = "Accelerometry reduced to standard deviation over a given interval\nAll values standardized\nkJ offset by 2 intervals",
    x = "Interval Index", y = "Standardized Value",
    caption = glue::glue("{label_accel_models('geneactiv')} ({label_placement('hip_right')})"),
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(
  plot = p_sdee_offset,
  filename = glue::glue("subj-022-accel-sd-kj-offset-standardized-geneactiv-hip-right.png"),
  path = here::here("output"),
  width = 13, height = 8
)

