library(ggplot2)
library(dplyr)
library(acceleep)

usethis::use_directory("output/raw-measurements")

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
        subtitle = glue::glue("Measure in {label_outcome(outcome, with_abbr = TRUE)}"),
        x = "Interval Index", y = label_outcome(outcome, with_abbr = FALSE)
      ) +
      tadaathemes::theme_ipsum_ss()

    ggsave(
      plot = p_ee,
      filename = glue::glue("orig-data-ee-{outcome}.png"),
      path = here::here("output/raw-measurements"),
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
    geom_path(alpha = .8) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = "Per-subject Accelerometry data",
      subtitle = glue::glue("{label_accel_models(model)} ({label_placement(placement)}) at {res}Hz"),
      x = "Time Index", y = "Measurement",
      color = "Axis"
    ) +
    tadaathemes::theme_ipsum_ss() +
    theme(legend.position = "top")

    ggsave(
      plot = p_accel,
      filename = glue::glue("orig-data-accel-{model}-{placement}-{res}Hz.png"),
      path = here::here("output/raw-measurements"),
      width = 20, height = 14
    )

}


# A single subject ----

resolution_comparison_plot <- function(model, placement, ID, intervals, res = c(1, 10, 100)) {
  single_subject_accel <- purrr::map_df(res, ~{
    # browser()
    get_combined_data(model = model, placement = placement, res = .x) %>%
      filter(.data$ID == .env$ID, interval %in% intervals) %>%
      select(-c("MET", "kJ", "Jrel")) %>%
      mutate(
        resolution = .x,
        seconds = (rowid / resolution) + (min(interval) * 30),
        seconds_global = (seq_along(rowid) / resolution) + (min(interval) * 30)
      ) %>%
      tidyr::pivot_longer(cols = c("X", "Y", "Z"), names_to = "axis", values_to = "value")
  })

  p <- single_subject_accel %>%
    mutate(resolution = forcats::fct_rev(factor(resolution))) %>%
    ggplot(aes(x = seconds_global/60, y = value, color = axis)) +
    facet_grid(rows = vars(resolution), labeller = as_labeller(function(x) paste0(x, "Hz"))) +
    geom_path() +
    scale_x_continuous(breaks = seq(0, 1e3, 1), minor_breaks = seq(0, 1e3, .5)) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = glue::glue("Raw Accelerometry: {label_accel_models(model)} ({label_placement(placement)})"),
      subtitle = glue::glue("6 intervals (3 minutes) of measurement from randomly selected subject
                            Original resolution ({max(res)}Hz) and downsampled data"),
      x = "Time (minutes from measurement start)", y = "Accelerometer value",
      color = "Axis"
    ) +
    tadaathemes::theme_ipsum_ss(grid = "xXY")

  ggsave(
    plot = p,
    filename = glue::glue("single-subject-ID{ID}-int{paste0(range(intervals), collapse = '_')}-accel-{model}-{placement}-all-res.png"),
    path = here::here("output/raw-measurements"),
    width = 12, height = 7
  )

  p
}

resolution_comparison_plot("actigraph", "hip_left", ID = "028", c(65:70), res = c(1, 10, 100))
resolution_comparison_plot("actigraph", "hip_right", ID = "028", c(65:70), res = c(1, 10, 100))
resolution_comparison_plot("activpal", "thigh_right", ID = "028", c(65:70), res = c(1, 10, 20))
resolution_comparison_plot("geneactiv", "wrist_left", ID = "028", c(65:70), res = c(1, 10, 100))
resolution_comparison_plot("geneactiv", "wrist_right", ID = "028", c(65:70), res = c(1, 10, 100))
resolution_comparison_plot("geneactiv", "hip_right", ID = "028", c(65:70), res = c(1, 10, 100))

single_subject_ee_plot <- function(ID, intervals, unit) {
  #browser()

  single_subject_ee <- get_combined_data(model = "geneactiv", placement = "hip_right", res = 1) %>%
    filter(.data$ID == .env$ID, interval %in% intervals) %>%
    select(-c("X", "Y", "Z"), outcome = .env$unit) %>%
    distinct(.data$interval, .keep_all = TRUE) %>%
    mutate(
      seconds_global = (seq_along(interval) * 30) + (min(interval) * 30)
    )

  p <- single_subject_ee %>%
    ggplot(aes(x = seconds_global/60, y = outcome)) +
    geom_path() +
    geom_point() +
    scale_x_continuous(breaks = seq(0, 1e3, 1), minor_breaks = seq(0, 1e3, .5)) +
    labs(
      title = glue::glue("Energy Expenditure: {label_outcome(unit)}"),
      subtitle = glue::glue("6 intervals (3 minutes) of measurement from randomly selected subject"),
      x = "Time (minutes from measurement start)", y = label_outcome(unit),
      color = "Axis"
    ) +
    tadaathemes::theme_ipsum_ss(grid = "xXY")

  ggsave(
    plot = p,
    filename = glue::glue("single-subject-ID{ID}-int{paste0(range(intervals), collapse = '_')}-EE-{unit}.png"),
    path = here::here("output/raw-measurements"),
    width = 12, height = 7
  )

  p
}

single_subject_ee_plot(ID = "028", intervals = c(65:70), unit = "kJ")
single_subject_ee_plot(ID = "028", intervals = c(65:70), unit = "MET")
single_subject_ee_plot(ID = "028", intervals = c(65:70), unit = "Jrel")
single_subject_ee_plot(ID = "028", intervals = c(0:1000), unit = "Jrel")

# Experiment: Accel SD only ----

files_overview <- get_overview_table() %>%
  distinct(model, placement)

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
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top")


ggsave(
  plot = p_sdee,
  filename = glue::glue("subj-022-accel-sd-kj-standardized-geneactiv-hip-right.png"),
  path = here::here("output"),
  width = 10, height = 6
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
    index = ifelse(name == "kJ", index, index + 1)
  ) %>%
  ggplot(aes(x = index, y = value, color = name)) +
  geom_path() +
  scale_color_manual(
    values = c(kJ = "red", x_sd = "royalblue1", y_sd = "royalblue2", z_sd = "royalblue3"),
    labels = c(kJ = "kJ", x_sd = "SD(X)", y_sd = "SD(Y)", z_sd = "SD(Z)")
  ) +
  labs(
    title = "Single-Subject Accelerometry Standard Deviation and EE (kJ)",
    subtitle = "Accelerometry reduced to standard deviation over a given interval\nAll values standardized\nkJ offset by 1 intervals",
    x = "Interval Index", y = "Standardized Value",
    caption = glue::glue("{label_accel_models('geneactiv')} ({label_placement('hip_right')})"),
    color = ""
  ) +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top")

ggsave(
  plot = p_sdee_offset,
  filename = glue::glue("subj-022-accel-sd-kj-offset-standardized-geneactiv-hip-right.png"),
  path = here::here("output"),
  width = 10, height = 6
)

# Axis ordering comparison ----

sample_accel_1hz <- bind_rows(
  get_combined_data(model = "actigraph", placement = "hip_right", res = 1) %>%
    mutate(accel = glue::glue("{label_accel_models('actigraph')} (right hip)")),
  get_combined_data(model = "geneactiv", placement = "hip_right", res = 1) %>%
    mutate(accel = glue::glue("{label_accel_models('geneactiv')} (right hip)"))
) %>%
  select(-c("kJ", "Jrel", "MET", "rowid")) %>%
  filter(ID %in% c("001", "008")) %>%
  group_by(ID, accel) %>%
  mutate(minute = seq_along(interval)/60) %>%
  tidyr::pivot_longer(cols = c("X", "Y", "Z"))

sample_accel_10hz <- bind_rows(
  get_combined_data(model = "actigraph", placement = "hip_right", res = 10) %>%
    mutate(accel = glue::glue("{label_accel_models('actigraph')} (right hip)")),
  get_combined_data(model = "geneactiv", placement = "hip_right", res = 10) %>%
    mutate(accel = glue::glue("{label_accel_models('geneactiv')} (right hip)"))
) %>%
  select(-c("kJ", "Jrel", "MET", "rowid")) %>%
  filter(ID %in% c("001", "008")) %>%
  group_by(ID, accel) %>%
  mutate(minute = seq_along(interval)/10/60) %>%
  tidyr::pivot_longer(cols = c("X", "Y", "Z"))

sample_accel_100hz <- bind_rows(
  get_combined_data(model = "actigraph", placement = "hip_right", res = 100) %>%
    mutate(accel = glue::glue("{label_accel_models('actigraph')} (right hip)")),
  get_combined_data(model = "geneactiv", placement = "hip_right", res = 100) %>%
    mutate(accel = glue::glue("{label_accel_models('geneactiv')} (right hip)"))
) %>%
  select(-c("kJ", "Jrel", "MET", "rowid")) %>%
  filter(ID %in% c("001", "008")) %>%
  group_by(ID, accel) %>%
  mutate(minute = seq_along(interval)/100/60) %>%
  tidyr::pivot_longer(cols = c("X", "Y", "Z"))

# 1Hz

p_axis_order_1 <- ggplot(sample_accel_1hz, aes(x = minute, y = value, color = name)) +
  facet_grid(rows = vars(accel), cols = vars(ID)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top") +
  labs(
    title = "Raw accelerometry data",
    subtitle = "Selected subjects and devices at the same placement",
    x = "Time (minutes from measurement start)", y = "Accelerometer Value",
    color = "Axis",
    caption = "Resolution: 1Hz"
  )

ggsave(
  plot = p_axis_order_1,
  filename = glue::glue("axis-order-comparison-hip-right-1Hz.png"),
  path = here::here("output/raw-measurements"),
  width = 13, height = 8
)

# 10Hz

p_axis_order_10 <- ggplot(sample_accel_10hz, aes(x = minute, y = value, color = name)) +
  facet_grid(rows = vars(accel), cols = vars(ID)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top") +
  labs(
    title = "Raw accelerometry data",
    subtitle = "Selected subjects and devices at the same placement",
    x = "Time (minutes from measurement start)", y = "Accelerometer Value",
    color = "Axis",
    caption = "Resolution: 10Hz"
  )

ggsave(
  plot = p_axis_order_10,
  filename = glue::glue("axis-order-comparison-hip-right-10Hz.png"),
  path = here::here("output/raw-measurements"),
  width = 13, height = 8
)

# 100Hz

p_axis_order_100 <- ggplot(sample_accel_100hz, aes(x = minute, y = value, color = name)) +
  facet_grid(rows = vars(accel), cols = vars(ID)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top") +
  labs(
    title = "Raw accelerometry data",
    subtitle = "Selected subjects and devices at the same placement",
    x = "Time (minutes from measurement start)", y = "Accelerometer Value",
    color = "Axis",
    caption = "Resolution: 100Hz"
  )

ggsave(
  plot = p_axis_order_100,
  filename = glue::glue("axis-order-comparison-hip-right-100Hz.png"),
  path = here::here("output/raw-measurements"),
  width = 13, height = 8
)


# Only first Interval ----

p_axis_order_1_int1 <- sample_accel_1hz %>%
  group_by(ID) %>%
  filter(interval == first(interval)) %>%
  ggplot(aes(x = minute, y = value, color = name)) +
  facet_grid(rows = vars(accel), cols = vars(ID)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top") +
  labs(
    title = "Raw accelerometry data",
    subtitle = "Selected subjects and devices at the same placement",
    x = "Time (minutes from measurement start)", y = "Accelerometer Value",
    color = "Axis",
    caption = "Resolution: 1Hz"
  )

ggsave(
  plot = p_axis_order_1_int1,
  filename = glue::glue("axis-order-comparison-int1-hip-right-1Hz.png"),
  path = here::here("output/raw-measurements"),
  width = 13, height = 8
)

p_axis_order_10_int1 <- sample_accel_10hz %>%
  group_by(ID) %>%
  filter(interval == first(interval)) %>%
  ggplot(aes(x = minute, y = value, color = name)) +
  facet_grid(rows = vars(accel), cols = vars(ID)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top") +
  labs(
    title = "Raw accelerometry data",
    subtitle = "Selected subjects and devices at the same placement",
    x = "Time (minutes from measurement start)", y = "Accelerometer Value",
    color = "Axis",
    caption = "Resolution: 10Hz"
  )

ggsave(
  plot = p_axis_order_10_int1,
  filename = glue::glue("axis-order-comparison-int1-hip-right-10Hz.png"),
  path = here::here("output/raw-measurements"),
  width = 13, height = 8
)


p_axis_order_100_int1 <- sample_accel_100hz %>%
  group_by(ID) %>%
  filter(interval == first(interval)) %>%
  ggplot(aes(x = minute, y = value, color = name)) +
  facet_grid(rows = vars(accel), cols = vars(ID)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top") +
  labs(
    title = "Raw accelerometry data",
    subtitle = "Selected subjects and devices at the same placement",
    x = "Time (minutes from measurement start)", y = "Accelerometer Value",
    color = "Axis",
    caption = "Resolution: 100Hz"
  )

ggsave(
  plot = p_axis_order_100_int1,
  filename = glue::glue("axis-order-comparison-int1-hip-right-100Hz.png"),
  path = here::here("output/raw-measurements"),
  width = 13, height = 8
)


# An interval in the middle

p_axis_order_1_int40 <- sample_accel_1hz %>%
  group_by(ID) %>%
  filter(interval == first(interval) + 40) %>%
  ggplot(aes(x = minute, y = value, color = name)) +
  facet_grid(rows = vars(accel), cols = vars(ID)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top") +
  labs(
    title = "Raw accelerometry data",
    subtitle = "Selected subjects and devices at the same placement",
    x = "Time (minutes from measurement start)", y = "Accelerometer Value",
    color = "Axis",
    caption = "Resolution: 1Hz"
  )

ggsave(
  plot = p_axis_order_1_int40,
  filename = glue::glue("axis-order-comparison-int40-hip-right-1Hz.png"),
  path = here::here("output/raw-measurements"),
  width = 13, height = 8
)

p_axis_order_10_int40 <- sample_accel_10hz %>%
  group_by(ID) %>%
  filter(interval == first(interval) + 40) %>%
  ggplot(aes(x = minute, y = value, color = name)) +
  facet_grid(rows = vars(accel), cols = vars(ID)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top") +
  labs(
    title = "Raw accelerometry data",
    subtitle = "Selected subjects and devices at the same placement",
    x = "Time (minutes from measurement start)", y = "Accelerometer Value",
    color = "Axis",
    caption = "Resolution: 10Hz"
  )

ggsave(
  plot = p_axis_order_10_int40,
  filename = glue::glue("axis-order-comparison-int40-hip-right-10Hz.png"),
  path = here::here("output/raw-measurements"),
  width = 13, height = 8
)


p_axis_order_100_int40 <- sample_accel_100hz %>%
  group_by(ID) %>%
  filter(interval == first(interval) + 40) %>%
  ggplot(aes(x = minute, y = value, color = name)) +
  facet_grid(rows = vars(accel), cols = vars(ID)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top") +
  labs(
    title = "Raw accelerometry data",
    subtitle = "Selected subjects and devices at the same placement",
    x = "Time (minutes from measurement start)", y = "Accelerometer Value",
    color = "Axis",
    caption = "Resolution: 100Hz"
  )

ggsave(
  plot = p_axis_order_100_int40,
  filename = glue::glue("axis-order-comparison-int40-hip-right-100Hz.png"),
  path = here::here("output/raw-measurements"),
  width = 13, height = 8
)

