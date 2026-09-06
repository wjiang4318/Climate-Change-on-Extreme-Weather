# 02_eda.R --------------------------------------------------------------------
# Yearly summaries and exploratory figures. Assumes 01_clean.R has been run.
# Figures are written to output/figures/.
# -----------------------------------------------------------------------------

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# 1. Reusable summarizers -----------------------------------------------------

summarize_precipitation <- function(weather_data, location_name) {
  weather_data %>%
    group_by(Year) %>%
    summarize(Total_Precipitation = sum(Precipitation, na.rm = TRUE), .groups = "drop") %>%
    mutate(Location = location_name)
}

summarize_temperature <- function(weather_data, location_name) {
  weather_data %>%
    group_by(Year) %>%
    summarize(
      Avg_Max_Temperature = mean(Max_Temperature, na.rm = TRUE),
      Avg_Min_Temperature = mean(Min_Temperature, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Location = location_name)
}

summarize_weather_events <- function(weather_data, weather_events) {
  weather_data %>%
    group_by(Year) %>%
    summarize(across(all_of(weather_events), ~ sum(. == 1, na.rm = TRUE)), .groups = "drop")
}

# Shared x-window for the three yearly figures, so stacked panels stay aligned.
# 1948 is the first year both stations record every variable used here; the
# series are filtered to it.
PLOT_DATA_START  <- 1948
PLOT_YEARS       <- c(PLOT_DATA_START, 2024)

# Default breaks start at 1960 and hide the cutoff, so mark it explicitly.
PLOT_YEAR_BREAKS <- c(PLOT_DATA_START, seq(1960, 2020, 20))


# 2. Yearly total precipitation -----------------------------------------------

yearly_precipitation_Cali     <- summarize_precipitation(California_weather, "Southern California")
yearly_precipitation_Carolina <- summarize_precipitation(Carolina_weather,   "South Carolina")

combined_yearly_precipitation <- bind_rows(yearly_precipitation_Cali, yearly_precipitation_Carolina) %>%
  # South Carolina's station reaches back to 1937, California's to 1945. Cut to
  # the common window for viewing consistency.
  filter(Year >= PLOT_DATA_START)

plot_precipitation <- ggplot(combined_yearly_precipitation,
                             aes(x = as.numeric(Year), y = Total_Precipitation, color = Location)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = PLOT_YEAR_BREAKS, expand = expansion(mult = 0.03)) +
  coord_cartesian(xlim = PLOT_YEARS) +
  labs(title = "Yearly Total Precipitation Comparison",
       x = "Year", y = "Total Precipitation (in)", color = "Location") +
  theme_minimal()

# 3. Yearly average temperatures ----------------------------------------------

combined_yearly_avg_temperature <- bind_rows(
  summarize_temperature(California_weather, "Southern California"),
  summarize_temperature(Carolina_weather,   "South Carolina")
) %>%
  filter(Year >= PLOT_DATA_START) %>%
  pivot_longer(cols = c(Avg_Max_Temperature, Avg_Min_Temperature),
               names_to = "Temperature_Type", values_to = "Temperature")

plot_temperature <- ggplot(combined_yearly_avg_temperature,
                           aes(x = as.numeric(Year), y = Temperature,
                               color = Location, linetype = Temperature_Type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = PLOT_YEAR_BREAKS, expand = expansion(mult = 0.03)) +
  coord_cartesian(xlim = PLOT_YEARS) +
  labs(title = "Yearly Average Temperatures Comparison",
       x = "Year", y = "Temperature (°F)",
       color = "Location", linetype = "Temperature Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4. Yearly weather event counts ----------------------------------------------

weather_events <- c("Fog", "Thunder", "Smoke", "Rain")
event_colors <- c("Fog" = "blue", "Rain" = "darkgreen",
                  "Smoke" = "orange", "Thunder" = "purple")

plot_weather_events <- function(weather_data, title) {
  summarize_weather_events(weather_data, weather_events) %>%
    filter(Year >= PLOT_DATA_START) %>%
    pivot_longer(-Year, names_to = "Weather_Type", values_to = "Count") %>%
    ggplot(aes(x = Year, y = Count, color = Weather_Type)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = event_colors) +
    scale_x_continuous(breaks = PLOT_YEAR_BREAKS, expand = expansion(mult = 0.03)) +
    coord_cartesian(xlim = PLOT_YEARS) +
    labs(title = title, x = "Year", y = "Occurrences", color = "Weather Type") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

plot_weather_Cali     <- plot_weather_events(California_weather,
                                             "Yearly Weather Events in Southern California")
plot_weather_Carolina <- plot_weather_events(Carolina_weather,
                                             "Yearly Weather Events in South Carolina")

# Rain recording at both stations thins out after ~2013; the models in
# 03_models.R cut the series at 2012 for that reason.

# 5. Storm-day severity metrics -----------------------------------------------

summarize_storm_severity <- function(weather_data, location_name) {
  weather_data %>%
    filter(Storm_Flag == 1) %>%
    group_by(Year) %>%
    summarize(
      Total_Storm_Precipitation     = sum(Precipitation, na.rm = TRUE),
      Storm_Days                    = n(),
      Avg_Daily_Storm_Precipitation = mean(Precipitation, na.rm = TRUE),
      Avg_Min_Temperature           = mean(Min_Temperature, na.rm = TRUE),
      Avg_Max_Temperature           = mean(Max_Temperature, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Location = location_name)
}

# Years with only a handful of storm days give unstable averages.
storm_day_threshold <- 5

storm_severity_combined <- bind_rows(
  summarize_storm_severity(California_weather, "Southern California"),
  summarize_storm_severity(Carolina_weather,   "South Carolina")
) %>%
  filter(Storm_Days >= storm_day_threshold)

storm_temperatures_filtered <- storm_severity_combined %>%
  select(Year, Location, Storm_Days, Avg_Min_Temperature, Avg_Max_Temperature) %>%
  pivot_longer(cols = c(Avg_Min_Temperature, Avg_Max_Temperature),
               names_to = "Temperature_Type", values_to = "Temperature")

plot_storm_metric <- function(data, y_variable, title, subtitle, y_label) {
  ggplot(data, aes(x = Year, y = !!sym(y_variable), color = Location)) +
    geom_line(linewidth = 1) +
    geom_point(aes(fill = Storm_Days), shape = 21, size = 3, color = "black", alpha = 0.8) +
    scale_fill_gradient(
      name = "Storm days in year", low = "yellow", high = "red",
      guide = guide_colorbar(title.position = "top", barwidth = unit(3.5, "cm"), barheight = unit(0.4, "cm"))
    ) +
    guides(color = guide_legend(title.position = "top")) +
    labs(title = title, subtitle = subtitle,
         x = "Year", y = y_label, color = "Location") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}

storm_subtitle <- "Line color is location; dot color is the number of storm days that year"

plot_storm_precip <- plot_storm_metric(
  storm_severity_combined,
  y_variable = "Total_Storm_Precipitation",
  title = "Total Precipitation During Storms Over Time",
  subtitle = storm_subtitle,
  y_label = "Total Storm Precipitation (in)"
)

plot_storm_tmax <- plot_storm_metric(
  storm_temperatures_filtered %>% filter(Temperature_Type == "Avg_Max_Temperature"),
  y_variable = "Temperature",
  title = "Average Max Temperatures During Storms Over Time",
  subtitle = storm_subtitle,
  y_label = "Temperature (°F)"
)

plot_storm_tmin <- plot_storm_metric(
  storm_temperatures_filtered %>% filter(Temperature_Type == "Avg_Min_Temperature"),
  y_variable = "Temperature",
  title = "Average Min Temperatures During Storms Over Time",
  subtitle = storm_subtitle,
  y_label = "Temperature (°F)"
)

# 6. Assemble and save --------------------------------------------------------

legend_theme <- theme(
  legend.position = "bottom",
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 9)
)

climate_panel <- (plot_precipitation / plot_temperature) +
  plot_layout(guides = "collect") & legend_theme
events_panel  <- (plot_weather_Cali / plot_weather_Carolina) +
  plot_layout(guides = "collect") & legend_theme
storm_panel   <- (plot_storm_precip / plot_storm_tmax / plot_storm_tmin) +
  plot_layout(guides = "collect") & legend_theme

print(climate_panel)
print(events_panel)
print(storm_panel)

ggsave("output/figures/climate_trends.png", climate_panel, width = 9, height = 8, dpi = 200)
ggsave("output/figures/weather_events.png", events_panel,  width = 9, height = 8, dpi = 200)
ggsave("output/figures/storm_severity.png", storm_panel,   width = 9, height = 11, dpi = 200)
