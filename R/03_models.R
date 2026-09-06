# 03_models.R -----------------------------------------------------------------
# Regression analysis. Assumes 01_clean.R and 02_eda.R have been run.
#
#   Section 1: smoke days and temperature vs. wildfire storm counts (California)
#   Section 2: rain days and total precipitation vs. precipitation/wind storm
#              counts (both regions)
#
# Both outcomes are yearly counts. Each model is fit as OLS first, then tested
# for overdispersion; If the ratio exceeds 1.5 a negative binomial is
# reported instead. San Diego's precipitation model is the one case that stays
# OLS, because its dispersion ratio is 1.1.
# -----------------------------------------------------------------------------

# Shared figure style, so every figure here reads as one set. Fit and reference
# lines use ink, not a second hue - they annotate the data, not add a series.
FIG_DPI   <- 300
VIZ_POINT <- "#2a78d6"
VIZ_INK   <- "#52514e"
VIZ_BAND  <- "grey85"

theme_report <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(color = "grey40", margin = margin(b = 10)),
      plot.caption     = element_text(color = "grey40", hjust = 0),
      axis.title       = element_text(color = "grey20")
    )
}

# Residuals-vs-fitted plot, QQ plot, and Shapiro-Wilk test for an lm object.
check_ols <- function(model, region) {
  res <- rstandard(model)
  fit <- fitted.values(model)

  plot(fit, res, pch = 19, col = "grey50",
       main = paste("Residuals vs Fitted (", region, ")"))
  abline(h = 0, col = "red")

  qqnorm(res, pch = 19, col = "grey50", main = paste("QQ Plot (", region, ")"))
  qqline(res, col = "red")

  print(shapiro.test(res))
}

# =============================================================================
# SECTION 1: Wildfire storms, California
# Hypothesis: more smoke days and higher average temperature in a year go with
# more wildfire storm events. California only — South Carolina's storm export
# has too few wildfire records to model.
# =============================================================================

temperature_california <- California_weather %>%
  group_by(Year) %>%
  summarize(Avg_Temperature = mean((Max_Temperature + Min_Temperature) / 2, na.rm = TRUE),
            .groups = "drop")

wildfire_storms_california <- California_storm_unique %>%
  filter(grepl("Wildfire", EVENT_TYPE, ignore.case = TRUE)) %>%
  group_by(Year = year(BEGIN_DATE)) %>%
  summarize(Wildfire_Storm_Count = n(), .groups = "drop")

smoke_days_california <- California_weather %>%
  group_by(Year) %>%
  summarize(Smoke_Days = sum(Smoke, na.rm = TRUE), .groups = "drop")

# --- Coverage check ----------------------------------------------------------
# NOAA only records all event types from 1996, so wildfire has no rows before
# then. Filling those years with 0 would code unmeasured as observed zero for
# 48 of 77 years, hence the 1996+ cut; the zero-fill below is now just a guard.
# 2024 is dropped as partial (316 of 365 days).
cat("Wildfire event years:",
    min(wildfire_storms_california$Year), "-",
    max(wildfire_storms_california$Year),
    "| years with a record:", nrow(wildfire_storms_california), "\n")

california_combined_data <- smoke_days_california %>%
  merge(wildfire_storms_california, by = "Year", all.x = TRUE) %>%
  merge(yearly_precipitation_Cali,  by = "Year", all.x = TRUE) %>%
  merge(temperature_california,     by = "Year", all.x = TRUE) %>%
  mutate(Wildfire_Storm_Count = ifelse(is.na(Wildfire_Storm_Count), 0, Wildfire_Storm_Count)) %>%
  filter(Smoke_Days > 0, Year >= 1996, Year < 2024)

# 1a. OLS baseline ------------------------------------------------------------

california_model <- lm(Wildfire_Storm_Count ~ Smoke_Days + Avg_Temperature + Total_Precipitation,
                       data = california_combined_data)
summary(california_model)

# All three predictors, faceted in a row rather than stacked.
#   - order runs smoke days, temperature, precipitation
wildfire_long <- california_combined_data %>%
  pivot_longer(
    cols = c(Smoke_Days, Avg_Temperature, Total_Precipitation),
    names_to = "Predictor",
    values_to = "Value"
  ) %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c("Smoke_Days", "Avg_Temperature", "Total_Precipitation"),
    labels = c("Smoke days", "Average temperature (\u00b0F)", "Total precipitation (in)")
  ))

wildfire_panel <- ggplot(wildfire_long, aes(x = Value, y = Wildfire_Storm_Count)) +
  geom_point(color = VIZ_POINT, alpha = 0.7, size = 1.8) +
  geom_smooth(method = MASS::glm.nb, formula = y ~ x,
              color = VIZ_INK, fill = VIZ_BAND, linewidth = 0.6) +
  facet_wrap(~ Predictor, scales = "free_x", strip.position = "bottom") +
  expand_limits(y = 0) +
  labs(title = "Wildfire storm counts against each predictor individually, San Diego",
       subtitle = "Negative binomial fits, one predictor at a time",
       x = NULL, y = "Wildfire storm count") +
  theme_report() +
  theme(
    # The bottom strip occupies the x-axis title slot, so style it as one.
    strip.placement = "outside",
    strip.text      = element_text(size = 11, color = "grey20",
                                   margin = margin(t = 3)),
    panel.spacing   = grid::unit(1.4, "lines")
  )

wildfire_panel
ggsave("output/figures/wildfire_scatter.png", wildfire_panel,
       width = 9, height = 4, dpi = FIG_DPI)

# 1b. OLS baseline diagnostics ------------------------------------------------
check_ols(california_model, "Wildfire OLS")
# Residuals pass on the 1996+ window (Shapiro W = 0.95, p = 0.24); 

# 1c. Poisson GLM and overdispersion ------------------------------------------
# Poisson assumes variance = mean; a ratio well above 1 means the standard
# errors are understated. Normality tests do not apply to GLM residuals.

california_model_glm <- glm(
  Wildfire_Storm_Count ~ Smoke_Days + Avg_Temperature + Total_Precipitation,
  data = california_combined_data,
  family = poisson
)
summary(california_model_glm)

overdispersion_ca <- sum(residuals(california_model_glm, type = "pearson")^2) /
  california_model_glm$df.residual
cat("California wildfire overdispersion ratio:", round(overdispersion_ca, 2), "\n")

# 1d. Negative binomial, fit only if the Poisson assumption fails --------------

if (overdispersion_ca > 1.5) {
  cat("Overdispersed - refitting as negative binomial.\n")
  california_model_nb <- glm.nb(
    Wildfire_Storm_Count ~ Smoke_Days + Avg_Temperature + Total_Precipitation,
    data = california_combined_data
  )
  print(summary(california_model_nb))
  california_final_model <- california_model_nb
} else {
  cat("Ratio near 1 - Poisson is adequate.\n")
  california_final_model <- california_model_glm
}

california_combined_data$Predicted_Count <-
  predict(california_final_model, type = "response")

plot_wildfire_fit <- ggplot(california_combined_data,
                            aes(x = Wildfire_Storm_Count, y = Predicted_Count)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = VIZ_INK, linewidth = 0.5) +
  geom_point(color = VIZ_POINT, alpha = 0.7, size = 1.8) +
  labs(title = "Predicted vs observed wildfire storm count, San Diego",
       subtitle = "Dashed line is perfect prediction",
       x = "Observed count", y = "Predicted count") +
  theme_report()

plot_wildfire_fit
ggsave("output/figures/wildfire_fit.png", plot_wildfire_fit, width = 7, height = 5, dpi = FIG_DPI)

# =============================================================================
# SECTION 2: Precipitation and wind storms, both regions
# Hypothesis: more rain days and more total precipitation in a year go with more
# wind- and rain-driven storm events.
# =============================================================================

precipitation_related_events <- c("Heavy Rain", "High Wind", "Thunderstorm Wind", "Strong Wind")

summarize_precipitation_storms <- function(storm_data, events) {
  storm_data %>%
    filter(EVENT_TYPE %in% events) %>%
    group_by(Year = year(BEGIN_DATE)) %>%
    summarize(Precipitation_Storm_Count = n(), .groups = "drop")
}

summarize_rain_days <- function(weather_data) {
  weather_data %>%
    group_by(Year) %>%
    summarize(Rain_Days = sum(Rain, na.rm = TRUE), .groups = "drop")
}

# Series is cut at 2012: rain flags at both stations stop being recorded reliably partway through 2013.
california_precipitation_data <-
  summarize_precipitation_storms(California_storm_unique, precipitation_related_events) %>%
  merge(summarize_rain_days(California_weather), by = "Year", all.x = TRUE) %>%
  merge(yearly_precipitation_Cali, by = "Year", all.x = TRUE) %>%
  filter(Rain_Days > 0, Year <= 2012, Year >= 1996)
# Cut at 1996 as in section 1: Heavy Rain, High Wind and Strong Wind have no
# records before then. 

carolina_precipitation_data <-
  summarize_precipitation_storms(Carolina_storm_unique, precipitation_related_events) %>%
  merge(summarize_rain_days(Carolina_weather), by = "Year", all.x = TRUE) %>%
  merge(yearly_precipitation_Carolina, by = "Year", all.x = TRUE) %>%
  filter(Rain_Days > 0, Year <= 2012)
# LIMITATION, not cut to 1996: only Thunderstorm Wind is recorded before then.
# Kept for sample size; 1996+ leaves n = 17 and the result is null either way.

# 2a. OLS baselines -----------------------------------------------------------

california_precipitation_model <- lm(
  Precipitation_Storm_Count ~ Total_Precipitation + Rain_Days,
  data = california_precipitation_data
)
summary(california_precipitation_model)

carolina_precipitation_model <- lm(
  Precipitation_Storm_Count ~ Total_Precipitation + Rain_Days,
  data = carolina_precipitation_data
)
summary(carolina_precipitation_model)

# One figure per region
plot_precip_scatter <- function(data, x_variable, x_label, y_max,
                                jitter_width, show_y_title = TRUE) {
  ggplot(data, aes(x = !!sym(x_variable), y = Precipitation_Storm_Count)) +
    geom_jitter(color = VIZ_POINT, alpha = 0.7, size = 1.8,
                height = 0, width = jitter_width) +
    geom_smooth(method = "lm", formula = y ~ x,
                color = VIZ_INK, fill = VIZ_BAND, linewidth = 0.6) +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(x = x_label,
         y = if (show_y_title) "Precipitation storm count" else NULL) +
    theme_report()
}

build_precip_panel <- function(data, subtitle) {
  y_max <- max(data$Precipitation_Storm_Count) + 1
  rain   <- plot_precip_scatter(data, "Rain_Days", "Rain days",
                                y_max, jitter_width = 0.5)
  precip <- plot_precip_scatter(data, "Total_Precipitation",
                                "Total precipitation (in)",
                                y_max, jitter_width = 0.15, show_y_title = FALSE)
  (rain | precip) + plot_annotation(
    title = "Neither rain days nor total precipitation predicts storm counts",
    subtitle = subtitle,
    caption = paste("Shaded bands are 95% confidence intervals; both flare wide",
                    "where the data thin out. Neither slope is distinguishable",
                    "from flat."),
    theme = theme_report()
  )
}

precip_panel_california <- build_precip_panel(
  california_precipitation_data,
  sprintf("Southern California, 1996-2012 (n = %d)",
          nrow(california_precipitation_data))
)
precip_panel_carolina <- build_precip_panel(
  carolina_precipitation_data,
  sprintf("South Carolina, 1955-2012 (n = %d)",
          nrow(carolina_precipitation_data))
)

precip_panel_california
precip_panel_carolina

ggsave("output/figures/precipitation_scatter_california.png",
       precip_panel_california, width = 9, height = 4, dpi = FIG_DPI)
ggsave("output/figures/precipitation_scatter_carolina.png",
       precip_panel_carolina, width = 9, height = 4, dpi = FIG_DPI)

# 2b. OLS baseline diagnostics ------------------------------------------------

check_ols(california_precipitation_model, "Southern California")
# Residuals look random; mild deviation in the tails; normality not rejected.

check_ols(carolina_precipitation_model, "South Carolina")
# Clear tail deviation and outliers; normality rejected.

# 2c. Overdispersion test and model choice ------------------------------------
# Both regions tested, same 1.5 threshold as 1d.
dispersion_ratio <- function(formula, data) {
  m <- glm(formula, data = data, family = poisson)
  sum(residuals(m, type = "pearson")^2) / m$df.residual
}

precip_formula <- Precipitation_Storm_Count ~ Total_Precipitation + Rain_Days

overdispersion_ca_precip <- dispersion_ratio(precip_formula, california_precipitation_data)
overdispersion_sc        <- dispersion_ratio(precip_formula, carolina_precipitation_data)
cat("Precipitation-storm overdispersion - Southern California:",
    round(overdispersion_ca_precip, 2),
    "| South Carolina:", round(overdispersion_sc, 2), "
")

# California stays OLS: ratio ~1.1, so there is nothing for a count model to fix.
california_precipitation_final_model <- california_precipitation_model

if (overdispersion_sc > 1.5) {
  cat("South Carolina overdispersed - fitting negative binomial.
")
  carolina_precipitation_final_model <- glm.nb(precip_formula,
                                               data = carolina_precipitation_data)
} else {
  cat("South Carolina ratio near 1 - Poisson is adequate.
")
  carolina_precipitation_final_model <- glm(precip_formula,
                                            data = carolina_precipitation_data,
                                            family = poisson)
}
summary(carolina_precipitation_final_model)

# GLM residuals are not expected to be normal, so no QQ plot or Shapiro test.
plot(fitted(carolina_precipitation_final_model),
     residuals(carolina_precipitation_final_model, type = "deviance"),
     pch = 19, col = "grey50",
     main = "Deviance Residuals vs Fitted (Negative Binomial, SC)",
     xlab = "Fitted count", ylab = "Deviance residual")
abline(h = 0, col = "red")

carolina_precipitation_data$Predicted_NB <-
  predict(carolina_precipitation_final_model, type = "response")

# Fitted values span far less than observed counts - the null result, numerically.
cat("Observed range:", range(carolina_precipitation_data$Precipitation_Storm_Count),
    "| fitted range:", round(range(carolina_precipitation_data$Predicted_NB), 1), "\n")

# 2d. Log-transformed OLS (kept for comparison) -------------------------------
# Tried before the negative binomial. Still OLS on a count outcome, and the
# residuals stay non-normal, so the NB model above is the one to report.

carolina_precipitation_model_log <- lm(
  Precipitation_Storm_Count ~ log(Total_Precipitation) + log(Rain_Days),
  data = carolina_precipitation_data
)
summary(carolina_precipitation_model_log)
check_ols(carolina_precipitation_model_log, "South Carolina, logged")