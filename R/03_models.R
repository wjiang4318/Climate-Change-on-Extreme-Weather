# 03_models.R -----------------------------------------------------------------
# Regression analysis. Assumes 01_clean.R and 02_eda.R have been run.
#
#   Section 1: smoke days and temperature vs. wildfire storm counts (California)
#   Section 2: rain days and total precipitation vs. precipitation/wind storm
#              counts (both regions)
#
# Both outcomes are yearly counts, so the OLS fits are kept only as a baseline
# and the count models are the ones to report.
# -----------------------------------------------------------------------------

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
# wildfire_storms_california only lists years with at least one recorded
# wildfire. Merging from it (inner-join style) would drop every zero-wildfire
# year and truncate the outcome, biasing the count model toward always
# predicting >= 1 event. Merging from smoke_days_california instead (all.x)
# and filling missing counts with 0 keeps those zero years in the data.
cat("Wildfire event years:",
    min(wildfire_storms_california$Year), "-",
    max(wildfire_storms_california$Year),
    "| years with a record:", nrow(wildfire_storms_california), "\n")

california_combined_data <- smoke_days_california %>%
  merge(wildfire_storms_california, by = "Year", all.x = TRUE) %>%
  merge(yearly_precipitation_Cali,  by = "Year", all.x = TRUE) %>%
  merge(temperature_california,     by = "Year", all.x = TRUE) %>%
  mutate(Wildfire_Storm_Count = ifelse(is.na(Wildfire_Storm_Count), 0, Wildfire_Storm_Count)) %>%
  filter(Smoke_Days > 0)

# 1a. OLS baseline ------------------------------------------------------------

california_model <- lm(Wildfire_Storm_Count ~ Smoke_Days + Avg_Temperature + Total_Precipitation,
                       data = california_combined_data)
summary(california_model)

plot_wildfire_scatter <- function(x_variable, x_label) {
  ggplot(california_combined_data, aes(x = !!sym(x_variable), y = Wildfire_Storm_Count)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red") +
    labs(title = paste("Wildfire Storm Count vs", x_label, "(San Diego)"),
         x = x_label, y = "Wildfire Storm Count") +
    theme_minimal()
}

wildfire_panel <- (plot_wildfire_scatter("Smoke_Days", "Smoke Days") /
                     plot_wildfire_scatter("Avg_Temperature", "Average Temperature (\u00B0F)") /
                     plot_wildfire_scatter("Total_Precipitation", "Total Precipitation (in)")) +
  plot_layout(guides = "collect")

wildfire_panel
ggsave("output/figures/wildfire_scatter.png", wildfire_panel, width = 8, height = 10, dpi = 200)

# 1b. OLS assumptions ---------------------------------------------------------

check_ols(california_model, "Wildfire OLS")
# Slight heteroscedasticity, centered near zero. Tails deviate a little in the
# QQ plot; the Shapiro-Wilk test does not reject normality.

# Stepwise AIC drops Smoke_Days, but only improves AIC by about 2, which is not
# a strong enough gap to remove the variable the hypothesis is about.
best_model <- step(california_model, direction = "both", trace = TRUE)

# 1c. Poisson GLM and overdispersion ------------------------------------------
# Poisson assumes variance = mean. The ratio below tests that: well above 1
# means the standard errors are understated and negative binomial is the right
# model. Normality tests do not apply to GLM residuals, so they are not run.

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
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Observed Wildfire Storm Count (California)",
       subtitle = "Dashed line is perfect prediction",
       x = "Observed Count", y = "Predicted Count") +
  theme_minimal()

plot_wildfire_fit
ggsave("output/figures/wildfire_fit.png", plot_wildfire_fit, width = 7, height = 5, dpi = 200)

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
  filter(Rain_Days > 0, Year <= 2012, Precipitation_Storm_Count > 2)
# The count > 2 filter drops early years where California records only one or
# two storms, which reads as thin reporting rather than a quiet year. 

carolina_precipitation_data <-
  summarize_precipitation_storms(Carolina_storm_unique, precipitation_related_events) %>%
  merge(summarize_rain_days(Carolina_weather), by = "Year", all.x = TRUE) %>%
  merge(yearly_precipitation_Carolina, by = "Year", all.x = TRUE) %>%
  filter(Rain_Days > 0, Year <= 2012)

# 2a. OLS baselines -----------------------------------------------------------

california_precipitation_model <- lm(
  Precipitation_Storm_Count ~ Rain_Days + Total_Precipitation,
  data = california_precipitation_data
)
summary(california_precipitation_model)

carolina_precipitation_model <- lm(
  Precipitation_Storm_Count ~ Rain_Days + Total_Precipitation,
  data = carolina_precipitation_data
)
summary(carolina_precipitation_model)

plot_precip_scatter <- function(data, x_variable, x_label, region) {
  ggplot(data, aes(x = !!sym(x_variable), y = Precipitation_Storm_Count)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(title = paste0("Precipitation-Related Storms vs ", x_label, " (", region, ")"),
         x = x_label, y = "Precipitation Storm Count") +
    theme_minimal()
}

precip_panel <- (
  plot_precip_scatter(california_precipitation_data, "Rain_Days", "Rain Days", "California") /
    plot_precip_scatter(carolina_precipitation_data,   "Rain_Days", "Rain Days", "South Carolina") /
    plot_precip_scatter(california_precipitation_data, "Total_Precipitation", "Total Precipitation (in)", "California") /
    plot_precip_scatter(carolina_precipitation_data,   "Total_Precipitation", "Total Precipitation (in)", "South Carolina")
) + plot_layout(guides = "collect")

precip_panel
ggsave("output/figures/precipitation_scatter.png", precip_panel, width = 8, height = 13, dpi = 200)

# 2b. OLS assumptions ---------------------------------------------------------

check_ols(california_precipitation_model, "California")
# Residuals look random; mild deviation in the tails; normality not rejected.

check_ols(carolina_precipitation_model, "South Carolina")
# Clear tail deviation and outliers; normality rejected. Count model below.

# 2c. Negative binomial for South Carolina ------------------------------------

carolina_poisson <- glm(Precipitation_Storm_Count ~ Total_Precipitation + Rain_Days,
                        data = carolina_precipitation_data, family = poisson)
overdispersion_sc <- sum(residuals(carolina_poisson, type = "pearson")^2) /
  carolina_poisson$df.residual
cat("South Carolina precipitation-storm overdispersion ratio:",
    round(overdispersion_sc, 2), "\n")

carolina_precipitation_model_nb <- glm.nb(
  Precipitation_Storm_Count ~ Total_Precipitation + Rain_Days,
  data = carolina_precipitation_data
)
summary(carolina_precipitation_model_nb)

# Deviance residuals against fitted values. No QQ plot or Shapiro test: GLM
# residuals are not expected to be normal, so those diagnostics say nothing.
plot(fitted(carolina_precipitation_model_nb),
     residuals(carolina_precipitation_model_nb, type = "deviance"),
     pch = 19, col = "grey50",
     main = "Deviance Residuals vs Fitted (Negative Binomial, SC)",
     xlab = "Fitted count", ylab = "Deviance residual")
abline(h = 0, col = "red")

carolina_precipitation_data$Predicted_NB <-
  predict(carolina_precipitation_model_nb, type = "response")

plot_nb_precip <- ggplot(carolina_precipitation_data,
                         aes(x = Total_Precipitation, y = Precipitation_Storm_Count)) +
  geom_point(color = "blue") +
  geom_line(aes(y = Predicted_NB), color = "red", linewidth = 1) +
  labs(title = "Negative Binomial: Total Precipitation vs Storm Count (SC)",
       x = "Total Precipitation (in)", y = "Observed and Predicted Storm Count") +
  theme_minimal()

plot_nb_rain <- ggplot(carolina_precipitation_data,
                       aes(x = Rain_Days, y = Precipitation_Storm_Count)) +
  geom_point(color = "blue") +
  geom_line(aes(y = Predicted_NB), color = "red", linewidth = 1) +
  labs(title = "Negative Binomial: Rain Days vs Storm Count (SC)",
       x = "Rain Days", y = "Observed and Predicted Storm Count") +
  theme_minimal()

nb_panel <- (plot_nb_precip / plot_nb_rain) + plot_layout(guides = "collect")
nb_panel
ggsave("output/figures/carolina_nb_fit.png", nb_panel, width = 8, height = 8, dpi = 200)

# 2d. Log-transformed OLS (kept for comparison) -------------------------------
# Tried before the negative binomial. Still OLS on a count outcome, and the
# residuals stay non-normal, so the NB model above is the one to report.

carolina_precipitation_data <- carolina_precipitation_data %>%
  mutate(Log_Total_Precipitation = log(Total_Precipitation),
         Log_Rain_Days = log(Rain_Days))

carolina_precipitation_model_log <- lm(
  Precipitation_Storm_Count ~ Log_Total_Precipitation + Log_Rain_Days,
  data = carolina_precipitation_data
)
summary(carolina_precipitation_model_log)
check_ols(carolina_precipitation_model_log, "South Carolina, logged")