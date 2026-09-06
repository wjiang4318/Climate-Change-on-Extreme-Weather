# Climate Change and Extreme Weather Events

Do local weather conditions predict extreme weather events? An analysis of NOAA
daily station records and storm event data for two climatically distinct
regions: San Diego, California (Mediterranean) and Charleston, South Carolina
(humid subtropical).

## Motivation

Rising temperatures, shifting rainfall, and intensifying storms are established
indicators of climate change, and research has linked them to increases in
wildfires and severe storms at a global scale. Less is understood about how
these relationships hold locally. This project tests whether the weather
variables a single station records (temperature, rain days, smoke days, total
precipitation) predict the extreme events logged in the same area.

The two regions were chosen for contrast. The hypothesis was that Charleston's
wetter climate would show a link between rainfall and precipitation-driven
storms, while San Diego's dry climate would show a link between dry conditions
and wildfires.

## Findings

**San Diego, wildfires:** supported. Rainfall predicts wildfire storms - wetter
years have fewer of them.

**Both regions, precipitation and wind storms:** not supported. Neither rain
days nor total rainfall predicts storm counts in either region.

Either way the effects are small. Local weather explains a limited share of
extreme weather activity, and the larger drivers are not in this data.

## Data

[NOAA Climate Online Database](https://www.ncdc.noaa.gov/cdo-web/), four files:

| File | Coverage |
| --- | --- |
| `South Cali_daily_1945.csv` | San Diego daily station records, 1945 to present (29,167 days) |
| `South Carolina_daily_1937.csv` | Charleston daily station records, 1937 to present (32,033 days) |
| `South California Storm Event.csv` | San Diego storm events, 1950 onward |
| `South Carolina Storm Event.csv` | Charleston storm events, 1950 onward |

Daily records carry precipitation, temperature, and condition flags (fog,
smoke, thunder, rain). Storm events log discrete severe-weather occurrences
with begin and end dates.

## Method

Station data is reduced to the relevant variables, overlapping condition flags
are collapsed, and storm events are deduplicated. Storm events are then matched 
to calendar days with an interval join, and everything is aggregated 
to yearly totals.

Both outcomes are yearly counts - whole numbers that cannot go below zero - so
a linear model is only a baseline. Count models (Poisson, negative binomial)
model the log of the expected count, which keeps predictions positive and makes
effects multiplicative rather than additive.

The dispersion ratio is the variance of the counts divided by their mean.
Poisson assumes the two are equal; a ratio well above 1 means the counts are
more spread out than it allows, and its standard errors are understated. Each
model is fit as OLS first and checked with residual, QQ, and Shapiro-Wilk
diagnostics, then tested for dispersion. If the ratio exceeds 1.5, negative
binomial is reported instead.

## Results

### Regional patterns

Charleston is wetter and more variable, with 0.14 in of precipitation per day
against San Diego's 0.026, and rain on 33% of days against 16%. San Diego
records smoke on 37% of days against Charleston's 26%, consistent with its
wildfire exposure.

![Yearly precipitation and temperature](output/figures/climate_trends.png)

### Wildfire storms, San Diego

To test whether smoke days, temperature, or rainfall predict wildfire storms,
yearly wildfire counts for 1996-2023 (28 years) were regressed on all three
using a negative binomial model, chosen because the counts vary about four times
more than their mean. Only rainfall mattered: each additional inch of annual
precipitation is associated with roughly 13% fewer wildfire storms
(coefficient -0.139, p = 0.002). Smoke days (p = 0.92) and average temperature
(p = 0.10) showed no significant effect.

![Wildfire predictors](output/figures/wildfire_scatter.png)

Each panel fits a single predictor on its own to show its individual influence;
the reported model fits all three together.

**Why 1996.** NOAA only catalogues all event types from that year, so wildfire
has no earlier rows.

### Precipitation and wind storms, both regions

To test whether rain days or total rainfall predict rain- and wind-driven
storms, yearly counts of heavy rain, high wind, thunderstorm wind, and strong
wind events were regressed on both predictors, each region separately. San Diego
used ordinary least squares (17 years, 1996-2012), since its counts vary no more
than expected; Charleston used a negative binomial (47 years, 1955-2012).
Neither predictor was significant in either region - San Diego R² = 0.066
(p = 0.34 and p = 0.67), Charleston p = 0.23 and p = 0.93.

![San Diego precipitation-storm predictors](output/figures/precipitation_scatter_california.png)

![Charleston precipitation-storm predictors](output/figures/precipitation_scatter_carolina.png)

The regions are plotted separately because storm events are logged per reporting
zone. Their windows differ too: only thunderstorm wind is recorded before
1996, so San Diego has no usable earlier years. Fit lines are OLS in all four
panels; Charleston's reported model is negative binomial.

Additional figures (weather event counts, storm-day severity, model fit) are
written to `output/figures/` when the scripts run.

## Limitations

**Reporting coverage changes over time.** NOAA records grow denser across the
series as practices change, so part of any trend is artifact. For example, 
the sharp decline in recorded rain days after 2012 likely reflects reporting 
changes rather than actual weather patterns, compounded
by substantial missing information in earlier records.


**Granularity mismatch.** Weather comes from one station; storm events are
logged by county and zone. A storm affecting several zones contributes several
counts, so the outcome is closer to storm-zone reports than distinct storms.
This is not symmetric between regions: San Diego's precipitation-storm events
span 12 zones against Charleston's 2, which rules out comparing raw counts
across the two.

**Narrow predictors.** Wind speed had too much missing data to use and is
likely a stronger driver of wind-related storms than anything included here.

**Reverse causation.** Smoke days are partly caused by the wildfires they are
used to predict, so that coefficient resists causal reading either way.

**Small samples.** Yearly aggregation leaves 17 to 47 observations per model,
which limits power. San Diego's precipitation-storm null in particular rests on
thin evidence, at 17 years with two predictors.

## Running it

```r
source("R/01_clean.R")   # load, clean, flag storm days
source("R/02_eda.R")     # yearly summaries and exploratory figures
source("R/03_models.R")  # regression models
```

Scripts run in order; each assumes the previous one's objects are in the
environment. R 4.x with `dplyr` (>= 1.1), `tidyr`, `ggplot2`, `lubridate`,
`patchwork`, `MASS`, `olsrr`.

```
R/               analysis scripts
data/raw/        NOAA CSVs
output/figures/  generated plots
```