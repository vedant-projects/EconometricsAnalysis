# Load required libraries
library(tidyverse)
library(plm)
library(ggplot2)
library(ggpubr)
library(viridis)
library(patchwork)
library(lmtest)
library(stargazer)

# Set seed for reproducibility
set.seed(123)

# Create more realistic environmental-economic panel data
n_countries <- 25
n_years <- 20
years <- 2000:2019

countries <- paste0("Country_", LETTERS[1:n_countries])

# Generate panel data with more realistic relationships
panel_data <- expand.grid(
  country = countries,
  year = years
) %>%
  group_by(country) %>%
  mutate(
    # Base GDP with country-specific trends
    base_gdp = runif(1, 8.5, 10.5),
    gdp_growth = rnorm(1, 0.03, 0.01),
    gdp_per_capita = base_gdp + gdp_growth * (year - 2000) + rnorm(n(), 0, 0.3),
    
    # CO2 emissions with EKC relationship
    co2_emissions = 1.5 + 0.9 * gdp_per_capita - 0.04 * (gdp_per_capita^2) +
                    rnorm(n(), 0, 0.4),
    
    # Renewable energy with decreasing cost trend
    renewable_share = 15 + 0.1 * (year - 2000) + 0.3 * gdp_per_capita +
                     rnorm(n(), 0, 2),
    
    # Environmental policy stringency (simulated)
    policy_stringency = 40 + 0.2 * (year - 2000) + 0.1 * gdp_per_capita +
                       rnorm(n(), 0, 5),
    
    # Industry structure
    industry_share = 30 - 0.1 * (year - 2000) + 0.2 * gdp_per_capita +
                    rnorm(n(), 0, 4),
    
    # Region classification
    region = sample(c("OECD", "Non-OECD", "Developing"), 1)
  ) %>%
  ungroup()

# Convert to panel data format
pdata <- pdata.frame(panel_data, index = c("country", "year"))

# GRAPH 1: Enhanced Environmental Kuznets Curve with Confidence Intervals
graph1 <- ggplot(panel_data, aes(x = gdp_per_capita, y = co2_emissions)) +
  geom_point(aes(color = region, size = renewable_share), alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "darkred", fill = "lightpink", alpha = 0.3) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Environmental Kuznets Curve with Regional Differentiation",
       subtitle = "CO2 Emissions vs GDP per Capita (2000-2019)",
       x = "Log GDP per Capita",
       y = "CO2 Emissions per Capita",
       color = "Region",
       size = "Renewable %") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12))

# GRAPH 2: Interactive Time Trends with Facets
graph2 <- panel_data %>%
  group_by(year, region) %>%
  summarise(
    avg_co2 = mean(co2_emissions),
    avg_gdp = mean(gdp_per_capita),
    avg_renewable = mean(renewable_share),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_co2, color = "CO2 Emissions"), size = 1.2) +
  geom_line(aes(y = avg_gdp * 0.5, color = "GDP (scaled)"), size = 1.2) +
  geom_line(aes(y = avg_renewable, color = "Renewable Share"), size = 1.2) +
  facet_wrap(~region, scales = "free_y") +
  scale_y_continuous(
    name = "CO2 Emissions / Renewable Share",
    sec.axis = sec_axis(~.*2, name = "GDP per Capita (scaled)")
  ) +
  scale_color_manual(values = c("CO2 Emissions" = "red", 
                               "GDP (scaled)" = "blue", 
                               "Renewable Share" = "green")) +
  labs(title = "Regional Trends: Environmental and Economic Indicators",
       x = "Year",
       color = "Indicator") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "lightgray"))

# GRAPH 3: Panel Regression Coefficient Plot
# Run fixed effects regression
fe_model <- plm(co2_emissions ~ gdp_per_capita + I(gdp_per_capita^2) + 
                renewable_share + policy_stringency + industry_share,
                data = pdata, model = "within")

# Extract coefficients and confidence intervals
coef_data <- data.frame(
  variable = c("GDP", "GDP²", "Renewable", "Policy", "Industry"),
  coefficient = coef(fe_model),
  se = sqrt(diag(vcov(fe_model)))
) %>%
  mutate(
    ci_lower = coefficient - 1.96 * se,
    ci_upper = coefficient + 1.96 * se
  )

graph3 <- ggplot(coef_data, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper, color = variable), 
                  size = 1.2, fatten = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Fixed Effects Regression Coefficients",
       subtitle = "Determinants of CO2 Emissions with 95% Confidence Intervals",
       x = "Explanatory Variables",
       y = "Coefficient Estimate") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

# GRAPH 4: Interactive Scatter Plot Matrix
graph4 <- ggplot(panel_data, aes(x = gdp_per_capita, y = co2_emissions)) +
  geom_point(aes(color = policy_stringency, size = industry_share), alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = FALSE) +
  facet_wrap(~cut_number(renewable_share, 3), 
             labeller = labeller(.default = function(x) paste("Renewable:", x))) +
  scale_color_viridis(name = "Policy Stringency") +
  scale_size_continuous(name = "Industry Share") +
  labs(title = "CO2 Emissions by GDP with Policy and Renewable Context",
       subtitle = "Faceted by Renewable Energy Share Tertiles",
       x = "Log GDP per Capita",
       y = "CO2 Emissions per Capita") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "lightblue"))

# Arrange all graphs in a professional layout
final_layout <- (graph1 + graph2) / (graph3 + graph4) +
  plot_annotation(
    title = "Comprehensive Environmental Econometrics Analysis",
    subtitle = "Intermediate-level graphical representation of environmental-economic relationships",
    theme = theme(plot.title = element_text(face = "bold", size = 16),
                 plot.subtitle = element_text(size = 12))
  )

# Display the final layout
print(final_layout)

# Additional regression output for completeness
cat("\n=== FIXED EFFECTS REGRESSION RESULTS ===\n")
summary(fe_model)

# Hausman test
random_model <- plm(co2_emissions ~ gdp_per_capita + I(gdp_per_capita^2) + 
                    renewable_share + policy_stringency + industry_share,
                    data = pdata, model = "random")

hausman_test <- phtest(fe_model, random_model)
cat("\n=== HAUSMAN TEST ===\n")
print(hausman_test)

# Save high-quality output
ggsave("environmental_econometrics_analysis.png", final_layout, 
       width = 16, height = 12, dpi = 300)