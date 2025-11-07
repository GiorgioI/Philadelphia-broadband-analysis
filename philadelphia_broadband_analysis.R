# Philadelphia Broadband & Affordability Analysis
# Giorgio Imbiscuso
# Data: Census ACS 2022

# install.packages(c("tidycensus", "ggplot2", "dplyr", "tidyr", "sf", "gridExtra"))

setwd("C:/Users/giorg/OneDrive/Business/Portfolio/RStudio/Philadelphia Broadband")

library(tidycensus)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)

# census_api_key("YOUR_KEY_HERE", install = TRUE)

philly <- get_acs(
  geography = "tract",
  variables = c(
    total_hh = "B28002_001",
    broadband = "B28002_004",
    no_internet = "B28002_013",
    med_income = "B19013_001"
  ),
  state = "PA",
  county = "Philadelphia",
  year = 2022,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
)

philly <- philly %>%
  mutate(
    pct_bb = (broadbandE / total_hhE) * 100,
    pct_none = (no_internetE / total_hhE) * 100,
    income_k = med_incomeE / 1000
  ) %>%
  filter(!is.na(pct_bb), !is.na(med_incomeE))

# Map: No internet access
p1 <- ggplot(philly) +
  geom_sf(aes(fill = pct_none), color = NA) +
  scale_fill_viridis_c(
    option = "magma",
    name = "% No\nInternet",
    labels = function(x) paste0(round(x), "%"),
    direction = -1
  ) +
  labs(
    title = "Philadelphia Digital Divide: No Internet Access",
    subtitle = "Percentage of households with no internet access at home",
    caption = "Source: ACS 2022 (5-year estimates)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("01_no_internet_map.png", p1, width = 10, height = 8, dpi = 300)

# Linear regression - shows the problem with bounded data
lm_mod <- lm(pct_bb ~ income_k, data = philly)
r2 <- summary(lm_mod)$r.squared

p2 <- ggplot(philly, aes(income_k, pct_bb)) +
  geom_point(alpha = 0.5, color = "#0f4c81", size = 2) +
  geom_smooth(method = "lm", color = "#d62828", fill = "#d62828", 
              alpha = 0.2, linetype = "dashed", fullrange = FALSE) +
  geom_smooth(method = "loess", color = "#f77f00", fill = "#f77f00", alpha = 0.2) +
  scale_x_continuous(labels = function(x) paste0("$", x, "k")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Income vs Broadband: Linear Model",
    subtitle = sprintf("R² = %.3f | Problem: can predict >100%%", r2),
    x = "Median Household Income",
    y = "% with Broadband",
    caption = "Red = linear, Orange = LOESS | Source: ACS 2022"
  ) +
  theme_minimal()

ggsave("02_linear_model.png", p2, width = 10, height = 7, dpi = 300)

# Logistic regression for bounded outcomes
philly_logit <- philly %>%
  st_drop_geometry() %>%
  mutate(
    prop_bb = pct_bb / 100,
    prop_bb = case_when(
      prop_bb == 0 ~ 0.001,
      prop_bb == 1 ~ 0.999,
      TRUE ~ prop_bb
    ),
    logit_bb = log(prop_bb / (1 - prop_bb))
  ) %>%
  filter(!is.na(prop_bb), !is.na(income_k))

logit_mod <- lm(logit_bb ~ income_k, data = philly_logit)

philly_logit$linear_pred <- predict(lm_mod, newdata = philly_logit)
philly_logit$logit_pred <- predict(logit_mod, newdata = philly_logit)
philly_logit$logistic_pred <- (exp(philly_logit$logit_pred) / 
                                 (1 + exp(philly_logit$logit_pred))) * 100

p3 <- ggplot(philly_logit, aes(income_k, pct_bb)) +
  geom_point(alpha = 0.4, color = "#0f4c81", size = 2) +
  geom_line(aes(y = linear_pred, color = "Linear"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = logistic_pred, color = "Logistic"), linewidth = 1.2) +
  scale_color_manual(
    name = "Model",
    values = c("Linear" = "#d62828", "Logistic" = "#2a9d8f"),
    labels = c("Linear (inappropriate)", "Logistic (appropriate)")
  ) +
  scale_x_continuous(labels = function(x) paste0("$", x, "k")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Linear vs Logistic Regression",
    subtitle = "Logistic properly handles 0-100% bounds",
    x = "Median Household Income",
    y = "% with Broadband",
    caption = "Source: ACS 2022"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03_model_comparison.png", p3, width = 10, height = 7, dpi = 300)

# Income bracket analysis
income_cats <- philly %>%
  st_drop_geometry() %>%
  mutate(
    bracket = case_when(
      med_incomeE < 30000 ~ "< $30k",
      med_incomeE < 50000 ~ "$30k-$50k",
      med_incomeE < 75000 ~ "$50k-$75k",
      med_incomeE < 100000 ~ "$75k-$100k",
      TRUE ~ "> $100k"
    ),
    bracket = factor(bracket, levels = c("< $30k", "$30k-$50k", "$50k-$75k", 
                                         "$75k-$100k", "> $100k"))
  ) %>%
  group_by(bracket) %>%
  summarise(avg_bb = mean(pct_bb, na.rm = TRUE))

p4 <- ggplot(income_cats, aes(bracket, avg_bb)) +
  geom_col(fill = "#2a9d8f", alpha = 0.8) +
  geom_text(aes(label = paste0(round(avg_bb, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Broadband Access by Income Level",
    subtitle = "Average across Philly census tracts",
    x = "Income Bracket",
    y = "Avg % with Broadband",
    caption = "Source: ACS 2022"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("04_income_brackets.png", p4, width = 10, height = 7, dpi = 300)

# Combined sharable image
library(gridExtra)
library(grid)

combined <- grid.arrange(
  p1, p3, p4,
  ncol = 2, nrow = 2,
  top = textGrob("Philadelphia's Digital Divide: Income & Broadband Access",
                 gp = gpar(fontsize = 22, fontface = "bold")),
  bottom = textGrob("Created by Giorgio Imbiscuso  |  Data: US Census Bureau ACS 2022",
                    gp = gpar(fontsize = 12, col = "gray40"))
)

ggsave("SHARE_Philadelphia_Broadband.png", combined, 
       width = 18, height = 11, dpi = 300)