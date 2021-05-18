library(tidyverse)
library(here)
library(janitor)
library(broom)

# Read in data
data_dmg <- read.csv(here("data", "damages.csv")) %>% clean_names()
data_wrm <- read.csv(here("data", "warming.csv")) %>% clean_names()

# Prelim plots
#data_dmg %>% ggplot(aes(x = warming, y = damages)) + geom_point() + stat_smooth(se = TRUE)
#data_wrm %>% ggplot(aes(x = x, y = warming_baseline)) + geom_point() + stat_smooth(se = TRUE)
#data_wrm %>% ggplot(aes(x = x, y = warming_pulse)) + geom_point() + stat_smooth(se = TRUE)

# Variables
pulse <- 3.5*10^9 # tons of CO2

# Add in explicit y-intercept at 0?
#data_dmg <- data_dmg %>% add_row(warming = 0, damages = 0, .before = 1)


####
# 1.
####

# Fit quadratic model
data_dmg <- data_dmg %>% mutate(warming_sq = warming^2)
quad_dmg <- lm(damages ~ warming + warming_sq, data = data_dmg)
#summary(quad_dmg)

# Store output into funtion
damages_model <- function(warming) {
  quad_dmg$coefficients[["warming_sq"]]*(warming^2) + quad_dmg$coefficients[["warming_sq"]]*warming
}

# Plot model over data points
ggplot(data = data_dmg, mapping = aes(x = warming, y = damages)) +
  xlim(0,10) +
  geom_point(col = "red") +
  stat_function(fun = damages_model, size = 2)


####
# 2.
####

# Use model to make predictions based on anticipated warming
data_wrm <- data_wrm %>%
  mutate(predicted_baseline = damages_model(warming_baseline)) %>% 
  mutate(predicted_pulse = damages_model(warming_pulse)) %>%
  mutate(predicted_diff = predicted_pulse - predicted_baseline) %>%
  mutate(dmg_per_ton = predicted_diff/pulse)

# Plot 1: Damages over time without the pulse
data_wrm %>% 
  ggplot(aes(x = x, y = predicted_baseline)) +
  geom_point() +
  labs(x = "Years (from 2020)", y = "Predicted damages ($)")

# Plot 2: Damages over time with the pulse
data_wrm %>% 
  ggplot(aes(x = x, y = predicted_pulse)) +
  geom_point() +
  labs(x = "Years (from 2020)", y = "Predicted damages ($)")

# Plot 3: Difference in damages over time between pulse/no-pulse
data_wrm %>% 
  ggplot(aes(x = x, y = predicted_diff)) +
  geom_point() +
  labs(x = "Years (from 2020)", y = "Predicted differences ($)")

# Plot 4: Difference in damages over time per ton of CO2
data_wrm %>% 
  ggplot(aes(x = x, y = dmg_per_ton)) +
  geom_point() +
  labs(x = "Years (from 2020)", y = "Difference in damage per ton of CO2 ($)")


####
# 3.
####

# Lecture 11, ~23:55, PV = B/r ????
# If Obama SCC = 51 using 3% discount rate

# this is incorrect
B <- 51 * 0.03

scc_model <- function(rate_pct) {
  B/(rate_pct/100)
}

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  xlim(0.5, 10) +
  stat_function(fun = scc_model, size = 2) +
  labs(x = "Discount Rate (%)", y = "SCC ($)")


####
# 4.
####

# Ramsey Rule variables
p <- 0.001
n <- 2
g <- 0.01

ramsey_rule <- function(p, n, g) {
  p + n*g
}

rr_discount_pct <- ramsey_rule(p,n,g) * 100
scc_rr <- scc_model(rr_discount_pct)

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  xlim(0.5, 10) +
  stat_function(fun = scc_model, size = 1) +
  labs(x = "Discount Rate (%)", y = "SCC ($)") +
  geom_point(aes(x = rr_discount_pct, y = scc_rr), size = 3, col = "red")


####
# 5.
####


