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
pulse <- 35*10^9 # tons of CO2

####
# 1.
####

# Fit quadratic model
data_dmg <- data_dmg %>% mutate(warming_sq = warming^2)
quad_dmg <- lm(damages ~ warming + warming_sq + 0, data = data_dmg)
#summary(quad_dmg)

# Store output into funtion
damages_model <- function(warming) {
  out <- quad_dmg$coefficients[["warming_sq"]]*(warming^2) + quad_dmg$coefficients[["warming"]]*warming
  return(out)
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

# We need to somehow use the dmg_per_ton values from #2 Plot 4 to infer SCC
# SCC = PV with carbon pulse - PV without carbon pulse

PV_calc <- function(values, discount_rate = 0.02) {
  sum <- 0;
  r <- discount_rate
  for (i in c(1:length(values))) {
    current <- values[i]/(1 + r)^i
    sum <- sum + current
  }
  
  return(sum)
}

# discount rates representin 1% intervals between 1%-10%
dr <- seq(0.01, .1, 0.005)


PV_calc(data_wrm$dmg_per_ton, dr)

scc_points <- data.frame(discount_rate <- dr, scc <- PV_calc(data_wrm$dmg_per_ton, dr))
colnames(scc_points) <- c("discount_rate","scc")

ggplot(data = scc_points, mapping = aes(x = discount_rate * 100, y = scc)) +
  geom_point() +
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

rr_discount <- ramsey_rule(p,n,g)
rr_scc <- PV_calc(data_wrm$dmg_per_ton, rr_discount)

# This will depend on #3
# scc_rr <- scc_model(rr_discount_pct)
ggplot(data = scc_points, mapping = aes(x = discount_rate * 100, y = scc)) +
  geom_line() +
  labs(x = "Discount Rate (%)", y = "SCC ($)") +
  geom_point(aes(x = rr_discount_pct, y = rr_scc), col = "red", size = 3)


####
# 5.
####

# Scenario A
data_wrm <- data_wrm %>%
  mutate(warming_baseline_150 = warming_baseline * 1.5) %>%
  mutate(predicted_baseline_150 = damages_model(warming_baseline_150)) %>%
  mutate(warming_baseline_2050 = 
           case_when( year > 2050 ~ 1.29,
                     TRUE ~ warming_baseline
          )) %>% 
  mutate(predicted_baseline_2050 = damages_model(warming_baseline_2050))
  

A_baseline <- PV_calc(data_wrm$predicted_baseline, 0.02)
A_baseline_150 <- PV_calc(data_wrm$warming_baseline_150, 0.02)

A_expected_value <- (A_baseline * 0.5) + (A_baseline_150 * 0.5)
B_expected_value <- PV_calc(data_wrm$predicted_baseline_2050, 0.02)

# How to get "max cost"
