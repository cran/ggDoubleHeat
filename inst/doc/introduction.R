## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width = 8, 
  fig.height = 5,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggplot2)
library(ggDoubleHeat)
pitts_tg


## -----------------------------------------------------------------------------
library(dplyr)
library(tidyr)
pitts_tg %>%
  pivot_longer(cols = c(Twitter:Google), names_to = "source", values_to = "incidence_rate") %>%
  ggplot(aes(week, category, fill = incidence_rate)) +
  geom_tile() +
  facet_wrap(~source)

## -----------------------------------------------------------------------------
pitts_tg %>%
  ggplot(aes(week, category)) +
  geom_heat_grid(outside = Google, inside = Twitter)

## -----------------------------------------------------------------------------
pitts_tg %>%
  ggplot(aes(week, category)) +
  geom_heat_grid(outside = Google, inside = Twitter, trans = "sqrt")

## -----------------------------------------------------------------------------
pitts_tg %>%
  ggplot(aes(week, category)) +
  geom_heat_grid(outside = Google, inside = Twitter, r = 5)

## -----------------------------------------------------------------------------
pitts_tg %>%
  ggplot(aes(week, category)) +
  geom_heat_grid(outside = Google, inside = Twitter) +
  remove_padding()

## -----------------------------------------------------------------------------
pitts_tg %>%
  ggplot(aes(week, category)) +
  geom_heat_grid(outside = Google, outside_colors = c("lightblue", "springgreen2"),
                 inside = Twitter, inside_colors = c("lightpink", "orange"))

## -----------------------------------------------------------------------------
pitts_tg %>%
  ggplot(aes(week, category)) +
  geom_heat_circle(outside = Google, inside = Twitter)

## -----------------------------------------------------------------------------
pitts_tg %>%
  filter(week >= 10, week <= 20) %>%
  ggplot(aes(week, category)) +
  geom_heat_circle(outside = Google, inside = Twitter, trans = "sqrt") +
  scale_x_continuous(breaks = seq(10,20))

## -----------------------------------------------------------------------------
pitts_tg %>%
  ggplot(aes(week, category)) +
  geom_heat_tri(lower = Google, upper = Twitter)

## -----------------------------------------------------------------------------
pitts_tg %>%
  ggplot(aes(week, category)) +
  geom_heat_grid(outside = Google, inside = Twitter, trans = "sqrt",
                 r = 3.6,
                 labels = scales::percent_format(scale = 1)) +
  theme_heat() +
  remove_padding() +
  labs(title = "Pittsburgh Google & Twitter Incidence Rate")

## ----fig.height=5, fig.width=8------------------------------------------------
states_tg %>%
  ggplot(aes(week, category)) +
  geom_heat_grid(outside = Google, inside = Twitter, r = 4, trans = "sqrt") +
  facet_wrap(~state) 

## -----------------------------------------------------------------------------
iris_summarized <- iris %>%
  pivot_longer(c(1:4), names_to = "metric") %>%
  group_by(Species, metric) %>%
  summarize(mean = mean(value),
            median = median(value)) %>%
  ungroup()

iris_summarized

## -----------------------------------------------------------------------------
iris_summarized %>%
  pivot_longer(c(mean:median), names_to = "summary_metric") %>%
  ggplot(aes(value, Species, fill = summary_metric)) +
  geom_col(position = "dodge") +
  facet_wrap(~metric) +
  labs(fill = NULL)

## -----------------------------------------------------------------------------
iris_summarized %>%
  ggplot(aes(Species, metric)) +
  geom_heat_grid(outside = mean, inside = median)

