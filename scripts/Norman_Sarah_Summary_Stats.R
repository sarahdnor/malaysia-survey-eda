# Loading data

library(tidyverse)
library(readr)
library(naniar)
library(skimr)
library(janitor)
library(knitr)

# Reading data

skincare_malaysia <- read_csv("data/skincare_malaysia_2021_cleaned.csv")
skincare_malaysia_original <- read_csv("data/skincare_survey_malaysia_2021.csv")

# Summary statistics

skim_without_charts(skincare_malaysia_original)

## Occupation

skincare_malaysia_original|>
  count(Occupation) |> 
  kable()

## Gender

skincare_malaysia_original|>
  count(Gender) |> 
  kable()

## Race

skincare_malaysia_original|>
  distinct(Race) |>
  kable()

## Missingness

missingness_plot <- skincare_malaysia_original |>
  select("Age", "Do share your skincare goals and motivation with us!") |>
  gg_miss_var() +
  labs(
    title = "Distribution of Missing Values"
  )

ggsave(
  filename = "plots/missingness_plot.png",
  plot = missingness_plot,
  width = 10,
  height = 5
)

