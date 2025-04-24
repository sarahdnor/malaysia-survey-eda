# ---- Loading packages ---- 

library(tidyverse)
library(readr)
library(naniar)
library(skimr)
library(janitor)
library(gridExtra)
library(ggcorrplot)

# ---- Reading data ---- 

skincare_malaysia <- read_csv("data/created_data/skincare_malaysia_2021_cleaned.csv")

# ---- Exploration ----

## ---- Correlation Analysis ----

skincare_malaysia_means <- skincare_malaysia |>
  group_by(understanding_of_products) |>
  mutate(
    skincare_spending = fct_recode(skincare_spending,
                                   "Less than RM 20" = "10",
                                   "RM 20 – RM 50" = "35",
                                   "RM 50 – RM 100" = "75",
                                   "RM 100 – RM 150" = "125",
                                   "> RM 150" = "175",
    )
  ) |>
  summarise(
    mean_skincare_spending = mean(as.numeric(skincare_spending), na.rm = TRUE),
    mean_skincare_importance = mean(skincare_importance, na.rm = TRUE),
    mean_new_skincare = mean(trying_new_skincare, na.rm = TRUE),
    mean_time_spent_finding_skincare = mean(time_spent_finding_routine, na.rm = TRUE),
    mean_doubts_skincare_media = mean(doubts_skincare_media, na.rm = TRUE),
    mean_product_improvement = mean(product_no_improvement, na.rm = TRUE),
    mean_unaware_ingredient_quality = mean(unaware_ingredient_quality, na.rm = TRUE),
    mean_allergy_experience = mean(experienced_allergies, na.rm = TRUE),
    mean_desired_skincare_efficiency = mean(desire_skincare_efficiency, na.rm = TRUE),
    mean_want_solutions = mean(want_solutions, na.rm = TRUE),
    mean_desire_rountine = mean(desire_for_routine, na.rm = TRUE),
    mean_desire_know_more = mean(desire_know_more, na.rm = TRUE),
    mean_desired_skincare_efficiency = mean(desire_skincare_efficiency, na.rm = TRUE),
    mean_want_solutions = mean(desire_better_lifestyle, na.rm = TRUE),
    mean_excitment_skincare_ai = mean(excitment_skincare_ai, na.rm = TRUE)
  )

skincare_malaysia_means_correlation_plot <- skincare_malaysia_means |>
  cor() |>
  ggcorrplot() +
  labs(
    title = "Correlation Matrix of Skincare Survey Variables"
  )

ggsave(
  filename = "plots/skincare_malaysia_means_correlation_plot.png",
  plot = skincare_malaysia_means_correlation_plot,
)

## ---- Distribution of Respondent Characteristics ----

### ---- Distribution of Cultural Identities ----

distribution_of_cultural_identity_plot <- skincare_malaysia |>
  ggplot(aes(y = cultural_identity)) +
  geom_bar() +
  labs(
    title = "Distribution of Cultural Identity",
    x = "Count",
    y = "Cultural Identity"
  )

ggsave(
  filename = "plots/distribution_of_cultural_identity_plot.png",
  plot = distribution_of_cultural_identity_plot,
  height = 3
)

### ---- Distribution of Malaysians and Other ----

distribution_of_cultural_identity_other_plot <- skincare_malaysia |>
  mutate(cultural_identity = fct_collapse(cultural_identity,
                                          "arab" =  "arab",
                                          "malaysian" = c("bumiputra sabah","malay"),
                                          "korean" = "korean",
                                          "chinese" = "chinese",
                                          "libya" = "libya",
                                          "indian" = "indian",
                                          "african" = "african",
                                          "javanesse" = "javanesse",
                                          "iban" = "iban",
                                          "nigerian" = "nigerian",
                                          "sudanese" = "sudanese"),
         cultural_identity  = fct_lump_lowfreq(cultural_identity)) |>
  ggplot(aes(y = cultural_identity)) +
  geom_bar() +
  labs(
    title = "Distribution of Cultural Identity: Malaysian and Other",
    x = "Count",
    y = "Cultural Identity"
  )

ggsave(
  filename = "plots/distribution_of_cultural_identity_other_plot.png",
  plot = distribution_of_cultural_identity_other_plot,
  height = 3
)


### ---- Distribution of Occupation ----

distribution_of_occupation_plot <- skincare_malaysia |>
  mutate(occupation = occupation |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = occupation)) +
  geom_bar() +
  labs(
    title = "Distribution of Occupation",
    x = "Occupaton",
    y = "Count"
  )

ggsave(
  filename = "plots/distribution_of_occupation_plot.png",
  plot = distribution_of_occupation_plot,
  height = 4
)

### ---- Distribution of Student and Other ----

distribution_of_occupation_other_plot <- skincare_malaysia |>
  mutate(
    occupation = fct_lump_lowfreq(occupation),
    occupation = occupation |> fct_infreq() |> fct_rev()
  ) |>
  ggplot(aes(x = occupation)) +
  geom_bar() +
  labs(
    title = "Distribution of Occupation: Student and Other",
    x = "Occupation",
    y = "Count"
  )

ggsave(
  filename = "plots/distribution_of_occupation_other_plot.png",
  plot = distribution_of_occupation_other_plot,
  height = 4
)


### ---- Distribution of Age ----

distribution_age_plot <- skincare_malaysia |>
  ggplot(aes(x = age)) +
  geom_bar() +
  labs(
    title = "Distribution of Respodent Age",
    x = "Age Bracket",
    y = "Count"
  )

ggsave(
  filename = "plots/distribution_age_plot.png",
  plot = distribution_age_plot,
  height = 4
)

### ---- Distribution of Gender ----

gender_proportion <- skincare_malaysia |>
  group_by(gender) |>
  summarise(count = n()) |>
  mutate(proportion = count / sum(count))

kable(gender_proportion, caption = "Proportion of Gender")

## ---- Gender Explicit Analysis ----

## ---- Skincare Experience by Gender ----

gender_experience_skincare <- skincare_malaysia |>
  group_by(gender) |>
  summarise(
    prop_skincare_experience_yes = mean(skincare_experience == "Yes", na.rm = TRUE)
  )

kable(gender_experience_skincare)

## ---- Ingredient Knowledge Versus Skincare Importance by Gender ----

skincare_important_and_ingredient_knowledge <- skincare_malaysia |>
  ggplot(aes(x = unaware_ingredient_quality, y = skincare_importance, color = gender)) +
  geom_jitter() +
  labs(
    title = "Relationship Between Skincare Importance and Ingrediedent Knowledge and Gender",
    x = "Rating of Understanding of Skincare Ingredients",
    y = "Rating of Skincare Importance"
  )

ggsave(
  filename = "plots/skincare_important_and_ingredient_knowledge.png",
  plot = skincare_important_and_ingredient_knowledge,
  width = 10,
  height = 6
)

## ---- Skincare Spending Versus Skincare Importance ----

skincare_spending_versus_importance_plot <- skincare_malaysia |>
  ggplot(aes(x = skincare_spending, y = skincare_importance)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Percieved Skincare Importance by Average Monthly Spending",
    y = "Skincare Importance Ranking",
    x = "Average Monthly Spending (MYR)"
  )

skincare_spending_versus_importance_plot

ggsave(
  filename = "plots/skincare_spending_versus_importance_plot.png",
  plot = skincare_spending_versus_importance_plot,
  height = 5,
  width = 8
)

## ---- Spending by Gender ----

skincare_malaysia_spending <- skincare_malaysia |>
  mutate(
    skincare_spending = fct_recode(skincare_spending,
                                   "10" = "Less than RM 20",
                                   "35" = "RM 20 - RM 50",
                                   "75" = "RM 50 - RM 100",
                                   "125" = "RM 100 - RM 150",
                                   "175" = "> RM 150"
    ),
    skincare_spending = fct_relevel(skincare_spending, "10", "35", "75", "125", "175")
  ) |>
  count(skincare_spending, gender) |>
  group_by(skincare_spending) |>
  mutate(
    percentage = n/sum(n)
  )

skincare_malaysia_spending_plot <- skincare_malaysia_spending |>
  ggplot(aes(x = skincare_spending, y = percentage, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Proportion of Monthly Spending Brackets by Gender",
    y = "Percentage",
    x = "Monthly Skincare Spending in Malaysian Ringgit",
    fill = "Gender"
  )

ggsave(
  filename = "plots/skincare_malaysia_spending_plot.png",
  plot = skincare_malaysia_spending_plot,
  height = 5
)


## ---- Prominent Skin Concerns ----

## ---- Most Common Skin Concerns ----

skincare_malaysia_skin_types <- skincare_malaysia |>
  mutate(
    oily_skin = if_else(str_detect(skin_type, "I have oily skin"), "yes", "no"),
    acne_breakouts = if_else(str_detect(skin_type, "I struggle with acne and breakout"), "yes", "no"),
    combination = if_else(str_detect(skin_type, "I have a combination skin with dry and oily places"), "yes", "no"),
    uneven_skin = if_else(str_detect(skin_type, "I have an uneven skin tone"), "yes", "no"),
    brown_spots = if_else(str_detect(skin_type, "I have brown spots from sun damages"), "yes", "no"),
    dry_skin = if_else(str_detect(skin_type, "I have dry and dull skin"), "yes", "no"),
    pigmentation = if_else(str_detect(skin_type, "I have pigmentation"), "yes", "no"),
    normal_skin = if_else(str_detect(skin_type, "I have normal skin with very minor problem"), "yes", "no"),
    redness = if_else(str_detect(skin_type, "I suffer from redness and sensitivity"), "yes", "no"),
    wrinkles = if_else(str_detect(skin_type, "wrinkles"), "yes", "no"),
    clogged_pores = if_else(str_detect(skin_type, "I have oily skin;blackheads and whiteheads due to clogged pores"), "yes", "no"),
    eczema = if_else(str_detect(skin_type, "eczema"), "yes", "no")
  ) |>
  relocate(oily_skin:eczema, .after = skin_type) |>
  mutate(cultural_identity = fct_collapse(cultural_identity,
                                          "arab" = "arab",
                                          "malaysian" = c("bumiputra sabah", "malay"),
                                          "korean" = "korean",
                                          "chinese" = "chinese",
                                          "libya" = "libya",
                                          "indian" = "indian",
                                          "african" = "african",
                                          "javanesse" = "javanesse",
                                          "iban" = "iban",
                                          "nigerian" = "nigerian",
                                          "sudanese" = "sudanese"))

write_csv(x = skincare_malaysia_skin_types, file = "data/skincare_malaysia_2021_skin_types.csv")

skincare_malaysia_skin_types_yes <- skincare_malaysia_skin_types |>
  summarise(
    oily_skin = sum(oily_skin == "yes"),
    acne_breakouts = sum(acne_breakouts == "yes"),
    combination = sum(combination == "yes"),
    uneven_skin = sum(uneven_skin == "yes"),
    brown_spots = sum(brown_spots == "yes"),
    dry_skin = sum(dry_skin == "yes"),
    pigmentation = sum(pigmentation == "yes"),
    normal_skin = sum(normal_skin == "yes"),
    redness = sum(redness == "yes"),
    wrinkles = sum(wrinkles == "yes"),
    clogged_pores = sum(clogged_pores == "yes"),
    eczema = sum(eczema == "yes")
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "skin_types", 
    values_to = "yes_count"
  )

skincare_malaysia_skin_types_yes_plot <- skincare_malaysia_skin_types_yes |>
  ggplot(aes(y = skin_types, x = yes_count)) +
  geom_col() +
  labs(
    title = "Skin Type Frequency Amongst Respondents",
    y = "Skin Types",
    x = "Occurrences"
  )

ggsave(
  filename = "plots/skincare_malaysia_skin_types_yes_plot.png",
  plot = skincare_malaysia_skin_types_yes_plot,
  height = 5
)

## ---- Combination Skin Across Cultural Identities ----

cultural_identity_combination_skin_plot <- 
  ggplot(skincare_malaysia_skin_types, aes(y = cultural_identity, fill = combination)) +
  geom_bar(position = "fill") + 
  labs(
    title = "Proportion of Combination Skin Across Cultural Identities",
    x = "Cultural Identity",
    y = "Proportion",
    fill = "Combination Skin"
  ) 

## ---- Acne Breakouts Across Cultural Identities ----

cultural_identity_acne_breakouts_plot <- 
  ggplot(skincare_malaysia_skin_types, aes(y = cultural_identity, fill = acne_breakouts)) +
  geom_bar(position = "fill") + 
  labs(
    title = "Proportion of Acne Breakouts Across Cultural Identities",
    x = "Cultural Identity",
    y = "Proportion",
    fill = "Acne Breakouts"
  ) 

## ---- Oily Skin Across Cultural Identities ----

cultural_identity_oily_skin_plot <-
  ggplot(skincare_malaysia_skin_types, aes(y = cultural_identity, fill = oily_skin)) +
  geom_bar(position = "fill") + 
  labs(
    title = "Proportion Oily Skin Across Cultural Identities",
    x = "Cultural Identity",
    y = "Proportion",
    fill = "Oily Skin"
  ) 

skin_types_cultural_identities_comparison_plot <- grid.arrange(
  cultural_identity_combination_skin_plot,
  cultural_identity_acne_breakouts_plot,
  cultural_identity_oily_skin_plot,
  ncol = 1
)

# saving 3 previous plots as one 

ggsave(
    filename = "plots/skin_types_cultural_identities_comparison_plot.png",
    plot = skin_types_cultural_identities_comparison_plot,
  )

## ---- Skin concerns by gender ----

skincare_malaysia_skin_types_yes_gender <- skincare_malaysia_skin_types |>
  summarise(
    oily_skin = mean(oily_skin == "yes"),
    acne_breakouts = mean(acne_breakouts == "yes"),
    combination = mean(combination == "yes"),
    uneven_skin = mean(uneven_skin == "yes"),
    brown_spots = mean(brown_spots == "yes"),
    dry_skin = mean(dry_skin == "yes"),
    pigmentation = mean(pigmentation == "yes"),
    normal_skin = mean(normal_skin == "yes"),
    redness = mean(redness == "yes"),
    wrinkles = mean(wrinkles == "yes"),
    clogged_pores = mean(clogged_pores == "yes"),
    eczema = mean(eczema == "yes"),
    .by = gender
  ) |>
  pivot_longer(
    cols = -gender, 
    names_to = "skin_types", 
    values_to = "yes_count"
  )


skincare_malaysia_skin_types_yes_gender_plot <- skincare_malaysia_skin_types_yes_gender |>
  ggplot(aes(y = skin_types, x = yes_count, fill = gender)) +
  geom_col(position = "dodge") +
  labs(
    title = "Proportion of Skin Concerns by Gender",
    x = "Proportion of Respondents",
    y = "Skin Concerns",
    fill = "Gender"
  )

ggsave(
  filename = "plots/skincare_malaysia_skin_types_yes_gender_plot.png",
  plot = skincare_malaysia_skin_types_yes_gender_plot,
  height = 5
)

## ---- Analysis of AI ----


## ---- Attitudes Towards General Technology Versus AI ----

skincare_malaysia |>
  filter(
    positive_perception_technology == "Yes"
  ) |>
  count()

skincare_malaysia |>
  filter(
    positive_perception_ai_skin == "Yes"
  ) |>
  count()

perceptions_technology_plot <- skincare_malaysia |>
  ggplot(aes(x = positive_perception_technology, fill = positive_perception_technology)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(breaks = seq(0, 70, 5), limits = c(0, 70)) +
  theme(legend.position = "none") +
  labs(
    title = "Positive Perception of General Technology",
    x = "Positive Perception",
    y = "Count"
  )

perceptions_skin_scanning_plot <- skincare_malaysia |>
  ggplot(aes(x = positive_perception_ai_skin, fill = positive_perception_ai_skin)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(breaks = seq(0, 70, 5), limits = c(0, 70)) +
  labs(
    title = "Positive Perception of AI in Skincare",
    x = "Positive Perception",
    y = "Count",
    fill = NULL
  )

technology_ai_perceptions_comparison_plot <- grid.arrange(
  perceptions_technology_plot,
  perceptions_skin_scanning_plot,
  ncol = 2,
  widths = c(1, 1) 
)

ggsave(
  filename = "plots/technology_ai_perceptions_comparison_plot.png",
  plot = technology_ai_perceptions_comparison_plot,
  height = 5
)

## ---- Ingredient Preferences ----

skincare_malaysia_ingredient_perceptions <- skincare_malaysia |>
  mutate(
    natural_ingredients = if_else(str_detect(perceptions_of_ingredients, "Natural ingredients"), "yes", "no"),
    aoncomedogenic_ingredients = if_else(str_detect(perceptions_of_ingredients, "Noncomedogenic ingredients (ingredients that do not block pores)"), "yes", "no"),
    alcohol_free_ingredients = if_else(str_detect(perceptions_of_ingredients, "Alcohol-free ingredients"), "yes", "no"),
    oil_free_ingredients = if_else(str_detect(perceptions_of_ingredients, "Oil-free ingredients"), "yes", "no"),
    petroleum_free_ingredients = if_else(str_detect(perceptions_of_ingredients, "Petroleum-free ingredients"), "yes", "no"),
    dye_free_ingredients = if_else(str_detect(perceptions_of_ingredients, "Dye-free ingredients"), "yes", "no"),
    low_ph_ingredients = if_else(str_detect(perceptions_of_ingredients, "Low pH"), "yes", "no"),
    fragrance_free_ingredients = if_else(str_detect(perceptions_of_ingredients, "fragrance-free"), "yes", "no"),
  ) |>
  relocate(
    natural_ingredients:fragrance_free_ingredients, 
    .after = perceptions_of_ingredients
  )

View(skincare_malaysia_ingredient_perceptions)

write_csv(x = skincare_malaysia_ingredient_perceptions, file = "data/skincare_malaysia_2021_ingredient_preferences.csv")

skincare_malaysia_ingredient_perceptions_yes <- skincare_malaysia_ingredient_perceptions |>
  summarize(
    natural_ingredients = sum(str_detect(ingredient_preference, "Natural ingredients")),
    aoncomedogenic_ingredients = sum(str_detect(ingredient_preference, "Noncomedogenic ingredients (ingredients that do not block pores)")),
    alcohol_free_ingredients = sum(str_detect(ingredient_preference, "Alcohol-free ingredients")),
    oil_free_ingredients = sum(str_detect(ingredient_preference, "Oil-free ingredients")),
    petroleum_free_ingredients = sum(str_detect(ingredient_preference, "Petroleum-free ingredients")),
    dye_free_ingredients = sum(str_detect(ingredient_preference, "Dye-free ingredients")),
    low_ph_ingredients = sum(str_detect(ingredient_preference, "Low pH")),
    fragrance_free_ingredients = sum(str_detect(ingredient_preference, "fragrance-free")) 
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "ingredient_preferences", 
    values_to = "yes_count"
  ) |>
  rename(
    "Ingredients" = ingredient_preferences,
    "Respondent Preferences" = yes_count
    
  )

kable(skincare_malaysia_ingredient_perceptions_yes)

## ---- Preference for Natural Ingredients Versus Excitement About AI ----

skincare_malaysia_ingredient_perceptions_plot <- skincare_malaysia_ingredient_perceptions |>
  ggplot(aes(x = natural_ingredients, y = excitment_skincare_ai)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of excitment about AI and preference for natural ingredients",
    x = "Preference for Natural Ingredients",
    y = "Excitment about AI"
  )

ggsave(
  filename = "plots/skincare_malaysia_ingredient_perceptions_plot.png",
  plot = skincare_malaysia_ingredient_perceptions_plot,
)

## ---- Time Spent Finding Routine Versus Excitement About AI ----

time_finding_routine_excitment_skincare_plot <- skincare_malaysia |>
  ggplot(aes(x = factor(time_spent_finding_routine), y = excitment_skincare_ai)) +
  geom_boxplot() +
  labs(
    title = "Time Spent on Skincare vs. Excitement About AI",
    x = "Time Spent Finding Routine",
    y = "Excitement About AI Skincare"
  )

ggsave(
  filename = "plots/time_finding_routine_excitment_skincare_plot.png",
  plot = time_finding_routine_excitment_skincare_plot,
  height = 4
)

## ---- Desire to Learn More About skincare Versus Excitement About AI ----


desire_know_more_excitment_skincare_plot <- skincare_malaysia |>
  ggplot(aes(x = factor(desire_know_more), 
             y = excitment_skincare_ai)) +
  geom_boxplot() +
  labs (
    title = "Desire to Learn About Skincare vs. Excitement for AI",
    x = "Desire to Know More About Skincare",  
    y = "Excitement About AI in Skincare"      
  )

ggsave(
  filename = "plots/desire_know_more_excitment_skincare_plot.png",
  plot = desire_know_more_excitment_skincare_plot,
  height = 4
)

## ---- Respondent Skincare Motivations (Glass Skin) ----

skincare_malaysia_motivations <- skincare_malaysia |>
  filter( 
    !is.na(motivations)
  ) |>
  mutate(
    motivations = str_to_lower(motivations),
    glass_skin = if_else(str_detect(motivations, c("glowing|glass|flawless|bright|clear")), "yes", "no")
  ) |>
  count(glass_skin) |>
  mutate(
    proportion = n / sum(n)
  )

skincare_malaysia_motivations

