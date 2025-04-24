## ---- Reading/Loading ----

# Loading packages
library(tidyverse)
library(readr)
library(naniar)
library(skimr)
library(janitor)

# Loading data
skincare_malaysia <- read_csv("data/skincare_survey_malaysia_2021.csv", na = c(".", "-", "NA"))

## ---- Tidying data ----

# Cleaning names

skincare_malaysia <- skincare_malaysia |>
  clean_names() 

# Renaming variables

skincare_malaysia <- skincare_malaysia |>
  rename(
    product_choice_process = how_do_did_you_choose_your_products,
    cultural_identity = race,
    purchase_frequency = how_often_do_you_buy_skincare_products,
    skincare_spending = on_average_how_much_do_you_spend_on_skincare_products_each_month,
    ingredient_preference = which_of_the_following_types_of_ingredients_would_make_you_more_likely_to_buy_a_skin_care_product,
    skincare_importance = do_you_agree_that_skincare_is_important,
    use_of_samples = do_you_use_samples_before_buying_skincare_products,
    trying_new_skincare = how_willing_are_you_to_try_different_skin_care_products,
    where_purchased = where_do_you_purchase_your_skin_care_products,
    time_spent_finding_routine = i_wasting_to_much_time_to_find_out_skincare_and_routine_that_suits_my_skin,
    doubts_skincare_media = i_being_doubtful_about_the_information_shared_by_influencers_and_brand_promoted_content,
    understanding_of_products = i_feel_difficult_to_understand_the_list_of_ingredients_on_the_products,
    product_no_improvement = i_bought_expensive_product_but_doesnt_see_any_improvement_on_my_skin,
    unaware_ingredient_quality = i_unaware_of_which_ingredients_is_the_best_or_to_avoid_according_my_skin_type,
    experienced_allergies = i_experienced_allergies_after_use_a_new_skincare_products_eg_rashness_acne_purging_etc,
    desire_skincare_efficiency = i_want_to_reduce_time_to_find_which_routine_products_suits_with_my_skin,
    want_solutions = i_want_have_solutions_from_expert_in_effortless_and_cheap_way,
    desire_for_routine = i_want_to_have_my_personalized_skincare_routine_that_suitable_with_my_current_products,
    desire_know_more = i_want_to_gain_knowledge_of_skincare_regime_in_easy_and_understandable_way,
    desire_better_lifestyle = i_want_to_adapt_a_healthy_lifestyle_for_a_glowing_and_healthy_skin,
    positive_perception_technology = do_you_think_that_technology_can_improve_your_skincare_routine,
    heard_of_ai = do_you_have_heard_about_ai_artificial_intelligent,
    positive_perception_ai_skin = after_you_know_about_ai_do_you_want_to_have_a_skin_scanning_app_that_can_customize_skincare_regime,
    excitment_skincare_ai = do_you_feel_excited_to_use_this_skincare_application,
    motivations = do_share_your_skincare_goals_and_motivation_with_us,
    skin_type = which_if_any_of_the_following_statements_applies_to_you,
    skincare_experience = have_you_ever_used_any_skin_care_products
  ) |>
  mutate(
    cultural_identity = factor(str_to_lower(cultural_identity)),
    cultural_identity = fct_collapse(cultural_identity,
                                     "bumiputra sabah" = c("bumiputra sabah", "bumiputra sabah (bajau)", "sabahan"),
                                     "arab" = c("arabian", "arab"),
                                     "malay" = "malay",
                                     "korean" = "korean",
                                     "chinese" = "chinese",
                                     "libya" = "libya",
                                     "indian" = "indian",
                                     "african" = "african",
                                     "javanesse" = "javanesse",
                                     "iban" = "iban",
                                     "nigerian" = "nigerian",
                                     "sudanese" = "sudanese"),
    occupation = factor(str_to_lower(occupation)),
    gender = factor(str_to_lower(gender)),
    purchase_frequency = factor(str_to_lower(purchase_frequency)),
    product_choice_process = str_to_lower(product_choice_process),
    skincare_spending = factor(skincare_spending),
    skincare_experience = factor(skincare_experience)
  )

# Change factor order

skincare_spending_levels <- c("Less than RM 20", "RM 20 - RM 50", "RM 50 - RM 100", "RM 100 - RM 150", "> RM 150")

age_levels <- c("NA", "15-20", "20-25", "25-30", "30-35", ">35")


skincare_malaysia <- skincare_malaysia |>
  mutate(
    skincare_spending = factor(skincare_spending, levels = skincare_spending_levels),
    age = factor(age, levels = age_levels)
  ) 

levels(skincare_malaysia$skincare_spending)

# Creating codebook

skincare_malaysia_codebook <- tibble(
  "Column Name" = c(
    "timestamp", "gender", "age", "cultural_identity", "occupation",
    "skincare_importance", "skincare_experience", "skin_type", "perceptions_of_ingredients",
    "product_choice_process", "use_of_samples", "purchase_frequency", "trying_new_skincare",
    "where_purchased", "skincare_spending", "time_spent_finding_routine", "doubts_skincare_media",
    "understanding_of_products", "product_no_improvement", "unaware_ingredient_quality",
    "experienced_allergies", "desire_skincare_efficiency", "want_solutions", "desire_for_routine",
    "desire_know_more", "desire_better_lifestyle", "perceptions_of_technology", "heard_of_ai",
    "perceptions_skin_scanning", "excitement_skincare_ai", "motivations"
  ),
  "Description" = c(
    "The timestamp indicating when the survey was submitted by the respondent",
    "The gender of the respondent",
    "The age bracket of the respondent, grouped in 5-year intervals",
    "The cultural identity of the respondent",
    "The occupation of the respondent",
    "The respondent's ranking of skincare importance on a scale of 1–5 (1 being low, 5 being very important)",
    "Whether the respondent has experience using skincare products",
    "Skin characteristics that apply to the respondent (e.g., oily skin, dark spots, etc.)",
    "Ingredients that make the respondent more likely to purchase a skincare product",
    "Factors that influence the respondent’s product purchasing decisions",
    "Whether the respondent uses samples before purchasing skincare products",
    "How often the respondent purchases new skincare products, on a scale of very often, often, sometimes, rarely, or never",
    "The respondent's willingness to try new products on a scale of 1–5 (1 being low, 5 being very high)",
    "The places where respondents purchase their skincare products",
    "The average amount spent on skincare products monthly, in Malaysian Ringgit",
    "The time wasted finding a suitable skincare routine, rated on a scale of 1–5 (1 being litte, 5 being a lot)",
    "The respondent's doubt about information shared by influencers and brand-promoted content, rated on a scale of 1–5 (1 being low, 5 being very high)",
    "The difficulty in understanding the list of ingredients in skincare products, rated on a scale of 1–5 (1 being easy, 5 being very difficult)",
    "Applicability of the statement: 'I have bought expensive products but didn't see any improvement in my skin,' rated on a scale of 1–5 (1 being not applicable, 5 being very applicable)",
    "The respondent's knowledge of which ingredients to avoid for their skin type, rated on a scale of 1–5 (1 being highly aware, 5 being unaware)",
    "Frequency of experiencing allergies after using a new skincare product (e.g., rashes, acne, purging), rated on a scale of 1–5 (1 being never, 5 being very frequent)",
    "The desire to reduce the time spent finding a suitable skincare routine, rated on a scale of 1–5 (1 being none, 5 being very high)",
    "The desire to receive expert skincare solutions in an effortless and affordable way, rated on a scale of 1–5 (1 being none, 5 being very high)",
    "The desire for a personalized skincare routine that suits the respondent's current products, rated on a scale of 1–5 (1 being none, 5 being very high)",
    "The desire to gain more knowledge about skincare routines, rated on a scale of 1–5 (1 being none, 5 being very high)",
    "The desire to adopt a healthy lifestyle for glowing and healthy skin, rated on a scale of 1–5 (1 being none, 5 being very high)",
    "Whether or not the respondent believes that technology can improve their skincare routine",
    "Whether the respondent has heard of AI face scanning",
    "Whether or not the respondent is willing to use an AI skin scanning app that can customize skincare routines",
    "The respondent's excitement about using AI skincare applications, rated on a scale of 1–5 (1 being little, 5 being very high)",
    "The respondent's goals and motivations for improving their skincare"
  )
)

# Adding to data subdirectory

write_csv(x = skincare_malaysia, file = "data/skincare_malaysia_2021_cleaned.csv")
write_csv(x = skincare_malaysia_codebook, file = "data/skincare_malaysia_2021_codebook.csv")




