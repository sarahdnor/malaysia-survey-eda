# Data

## Raw Data 

- [`skincare_survey_malaysia_2021.csv`](skincare_survey_malaysia_2021.csv): data downloaded from Kaggle of Skincare Survey conducted in Malaysia in 2021 collected by students at Universiti Teknologi Malaysia

## Created Data 
- [`data/`](data), [`created_data/`](data/created_data): contains all datasets derived from the originally downloaded data, which have been manipulated or cleaned for analysis
  - **skincare_malaysia_2021_cleaned.csv**: tidied dataset (e.g., cleaned/renamed names, updated variable types, reordered factors) for EDA.
  - **skincare_malaysia_2021_coodebook.csv**: created codebook of variable names and definitions
  - **skincare_malaysia_2021_ingredient_preferences.csv**: pivoted dataset with binary "yes" or "no" values indicating respondent ingredient preferences
  - **skincare_malaysia_2021_skin_types.csv**: pivoted dataset with binary "yes" or "no" values indicating respondent skin type(s)

