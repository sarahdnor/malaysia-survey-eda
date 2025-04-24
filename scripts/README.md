# Skincare and AI Perceptions Malaysia: An EDA

This repository contains an analysis of a skincare survey conducted in Malaysia in 2021 by students from Universiti Teknologi Malaysia.

## What's in the repo

- [`data/`](data): contains all data for this project
- Within [`data/`](data), [`created_data/`](data/created_data): contains all datasets derived from the originally downloaded data, which have been manipulated or cleaned for analysis
- `plots/`: contains all figures and plots used in reports and memos

## R scripts

- `@Norman_Sarah_Summary_Stats.R`: Contains preliminary data summaries, including missingness, types and counts of observations, and variable types (e.g., number of females and males).
- `@Norman_Sarah_Tidying_Data.R`: Contains the tidied data (e.g., cleaned/renamed names, updated variable types, reordered factors) for EDA and includes a created codebook of variables.
- `@Norman_Sarah_Data_Analysis.R`: Contains all code for data analysis including plot creation and data manipulation.
    - **Univariate Analysis**: Examines the distribution of variables such as cultural identity, gender, occupation, and age.
    - **Bivariate Analysis**: Explores data segmented by gender and analyzes the frequency of different skin types.
    - **Correlation Analysis**: Conducts correlation analysis on the averages of all numerical variables.


## Reports

- `Norman_Sarah_Memo_1.qmd`: memo of data source, evaluation of data complexity and quality, potential data issues and rational for chosen data
- `Norman_Sarah_Memo_1.html`: rendered html version of memo 1
- `Norman_Sarah_Memo_2.qmd`: progress report memo containing cleaned data and preliminary univariate and bivariate analysis
- `Norman_Sarah_Memo_2.html`: rendered html version of memo 2
- `Last_First_final_report.qmd`: file for creating final report (to be adjusted)
- `Last_First_final_report.html`: rendered html version of final report (to be adjutsed)

