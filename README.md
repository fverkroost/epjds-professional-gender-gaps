Introduction
------------

This code was written by Florianne Verkroost (florianne.verkroost (at) nuffield.ox.ac.uk), with input from Ridhi Kashyap (ridhi.kashyap (at) nuffield.ox.ac.uk). This `R` script contains the code necessary to generate the results discussed in the paper: "Analyzing global professional gender gaps using LinkedIn advertising data" by Ridhi Kashyap and Florianne Verkroost, EPJ Data Science.

This README file offers a brief overview of the `R` code (`script-epjds-external.R`) that executes the analyses described in the paper. The `R` script has been commented as much as possible to be helpful and understandable for external users. Please get in touch if any help is needed or something is not clear.

Set-up
------

The code is written in such a way that a folder for outputting the results is created automatically within the folder where the main R script is located. This folder will be called *results*. No working directories need to be specified, but the code does assume that the data files (see later on) are located in the same directory as the main `R` script. I recommend creating an `R` project in this same directory so it will not be necessary to specify any working directories when relocating the files to another folder.

Code
----

The code proceeds in a number of steps.

1) Required packages are loaded, the output folder is created and some global variables (e.g. the number of digits for outputting results, the independent and dependent variables) are specified.

2) Manual functions to be used are specified. These functions are specified in `functions-epjds-external.R` and these are loaded in the analysis script (`script-epjds-external.R`).

- `is_outlier`: This function checks which values in a vector are outliers.
- `my_theme`: This function specifies a `ggplot2` theme for nice aesthetics in every plot.
- `make_labels`: This function makes sure long plotting labels are reduced (by splitting by dot).
- `save_table_2_txt`: This function takes a table and converts it to a formatted LaTeX table and saves it in the file specified by the user.
- `pval_to_stars`: This function assigns significance stars based on the p-value.
- `gender_data_labels`: This function formats plotting labels by replacing dots by spaces.
- `isic1_labels`: This function shortens plotting labels for industries by using only its section letter and first word.
- `compute_perf_from_CV`: This function uses a `caret` cross-validation result and computes resampled performance measures.
- `correlation_matrix`: This function takes a data frame and outputs a correlation matrix of pairwise correlations, and shows either the full table or just the upper or lower part (as the matrix is symmetric).
- `performance_metrics`: This function takes a vector of empirical values and a vector of estimated values and computes prediction performance measures.
- `variable_selection_adj_r_squared`: This function performs forward, backward or hybrid variable selection on the basis of the adjusted R squared criterion.

3) The three data sets are loaded. See paper for full description of the data sources.

- `df` (`data_indicators_country.csv`): This data frame contains 298 observations of 68 variables. Each observation row corresponds to a country. These variables include the country (`Location`), ISO3 code (`iso3code`), a total of 8 filtered and unfiltered LinkedIn gender gap indices (GGIs) (`sex_ratio_linkedin_overall`) and International Labour Organisation (ILO) GGIs (i.e. professional/technical (`sex_ratio_ilostat_prof_tech`), total management (`sex_ratio_ilostat_total_management`) and senior/middle management (`sex_ratio_ilostat_senior_middle_management`)), whether audience counts satisfy the observation filter (`audience_counts_satisfy_observation_filter`), 16 external variables (e.g. internet access GGI (`Internet.Online.model.prediction`) or internet penetration (`iws_int_pentr`)), some variables containing proportions of age groups in LinkedIn (`Group_18.24`, `Group_25.34`, `Group_35.54`), and 38 LinkedIn GGIs specific by group (e.g. `sex_ratio_Age_18.24`, `sex_ratio_Job.seniority_Manager` or `sex_ratio_Company.industry_M.Professional.scientificandtechnicalactivities`).
- `df_group` (`data_indicators_country_group.csv`): This data frame contains 126,312 observations of 7 variables. It contains LinkedIn GGIs (and, if available (for age and industry) ILO GGIs) by category, so by age, industry, seniority etc. The `Group` variable indicates the specific group, so for example within `Category` *Age* there are four groups: 18-24, 25-34, 35-54 and 55+. The data also contain the location and ISO3 code as well as an indicator for whether the observation filter is satisfied. The observation filter identifies those countries for which audience counts in that category of at least 1000 were available.
- `stackbox` (`data_proportions_group.csv`): This data frame contains per category (e.g. age) and group (e.g. 18-24), the total audience counts for men and women separately, aggregated across all available countries.

These datasets contain the gender gap indicators constructed from audience counts for different categories and countries. Contact the authors if specific country and category-specific audience counts are of interest.

4) The analyses are performed.

- The box plot of Figure 2 is created and saved, both on the normal and log10 scale.
- The table with values from the box plot (Table 2) is created and saved.
- The stacked bar graph of Figure 3 is created and saved.
- The world map of Figure 1 is created and saved, in standard, New Equal Earth and Robinson projections.
- The correlations and scatter plots between LinkedIn and ILO GGIs of Figures 4, 5 and 6 are created and saved.
- Regression and cross-validation for total, low and high scenarios (Table 4) are performed and saved, whereby ILO GGIs are predicted from the LinkedIn GGI.
- The correlation matrix of Table 3 is created and saved.
- Lasso regression is performed and saved (Table 5).
- Residual regressions are performed using hybrid variable selection with the adjusted R squared criterion (Table 6) and corresponding bias scatter plots are created and saved (Figures 7, 8 and 9).
