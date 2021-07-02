######################################################
# This code was written by Florianne Verkroost.
#
# This R script contains the code necessary to 
# generate the results discussed in the paper:
#
# "Analyzing global professional gender gaps using 
# LinkedIn advertising data" by 
# Ridhi Kashyap (ridhi.kashyap (at) nuffield.ox.ac.uk) and
# Florianne Verkroost 
# (florianne.verkroost (at) nuffield.ox.ac.uk), EPJ Data Science.
#
# Explanations for this code can be found in the 
# adjoining README file.
#
# Date: 18-06-2021
######################################################

#--------------------------------------------------------
# Step 1: Loading packages and setting global variables
#--------------------------------------------------------

# Load packages
library(readxl)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(grid)
library(gridExtra)
library(maps)
library(RColorBrewer)
library(caret)
library(xtable)
library(stargazer)
library(ggalt)
library(gtools)
library(stats)
library(parallel)
library(cowplot)
library(raster)
library(glmnet)

# Specify and, if not yet existent, create directory to output results 
out_dir = 'results/'
if (!dir.exists(out_dir)){ dir.create(out_dir) }

# Set minimum number of total (female + male) observations per country and country filter 
# to remove unstable and small observations
# and specify the number of digits in output format
obs_filter = 1e3
countries_filter = 125
number_digits = 2

# Specify main dependent and independent variables 
# Filtered means that the observation filter has been applied 
# (if not satisfied, the observation is set to NA)
# Uniltered means that the observation filter has not been applied
depvars_uf = paste0("sex_ratio_ilostat_", c("prof_tech", "total_management", "senior_middle_management"), "_unfiltered")
depvars_f = paste0("sex_ratio_ilostat_", c("prof_tech", "total_management", "senior_middle_management"), "_filtered")
indepvar_uf = "sex_ratio_linkedin_overall_unfiltered"
indepvar_f = "sex_ratio_linkedin_overall_filtered"
depvar_f_prof = depvars_f[grepl("prof_tech", depvars_f)]
depvar_f_total = depvars_f[grepl("total", depvars_f)]
depvar_f_senior = depvars_f[grepl("senior", depvars_f)]

#--------------------------------------------------------
# Step 2: Load functions needed for analysis
# (from functions-epjds-external.R)
#--------------------------------------------------------
source("functions-epjds-external.R")

#----------------------------------------------------------
# Step 3a: Load the data needed for generating the results
#----------------------------------------------------------

df = read.csv("data_indicators_country.csv")
df_group = read.csv('data_indicators_country_group.csv')
stackbox = read.csv('data_proportions_group.csv')

#--------------------------------------------------------
# Step 4: Perform the analyses
#--------------------------------------------------------

#--------------------------------------------------------
# Step 4a: Descriptive analyses and plots
#--------------------------------------------------------

# Boxplot of GGI across strata, normal scale and log10 scale 
selcats = c("Age", "Company Industry STEM", "Field of Study STEM", "Job Function STEM", "Job seniority")
boxdata = df_group[df_group$Category %in% selcats, ] %>%
  dplyr::filter(!is.infinite(sex_ratio_group)) %>% 
  dplyr::filter(satisfy_observation_filter == TRUE)
boxdf = boxdata %>%
  dplyr::group_by(Category, Group) %>%
  dplyr::mutate(outlier = ifelse(is_outlier(sex_ratio_group), as.character(iso3code), ""))
g = boxdf %>% 
  ggplot(., aes(x = interaction(Group, Category), y = sex_ratio_group, color = Category)) +
  geom_boxplot() +
  geom_text_repel(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  labs(x = NULL, y = "LinkedIn GGI", color = "Stratification") +
  my_theme() + 
  scale_x_discrete(labels = make_labels) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") 
ggsave(paste0(out_dir, 'boxplot_stratification.png'), g, width = 15, height = 10)

# Same boxplot on log10 scale, make sure to mark adjusted outliers using plotting data (Figure 2 in paper)
g_data = boxdf %>% 
  ggplot(., aes(x = interaction(Group, Category), y = sex_ratio_group, color = Category)) +
  geom_boxplot() +
  scale_y_log10()
pg = ggplot_build(g_data)
splits = split(boxdata, list(boxdata$Category, boxdata$Group))
splits = splits[unlist(lapply(splits, nrow)) > 0]
splits = splits[order(names(splits))]
for (i in 1:length(splits)){
  splits[[i]]["outlier"] = ifelse(round(splits[[i]]$sex_ratio_group, 3) %in% round(10^(pg$data[[1]]$outliers[[i]]), 3), 
                                  as.character(splits[[i]]$iso3code), "")
}
boxdf = rbind.fill(splits)
g_log = boxdf %>% 
  ggplot(., aes(x = interaction(Group, Category), y = sex_ratio_group, color = Category)) +
  geom_boxplot() +
  geom_text_repel(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  labs(x = NULL, y = "LinkedIn GGI", color = "Stratification") +
  my_theme() + 
  scale_x_discrete(labels = make_labels) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  scale_y_log10()
ggsave(paste0(out_dir, 'boxplot_stratification_log.png'), g_log, width = 15, height = 10)

# Table with values in above boxplot (Table 2 in paper)
box_table = boxdf %>%
  dplyr::group_by(Group, Category) %>%
  dplyr::mutate(my_min = min(sex_ratio_group),
                my_first = quantile(sex_ratio_group, probs = 0.25),
                my_median = median(sex_ratio_group),
                my_third = quantile(sex_ratio_group, probs = 0.75),
                my_max = max(sex_ratio_group),
                number_countries = n()) %>%
  dplyr::distinct(Category, Group, my_min, my_first, my_median, my_third, my_max, number_countries)
save_table_2_txt(box_table, "boxplot_stratification")

# Stacked bar graph of characteristics by gender (Figure 3 in paper)
out = by(data = stackbox, 
         INDICES = stackbox$Category, 
         FUN = function(m) {
           m = droplevels(m)
           if (unique(m$Category) == "Job seniority"){
             m$Group = factor(as.character(m$Group), levels = c("Unpaid", "Training", "Entry-level", "Owner", "Senior", "Manager", 
                                                                "Partner", "Director", "Vice President (VP)", "Chief X Officer (CxO)"))
           }
           m = ggplot(m, aes(x = Sex, y = Audience_total, fill = Group)) + 
             geom_bar(position = "fill", stat = "identity") +
             labs(x = NULL, y = "Proportion", fill = NULL) +
             my_theme() +
             ggtitle(gsub("STEM", "", unique(m$Category))) +
             guides(fill = guide_legend(nrow = 5))
         })
g = do.call(grid.arrange, c(out, ncol = length(unique(stackbox$Category))))
ggsave(paste0(out_dir, 'characteristics_sex_stack.png'), g, width = 21, height = 9)

## World map of LinkedIn overall GGI (Figure 1 in paper)
# Subset LinkedIn data (with observation filter applied) and load world map data
map_li = df[df$audience_counts_satisfy_observation_filter == TRUE, c("iso3code", indepvar_f)]
world_data = map_data('world') %>% dplyr::filter(region != "Antarctica") %>% fortify

# Add ISO3 codes to world map data to match it to LinkedIn data
# As names in ISO3 codes and world_data do not match exactly for some cases, change them manually
codes = raster::ccodes()
world_data['iso3code'] = codes$ISO3[match(world_data$region, codes$NAME)]
world_data$iso3code[world_data$region == "French Southern and Antarctic Lands"] = unique(codes$ISO3[codes$NAME == "French Southern Territories"])
world_data$iso3code[world_data$region == "Antigua"] = unique(codes$ISO3[codes$NAME == "Antigua and Barbuda"])
world_data$iso3code[world_data$region == "Barbuda"] = unique(codes$ISO3[codes$NAME == "Antigua and Barbuda"])
world_data$iso3code[world_data$region == "Saint Barthelemy"] = unique(codes$ISO3[codes$NAME == "Saint-Barthélemy"])
world_data$iso3code[world_data$region == "Ivory Coast"] = unique(codes$ISO3[codes$NAME == "Côte d'Ivoire"])
world_data$iso3code[world_data$region == "Curacao"] = unique(codes$ISO3[codes$NAME == "Curaçao"])
world_data$iso3code[world_data$region == "UK"] = unique(codes$ISO3[codes$NAME == "United Kingdom"])
world_data$iso3code[world_data$region == "Heard Island"] = unique(codes$ISO3[codes$NAME == "Heard Island and McDonald Islands"])
world_data$iso3code[world_data$region == "Nevis"] = unique(codes$ISO3[codes$NAME == "Saint Kitts and Nevis"])
world_data$iso3code[world_data$region == "Saint Kitts"] = unique(codes$ISO3[codes$NAME == "Saint Kitts and Nevis"])
world_data$iso3code[world_data$region == "Saint Martin"] = unique(codes$ISO3[codes$NAME == "Saint-Martin"])
world_data$iso3code[world_data$region == "Bonaire"] = unique(codes$ISO3[codes$NAME == "Bonaire, Saint Eustatius and Saba"])
world_data$iso3code[world_data$region == "Saba"] = unique(codes$ISO3[codes$NAME == "Bonaire, Saint Eustatius and Saba"])
world_data$iso3code[world_data$region == "Sint Eustatius"] = unique(codes$ISO3[codes$NAME == "Bonaire, Saint Eustatius and Saba"])
world_data$iso3code[world_data$region == "South Georgia"] = unique(codes$ISO3[codes$NAME == "South Georgia and the South Sandwich Islands"])
world_data$iso3code[world_data$region == "South Sandwich Islands"] = unique(codes$ISO3[codes$NAME == "South Georgia and the South Sandwich Islands"])
world_data$iso3code[world_data$region == "Palestine"] = unique(codes$ISO3[codes$NAME == "Palestina"])
world_data$iso3code[world_data$region == "Timor-Leste"] = unique(codes$ISO3[codes$NAME == "East Timor"])
world_data$iso3code[world_data$region == "Tobago"] = unique(codes$ISO3[codes$NAME == "Trinidad and Tobago"])
world_data$iso3code[world_data$region == "Trinidad"] = unique(codes$ISO3[codes$NAME == "Trinidad and Tobago"])
world_data$iso3code[world_data$region == "USA"] = unique(codes$ISO3[codes$NAME == "United States"])
world_data$iso3code[world_data$region == "Vatican"] = unique(codes$ISO3[codes$NAME == "Vatican City"])
world_data$iso3code[world_data$region == "Saint Vincent"] = unique(codes$ISO3[codes$NAME == "Saint Vincent and the Grenadines"])
world_data$iso3code[world_data$region == "Grenadines"] = unique(codes$ISO3[codes$NAME == "Saint Vincent and the Grenadines"])
world_data$iso3code[world_data$region == "Kosovo"] = "RKS"
map_li['region'] = world_data$region[match(map_li$iso3code, world_data$iso3code)]

# Save standard, New Equal Earth and Robinson projections
g = ggplot() +
  geom_map(data = world_data, map = world_data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "white", colour = "#E6E6E6", size = 0.05) + 
  geom_map(data = map_li, map = world_data,
           aes_string(fill = indepvar_f, map_id = "region"),
           colour = "#E6E6E6", size = 0.05) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180,180), ylim = c(-60, 90)) +
  scale_y_continuous(breaks = c()) +
  scale_x_continuous(breaks = c()) +
  labs(fill = "LinkedIn GGI", x = NULL, y = NULL) +
  theme_bw() +
  scale_fill_gradient2(low = "#D53E4F", mid = "#ABDDA4", high = "#3288BD", midpoint = 1) +
  theme(legend.position = "bottom")
ggsave(paste0(out_dir, 'world_map_linkedin_ggi.png'), g, width = 10.5, height = 7)
g_neep = g + coord_proj("+proj=eqearth")
ggsave(paste0(out_dir, 'world_map_linkedin_ggi_neep.png'), g_neep, width = 10.5, height = 7)
g_rob = g + coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
ggsave(paste0(out_dir, 'world_map_linkedin_ggi_rob.png'), g_rob, width = 10.5, height = 7)

#--------------------------------------------------------
# Step 4b: Correlation LinkedIn and ILO GGI (Figures 4-6)
#--------------------------------------------------------

# Check correlation between LinkedIn and ILO professional GGI if GGI is capped at one
test = df[complete.cases(df[, c(indepvar_f, depvar_f_prof)]), ]
test[, indepvar_f] = ifelse(test[, indepvar_f] > 1, 1, test[, indepvar_f])
test[, depvar_f_prof] = ifelse(test[, depvar_f_prof] > 1, 1, test[, depvar_f_prof])
cor.test(test[, indepvar_f], test[, depvar_f_prof])

# Scatter plot and correlation LinkedIn and ILOSTAT GGI (Figure 4 in paper)
nams = paste0("ILO ", c("professional", "total management", "senior/middle management"), " GGI")
clean = df[, c(indepvar_f, depvars_f)]
melted = reshape2::melt(clean, id.vars = c(indepvar_f))
max_ggi = max(max(na.omit(melted[, indepvar_f])), max(na.omit(melted$value)))
melted$variable = str_wrap(ifelse(melted$variable == depvar_f_prof, "ILO professional GGI",
                                  ifelse(melted$variable == depvar_f_senior, "ILO senior/middle management GGI",
                                         ifelse(melted$variable == depvar_f_total, "ILO total management GGI", NA))), width = 17)
melted$variable = factor(melted$variable, levels = c("ILO professional\nGGI", "ILO total\nmanagement GGI", "ILO senior/middle\nmanagement GGI"))
rho = unlist(by(melted[, c(indepvar_f, "value")], melted$variable, 
                function(x){cor(x[complete.cases(x), ])[1,2]}, simplify = FALSE))
rho_pvalue = unlist(by(melted[, c(indepvar_f, "value")], melted$variable, 
                       function(x){cor.test(x[complete.cases(x), 1], x[complete.cases(x), 2])$p.value}, simplify = FALSE))
ann_text = data.frame(sr = max_ggi / 6,  
                      value = max_ggi,
                      variable = names(rho), 
                      lab = paste0("Corr = ", paste0(round(rho, number_digits), pval_to_stars(rho_pvalue))))
names(ann_text)[names(ann_text) == "sr"] = indepvar_f
g = ggplot() + 
  geom_point(data = melted, aes_string(x = indepvar_f, y = "value")) +
  my_theme() +
  labs(x = "LinkedIn GGI", y = "ILO GGI") +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  scale_x_continuous(limits = c(0, max_ggi)) +
  scale_y_continuous(limits = c(0, max_ggi)) +
  coord_fixed() +
  geom_smooth(data = melted, aes_string(x = indepvar_f, y = "value"), method = "lm") +
  facet_wrap(~ variable) +
  geom_label(data = ann_text, aes_string(x = indepvar_f, y = "value", label = "lab"))
ggsave(paste0(out_dir, 'corr_ggi_linkedin_ilo_all.png'), g, height = 7.5, width = 10)

# Scatter plot and correlation LinkedIn and ILOSTAT by age GGI (Figure 5 in paper)
vars = c("sex_ratio_group", "sex_ratio_ilostat")
clean = df_group[df_group$Category == "Age", ]
clean = clean[complete.cases(clean[, vars]), ]
clean = clean[!is.infinite(clean$sex_ratio_group) & !is.infinite(clean$sex_ratio_ilostat), ]
clean = clean %>% filter(satisfy_observation_filter == TRUE)
rho = unlist(by(clean[, vars], clean$Group, function(x){cor(x)[1,2]}, simplify = FALSE))
rho_pvalue = unlist(by(clean[, vars], clean$Group, function(x){cor.test(x[, 1], x[, 2])$p.value}, simplify = FALSE))
max_ggi = max(max(clean$sex_ratio_group), max(clean$sex_ratio_ilostat))
ann_text = data.frame(sex_ratio_group = max_ggi / 6,  sex_ratio_ilostat = max_ggi,
                      Group = names(rho), lab = paste0("Corr = ", paste0(round(rho, number_digits), pval_to_stars(rho_pvalue))))
g = ggplot(data = clean, aes(x = sex_ratio_group, y = sex_ratio_ilostat)) +
  geom_point() +
  my_theme() +
  labs(x = "LinkedIn GGI", y = "ILO GGI") +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  scale_x_continuous(limits = c(0, max_ggi)) +
  scale_y_continuous(limits = c(0, max_ggi)) +
  coord_fixed() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Group) +
  geom_label(data = ann_text, aes(label = lab))
ggsave(paste0(out_dir, 'corr_ggi_linkedin_ilo_age.png'), g, height = 7.5, width = 7.5)

# Scatter plot and correlation LinkedIn and ILOSTAT by industry GGI (Figure 6 in paper)
clean = df_group[df_group$Category == "Company industry ISIC1", ]
clean = clean[complete.cases(clean[, vars]), ]
clean = clean[!is.infinite(clean$sex_ratio_group) & !is.infinite(clean$sex_ratio_ilostat), ] %>%
  dplyr::filter(satisfy_observation_filter == TRUE) %>%
  dplyr::rename(isic = Group)
clean["pretty_varname"] = str_wrap(isic1_labels(clean$isic), width = 20)
plist = list()
unis = unique(clean$pretty_varname)
for (j in 1:length(unis)){
  sub = data.frame(clean %>% filter(pretty_varname == unis[j]))
  rho = cor(sub[, vars])[1, 2]
  rho_pvalue = cor.test(sub[, vars[1]], sub[, vars[2]])$p.value
  max_ggi = max(max(sub[, vars[1]]), max(sub[, vars[2]]))
  labdf = data.frame(x = max_ggi / 5, y = max_ggi / 6 * 5.75, 
                     lab = paste0("Corr = ", paste0(round(rho, number_digits), pval_to_stars(rho_pvalue))))
  plist[[j]] = ggplot() +
    geom_point(data = sub, aes(x = sex_ratio_group, y = sex_ratio_ilostat)) +
    geom_label(data = labdf, aes(x = x, y = y, label = lab)) +
    my_theme() +
    labs(x = "LinkedIn GGI", y = "ILO GGI") +
    labs(x = NULL, y = NULL) +
    ggtitle(unis[j]) +
    geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
    scale_x_continuous(limits = c(0, max_ggi)) +
    scale_y_continuous(limits = c(0, max_ggi)) +
    coord_fixed() +
    geom_smooth(data = sub, aes(x = sex_ratio_group, y = sex_ratio_ilostat), method = "lm") 
}
p = grid.arrange(
  arrangeGrob(grobs = plist,
              ncol = floor(sqrt(length(plist))),
              left = textGrob("ILO GGI", rot = 90, vjust = 1),
              bottom  = textGrob("LinkedIn GGI"))
)
ggsave(paste0(out_dir, 'corr_ggi_linkedin_ilo_industry_isic1_freescales.png'), p, height = 15, width = 15)

#--------------------------------------------------------
# Step 4c: Regression LinkedIn and ILO GGI (Table 2)
#--------------------------------------------------------

# Remove rows with incomplete observations
clean_proftech = df[complete.cases(df[, c(indepvar_f, depvar_f_prof)]), ]
clean_totman = df[complete.cases(df[, c(indepvar_f, depvar_f_total)]), ]
clean_midsenman = df[complete.cases(df[, c(indepvar_f, depvar_f_senior)]), ]

# Cutoffs for high and low equality by median
cutoff_proftech = median(clean_proftech[, depvar_f_prof])
cutoff_totman = median(clean_totman[, depvar_f_total])
cutoff_midsenman = median(clean_midsenman[, depvar_f_senior])

## Create nested list with data sets (total, low and high equality 
# ILO professional/technical GGI
proftechvars = c(indepvar_f, depvar_f_prof)
srip = list(total = clean_proftech[, proftechvars],
            low = clean_proftech[clean_proftech[, depvar_f_prof] < cutoff_proftech, proftechvars],
            high = clean_proftech[clean_proftech[, depvar_f_prof] >= cutoff_proftech, proftechvars])

# ILO total management GGI
totmanvars = c(indepvar_f, depvar_f_total)
sritm = list(total = clean_totman[, totmanvars],
             low = clean_totman[clean_totman[, depvar_f_total] < cutoff_totman, totmanvars],
             high = clean_totman[clean_totman[, depvar_f_total] >= cutoff_totman, totmanvars])

# ILO senior/middle management GGI
senmanvars = c(indepvar_f, depvar_f_senior)
srismm = list(total = clean_midsenman[, senmanvars],
              low = clean_midsenman[clean_midsenman[, depvar_f_senior] < cutoff_midsenman, senmanvars],
              high = clean_midsenman[clean_midsenman[, depvar_f_senior] >= cutoff_midsenman, senmanvars])

# Combine scenarios for three ILO vars into list
scenarios = list(sex_ratio_ilostat_prof_tech = srip,
                 sex_ratio_ilostat_total_management = sritm,
                 sex_ratio_ilostat_senior_middle_management = srismm)

# Perform linear regressions on the training data and predictions on the testing data
# and output results to a table (note: k is the number of folds used in cross-validation)
# CV results may differ slightly from table in paper because of resampling randomness
K = 5
cv_control = trainControl(method = "repeatedcv", number = K, repeats = 5, savePredictions = TRUE)
full_fit = model_k_fold = res_k_fold = npreds = list()
for (i in 1:length(scenarios)){
  npreds[[i]] = c(total = length(which(!is.na(df[, indepvar_f]))),
                  low = length(which(!is.na(df[df[, depvars_f[i]] < median(na.omit(df[, depvars_f[i]])), indepvar_f]))),
                  high = length(which(!is.na(df[df[, depvars_f[i]] >= median(na.omit(df[, depvars_f[i]])), indepvar_f]))))
  full_fit[[i]] = model_k_fold[[i]] = res_k_fold[[i]] = list()
  fmla = as.formula(paste0(depvars_f[i], " ~ ", indepvar_f))
  for (j in 1:length(scenarios[[i]])){
    full_fit[[i]][[j]] = lm(fmla, data = scenarios[[i]][[j]]) 
    set.seed(1234)
    model_k_fold[[i]][[j]] = train(fmla, data = scenarios[[i]][[j]], method = "lm", trControl = cv_control)
    res_k_fold[[i]][[j]] = cbind(model_k_fold[[i]][[j]]$results, compute_perf_from_CV(model_k_fold[[i]][[j]]$pred, "Resample", k = 1))
  }
  names(full_fit[[i]]) = names(model_k_fold[[i]]) = names(scenarios[[i]])
}
names(full_fit) = names(model_k_fold) = names(res_k_fold) = names(npreds) = depvars_f
stargazer(full_fit, single.row = FALSE, no.space = TRUE, keep.stat = c("n", "rsq", "adj.rsq", "aic", "bic"), 
          digits = number_digits, out = paste0(out_dir, "regr_li_ilo_low_high_full_data.tex"))
nams_k = paste0(rep(names(model_k_fold), each = 3), "_", unlist(lapply(model_k_fold, names)))
result_k_fold = t(rbind.fill(lapply(res_k_fold, function(x){ rbind.fill(x) }))) 
colnames(result_k_fold) = nams_k
save_table_2_txt(result_k_fold, paste0("regr_li_ilo_low_high_", K, "_fold"), TRUE)
save_table_2_txt(data.frame(unlist(npreds)), paste0("regr_li_ilo_low_high_", K, "_fold_npreds"), TRUE)

#--------------------------------------------------------
# Step 4d: Correlation LinkedIn GGI & external variables
#--------------------------------------------------------

# Correlation table with external variables (Table 3 in paper)
selected_vars = c(indepvar_f, depvars_f, "sex_ratio_edu_ict", "sex_ratio_edu_stem", 
                  "sex_ratio_labour_force_part", "educ_attn_gg_subindex_raw",  
                  "econ_opp_gg_subindex_raw","gdp_capita_2020_recent")
mcor = correlation_matrix(df[, names(df) %in% selected_vars], format = "upper")
save_table_2_txt(mcor, "correlation_table_external")

#-------------------------------------------------------------
# Step 4e: Lasso regression for predicting ILO GGIs (Table 3)
#-------------------------------------------------------------

# Subset data for lasso regression
idvars = names(df)[grepl("_filtered", names(df))]
gvars = names(df)[grepl("sex_ratio_Age", names(df)) | grepl("sex_ratio_Company", names(df)) | grepl("sex_ratio_Job", names(df))]
li_feat = df[, c(idvars, gvars)]
li_feat = li_feat[rowSums(is.na(li_feat)) != ncol(li_feat), ]

# Perform lasso regression to select variables for each of the three ILO variables
# Use cross-validation to find the optimal value of lambda
tr_ctrl = caret::trainControl(method = "repeatedcv", number = K, repeats = 5, savePredictions = TRUE)
hyperparams = expand.grid(lambda = c(seq(0, 1, .001)), alpha = 1)
lasso_list = list()
for (i in 1:length(depvars_f)){
  
  # Clean data from missing and infinite cases
  vars = names(li_feat)[!names(li_feat) %in% depvars_f[-i] & !names(li_feat) %in% c("iso3code", "Location")]
  n_complete_depvar = length(which(!is.na(li_feat[, depvars_f[i]])))
  n_complete_vars = apply(li_feat[, vars], 2, function(x){length(which(!is.na(x)))})
  compvars = vars[n_complete_vars >= n_complete_depvar]
  clean = li_feat[complete.cases(li_feat[, compvars]), compvars]
  clean = clean[!is.infinite(rowSums(clean)), ]
  
  # Run model without cross-validation (alpha = 1 for lasso)
  x = as.matrix(clean[, !names(clean) %in% depvars_f[i]])
  y = clean[, depvars_f[i]]
  
  # Use cross-validation to find the optimal value of lambda
  set.seed(1234)
  cv_models = caret::train(
    x = x, y = y,
    family = "gaussian",
    metric = "RMSE",
    method = "glmnet",
    trControl = tr_ctrl,
    tuneGrid = hyperparams
  )
  
  # Using the optimal value of lambda, fit the model on the training data
  best_lambda = cv_models$bestTune[,"lambda"]
  final_model = glmnet::glmnet(x = x, y = y, family = "gaussian", alpha = 1, lambda = best_lambda)
  y_hat = predict(final_model, newx = x) 
  
  # Compute performance metrics for the model
  selvars = broom::tidy(final_model)$term
  selvars = selvars[!grepl("Intercept", selvars)]
  pm = performance_metrics(real = y, pred = y_hat, nvars = length(selvars))
  n_pred = length(which(complete.cases(df[, selvars])))
  n_obs = length(which(complete.cases(df[, c(depvars_f[i], selvars)])))
  
  # Compute CV results
  cv_res = cv_models$pred[cv_models$pred$lambda == best_lambda, ]
  perf_res = compute_perf_from_CV(cv_res, "Resample", k = length(selvars))
  
  # Create table with results
  tab_caret_full = data.frame(
    Variable = c(broom::tidy(final_model)$term, "N_pred", "N_obs", "N", "lambda",
                 "R2", "adjR2", "RMSE", "SMAPE", paste0("CV ", names(perf_res))),
    Coefficient = round(c(broom::tidy(final_model)$estimate, n_pred, n_obs, nrow(x), best_lambda,
                          pm$r2, pm$adj.r2, pm$rmse, pm$smape, unlist(perf_res)), number_digits)
  )
  names(tab_caret_full)[names(tab_caret_full) == "Coefficient"] = depvars_f[i]
  if (i == 1){
    table_caret_full = tab_caret_full
  } else {
    table_caret_full = merge(table_caret_full, tab_caret_full, by = "Variable", all = TRUE)
  }
  
}
caret_nonjoint_filter_cv5 = table_caret_full
save_table_2_txt(caret_nonjoint_filter_cv5, "regr_lasso_lambda_caret_full_data_nonjoint_filter_cv5")

#--------------------------------------------------------
# Step 4f: Residual regression (Table 4)
#--------------------------------------------------------

# Add regression residuals to data
resdf_full = df
resdf_full = merge(resdf_full, 
                   data.frame(
                     iso3code = clean_totman$iso3code, 
                     residuals_total_management = full_fit[[depvar_f_total]]$total$residuals,
                     yhat_total_management = full_fit[[depvar_f_total]]$total$fitted.values
                   ), 
                   by = "iso3code", all = TRUE)
resdf_full = merge(resdf_full, 
                   data.frame(
                     iso3code = clean_midsenman$iso3code, 
                     residuals_midsen_management = full_fit[[depvar_f_senior]]$total$residuals,
                     yhat_midsen_management = full_fit[[depvar_f_senior]]$total$fitted.values
                   ), 
                   by = "iso3code", all = TRUE)
resdf_full = merge(resdf_full, 
                   data.frame(
                     iso3code = clean_proftech$iso3code, 
                     residuals_proftech = full_fit[[depvar_f_prof]]$total$residuals,
                     yhat_proftech = full_fit[[depvar_f_prof]]$total$fitted.values
                   ), 
                   by = "iso3code", all = TRUE)

# Check results without two outliers
# totdf = totdf[totdf$sex_ratio_ilostat_total_management <= 2 & totdf$li_pen_labor_force_proftech_sex_ratio <= 2, ]

# Perform variable selection from candidates using the adjusted R-squared criterion
# Note: forward and hybrid selection can be performed by changing method in code below
# Remove candidate variables that are missing in more than half of the observations
depvars_res = c("residuals_proftech", "residuals_total_management", "residuals_midsen_management")
candidate_vars = c("iws_int_pentr", "educ_attn_gg_subindex_raw", "enrol_sec_educ_gg_raw",
                   "labr_force_gg_raw", "enrol_ter_educ_gg_raw", "hdi_fem", "hdi_male", "hdi_tot",
                   "li_pen_labor_force_proftech_female_male", "li_pen_labor_force_proftech_sex_ratio", 
                   "Internet.Online.model.prediction", "Group_18.24", "Group_25.34", "Group_35.54") 
resdf = resdf_full[, c(candidate_vars, depvars_res)]
resdf = resdf[rowSums(is.na(resdf)) != ncol(resdf), ]
nas = apply(resdf[, !names(resdf) %in% depvars_res], 2, function(x){length(which(is.na(x))) / length(x)})
delete_vars = names(nas)[nas >= 0.5]
resdf = resdf[, !names(resdf) %in% delete_vars]
fit_residuals_mixed_adj_r2_fb = lapply(
  as.list(depvars_res), 
  variable_selection_adj_r_squared, 
  df = resdf, 
  potential_indep_vars = names(resdf)[!names(resdf) %in% depvars_res], 
  method = "mixed"
)
stargazer(
  fit_residuals_mixed_adj_r2_fb, 
  no.space = TRUE, 
  keep.stat = c("n", "rsq", "adj.rsq"),
  digits = number_digits, 
  out = paste0(out_dir, "regression_residuals_mixed_r2_fb.tex")
)

#-----------------------------------------------------------
# Step 4g: Bias correlation plots by variable (Figures 7-9)
#-----------------------------------------------------------

# Create data frame to match variables to nicely looking names
matchdf = data.frame(
  original = c("iws_int_pentr", "pooled_estimate_int_pentr", "educ_attn_gg_subindex_raw", "enrol_sec_educ_gg_raw",
               "labr_force_gg_raw", "enrol_ter_educ_gg_raw", "hdi_fem", "hdi_male", "hdi_tot",
               "li_pen_labor_force_total_female", "li_pen_labor_force_total_male", "li_pen_labor_force_total_female_male",
               "li_pen_labor_force_proftech_female","li_pen_labor_force_proftech_male",
               "li_pen_labor_force_proftech_female_male", "li_pen_labor_force_proftech_sex_ratio", 
               "li_pen_labor_force_total_sex_ratio",  "Internet.Online.model.prediction",
               "Group_18-24", "Group_25-34", "Group_35-54", "median_age_group"), 
  new = c("IWS internet penetration", "Internet penetration (pooled)", "Educational attainment GGI",
          "Secondary enrolment GGI", "Labour force GGI", "Tertiary enrolment GGI", "HDI female", "HDI male",
          "HDI overall", "LinkedIn female penetration (ILO total)", "LinkedIn male penetration (ILO total)",
          "LinkedIn overall penetration (ILO total)", "LinkedIn female penetration (ILO prof.)",
          "LinkedIn male penetration (ILO prof.)", "LinkedIn overall penetration (ILO prof.)",
          "LinkedIn penetration GGI (ILO prof.)", "LinkedIn penetration GGI (ILO total)", "DGG internet GGI", 
          "LinkedIn ages 18-24", "LinkedIn ages 25-34", "LinkedIn ages 35-54", "LinkedIn median age group")
)

# Plot correlation coloured by bias variable
yhats = c("yhat_proftech", "yhat_total_management", "yhat_midsen_management")
new_ssize = 15
for (i in 1:length(fit_residuals_mixed_adj_r2_fb)){
  pvals = summary(fit_residuals_mixed_adj_r2_fb[[i]])$coefficients[, "Pr(>|t|)"]
  stratvars = names(fit_residuals_mixed_adj_r2_fb[[i]]$coefficients)
  stratvars = stratvars[which(pvals < .05)]
  if (length(stratvars) > 1){
    stratvars = stratvars[!grepl("Intercept", stratvars)]
    stratvars = gsub("`", "", stratvars)
    plotdf = resdf_full[, c(depvars_f[i], yhats[i], stratvars)]
    mplotdf = reshape2::melt(plotdf, id.var = c(depvars_f[i], yhats[i]))
    mplotdf$variable = matchdf$new[match(mplotdf$variable, matchdf$original)]
    mplotdf = mplotdf[complete.cases(mplotdf), ]
    if (i == 2){ mplotdf = mplotdf[mplotdf[, depvar_f_total] <= 2, ]} # remove 5 (out of 808) values larger than 2 to prevent skewness
    mplotdf = mplotdf[!(mplotdf$variable == "LinkedIn penetration GGI (ILO prof.)" & 
                          mplotdf$value > 2), ] # remove 15 (out of 701) values larger than 2 to not skew legend scale
    max_ggi = max(max(na.omit(mplotdf[, yhats[i]])), max(na.omit(mplotdf[, depvars_f[i]])))
    mplotdf["pretty_variable"] = str_wrap(mplotdf$variable, width = 25)
    g = mplotdf %>% 
      group_split(variable) %>% 
      purrr::map(
        ~ggplot(., aes_string(x = depvars_f[i], y = yhats[i], color = "value")) + 
          geom_point() + 
          facet_grid(~ pretty_variable) +
          coord_fixed() + 
          labs(x = paste0(nams[i], " (actual)"), y = paste0(nams[i], " (predicted)"), color = "") +
          geom_abline(intercept = 0, slope = 1, color = "black") +
          scale_y_continuous(limits = c(0, max_ggi)) +
          scale_x_continuous(limits = c(0, max_ggi)) +
          scale_colour_gradientn(colors = rainbow(5)) +
          theme(strip.background = element_rect(fill = "white"),
                legend.position = "top", 
                legend.box = "horizontal",
                legend.key.width = unit(1.5, "cm"),
                axis.text = element_text(size = new_ssize),
                axis.title = element_text(size = new_ssize),
                strip.text = element_text(size = new_ssize),
                legend.text = element_text(size = new_ssize),
                legend.title = element_text(size = new_ssize),
                plot.title = element_text(size = new_ssize))
      ) %>% 
      plot_grid(plotlist = ., align = 'hv', ncol = ceiling(sqrt(length(unique(mplotdf$variable)))))
    wdt = ceiling(sqrt(length(unique(mplotdf$variable)))) / 3 * 15
    hgt = ifelse(ceiling(sqrt(length(unique(mplotdf$variable)))) == sqrt(length(unique(mplotdf$variable))), 15, 10)
    hgt = ceiling(sqrt(length(unique(mplotdf$variable)))) / 3 * hgt
    ggsave(paste0(out_dir, "correlation_bias_", gsub("sex_ratio_", "", depvars_f[i]), ".png"), g, height = hgt, width = wdt)
  }
}
