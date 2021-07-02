
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
# Step 2: Define the manual functions needed for analysis 
#--------------------------------------------------------

# Check which values in a vector x are outliers
is_outlier = function(x) {
  return(x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE) | 
           x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
}

# Specify manual and aesthetically pleasing plotting theme with font size "ssize"
ssize = 20
my_theme = function () { 
  theme_bw() + 
    theme(axis.text = element_text(size = ssize),
          axis.title = element_text(size = ssize),
          strip.text = element_text(size = ssize),
          legend.text = element_text(size = ssize),
          legend.title = element_text(size = ssize),
          plot.title = element_text(size = ssize),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position = "bottom")
}

# Make nice looking labels split across several lines
make_labels = function(labels) {
  result = str_split(labels, "\\.")
  unlist(lapply(result, function(x) x[1]))
}

# Save R table to nicely formatted .txt file in LaTeX code
save_table_2_txt = function(table, filename, rownames = FALSE, ndigits = number_digits){
  output = print(xtable(table, digits = rep(ndigits, ncol(table) + 1)), 
                 include.rownames = rownames, sanitize.text.function = function(x){x})
  fileConn = file(paste0(out_dir, paste0(filename, ".txt")))
  writeLines(output, fileConn)
  close(fileConn)
}

# Convert p-value to significance stars according to EPJDS formatting
pval_to_stars = function(p.value){
  unclass(
    symnum(p.value, corr = FALSE, na = FALSE,
           cutpoints = c(0, 0.01, 0.05, 0.1, 1),
           symbols = c("***", "**", "*", ""))
  )
}

# Define functions for plotting labels
gender_data_labels = function(labels) {
  result = str_split(labels, "\\.")
  unlist(lapply(result, function(x){ paste0(x, collapse = " ") }))
}
isic1_labels = function(labels) {
  result = str_split(labels, " ")
  unlist(lapply(result, function(x){ ifelse(length(x) == 1, x[1], paste(x[1], gsub('[[:punct:] ]+','',x[2]), collapse = " ")) }))
}

# Compute cross-validated resampled measures from caret results
compute_perf_from_CV = function(cv_result, groupvar, k){
  smape_res = cv_result %>% 
    dplyr::group_by_at(vars(one_of(groupvar))) %>%
    dplyr::mutate(smape = 100/n() * sum( (abs(pred - obs)) / ( (abs(obs) + abs(pred)) / 2 ) ),
                  r2 = 1 - (sum((obs - pred)^2) / sum((obs - mean(obs))^2)),
                  adjr2 = 1 - ((1 - r2)*(n() - 1) / (n() - k - 1)),
                  r2_resamp = cor(pred, obs, use = "pairwise.complete.obs")^2,
                  adjr2_resamp = 1 - ((1 - r2_resamp)*(n() - 1) / (n() - k - 1)),
                  rmse = sqrt(mean((pred - obs)^2)),
                  mae = mean(abs(pred - obs))) %>%
    dplyr::distinct_at(vars(one_of(c(groupvar, "smape", "r2", "adjr2", "r2_resamp", "adjr2_resamp","rmse", "mae")))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(SMAPE = mean(smape), SMAPESD = sd(smape),
                  R2 = mean(r2), R2SD = sd(r2),
                  adjR2 = mean(adjr2), adjR2SD = sd(adjr2),
                  R2resamp = mean(r2_resamp), R2resampSD = sd(r2_resamp),
                  adjR2resamp = mean(adjr2_resamp), adjR2resampSD = sd(adjr2_resamp),
                  RMSE = mean(rmse), RMSESD = sd(rmse),
                  MAE = mean(mae), MAESD = sd(mae)) %>%
    dplyr::distinct(SMAPE, SMAPESD, R2, R2SD, adjR2, adjR2SD, R2resamp, R2resampSD, 
                    adjR2resamp, adjR2resampSD,RMSE, RMSESD, MAE, MAESD)
  return(smape_res)
}

# Create matrix of pairwise correlations for a data frame df
correlation_matrix = function(df, format = c("upper", "lower", "full"), ndigits = number_digits){
  combs = combn(names(df), 2)
  n = length(names(df))
  corrmat = data.frame(matrix(NA, nrow = n, ncol = n))
  rownames(corrmat) = colnames(corrmat) = names(df)
  for (i in 1:ncol(combs)){
    subdf = df[complete.cases(df[, combs[, i]]), combs[, i]]
    subdf = subdf[!is.infinite(rowSums(subdf)), ]
    rho = cor(subdf)[1, 2]
    rho_pvalue = cor.test(subdf[, 1], subdf[, 2])$p.value
    row = which(rownames(corrmat) == combs[1, i])
    col = which(colnames(corrmat) == combs[2, i])
    filling = paste0(round(rho, ndigits), "$^{", pval_to_stars(rho_pvalue), "}$")
    if (format == "upper"){
      corrmat[row, col] = filling
    } else if (format == "lower"){
      corrmat[col, row] = filling
    } else {
      corrmat[row, col] = corrmat[col, row] = filling
    }
  }
  diag(corrmat) = paste0("1.", paste(rep("0", ndigits), collapse = ""), "$^{***}$")
  return(corrmat)
}

# Performance metrics for caret lasso
performance_metrics = function(real, pred, nvars){
  r2 = 1 - (sum((real - pred)^2) / sum((real - mean(real))^2))
  rmse = sqrt(mean((real - pred)^2))
  adj.r2 = 1 - (((1 - r2)*(length(real) - 1)) / (length(real) - nvars - 1))
  smape = 100 / length(real) * sum( (abs(pred - real)) / ((abs(real) + abs(pred))/2) )
  return(list(r2 = r2, rmse = rmse, adj.r2 = adj.r2, smape = smape))
}

# Perform forward, backward or mixed variable selection
variable_selection_adj_r_squared = function(df, dep_var, potential_indep_vars, method = c("forward", "backward", "mixed")){
  df = df[, c(dep_var, potential_indep_vars)]
  potential_indep_vars[grepl("-", potential_indep_vars)] = paste0("`", potential_indep_vars[grepl("-", potential_indep_vars)], "`")
  if (method == "forward"){
    candidates = potential_indep_vars
    p = length(candidates)
    fit = lm(as.formula(paste0(dep_var, " ~ 1")), data = df) # intercept only model
    res = summary(fit)$adj.r.squared
    selected = vector()
    for (i in seq_len(p)){
      values = sapply(candidates, function(v){
        fmla = as.formula(paste0(dep_var, " ~ ", paste(c(selected, v), collapse = " + ")))
        fit = lm(fmla, data = df)
        summary(fit)$adj.r.squared
      })
      best = which.max(values)
      selected = c(selected, candidates[best])
      res = c(res, values[best])
      candidates = candidates[-best]
    }
    sOpt = which.max(res) - 1 
    rhs = paste(head(selected, sOpt), collapse = " + ")
    final_fmla = as.formula(paste0(dep_var, " ~ ", rhs))
    final_fit = lm(final_fmla, data = df)
  } else if (method == "backward"){
    selected = potential_indep_vars
    p = length(selected)
    fit = lm(as.formula(paste0(dep_var, " ~ ", paste0(selected, collapse = " + "))), data = df) # all variable model
    res = summary(fit)$adj.r.squared
    candidates = vector()
    for (i in seq_len(p)){
      values = sapply(selected, function(v){
        xvars = selected[selected != v]
        if (length(xvars) > 0){
          fmla = as.formula(paste0(dep_var, " ~ 1 + ", paste(xvars, collapse = " + ")))
        } else {
          fmla = as.formula(paste0(dep_var, " ~ 1"))
        }
        fit = lm(fmla, data = df)
        summary(fit)$adj.r.squared
      })
      best = which.max(values)
      selected = selected[selected != names(best)]
      res = c(res, values[best])
      candidates = c(candidates, names(best))
    }
    sOpt = which.max(res) - 1 
    rhs = paste(candidates[-c(1:sOpt)], collapse = " + ")
    final_fmla = as.formula(paste0(dep_var, " ~ ", rhs))
    final_fit = lm(final_fmla, data = df)
  } else if (method == "mixed"){
    candidates = potential_indep_vars
    p = length(candidates)
    fit = lm(as.formula(paste0(dep_var, " ~ 1")), data = df) # intercept only model
    res = summary(fit)$adj.r.squared
    selected = vector()
    for (i in seq_len(p)){
      values_add = sapply(candidates, function(v){
        fmla = as.formula(paste0(dep_var, " ~ ", paste(c(selected, v), collapse = " + ")))
        fit = lm(fmla, data = df)
        summary(fit)$adj.r.squared
      })
      values_drop = sapply(selected, function(v){
        xvars = selected[selected != v]
        if (length(xvars) > 0){
          fmla = as.formula(paste0(dep_var, " ~ 1 + ", paste(xvars, collapse = " + ")))
        } else {
          fmla = as.formula(paste0(dep_var, " ~ 1"))
        }
        fit = lm(fmla, data = df)
        summary(fit)$adj.r.squared
      })
      best_add = which.max(values_add)
      best_drop = which.max(values_drop)
      if (length(best_add) > 0 & length(best_drop) > 0){
        if (max(values_add) > max(values_drop)){
          selected = c(selected, candidates[best_add])
          res = c(res, values_add[best_add])
          candidates = candidates[-best_add]
        } else if (max(values_add) < max(values_drop)){
          selected = selected[selected != names(best_drop)]
          res = c(res, values_drop[best_drop])
          candidates = c(candidates, names(best_drop))
        } else {
          break
        }
      } else {
        if (length(best_add) == 0 & length(best_drop) > 0){
          selected = selected[selected != names(best_drop)]
          res = c(res, values_drop[best_drop])
          candidates = c(candidates, names(best_drop))
        } else if (length(best_add) > 0 & length(best_drop) == 0){
          selected = c(selected, candidates[best_add])
          res = c(res, values_add[best_add])
          candidates = candidates[-best_add]
        } else {
          break
        }
      }
    }
    sOpt = which.max(res) - 1 
    rhs = paste(head(selected, sOpt), collapse = " + ")
    final_fmla = as.formula(paste0(dep_var, " ~ ", rhs))
    final_fit = lm(final_fmla, data = df)
  } else {
    stop("Argument 'method' should be either 'forward', 'backward', or 'mixed'!")
  }
  return(final_fit)
}
