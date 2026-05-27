pacman::p_load(data.table, ggplot2, skimr, missForest, forcats)

options(java.parameters = "-Xmx8000m")
pacman::p_load(rJava)
.jinit()

if (!pacman::p_isinstalled(YARF)){
  pacman::p_install_gh("kapelner/YARF/YARFJARs", ref = "dev")
  pacman::p_install_gh("kapelner/YARF/YARF", ref = "dev", force = TRUE)
}
pacman::p_load(YARF)

housing = fread("housing_data.csv")
dim(housing)
head(housing)
skim(housing)

housing = housing[, .(
  coop_condo, full_address_or_zip_code, approx_year_built, cats_allowed, dogs_allowed, common_charges, maintenance_cost,
  parking_charges, dining_room_type, fuel_type, garage_exists, kitchen_type, num_bedrooms, num_floors_in_building,
  num_full_bathrooms, num_half_bathrooms, num_total_rooms, pct_tax_deductibl, sale_price, sq_footage, total_taxes, walk_score
)]

housing[, approx_year_built := as.integer(approx_year_built)]
housing[, coop_condo := factor(coop_condo)]
housing[, sq_footage := as.numeric(gsub("[^0-9.]", "", sq_footage))]

housing[, parking_charges := as.numeric(gsub("[$, ]", "", parking_charges))]

housing[, total_taxes := as.numeric(gsub("[$, ]", "", total_taxes))]
housing[, pct_tax_deductibl := as.numeric(pct_tax_deductibl)]

housing[, maintenance_cost := as.numeric(gsub("[$, ]", "", maintenance_cost))]
housing[, common_charges := as.numeric(gsub("[$, ]", "", common_charges))]
housing[, monthly_cost := ifelse(!is.na(maintenance_cost), maintenance_cost, common_charges)]
housing[, num_bedrooms := as.integer(num_bedrooms)]
housing[, num_total_rooms := as.integer(num_total_rooms)]
housing[, num_floors_in_building := as.integer(num_floors_in_building)]
housing[, num_full_bathrooms := as.integer(num_full_bathrooms)]
housing[, num_half_bathrooms := as.integer(num_half_bathrooms)]

housing[, bath_total := num_full_bathrooms + 0.5 * ifelse(is.na(num_half_bathrooms), 0, num_half_bathrooms)]

housing[, kitchen_type := tolower(trimws(kitchen_type))]
housing[kitchen_type %in% c("eatin", "eat in"), kitchen_type := "eat in"]
housing[kitchen_type %in% c("efficiency kitchen", "efficiency ktchen", "efficiemcy", "efficiency kitchene"), kitchen_type := "efficiency"]
housing[!(kitchen_type %in% c("eat in", "efficiency", "combo", "none")), kitchen_type := NA]
housing[, kitchen_type := factor(kitchen_type)]

housing[, dining_room_type := tolower(trimws(dining_room_type))]
housing[dining_room_type == "na", dining_room_type := NA]
housing[dining_room_type %in% c("dining area"), dining_room_type := "other"]
housing[, dining_room_type := factor(dining_room_type)]

housing[, fuel_type := tolower(trimws(fuel_type))]
housing[fuel_type == "na", fuel_type := NA]
housing[, fuel_type := factor(fuel_type)]

housing[, garage_exists := tolower(trimws(garage_exists))]
housing[, garage_exists := ifelse(garage_exists %in% c("na", ""), NA, ifelse(garage_exists %in% c("yes", "1", "ug", "underground", "eys"), 1, 0))]
housing[, garage_exists := as.integer(garage_exists)]

housing[, cats_allowed := as.integer(tolower(trimws(cats_allowed)) %in% c("yes", "y"))]
housing[, dogs_allowed := as.integer(tolower(trimws(dogs_allowed)) %in% c("yes", "y", "yes89"))]

housing[, miss_sq_footage := as.integer(is.na(sq_footage))]
housing[, miss_monthly_cost := as.integer(is.na(monthly_cost))]
housing[, miss_parking_charges := as.integer(is.na(parking_charges))]
housing[, miss_total_taxes := as.integer(is.na(total_taxes))]
housing[, miss_num_floors_in_building := as.integer(is.na(num_floors_in_building))]
housing[, miss_pct_tax_deductibl := as.integer(is.na(pct_tax_deductibl))]
housing[, miss_dining_room_type := as.integer(is.na(dining_room_type))]
housing[, miss_fuel_type := as.integer(is.na(fuel_type))]

sale_price_raw = housing$sale_price
address_raw = housing$full_address_or_zip_code
set.seed(1984)
housing_imp = missForest(data.frame(housing[, !c("sale_price", "full_address_or_zip_code")]))$ximp
housing = as.data.table(housing_imp)
housing[, sale_price := sale_price_raw]
housing[, full_address_or_zip_code := address_raw]


housing[, sale_price := as.numeric(gsub("[$, ]", "", sale_price))]
housing = housing[!is.na(sale_price)]

housing[, zip := as.integer(regmatches(
  full_address_or_zip_code,
  regexpr("[0-9]{5}", full_address_or_zip_code)
))]

valid_zips = c(
  11361, 11362, 11363, 11364, 11354, 11355, 11356, 11357, 11358, 11359, 11360, 11365, 11366, 11367, 11412, 11423, 
  11432, 11433, 11434, 11435, 11436, 11101, 11102, 11103, 11104, 11105, 11106, 11374, 11375, 11379, 11385, 11004, 
  11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429, 11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421,
  11368, 11369, 11370, 11372, 11373, 11377, 11378
)

housing = housing[zip %in% valid_zips]
housing = housing[sale_price <= 1e6]
dim(housing)

ggplot(housing) + 
  aes(x = sale_price) + 
  geom_histogram(bins = 50)

ggplot(housing) + 
  aes(x = log(sale_price)) + 
  geom_histogram(bins = 50)

housing[, ln_sale_price := log(sale_price)]

housing[, zip_f := factor(zip)]
housing[, neighborhood := ifelse(zip %in% c(11361, 11362, 11363, 11364), "northeast",
                                 ifelse(zip %in% c(11354, 11355, 11356, 11357, 11358, 11359, 11360), "north",
                                        ifelse(zip %in% c(11365, 11366, 11367), "central",
                                               ifelse(zip %in% c(11412, 11423, 11432, 11433, 11434, 11435, 11436), "jamaica",
                                                      ifelse(zip %in% c(11101, 11102, 11103, 11104, 11105, 11106), "northwest",
                                                             ifelse(zip %in% c(11374, 11375, 11379, 11385), "west_central",
                                                                    ifelse(zip %in% c(11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429), "southeast",
                                                                           ifelse(zip %in% c(11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421), "southwest",
                                                                                  "west"))))))))]
housing[, neighborhood := factor(neighborhood)]
housing[, ln_sq_footage := log(sq_footage)]
housing[, bldg_age := 2017 - approx_year_built]

modeling_cols = c(
  "ln_sale_price", "coop_condo", "zip_f", "neighborhood", "ln_sq_footage", "bldg_age", "num_bedrooms", "bath_total", "num_total_rooms",
  "num_floors_in_building", "monthly_cost", "total_taxes", "walk_score", "parking_charges", "miss_parking_charges", "miss_total_taxes", "pct_tax_deductibl",
  "kitchen_type", "dining_room_type", "fuel_type", "garage_exists", "cats_allowed", "dogs_allowed", "miss_sq_footage",
  "miss_monthly_cost", "miss_num_floors_in_building", "miss_pct_tax_deductibl", "miss_dining_room_type", "miss_fuel_type"
)

cleaned_dataset = housing[, ..modeling_cols]
dim(cleaned_dataset)
skim(cleaned_dataset)

y_all = cleaned_dataset$ln_sale_price
X_all = cleaned_dataset[, !"ln_sale_price"]

X_imp = as.data.table(data.frame(X_all))

set.seed(1984)
n = nrow(X_imp)
test_indices = sample(1 : n, round(0.1 * n))
train_indices = setdiff(1 : n, test_indices)

X_train = X_imp[train_indices, ]
y_train = y_all[train_indices]
X_test = X_imp[test_indices, ]
y_test = y_all[test_indices]


# Regression Tree
tree_mod = YARFCART(X_train, y_train, calculate_oob_error = FALSE, seed = 1984)

get_tree_num_nodes_leaves_max_depths(tree_mod)
illustrate_trees(tree_mod, max_depth = 4, open_file = TRUE)

var_counts_tree = query_variable_counts(tree_mod)
top10 = sort(var_counts_tree, decreasing = TRUE)[1:10]
ggplot(data.frame(variable = fct_reorder(names(top10), top10), count = top10)) +
  geom_col(aes(x = variable, y = count)) +
  coord_flip() +
  xlab("Feature") + ylab("Split Count") +
  ggtitle("Top 10 Features by Split Count")
sort(query_variable_counts(tree_mod), decreasing = TRUE)

yhat_train_tree = predict(tree_mod, X_train)
e_train_tree = y_train - yhat_train_tree
tree_in_Rmse = sd(e_train_tree)
tree_in_Rsq = 1 - sum(e_train_tree^2) / sum((y_train - mean(y_train))^2)
tree_in_Rmse_dollars = sqrt(mean((exp(y_train) - exp(yhat_train_tree))^2))
cat("tree in-sample RMSE:", tree_in_Rmse, "\n")
cat("tree in-sample Rsq:", tree_in_Rsq, "\n")
cat("tree in-sample RMSE ($):", tree_in_Rmse_dollars, "\n")

yhat_test_tree = predict(tree_mod, X_test)
e_test_tree = y_test - yhat_test_tree
tree_oos_Rmse = sd(e_test_tree)
tree_oos_Rsq = 1 - sum(e_test_tree^2) / sum((y_test  - mean(y_test))^2)
tree_oos_Rmse_dollars = sqrt(mean((exp(y_test) - exp(yhat_test_tree))^2))
cat("tree oos RMSE:", tree_oos_Rmse, "\n")
cat("tree oos Rsq:", tree_oos_Rsq, "\n")
cat("tree oos RMSE ($):", tree_oos_Rmse_dollars, "\n")

# OLS
train_df = data.frame(y = y_train, X_train)
ols_mod = lm(y ~ ., train_df)
summary(ols_mod)

e_train_ols = y_train - ols_mod$fitted.values
ols_in_Rmse = sd(e_train_ols)
ols_in_Rsq = summary(ols_mod)$r.squared
ols_in_Rmse_dollars = sqrt(mean((exp(y_train) - exp(ols_mod$fitted.values))^2))
cat("OLS in-sample RMSE:", ols_in_Rmse, "\n")
cat("OLS in-sample Rsq:", ols_in_Rsq, "\n")
cat("OLS in-sample RMSE ($):", ols_in_Rmse_dollars, "\n")

yhat_test_ols = predict(ols_mod, data.frame(X_test))
e_test_ols    = y_test - yhat_test_ols
ols_oos_Rmse = sd(e_test_ols)
ols_oos_Rsq = 1 - sum(e_test_ols^2) / sum((y_test  - mean(y_test))^2)
ols_oos_Rmse_dollars = sqrt(mean((exp(y_test) - exp(yhat_test_ols))^2))
cat("OLS oos RMSE:", ols_oos_Rmse, "\n")
cat("OLS oos Rsq:", ols_oos_Rsq, "\n")
cat("OLS oos RMSE ($):", ols_oos_Rmse_dollars, "\n")

# Random Forest
rf_mod = YARF(X_train, y_train, num_trees = 1000, seed = 1984)
rf_mod

yhat_test_rf = predict(rf_mod, X_test)
e_test_rf = y_test - yhat_test_rf
rf_oos_Rmse = sd(e_test_rf)
rf_oos_Rsq = 1 - sum(e_test_rf^2) / sum((y_test  - mean(y_test))^2)
rf_oos_Rmse_dollars = sqrt(mean((exp(y_test) - exp(yhat_test_rf))^2))
cat("RF oos RMSE:", rf_oos_Rmse, "\n")
cat("RF oos Rsq:", rf_oos_Rsq, "\n")
cat("RF oos RMSE ($):", rf_oos_Rmse_dollars, "\n")

var_counts_rf = query_variable_counts(rf_mod)
ggplot(data.frame(variable = fct_reorder(names(var_counts_rf), var_counts_rf), count = var_counts_rf)) +
  geom_col(aes(x = variable, y = count)) +
  coord_flip()
sort(query_variable_counts(rf_mod), decreasing = TRUE)

# mtry tuning
p = ncol(X_train)
mtry_candidates = c(2, 3, 4, 7, 11, 16, 23, 30, 35)
oob_by_mtry = numeric(length(mtry_candidates))
for (i in seq_along(mtry_candidates)){
  mod_tmp = YARF(X_train, y_train, num_trees = 1000, mtry = mtry_candidates[i], verbose = FALSE, seed = 1984)
  oob_by_mtry[i] = mod_tmp$rmse_oob
  cat("mtry =", mtry_candidates[i], " oob rmse =", round(oob_by_mtry[i], 4), "\n")
}
best_mtry = mtry_candidates[which.min(oob_by_mtry)]

rf_tuned = YARF(X_train, y_train, num_trees = 1000, mtry = best_mtry, seed = 1984)
yhat_test_rf_tuned = predict(rf_tuned, X_test)
e_test_rf_tuned = y_test - yhat_test_rf_tuned
rft_oos_Rmse = sd(e_test_rf_tuned)
rft_oos_Rsq = 1 - sum(e_test_rf_tuned^2) / sum((y_test - mean(y_test))^2)
rft_oos_Rmse_dollars = sqrt(mean((exp(y_test) - exp(yhat_test_rf_tuned))^2))
cat("RF tuned oos RMSE:", rft_oos_Rmse, "\n")
cat("RF tuned oos Rsq:", rft_oos_Rsq, "\n")
cat("RF tuned oos RMSE ($):", rft_oos_Rmse_dollars, "\n")


ggplot(data.frame(actual = exp(y_test), predicted = exp(yhat_test_ols))) +
  geom_point(aes(x = predicted, y = actual), alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  xlab("predicted sale price") + ylab("actual sale price")


results = data.frame(
  model = c("OLS", "Tree", "RF", "RF tuned"),
  in_sample_rsq = c(ols_in_Rsq,  tree_in_Rsq,  NA, NA),
  in_sample_rmse = c(ols_in_Rmse, tree_in_Rmse, NA, NA),
  oob_rmse       = c(NA, NA, rf_mod$rmse_oob,      rf_tuned$rmse_oob),
  oos_rsq = c(ols_oos_Rsq, tree_oos_Rsq, rf_oos_Rsq, rft_oos_Rsq),
  oos_rmse = c(ols_oos_Rmse, tree_oos_Rmse, rf_oos_Rmse, rft_oos_Rmse),
  oos_rmse_dollars = c(ols_oos_Rmse_dollars,  tree_oos_Rmse_dollars, rf_oos_Rmse_dollars, rft_oos_Rmse_dollars)
)
results

best_model = which.min(c(ols_oos_Rmse, tree_oos_Rmse, rf_oos_Rmse, rft_oos_Rmse))
cat("Best model:", c("OLS", "Tree", "RF", "RF tuned")[best_model], "\n")

best_predictor = list(yhat_test_ols, yhat_test_tree, yhat_test_rf, yhat_test_rf_tuned)[[best_model]]
oos_preds = exp(best_predictor)
y_true = exp(y_test)

abs_errors = abs(y_true - oos_preds)
median_pct_error = median(abs_errors / y_true) * 100

pct_within_5 = mean(abs_errors / y_true < 0.05) * 100
pct_within_10 = mean(abs_errors / y_true < 0.10) * 100
pct_within_20 = mean(abs_errors / y_true < 0.20) * 100

cat("Median Error:", round(median_pct_error, 1), "%\n")
cat("Within  5%:", round(pct_within_5,  1), "%\n")
cat("Within 10%:", round(pct_within_10, 1), "%\n")
cat("Within 20%:", round(pct_within_20, 1), "%\n")

final_mod = list(ols_mod, tree_mod, rf_mod, rf_tuned)[[best_model]]
final_mod