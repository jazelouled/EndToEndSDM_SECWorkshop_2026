# =========================================================
# SCRIPT NAME:
# 01_fitRF_Balaenoptera_artificialis.R
#
# DESCRIPTION:
# End-to-end Random Forest workflow for the workshop project
# using caret for native grid search and grouped cross-validation.
#
# STEPS:
#   1) Load Balaenoptera_artificialis_PresAbs_with_env.csv
#   2) Check which dates have environmental stacks available
#   3) Keep only records whose dates are represented in 02presentStacks
#   4) Remove non-predictor columns
#   5) Run grouped RF grid search with caret
#   6) Save tuning summary + winner hyperparameters
#   7) Fit full RF model with winning hyperparameters
#   8) Save variable importance + PDPs
#   9) Run a quick sanity prediction on one available stack
#
# IMPORTANT:
# This version uses:
# - caret::train()
# - method = "ranger"
# - grouped cross-validation through trainControl(index = ...)
# =========================================================

suppressPackageStartupMessages({
  library(data.table)
  library(caret)
  library(ranger)
  library(pROC)
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(pdp)
  library(viridis)
  library(raster)
  library(stringr)
  library(here)
  library(randomForest)
})

set.seed(123)

# ---------------------------------------------------------
# PATHS
# ---------------------------------------------------------

root <- here::here()

data_file <- file.path(
  root,
  "00inputOutput", "00input", "01processedData", "02habitatModel",
  "Balaenoptera_artificialis_PresAbs_with_env.csv"
)

env_dir <- file.path(
  root,
  "00inputOutput", "00input", "01processedData", "00enviro",
  "02presentStacks"
)

tuning_dir <- file.path(
  root,
  "00inputOutput", "01output", "02models", "00modelTuning"
)

full_dir <- file.path(
  root,
  "00inputOutput", "01output", "02models", "01fullModel"
)

dir.create(tuning_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(full_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_file)) {
  stop("Input data file not found: ", data_file)
}

if (!dir.exists(env_dir)) {
  stop("Environmental stack directory not found: ", env_dir)
}

# ---------------------------------------------------------
# CHECK STACK DATES AVAILABLE
# ---------------------------------------------------------

stack_files <- list.files(
  env_dir,
  pattern = "^present_stack_\\d{4}-\\d{2}-\\d{2}\\.grd$",
  full.names = TRUE
)

if (length(stack_files) == 0) {
  stop("No daily .grd stacks found in 02presentStacks")
}

stack_dates <- as.Date(
  str_extract(basename(stack_files), "\\d{4}-\\d{2}-\\d{2}")
)

if (any(is.na(stack_dates))) {
  stop("Could not correctly extract dates from stack filenames")
}

cat("\n====================================\n")
cat("STACK AVAILABILITY CHECK\n")
cat("====================================\n")
cat("Stacks available:", length(stack_files), "\n")
cat("Unique stack dates:", length(unique(stack_dates)), "\n")
cat("First available stack dates:\n")
print(head(sort(unique(stack_dates))))
cat("Last available stack dates:\n")
print(tail(sort(unique(stack_dates))))

# ---------------------------------------------------------
# LOAD EXTRACT DATA
# ---------------------------------------------------------

df <- fread(data_file)

if ("date" %in% names(df)) {
  df[, date := as.Date(date)]
} else if ("datetime" %in% names(df)) {
  df[, date := as.Date(datetime)]
} else if ("dateTime" %in% names(df)) {
  df[, date := as.Date(dateTime)]
} else {
  stop("No valid date column found (expected date, datetime, or dateTime)")
}

if (!"occ" %in% names(df)) {
  stop("Column 'occ' not found in Balaenoptera_artificialis_PresAbs_with_env.csv")
}

cat("\n====================================\n")
cat("EXTRACT DATA LOADED\n")
cat("====================================\n")
cat("Rows loaded:", nrow(df), "\n")
cat("Columns loaded:", ncol(df), "\n")
cat("Unique dates in extract data:", uniqueN(df$date), "\n")

missing_dates <- setdiff(sort(unique(df$date)), sort(unique(stack_dates)))

cat("\nDates in extract data without stack:", length(missing_dates), "\n")
if (length(missing_dates) > 0) {
  print(head(missing_dates, 20))
}

df <- df[date %in% stack_dates]

cat("\nRows after keeping only dates with stacks:", nrow(df), "\n")
cat("Unique dates after stack filter:", uniqueN(df$date), "\n")

if (nrow(df) == 0) {
  stop("No rows left after filtering to dates with available stacks")
}

# ---------------------------------------------------------
# DEFINE GROUPS (id + segment_id)
# ---------------------------------------------------------

if (!all(c("id", "segment_id") %in% names(df))) {
  stop("Columns 'id' and 'segment_id' are required but not found")
}

df[, group := paste(id, segment_id, sep = "_")]

# caret expects factor levels with valid names for twoClassSummary
# and the second level will be treated as the "event" class
df[, occ := factor(ifelse(as.integer(occ) == 1, "pres", "abs"),
                   levels = c("abs", "pres"))]

cat("Presences:", sum(df$occ == "pres", na.rm = TRUE), "\n")
cat("Absences:", sum(df$occ == "abs", na.rm = TRUE), "\n")
cat("Groups:", uniqueN(df$group), "\n")

# ---------------------------------------------------------
# SAFE PREDICTOR FILTER
# ---------------------------------------------------------

meta_patterns <- c(
  "^id$",
  "^segment_id$",
  "^group$",
  "^date$",
  "^datetime$",
  "^dateTime$",
  "^day$",
  "^occ$",
  "^cell$",
  "^lon$",
  "^lat$",
  "^simid$",
  "^date_day$"
)

meta_regex <- paste(meta_patterns, collapse = "|")

pred_cols <- names(df)[
  sapply(df, is.numeric) &
    !grepl(meta_regex, names(df), ignore.case = TRUE)
]

drop_vars <- c(
  "mlotst",
  "zos"
)

pred_cols <- setdiff(pred_cols, drop_vars)

cat("\n====================================\n")
cat("PREDICTORS USED\n")
cat("====================================\n")
print(pred_cols)

if (length(pred_cols) == 0) {
  stop("No predictors left after filtering")
}

df <- df[complete.cases(df[, c(pred_cols, "occ"), with = FALSE])]

cat("\nRows after NA cleaning:", nrow(df), "\n")
cat("Presences after NA cleaning:", sum(df$occ == "pres", na.rm = TRUE), "\n")
cat("Absences after NA cleaning:", sum(df$occ == "abs", na.rm = TRUE), "\n")

groups <- unique(df$group)
p <- length(pred_cols)

if (length(groups) < 2) {
  stop("Not enough groups for grouped cross-validation")
}

# ---------------------------------------------------------
# GROUPED CROSS-VALIDATION INDEX FOR CARET
# ---------------------------------------------------------
# Each fold leaves one group out
# caret index = training row indices for each resample
# ---------------------------------------------------------

folds <- vector("list", length(groups))
names(folds) <- paste0("Fold", seq_along(groups))

for (i in seq_along(groups)) {
  g <- groups[i]
  folds[[i]] <- which(df$group != g)
}

# ---------------------------------------------------------
# CARET TRAIN CONTROL
# ---------------------------------------------------------

tc <- trainControl(
  method = "LOOCV",
  index = folds,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  returnResamp = "final",
  verboseIter = TRUE
)

# ---------------------------------------------------------
# TUNING GRID
# ---------------------------------------------------------
# ranger parameters:
# - mtry
# - splitrule
# - min.node.size
# ---------------------------------------------------------

mtry_values <- unique(round(seq(2, p, length.out = min(5, max(1, p - 1)))))
mtry_values <- mtry_values[mtry_values <= p]

rfGrid <- expand.grid(
  mtry = mtry_values,
  splitrule = "gini",
  min.node.size = c(1, 3, 5)
)

cat("\n====================================\n")
cat("GRID SEARCH SETUP\n")
cat("====================================\n")
cat("Number of combinations:", nrow(rfGrid), "\n")
print(rfGrid)

# ---------------------------------------------------------
# PREPARE DATA FOR CARET
# ---------------------------------------------------------

x <- as.data.frame(df[, ..pred_cols])
y <- df$occ

# ---------------------------------------------------------
# GRID SEARCH WITH CARET
# ---------------------------------------------------------

cat("\n====================================\n")
cat("STARTING CARET GRID SEARCH\n")
cat("====================================\n")

rf_tuned <- train(
  x = x,
  y = y,
  method = "ranger",
  metric = "ROC",
  trControl = tc,
  tuneGrid = rfGrid,
  num.trees = 500,
  importance = "impurity"
)



# ---------------------------------------------------------
# SAVE TUNING SUMMARY
# ---------------------------------------------------------

summary_df <- as.data.table(rf_tuned$results)
fwrite(summary_df, file.path(tuning_dir, "hyperparameter_summary.csv"))

best <- as.data.table(rf_tuned$bestTune)
best[, ROC := rf_tuned$results$ROC[
  rf_tuned$results$mtry == best$mtry &
    rf_tuned$results$splitrule == best$splitrule &
    rf_tuned$results$min.node.size == best$min.node.size
]]

fwrite(best, file.path(tuning_dir, "winner_hyperparameters.csv"))

cat("\n====================================\n")
cat("WINNER HYPERPARAMETERS\n")
cat("====================================\n")
print(best)

# ---------------------------------------------------------
# SAVE RESAMPLING DETAILS
# ---------------------------------------------------------

if (!is.null(rf_tuned$resample)) {
  write.csv(
    rf_tuned$resample,
    file.path(tuning_dir, "resample_results.csv"),
    row.names = FALSE
  )
}

if (!is.null(rf_tuned$pred)) {
  write.csv(
    rf_tuned$pred,
    file.path(tuning_dir, "fold_predictions.csv"),
    row.names = FALSE
  )
}

# ---------------------------------------------------------
# HEATMAP
# ---------------------------------------------------------

p_heat <- ggplot(summary_df, aes(mtry, min.node.size, fill = ROC)) +
  geom_tile() +
  facet_wrap(~splitrule) +
  scale_fill_viridis_c() +
  theme_bw() +
  labs(
    title = "Balaenoptera artificialis RF AUC heatmap",
    x = "mtry",
    y = "min.node.size"
  )

ggsave(
  file.path(tuning_dir, "AUC_heatmap.png"),
  p_heat,
  width = 10,
  height = 7,
  dpi = 300
)

# ---------------------------------------------------------
# SCATTER
# ---------------------------------------------------------

p_scatter <- ggplot(summary_df, aes(mtry, ROC, color = factor(min.node.size))) +
  geom_point(size = 2) +
  facet_wrap(~splitrule) +
  theme_bw() +
  labs(
    title = "Balaenoptera artificialis RF performance",
    x = "mtry",
    y = "Mean AUC",
    color = "min.node.size"
  )

ggsave(
  file.path(tuning_dir, "AUC_scatter.png"),
  p_scatter,
  width = 10,
  height = 7,
  dpi = 300
)

# ---------------------------------------------------------
# TOP MODELS
# ---------------------------------------------------------

top <- summary_df %>%
  arrange(desc(ROC), desc(mtry)) %>%
  head(10)

write.csv(
  top,
  file.path(tuning_dir, "top_models.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------
# TRAIN FULL RF MODEL WITH WINNER HYPERPARAMETERS
# ---------------------------------------------------------

cat("\nTraining FULL RF model with winner hyperparameters...\n")

closeAllConnections()

rf_final <- ranger(
  dependent.variable.name = "occ",
  data = as.data.frame(df[, c("occ", pred_cols), with = FALSE]),
  num.trees = 500,
  mtry = best$mtry,
  splitrule = as.character(best$splitrule),
  min.node.size = best$min.node.size,
  probability = TRUE,
  importance = "impurity"
)

saveRDS(
  rf_final,
  file.path(full_dir, "rf_full_Balaenoptera_artificialis.rds")
)

fwrite(
  data.table(predictors = pred_cols),
  file.path(full_dir, "predictors_used.csv")
)

# ---------------------------------------------------------
# VARIABLE IMPORTANCE
# ---------------------------------------------------------

imp <- data.frame(
  variable = names(rf_final$variable.importance),
  MeanDecreaseGini = as.numeric(rf_final$variable.importance),
  stringsAsFactors = FALSE
)

fwrite(
  imp,
  file.path(full_dir, "importance.csv")
)

imp_plot <- imp %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(variable = factor(variable, levels = variable))

p_imp <- ggplot(
  imp_plot,
  aes(variable, MeanDecreaseGini, fill = MeanDecreaseGini)
) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis() +
  theme_bw() +
  labs(
    title = "Balaenoptera artificialis variable importance",
    x = "",
    y = "Mean decrease Gini"
  )

ggsave(
  file.path(full_dir, "importance_barplot.png"),
  p_imp,
  width = 8,
  height = 6,
  dpi = 300
)

cat("Importance plot saved\n")

# ---------------------------------------------------------
# PDP — TOP VARIABLES
# ---------------------------------------------------------
# pdp works more naturally with randomForest than ranger,
# so we fit a helper randomForest model using the winning mtry.
# This is only for PDP visualization.
# ---------------------------------------------------------

cat("\nCreating PDP grid...\n")

rf_pdp <- randomForest(
  x = as.data.frame(df[, ..pred_cols]),
  y = df$occ,
  ntree = 500,
  mtry = min(best$mtry, length(pred_cols)),
  importance = TRUE
)

top_vars <- imp_plot$variable[1:min(6, nrow(imp_plot))]
pdp_plots <- list()

for (v in top_vars) {
  
  cat("PDP:", v, "\n")
  
  pd <- partial(
    rf_pdp,
    pred.var = as.character(v),
    train = as.data.frame(df[, ..pred_cols]),
    prob = TRUE,
    which.class = "pres"
  )
  
  p <- autoplot(pd) +
    theme_bw() +
    labs(title = as.character(v))
  
  pdp_plots[[as.character(v)]] <- p
}

pdp_grid <- patchwork::wrap_plots(
  pdp_plots,
  ncol = 3
) +
  plot_annotation(
    title = "Balaenoptera artificialis — PDP top predictors"
  )

ggsave(
  file.path(full_dir, "pdp_grid.png"),
  pdp_grid,
  width = 12,
  height = 8,
  dpi = 300
)

print(pdp_grid)

cat("PDP grid saved\n")

# ---------------------------------------------------------
# QUICK PREDICTION CHECK
# ---------------------------------------------------------

stack_file <- stack_files[1]
cat("\nLoading stack for quick prediction:\n", basename(stack_file), "\n")

stk <- stack(stack_file)

missing_vars <- setdiff(pred_cols, names(stk))

if (length(missing_vars) > 0) {
  
  cat("\nMissing stack variables:\n")
  print(missing_vars)
  
} else {
  
  stk_sub <- subset(stk, pred_cols)
  
  pred <- raster::predict(
    stk_sub,
    rf_pdp,
    type = "prob",
    index = 2,
    progress = "text"
  )
  
  png(
    file.path(full_dir, "quick_prediction.png"),
    width = 1200,
    height = 900,
    res = 150
  )
  plot(pred, main = "Balaenoptera artificialis — quick prediction")
  dev.off()
}

cat("\nRF GRID SEARCH + FULL MODEL COMPLETE\n")