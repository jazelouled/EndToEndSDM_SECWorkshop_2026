# ============================================================
# SCRIPT NAME:
# 08_exploratoryDataAnalysis_Balaenoptera_artificialis.R
#
# PURPOSE:
# Exploratory data analysis (EDA) of the extracted
# presence-absence dataset for Balaenoptera artificialis.
#
# INPUT:
# - Balaenoptera_artificialis_PresAbs_with_env.csv
#
# OUTPUT:
# - correlation matrix
# - correlation heatmap
# - density plots (presences vs absences)
# - summary statistics
# - collinearity dendrogram
#
# WHY THIS STEP IS USEFUL:
# Before fitting habitat models, we need to understand:
# - how variables are distributed
# - how presences and absences differ
# - which predictors are strongly correlated
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(corrplot)
  library(here)
})

set.seed(123)

message("Starting exploratory data analysis for Balaenoptera artificialis...")


# ============================================================
# 2. DEFINE INPUT AND OUTPUT PATHS
# ============================================================

root <- here::here()

in_file <- file.path(
  root,
  "00inputOutput", "00input", "01processedData", "02habitatModel",
  "Balaenoptera_artificialis_PresAbs_with_env.csv"
)

out_dir <- file.path(
  root,
  "00inputOutput", "01output", "00ExploratoryDataAnalysis"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(in_file)) {
  stop("Input file not found: ", in_file)
}


# ============================================================
# 3. DEFINE VARIABLES TO REMOVE
# ============================================================
# These are non-environmental or metadata variables that should
# not be treated as predictors in the EDA.
# ============================================================

drop_vars <- c(
  "id",
  "segment_id",
  "group",
  "simid",
  "date",
  "datetime",
  "dateTime",
  "day",
  "date_day",
  "cell",
  "lon",
  "lat",
  "occ_label"
)


# ============================================================
# 4. LOAD DATA
# ============================================================

message("Reading extracted presence-absence dataset...")

df <- fread(in_file)

message("Rows: ", nrow(df))
message("Columns: ", ncol(df))

if (!("occ" %in% names(df))) {
  stop("The input dataset must contain a column called 'occ'.")
}


# ============================================================
# 5. REMOVE NON-ENVIRONMENTAL VARIABLES
# ============================================================
# We keep:
# - occ
# - environmental predictors
# ============================================================

keep_cols <- setdiff(names(df), drop_vars)
df <- df[, ..keep_cols]

message("Columns kept after removing metadata: ", ncol(df))


# ============================================================
# 6. FORMAT RESPONSE VARIABLE
# ============================================================
# Convert occ:
# - 0 -> abs
# - 1 -> pres
# If already character/factor, standardize it.
# ============================================================

if (is.numeric(df$occ) || is.integer(df$occ)) {
  df$occ <- factor(df$occ, levels = c(0, 1), labels = c("abs", "pres"))
} else {
  df$occ <- as.character(df$occ)
  df$occ[df$occ %in% c("0", "abs", "absence")] <- "abs"
  df$occ[df$occ %in% c("1", "pres", "presence")] <- "pres"
  df$occ <- factor(df$occ, levels = c("abs", "pres"))
}


# ============================================================
# 7. IDENTIFY NUMERIC PREDICTORS
# ============================================================
# These are the variables we will use for:
# - correlations
# - summary statistics
# - distributions
# ============================================================

num_cols <- names(df)[sapply(df, is.numeric)]
num_cols <- setdiff(num_cols, "occ")

if (length(num_cols) == 0) {
  stop("No numeric environmental predictors found after removing metadata.")
}

message("Number of numeric predictors: ", length(num_cols))
message("Predictors:")
print(num_cols)

df_env <- df[, c("occ", num_cols), with = FALSE]


# ============================================================
# 8. CORRELATION MATRIX
# ============================================================
# We compute Pearson correlations among numeric predictors.
# ============================================================

message("Building correlation matrix...")

num_df <- na.omit(df_env[, ..num_cols])

cor_mat <- cor(num_df, use = "pairwise.complete.obs")

write.csv(
  cor_mat,
  file.path(out_dir, "correlation_matrix.csv")
)

png(
  file.path(out_dir, "correlation_heatmap.png"),
  width = 1200,
  height = 1200,
  res = 150
)

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.col = "black",
  title = "Correlation matrix — Balaenoptera artificialis",
  mar = c(0, 0, 2, 0)
)

dev.off()

message("Saved correlation matrix and heatmap.")


# ============================================================
# 9. DENSITY PLOTS: PRESENCES VS ABSENCES
# ============================================================
# We reshape the data into long format and compare the
# environmental distributions of presences and absences.
# ============================================================

message("Building density plots...")

long_df <- df_env %>%
  pivot_longer(
    cols = -occ,
    names_to = "variable",
    values_to = "value"
  )

p_density <- ggplot(
  long_df,
  aes(x = value, fill = occ)
) +
  geom_density(alpha = 0.5) +
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 4
  ) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw() +
  labs(
    title = "Environmental distributions — Balaenoptera artificialis",
    subtitle = "Presences vs absences",
    x = NULL,
    y = "Density",
    fill = NULL
  )

ggsave(
  file.path(out_dir, "density_pres_abs.png"),
  p_density,
  width = 16,
  height = 12,
  dpi = 300
)

message("Saved density plots.")


# ============================================================
# 10. SUMMARY STATISTICS
# ============================================================
# We compute mean and standard deviation of each numeric
# environmental variable for presences and absences.
# ============================================================

message("Computing summary statistics...")

summary_df <- df_env %>%
  group_by(occ) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      )
    )
  )

write.csv(
  summary_df,
  file.path(out_dir, "summary_stats.csv"),
  row.names = FALSE
)

message("Saved summary statistics.")


# ============================================================
# 11. DENDROGRAM OF COLLINEARITY
# ============================================================
# We use:
# 1 - absolute Pearson correlation
# as a distance measure between variables.
# ============================================================

message("Building collinearity dendrogram...")

cor_dist <- as.dist(1 - abs(cor_mat))
hc_vars  <- hclust(cor_dist, method = "average")

ymax <- max(hc_vars$height)

png(
  file.path(out_dir, "dendrogram_pearson.png"),
  width = 1200,
  height = 1200,
  res = 150
)

plot(
  hc_vars,
  main = "Collinearity dendrogram — Balaenoptera artificialis",
  xlab = "",
  ylab = "",
  sub = "",
  cex = 0.7,
  hang = 0.1,
  ylim = c(0, ymax),
  yaxt = "n"
)

axis_vals <- seq(0, 1, by = 0.1)
axis_pos  <- (1 - axis_vals) * ymax

axis(
  2,
  at = axis_pos,
  labels = axis_vals,
  las = 1
)

mtext(
  "Absolute Pearson correlation (|r|)",
  side = 2,
  line = 3
)

abline(
  h = (1 - 0.7) * ymax,
  col = "red",
  lwd = 2,
  lty = 2
)

dev.off()

message("Saved dendrogram.")


# ============================================================
# 12. FINAL MESSAGE
# ============================================================

message("====================================")
message("EDA completed successfully.")
message("Input file: ", in_file)
message("Outputs saved in: ", out_dir)
message("Generated:")
message(" - correlation_matrix.csv")
message(" - correlation_heatmap.png")
message(" - density_pres_abs.png")
message(" - summary_stats.csv")
message(" - dendrogram_pearson.png")
message("====================================")