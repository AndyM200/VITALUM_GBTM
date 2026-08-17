# ==============================================================================
# Script 02: Cramer's V Association Matrix & Heatmap
# ==============================================================================

library(readxl)
library(vcd)
library(reshape2)
library(ggplot2)


# 1. Read Data -----------------------------------------------------------------

df <- read_excel("data/CramerV_DataSet.xlsx")
View(df)
df_factor <- df
if (tolower(names(df_factor)[1]) %in% c("id", "rowid")) {
  start_col <- 2
} else {
  start_col <- 1
}
for (col in names(df_factor)[start_col:length(names(df_factor))]) {
  df_factor[[col]] <- as.factor(df_factor[[col]])
}

# Convert columns to factors
start_col <- if (tolower(names(df_factor)[1]) %in% c("id", "rowid")) 2 else 1
for (col in names(df_factor)[start_col:length(names(df_factor))]) {
  df_factor[[col]] <- as.factor(df_factor[[col]])
}

# Remove columns with only one unique value
keep_vars <- sapply(df_factor, function(x) length(unique(x)) > 1)
df_factor <- df_factor[, keep_vars]
categoricals <- names(df_factor)

# Prepare ordered variable lists (dataset specific logic)
categoricals <- names(df_factor)[start_col:length(names(df_factor))]
v_vars <- categoricals[grepl("V$", categoricals)]
f_vars <- categoricals[grepl("F$", categoricals)]
veg_var <- categoricals[tolower(categoricals) %in% c("vegetable intake")]
fruit_var <- categoricals[tolower(categoricals) %in% c("fruit intake")]
ordered_vars <- c(
  v_vars,
  veg_var,
  f_vars,
  fruit_var,
  setdiff(categoricals, c(v_vars, f_vars, veg_var, fruit_var))
)

cat("Ordered variables used for heatmap:\n")
print(ordered_vars)

# All combinations
all_combs <- expand.grid(ordered_vars, ordered_vars, stringsAsFactors = FALSE)
names(all_combs) <- c("Var1", "Var2")

# Robust function to compute Cramer's V and p-value
cramers_v_fun <- function(x, y, data) {
  tryCatch({
    complete_data <- data[!is.na(data[[x]]) & !is.na(data[[y]]), ]
    if (length(unique(complete_data[[x]])) < 2 | length(unique(complete_data[[y]])) < 2) return(list(CramersV=NA, p.value=NA))
    tbl <- table(complete_data[[x]], complete_data[[y]])
    if (min(dim(tbl)) < 2) return(list(CramersV=NA, p.value=NA))
    v <- assocstats(tbl)$cramer
    pval <- suppressWarnings(chisq.test(tbl)$p.value)
    list(CramersV=v, p.value=pval)
  }, error = function(e) list(CramersV=NA, p.value=NA))
}

tmp <- mapply(cramers_v_fun, all_combs$Var1, all_combs$Var2, MoreArgs=list(data=df_factor), SIMPLIFY=FALSE)
all_combs$CramersV <- sapply(tmp, function(x) x$CramersV)
all_combs$p.value <- sapply(tmp, function(x) x$p.value)
all_combs$CramersV[all_combs$Var1 == all_combs$Var2] <- 1
all_combs$p.value[all_combs$Var1 == all_combs$Var2] <- NA

# Result matrix for Cramer's V
result_matrix <- acast(all_combs, Var1 ~ Var2, value.var = "CramersV")
result_matrix <- result_matrix[ordered_vars, ordered_vars]

# Dataframe for heatmap (bottom right triangle)
heat_df_bottom_right <- melt(result_matrix)
names(heat_df_bottom_right) <- c("Var1", "Var2", "value")
var1_num <- as.numeric(factor(heat_df_bottom_right$Var1, levels=ordered_vars))
var2_num <- as.numeric(factor(heat_df_bottom_right$Var2, levels=ordered_vars))
heat_df_bottom_right <- heat_df_bottom_right[var1_num >= var2_num, ]
heat_df_bottom_right$Var1 <- factor(heat_df_bottom_right$Var1, levels = rev(ordered_vars))
heat_df_bottom_right$Var2 <- factor(heat_df_bottom_right$Var2, levels = rev(ordered_vars))

# Merge p-values for annotation
pd <- all_combs[, c("Var1", "Var2", "p.value")]
heat_df_bottom_right <- merge(heat_df_bottom_right, pd, by = c("Var1", "Var2"))

# Function to assign significance stars
get_significance_stars <- function(p) {
  if (is.na(p)) return("")
  #else if (p < 0.001) return("***") #optional because it visual clutters the heatplot
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Create label with significance stars for heatmap
heat_df_bottom_right$label <- ifelse(is.na(heat_df_bottom_right$value), "NA",
                                     ifelse(heat_df_bottom_right$Var1 == heat_df_bottom_right$Var2, "1",
                                            sprintf("%.2f%s", heat_df_bottom_right$value,
                                                    sapply(heat_df_bottom_right$p.value, get_significance_stars))))

# Heatmap plot with legend for significance asterisk
p2 <- ggplot(heat_df_bottom_right, aes(Var2, Var1)) +
  geom_tile(aes(fill = value), color = "white", linewidth = 0.5) +
  scale_fill_gradientn(colors = c("white", "yellow", "orange", "red", "darkred"),
                       values = c(0, 0.2, 0.4, 0.6, 1),
                       na.value = "lightgrey",
                       name = "Cramer's V",
                       limits = c(0, 1)) +
  geom_text(aes(label = label), color = "black", size = 3) +
  labs(title = "Cramer's V Heatplot", x = "Variable", y = "Variable",
       caption = "* p < 0.05, ** p < 0.01, *** p < 0.001 (statistically significant)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 8),
        panel.grid = element_blank()) +
  coord_fixed()
print(p2)

# Create and print table of Cramer's V with p-values and stars
cramers_table <- all_combs
cramers_table$p.signif <- sapply(cramers_table$p.value, get_significance_stars)
cramers_table <- cramers_table[, c("Var1", "Var2", "CramersV", "p.value", "p.signif")]

# Optionally round values for display
cramers_table$CramersV <- round(cramers_table$CramersV, 3)
cramers_table$p.value <- signif(cramers_table$p.value, 3)

# Print the table
print(cramers_table)
