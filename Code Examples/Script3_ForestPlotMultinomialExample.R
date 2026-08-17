# ==============================================================================
# Script 03: Multinomial Logistic Regression Forest Plot
# ==============================================================================

library(readxl)
library(tidyverse)
library(grid)
library(ggh4x)


# 1. Read and Prepare Data -----------------------------------------------------------------

Vege <- read_excel("data/Vege.xlsx")
View(Vege)  

data <- Vege

data_clean <- data |>
  filter(
    !is.na(FIT), !is.na(OR), !is.na(Labels),
    !is.na(CI_Lower), !is.na(CI_Upper)
  ) |>
  mutate(
    OR_num       = as.numeric(OR),
    CI_Lower_num = as.numeric(CI_Lower),
    CI_Upper_num = as.numeric(CI_Upper)
  ) |>
  filter(
    CI_Lower_num > 0, CI_Upper_num > 0,
    OR_num > 0, OR_num < 1e6,
    CI_Lower_num < CI_Upper_num,
    CI_Upper_num < 1000
  )

# p‑values and right‑hand text
data_clean <- data_clean |>
  mutate(
    p_numeric = suppressWarnings(as.numeric(p_value)),
    sig_level = case_when(
      p_numeric < 0.001 ~ "***",
      p_numeric < 0.01  ~ "**",
      p_numeric < 0.05  ~ "*",
      TRUE              ~ ""
    ),
    OR_fmt  = sprintf("%.2f", OR_num),
    LCL_fmt = sprintf("%.2f", CI_Lower_num),
    UCL_fmt = sprintf("%.2f", CI_Upper_num),
    p_fmt   = case_when(
      is.na(p_numeric)        ~ "",
      p_numeric < 0.001       ~ "<0.001",
      TRUE                    ~ sprintf("%.3f", p_numeric)
    ),
    p_with_stars = ifelse(sig_level == "", p_fmt,
                          paste0(p_fmt, " ", sig_level)),
    rhs_label = paste0(
      OR_fmt, "  [", LCL_fmt, "; ", UCL_fmt, "]  ", p_with_stars
    )
  )

# Facet labels, trajectory names, and ordering ----------------------------

trajectory_labels <- c(
  "1" = "1: Minimal Consumer",
  "2" = "2: Temporary Increaser",
  "3" = "3: Fluctuator",
  "4" = "4: Increaser",
  "5" = "5: High Maintainer"
)

data_clean <- data_clean |>
  mutate(
    trajectory_group = factor(
      trajectory_labels[as.character(FIT)],
      levels = trajectory_labels
    ),
    label_ordered = factor(
      Labels,
      levels = rev(c(
        "age",
        "sex (men)",
        sort(setdiff(
          unique(Labels),
          c("age", "sex (men)")
        ))
      ))
    )
  ) |>
  arrange(FIT, label_ordered)


# Trajectory colours ------------------------------------------------------

trajectory_colors <- c(
  "1" = "#1b9e77",  # teal
  "2" = "#d95f02",  # burnt orange
  "3" = "#7570b3",  # muted purple
  "4" = "#FF0000",  # red
  "5" = "#66a61e"   # olive green
)

data_clean$FIT_f <- factor(
  data_clean$FIT,
  levels = names(trajectory_colors)
)
# ----------------------------
# Plot with black CI bars
# ----------------------------

p <- ggplot(
  data_clean,
  aes(x = OR_num, y = label_ordered, color = FIT_f)
) +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "red", linewidth = 0.7, alpha = 0.7) +
  # black CI bars
  geom_errorbarh(
    aes(xmin = pmax(CI_Lower_num, 0.1),
        xmax = pmin(CI_Upper_num, 100)),
    height = 0.15, linewidth = 0.7, color = "black"
  ) +
  geom_point(size = 1.8, alpha = 0.9) +
  geom_text(
    aes(x = 110, label = rhs_label),
    color = "black", hjust = 0,
    size = 2.6
  ) +
  scale_color_manual(values = trajectory_colors, guide = "none") +
  facet_grid2(
    rows = vars(trajectory_group),
    scales = "free_y", space = "free",
    strip = strip_themed(
      background_y = elem_list_rect(
        fill  = unname(trajectory_colors[as.character(data_clean$FIT_f) |> unique()]),
        color = "grey70"
      ),
      text_y = elem_list_text(
        colour = "white",
        face   = "bold",
        size   = 7
      )
    )
  ) +
  scale_x_log10(
    breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100),
    labels = c("0.1", "0.5", "1", "2", "5", "10", "20", "50", "100"),
    limits = c(0.1, 150),
    name   = "Odds Ratio (OR) and 95% CI | log scale",
    expand = expansion(mult = c(0.02, 0.25))
  ) +
  labs(
    title   = "Determinants of Behavior Trajectories vs. Vegetables Intake Trajectories",
    y       = "Determinants of Behavior Trajectories with Age and Sex",
    caption = "*** p<0.001, ** p<0.01, * p<0.05 | Extreme values filtered"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title   = element_text(size = 11, face = "bold",
                                hjust = 0.5, margin = margin(b = 2)),
    axis.text.y  = element_text(size = 7),
    axis.text.x  = element_text(size = 7),
    axis.title.x = element_text(size = 9, face = "bold"),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(t = 0, r = 5, b = 8, l = 0),  # b increases distance from axis text)
    ),
    panel.grid.minor.x = element_line(linewidth = 0.2),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey90"),
    panel.spacing = unit(0.35, "lines"),
    legend.position = "none",
    plot.margin = margin(t = 28, r = 12, b = 12, l = 10)
  )

# Add this to your plot object definition (so text outside panel is drawn)
p <- p + coord_cartesian(clip = "off")

# Adjust title margin so there is space between title and the header line
p <- p +
  theme(
    plot.title = element_text(
      size = 11, face = "bold",
      hjust = 0.5,
      margin = margin(b = 14)  # more space under title
    ),
    plot.margin = margin(t = 25, r = 12, b = 12, l = 10)
  )

#-----------------------------
# Draw + OR / CI / p header
#-----------------------------

pg <- ggplotGrob(p)
grid.newpage()
grid.draw(pg)

# Header aligned above numeric column (tweak x a little if needed)
grid.text(
  label = "OR     95% CI       p",
  x = unit(0.77, "npc"),   # horizontal position over number column
  y = unit(0.91, "npc"),  # just below the title but above numbers
  just = c("left", "top"),
  gp = gpar(fontsize = 7, fontface = "bold")
)