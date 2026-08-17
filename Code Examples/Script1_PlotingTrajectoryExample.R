# ==============================================================================
# Script 01: Longitudinal Trajectory Plot
# ==============================================================================

library(tidyverse)
library(readxl)
library(scales)


# 1. Read data -----------------------------------------------------------------
library(readr)
op <- read_csv("data/Vege_OP.csv")

# 2. Setup constants -----------------------------------------------------------
time_labels <- c("week0", "week25", "week47", "week73")
x_label     <- "Time Points"
y_label     <- "Vegetable Intake (g/day)"
groups      <- 5

colours_manual <- c(
  "#1b9e77",  # 1 teal
  "#d95f02",  # 2 burnt orange
  "#7570b3",  # 3 muted purple
  "#FF0000",  # 4 red
  "#66a61e"   # 5 olive green
)

pct_vec <- c(46.74319, 17.85798, 22.4118, 8.840521, 4.146511)
group_names <- paste0("Trajectory ", 1:5, ": ", sprintf("%.1f", pct_vec), "%")

trajectory_descriptions <- c(
  "Minimal Consumer",
  "Temporary Increaser",
  "Fluctuator",
  "Increaser",
  "High Maintainer"
)

# 3. Build long dataset --------------------------------------------------------
make_group_df <- function(g) {
  tibble(
    t     = as.integer(op$T),
    group = g,
    PRED  = op[[paste0("PRED", g)]],
    L95M  = op[[paste0("L95M", g)]],
    U95M  = op[[paste0("U95M", g)]]
  )
}

plot_data <- map_dfr(1:5, make_group_df)

# 4. Generate label data -------------------------------------------------------
label_data <- plot_data %>%
  filter(t == 3) %>%
  mutate(
    x_lab = factor(t, levels = 0:3, labels = time_labels),
    label = paste0(group, ": ", trajectory_descriptions[group]),
    y_lab = case_when(
      group == 2 ~ PRED - 25,
      group == 3 ~ PRED + 12,
      TRUE       ~ PRED
    )
  )

# 5. Build ggplot --------------------------------------------------------------
v_plot <- ggplot(
  data = plot_data,
  aes(
    x = factor(t, levels = 0:3, labels = time_labels),
    group = as.factor(group),
    colour = as.factor(group),
    fill = as.factor(group)
  )
) +
  geom_ribbon(aes(ymin = L95M, ymax = U95M), alpha = 0.2, colour = NA) +
  geom_hline(yintercept = 200, linetype = "dotted", colour = "grey50", linewidth = 0.8) +
  annotate("text", x = 4, y = 210, label = "Recommended intake: 200 g/day",
           hjust = 1, vjust = -0.2, size = 3.2, colour = "grey30") +
  geom_line(aes(y = PRED), linewidth = 1.5) +
  geom_point(aes(y = PRED), shape = 21, fill = "white", size = 2) +
  geom_text(
    data = label_data,
    aes(x = x_lab, y = y_lab, label = label, colour = as.factor(group)),
    vjust = -1, size = 4, fontface = "bold", show.legend = FALSE
  ) +
  labs(
    x = x_label,
    y = y_label,
    colour = NULL,
    fill = NULL,
    caption = "Trajectories and percent of participants within each trajectory"
  ) +
  scale_colour_manual(values = colours_manual, breaks = as.character(1:groups), labels = group_names) +
  scale_fill_manual(values = colours_manual, breaks = as.character(1:groups), labels = group_names) +
  guides(
    shape = "none", linetype = "none", alpha = "none", fill = "none",
    colour = guide_legend(override.aes = list(alpha = 1))
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = "#DDDDDD"),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, 0.8, 0.1, 0.1), "cm"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    legend.justification = "center",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, size = 10)
  )
print(v_plot)

