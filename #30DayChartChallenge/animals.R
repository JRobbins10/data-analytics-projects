# Import libraries.
library(tidyverse)
library(ggplot2)

# Import data
animals <- read.csv('animals.csv', header=T)

# Create radial chart.

# Sort data by 'Decibels' in descending order
animals <- animals[order(-animals$Decibels), ]

# Reorder factor levels based on sorted data
animals$Animals <- factor(animals$Animals, levels = animals$Animals)

# Create radial chart
radial <- ggplot(animals, aes(x = Animals, y = Decibels, fill = Decibels)) +
  geom_bar(stat = "identity", size = 0.8, color = "black") +
  coord_polar(theta = "x", start = -pi / length(animals$Animals)) +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.position = c(1, 0.5),
    legend.justification = c(0, 0.5),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "#e6e1e1", linewidth = 0.5),
    legend.title = element_text(size = 14, color = "#e6e1e1",face = "bold"),
    legend.text = element_text(size = 12, color = "#e6e1e1"),
    legend.key.size = unit(1.5, "cm")) + 
  scale_fill_gradient(low = "#f05d5d", high = "#f00a0a") +
  geom_text(aes(label = paste0(Decibels, " dB")), color = "black", size = 5, 
            position = position_stack(vjust = 0.5),
            fontface = "bold",
            angle = 0, hjust = 0.5, vjust = 0.5)
radial

# Save  plot.
ggsave("radial_chart.png", radial, bg = "transparent", 
       width = 14, height = 12, dpi = 300)
